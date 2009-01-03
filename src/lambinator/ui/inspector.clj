(ns lambinator.ui.inspector
  (:use lambinator.util lambinator.ui)
  (:import (javax.swing JFrame JMenu JMenuBar JMenuItem UIManager JDialog JLabel
			JScrollPane ScrollPaneConstants JTextField JTextPane
			SwingUtilities JButton JPanel
			JList JComboBox JSlider)
	   (javax.swing.event ListSelectionListener ChangeListener HyperlinkListener HyperlinkEvent$EventType)
	   (java.awt.event ActionListener)
	   (java.awt BorderLayout GridBagLayout GridBagConstraints Dimension FlowLayout)
	   (javax.swing.text.html HTMLEditorKit)))

;name is the name to display
;editor is the component to use to edit the items
;updater sets the component to the current value specified by getter
;getter gets current value from the model
;setter is used to set the item 
(defstruct inspector_item :name :editor :updater)

(defn list_selection_update [selection_index indexed_str_options setter]
  (let [item (first (filter (fn [[index str_opt opt]]
			      (== index selection_index))
			    indexed_str_options))]
    (when item
      (let [[index str_opt option] item]
	(setter option)))))

(defn create_list_inspector_item 
  ([name options getter setter stringifier]
     (let [stringifier (if stringifier
			 stringifier
			 (memfn toString))
	   indexed_str_options (map 
				(fn [index option] 
				  [index (stringifier option) option])
				(iterate inc 0) options )
	   str_values (map (fn [[_ str _]] str) indexed_str_options)
	   str_array (into-array String str_values)
	   editor (JComboBox. str_array)
	   events_masked (ref false)
	   selection_listener (proxy [Object ActionListener] []
				(actionPerformed [_]
						 (when (not @events_masked)
						   (list_selection_update 
						    (. editor getSelectedIndex) indexed_str_options setter))))
	   updater (fn []
		     (let [current (getter)
			   item (first (filter 
					(fn [[index str_opt option]] (= option current))
					indexed_str_options))]
		       (when item
			 (let [[index str_opt option] item]
			   (dosync (ref-set events_masked true))
			   (. editor setSelectedIndex index)
			   (dosync (ref-set events_masked false))
			   nil))))]
       (. editor addActionListener selection_listener)
       (updater)
       (struct-map inspector_item :name name :editor editor :updater updater)))
  ([name options getter setter]
     (create_list_inspector_item name options getter setter nil)))

(defn count_zeros [str]
	(let [len (.length str)
	      point (.indexOf str ".")
	      chars_after (- len point 1)
	      chars_before (- len chars_after 1)]
	  (if (>= chars_before 0)
	    [chars_before chars_after]
	    [chars_after 0])))

(defn try_parse_float[str]
  (if str
    (try
     [true (Float/parseFloat str)]
     (catch NumberFormatException exc
       [false (float 0.0)]))
    [false 0.0]))

(defn create_min_size_text_field[preferred_size_ref]
  (proxy [JTextField] []
    (getPreferredSize [] 
		      (if @preferred_size_ref
			@preferred_size_ref
			(proxy-super getPreferredSize)))))

;format_str is in the form of:
;00.00 where the zeros indicate how many digits
;to account for
(defn create_float_slider_inspector_item [name min_val max_val getter setter format_str]
  (let [slider (JSlider. )
	pref_sized_ref (ref nil)
	input_box (create_min_size_text_field pref_sized_ref)
	panel (JPanel.)
	event_mask (ref false)
	[zeros_before zeros_after] (count_zeros format_str)
	zeros_before (max zeros_before 1) ;initial decimal in printf has to be 1 or more
	printf_str (with-out-str (printf "%%%d.%df" zeros_before zeros_after))
	min_val (min max_val min_val)
	max_val (max max_val min_val)
	range (- max_val min_val)
	set_text_val (fn [val]
		       (let [text_val (with-out-str (printf printf_str (float val)))]
			 (. input_box setText text_val)))
	set_slider_val (fn [val]
			 (let [slider_val (* (/ (- val min_val) range) 100)]
			   (. slider setValue slider_val)))
	updater (fn []
		  (let [cur_val (getter)]
		    (dosync (ref-set event_mask true))
		    (set_text_val cur_val)
		    (set_slider_val cur_val)
		    (dosync (ref-set event_mask false))))
	slider_listener (proxy [Object ChangeListener] []
			    (stateChanged [_]
					  (when (not @event_mask)
					    (let [int_val (. slider getValue)
						  rel_val (+ (* range
								(/ int_val 100))
							     min_val)]
					      (setter rel_val)
					      (updater)))))
	text_listener (proxy [Object ActionListener] []
			(actionPerformed [event]
					 (when (not @event_mask)
					   (let [[parsed fval] (try_parse_float (. input_box getText))]
					     (when parsed
					       (setter fval)
					      (updater))))))
	layout (GridBagLayout. )
	constraints (GridBagConstraints.)]
    (. input_box setText format_str)
    (. input_box setHorizontalAlignment JTextField/RIGHT)
    (dosync (ref-set pref_sized_ref (. input_box getPreferredSize)))
    (. input_box setMinimumSize (. input_box getPreferredSize))
    (. input_box addActionListener text_listener)
    (. slider addChangeListener slider_listener)
    (. panel setLayout layout)
    (add_with_constraints slider constraints panel
			  gridx 0
			  gridy 0
			  anchor GridBagConstraints/WEST
			  fill GridBagConstraints/HORIZONTAL
			  weightx 1.0)
    (add_with_constraints input_box constraints panel
			  gridx 1
			  gridy 0
			  anchor GridBagConstraints/EAST
			  fill GridBagConstraints/NONE
			  weightx 0.0)
    (updater)
    (struct-map inspector_item :name name :editor panel :updater updater)
    ))

;onclick takes no arguments.
(defn create_read_only_hyperlink_inspector_item [name getter onclick]
  (let [retval (JTextPane. )
	value (getter)
	updater (fn []
		  (. retval setText (stringify "<html><a href=\"" value "\">" value "</a></html>")))
	listener (proxy [Object HyperlinkListener][]
		    (hyperlinkUpdate
		     [event]
		     (when (= (. event getEventType) HyperlinkEvent$EventType/ACTIVATED)
		       (onclick))))]
    (doto retval
      (.setEditorKit (HTMLEditorKit. ))
      (.setBorder nil)
      (.setOpaque false)
      (.setEditable false)
      (.addHyperlinkListener listener))
    (updater)
    (struct-map inspector_item :name name :editor retval :updater updater)))
      

(defn setup_inspector_panel [inPanel inspector_item_seq]
  (. inPanel removeAll)
  (let [layout (GridBagLayout. )
	constraints (GridBagConstraints. )]
    (. inPanel setLayout (GridBagLayout.))
    (sets! constraints ipadx 1 ipady 1)
    (doseq [[index item] (map vector (iterate inc 0) inspector_item_seq)]
      (add_with_constraints (JLabel. (item :name)) constraints inPanel
			  gridx 0
			  gridy index
			  ipadx 0
			  anchor GridBagConstraints/EAST
			  fill GridBagConstraints/NONE
			  weightx 0.0)
      (add_with_constraints (item :editor) constraints inPanel
			  gridx 1
			  gridy index
			  ipadx 0
			  anchor GridBagConstraints/WEST
			  fill GridBagConstraints/HORIZONTAL
			  weightx 1.0))
    (. inPanel setPreferredSize  (. inPanel getMinimumSize))))
      