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
(defstruct inspector-item :name :editor :updater)

(defn list-selection-update [selection-index indexed-str-options setter]
  (let [item (first (filter (fn [[index str-opt opt]]
			      (== index selection-index))
			    indexed-str-options))]
    (when item
      (let [[index str-opt option] item]
	(setter option)))))

(defn uii-create-list-inspector-item 
  "Create a combo-box inspector item.  This is meant for a static list of
objects and does not at the moment have the capability to add items dynamically.
name - name of the object
options - full list of possible items
getter - returns the current value from the model
setter - sets the new value on the model
stringifier - function used to create display strings out of the items in the options sequence"
  ([name options getter setter stringifier]
     (let [stringifier (if stringifier
			 stringifier
			 (memfn toString))
	   indexed-str-options (map 
				(fn [index option] 
				  [index (stringifier option) option])
				(iterate inc 0) options )
	   str-values (map (fn [[- str -]] str) indexed-str-options)
	   str-array (into-array String str-values)
	   editor (JComboBox. str-array)
	   events-masked (ref false)
	   selection-listener (proxy [Object ActionListener] []
				(actionPerformed [-]
						 (when (not @events-masked)
						   (list-selection-update 
						    (. editor getSelectedIndex) indexed-str-options setter))))
	   updater (fn []
		     (let [current (getter)
			   item (first (filter 
					(fn [[index str-opt option]] (= option current))
					indexed-str-options))]
		       (when item
			 (let [[index str-opt option] item]
			   (dosync (ref-set events-masked true))
			   (. editor setSelectedIndex index)
			   (dosync (ref-set events-masked false))
			   nil))))]
       (. editor addActionListener selection-listener)
       (updater)
       (struct-map inspector-item :name name :editor editor :updater updater)))
  ([name options getter setter]
     (uii-create-list-inspector-item name options getter setter nil)))

(defn- count-zeros [str]
	(let [len (.length str)
	      point (.indexOf str ".")
	      chars-after (- len point 1)
	      chars-before (- len chars-after 1)]
	  (if (>= chars-before 0)
	    [chars-before chars-after]
	    [chars-after 0])))

(defn- try-parse-float[str]
  (if str
    (try
     [true (Float/parseFloat str)]
     (catch NumberFormatException exc
       [false (float 0.0)]))
    [false 0.0]))

(defn- create-min-size-text-field[preferred-size-ref]
  (proxy [JTextField] []
    (getPreferredSize [] 
		      (if @preferred-size-ref
			@preferred-size-ref
			(proxy-super getPreferredSize)))))

;format-str is in the form of:
;00.00 where the zeros indicate how many digits
;to account for
(defn uii-create-float-slider-inspector-item 
  "Create a float slider inspector item.  This is a slider followed
by a text box that enables the user to set the values both exactly
and by using the slider.
name - name of the item
min-val - minimum value of the data
max-val - maximum value of the data
getter - get the current value from the model.
setter - set the current value on the model.
format-str - string in the form of 00.00 where the number of zeros
  used indicates the number of decimal places you would like.  This string
  is used for measuring the text box and setting its final size as well."
  [name min-val max-val getter setter format-str]
  (let [slider (JSlider. )
	pref-sized-ref (ref nil)
	input-box (create-min-size-text-field pref-sized-ref)
	panel (JPanel.)
	event-mask (ref false)
	[zeros-before zeros-after] (count-zeros format-str)
	zeros-before (max zeros-before 1) ;initial decimal in printf has to be 1 or more
	printf-str (with-out-str (printf "%%%d.%df" zeros-before zeros-after))
	min-val (min max-val min-val)
	max-val (max max-val min-val)
	range (- max-val min-val)
	set-text-val (fn [val]
		       (let [text-val (with-out-str (printf printf-str (float val)))]
			 (. input-box setText text-val)))
	set-slider-val (fn [val]
			 (let [slider-val (* (/ (- val min-val) range) 100)]
			   (. slider setValue slider-val)))
	updater (fn []
		  (let [cur-val (getter)]
		    (dosync (ref-set event-mask true))
		    (set-text-val cur-val)
		    (set-slider-val cur-val)
		    (dosync (ref-set event-mask false))))
	slider-listener (proxy [Object ChangeListener] []
			    (stateChanged [-]
					  (when (not @event-mask)
					    (let [int-val (. slider getValue)
						  rel-val (+ (* range
								(/ int-val 100))
							     min-val)]
					      (setter rel-val)
					      (updater)))))
	text-listener (proxy [Object ActionListener] []
			(actionPerformed [event]
					 (when (not @event-mask)
					   (let [[parsed fval] (try-parse-float (. input-box getText))]
					     (when parsed
					       (setter fval)
					      (updater))))))
	layout (GridBagLayout. )
	constraints (GridBagConstraints.)]
    (. input-box setText format-str)
    (. input-box setHorizontalAlignment JTextField/RIGHT)
    (dosync (ref-set pref-sized-ref (. input-box getPreferredSize)))
    (. input-box setMinimumSize (. input-box getPreferredSize))
    (. input-box addActionListener text-listener)
    (. slider addChangeListener slider-listener)
    (. panel setLayout layout)
    (util-add-with-constraints slider constraints panel
			  gridx 0
			  gridy 0
			  anchor GridBagConstraints/WEST
			  fill GridBagConstraints/HORIZONTAL
			  weightx 1.0)
    (util-add-with-constraints input-box constraints panel
			  gridx 1
			  gridy 0
			  anchor GridBagConstraints/EAST
			  fill GridBagConstraints/NONE
			  weightx 0.0)
    (updater)
    (struct-map inspector-item :name name :editor panel :updater updater)
    ))

;onclick takes no arguments.
(defn uii-create-read-only-hyperlink-inspector-item 
  "Create a hyperlinked inspector item along with an action to perform on click.
name - name of the item.
getter - getter for the current value.
onclick - operation to perform.  Takes no arguments, returns no value"
  [name getter onclick]
  (let [retval (JTextPane. )
	value (getter)
	updater (fn []
		  (. retval setText (util-stringify "<html><a href=\"" value "\">" value "</a></html>")))
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
    (struct-map inspector-item :name name :editor retval :updater updater)))
      

(defn uii-setup-inspector-panel 
  "Given a panel and a sequence of inspector items, setup the items in the panel.
inPanel - panel to use to setup information
inspector-item-seq - Sequence of inspector items"
[inPanel inspector-item-seq]
  (. inPanel removeAll)
  (let [layout (GridBagLayout. )
	constraints (GridBagConstraints. )]
    (. inPanel setLayout (GridBagLayout.))
    (sets! constraints ipadx 1 ipady 1)
    (doseq [[index item next] 
	    (map vector 
		 (iterate inc 0) 
		 inspector-item-seq
		 (rest (lazy-cat inspector-item-seq (list nil))))]
      (let [[wy anchor-left anchor-right] (if next
					    [0.0 GridBagConstraints/EAST GridBagConstraints/WEST]
					    [1.0 GridBagConstraints/NORTHEAST GridBagConstraints/NORTHWEST])]
	(util-add-with-constraints (JLabel. (item :name)) constraints inPanel
				   gridx 0
				   gridy index
				   ipadx 0
				   anchor anchor-left
				   fill GridBagConstraints/NONE
				   weightx 0.0
				   weighty wy)
	(util-add-with-constraints (item :editor) constraints inPanel
				   gridx 1
				   gridy index
				   ipadx 0
				   anchor anchor-right
				   fill GridBagConstraints/HORIZONTAL
				   weightx 1.0
				   weighty wy)))
    (. inPanel setPreferredSize  (. inPanel getMinimumSize))))
      