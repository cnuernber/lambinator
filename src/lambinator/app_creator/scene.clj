(ns lambinator.app-creator.scene
  (:use (lambinator graph-util util graphics-math scenegraph)))

(defstruct scene
  :width
  :height
  :nodes
  :root-node
  :next-node-id)

(defonce acs-item-types [:image])

(defstruct scene-image
  :item-type ;acs-item-types
  :image) ;acs-image

(defstruct scene-node
  :id
  :parent
  :translation
  :scale
  :rotation
  :items
  :children)

(def acs-empty-scene-node
  (struct-map scene-node
    :id 0
    :translation [0 0 0]
    :scale [1 1 1]
    :rotation (gm-axis-angle-to-quat [1 0 0] 0) ;quaternion rotation
    :items [] ;collection of items, may be an image, may be a camera, may be a light, etc.
    :children []))

(defn acs-create-node
  "Create a new node.
returns [scene, node-id]"
  [scene]
  (let [node-id (scene :next-node-id)
	next-id (inc node-id)
	nodes (scene :nodes)
	new-node (assoc acs-empty-scene-node :id node-id)
	nodes (assoc nodes node-id new-node)]
    [(assoc scene :nodes nodes :next-node-id next-id) node-id]))

(def acs-empty-scene
  (let [base-scene
	(struct-map scene
	  :width 800
	  :height 600
	  :nodes {}
	  :root-node nil
	  :next-node-id 1)
	base-scene (first (acs-create-node base-scene))]
    (assoc base-scene :root-node 1)))

(defn acs-get-node
  "Return the node for a given id"
  [scene id]
  ((scene :nodes) id))
    
(defn- update-node
  [scene node]
  (let [node-id (node :id)
	nodes (scene :nodes)
	nodes (assoc nodes node-id node)]
    (assoc scene :nodes nodes)))
	
(defn acs-remove-child
  "Remove this node from its parent's child list"
  [scene node-id]
  (let [node (acs-get-node scene node-id)]
    (when node
      (let [parent-id (node :parent)
	    parent (acs-get-node scene parent-id)]
	(if (and node parent)
	  (gu-remove-child scene parent node parent-id node-id)
	  scene)))))
		
(defn acs-insert-child
  "Insert this child into this parent's children list at index.
Returns a new scene if both parent and child exist"
  [scene parent-id node-id index]
  (let [node (acs-get-node scene node-id)
	parent (acs-get-node scene parent-id)]
    (when (and node parent)
      (let [scene (acs-remove-child scene node-id)]
	(gu-insert-child scene parent node parent-id node-id index)))))

(defn acs-destroy-node
  "Remove a node given an id.  Returns the scene.  Removes all children
and removes this node's id from its parent's list"
  [scene node-id]
  (let [node (acs-get-node scene node-id)
	scene (acs-remove-child scene node-id)]
    (when node
      (let [scene (reduce (fn [child-id]
			    (acs-remove-child scene child-id))
			  scene
			  (node :children))
	    nodes (dissoc (scene :nodes) node-id)]
	(assoc scene :nodes nodes)))))

(defn- acs-set-node-prop
  [scene node-id prop-keyword prop-val]
  (let [node (acs-get-node scene node-id)]
    (when node
      (let [new-node (assoc node prop-keyword prop-val)]
	(update-node scene new-node)))))
      
(defn acs-translate
  "Set a given nodes translation.  Returns a new scene if
node-id is a valid node, nil otherwise"
  [scene node-id trans-vec]
  (acs-set-node-prop scene node-id :translation trans-vec))

(defn acs-rotate
  "Set a given node's rotation quaternion.  Vec should be 4 floats.
Returns a new scene if node-id is valid, nil otherwise."
  [scene node-id rot-vec]
  (acs-set-node-prop scene node-id :rotation rot-vec))

(defn acs-scale
  "Set a given node's scale.  Returns a new scene if node-id is valid,
nil otherwise"
  [scene node-id scale-vec]
  (acs-set-node-prop scene node-id :scale scale-vec))

(defn acs-insert-item
  "Insert an item into the node's sequence of items.
Returns a new scene or nil if node isn't found."
  [scene node-id item index]
  (let [node (acs-get-node scene node-id)]
    (when node
      (let [items (node :items)
	    new-items (seq-insert items item index)
	    node (assoc node :items new-items)]
	(update-node scene node)))))

(defn acs-remove-item
  "Remove the item at a given index from the node's sequence of items.
Returns a new scene or nil if node isn't found"
  [scene node-id index]
  (let [node (acs-get-node scene node-id)]
    (when node
      (let [items (node :items)
	    items (seq-remove items index)
	    node (assoc node :items items)]
	(update-node scene node)))))

(defn acs-create-image-item
  "Create an image acs item.  You can then insert this item into
a node's child list"
  [scene image]
  (struct-map scene-image
    :item-type :image
    :image image))

(defstruct scene-scenegraph-item
  :scene-id
  :scene-object
  :scenegraph-id)

(defn- create-scene-local-xform[scene-node]
  ;Order of these is important
  ;not exactly sure what it should be right now
  (let [trans-mat (apply gm-trans-44 (scene-node :translation))
	scale-mat (apply gm-scale-44 (scene-node :scale))
	rot-mat (gm-33-44 (gm-quat-to-matrix (scene-node :rotation)))
	retval (gm-mm-44 scale-mat rot-mat)]
    (gm-mm-44 retval trans-mat)))

(defn get-scenegraph-id
  [scene-id scene-graph-map scenegraph]
  (let [existing (scene-graph-map scene-id)]
    (if existing
      [scene-graph-map scenegraph (existing :scenegraph-id)]
      (let [[scenegraph node-id]
	    (sg-create-node scenegraph)
	    new-existing (struct-map scene-scenegraph-item
			   :scene-id scene-id
			   :scene-object nil
			   :scenegraph-id node-id)
	    scene-graph-map (assoc scene-graph-map scene-id new-existing)]
	[scene-graph-map scenegraph node-id]))))

(defn update-scenegraph-node 
  [scene-node graph-node scene-graph-map scenegraph]
   (let [new-xform (create-scene-local-xform scene-node)
	parent-id (scene-node :parent)
	scene-children (scene-node :children)
	[scene-graph-map 
	 scenegraph 
	 graph-parent-id] (if parent-id
			    (get-scenegraph-id parent-id scene-graph-map scenegraph)
			    [scene-graph-map scenegraph nil])
	[scene-graph-map 
	 scenegraph 
	 graph-child-ids] (reduce (fn [[scene-graph-map
					scenegraph
					graph-child-ids]
				       child-id]
				    (let [[scene-graph-map
					   scenegraph
					   graph-child-id]
					  (get-scenegraph-id
					   child-id
					   scene-graph-map
					   scenegraph)]
				      [scene-graph-map
				       scenegraph
				       (conj graph-child-ids graph-child-id)]))
				  [scene-graph-map
				   scenegraph
				   []]
				  scene-children)
	graph-node (assoc graph-node 
		     :parent graph-parent-id
		     :children graph-child-ids
		     :local-transform new-xform)]
    [scene-graph-map (sg-update-node scenegraph graph-node)]))
	    

(defn maybe-update-scenegraph-entry [scene-node-id scene-graph-map scene scenegraph]
  (let [existing (scene-graph-map scene-node-id)
	scene-node (acs-get-node scene scene-node-id)
	[scenegraph 
	 graph-id] (if existing
		     [scenegraph (existing :scenegraph-id)]
		     (sg-create-node scenegraph))
	graph-node (sg-get-node scenegraph graph-id)
	do-update (if existing
		    (not (identical? (existing :scene-object) scene-node))
		    true)
	existing (if existing
		   existing
		   (struct-map scene-scenegraph-item
		     :scene-id scene-node-id
		     :scene-object nil
		     :scenegraph-id graph-id))]
    (if do-update
      (let [existing (assoc existing :scene-object scene-node)
	    scene-graph-map (assoc scene-graph-map scene-node-id existing)]
	(update-scenegraph-node 
	 (acs-get-node scene scene-node-id)
	 graph-node
	 scene-graph-map
	 scenegraph))
      [scene-graph-map scenegraph])))
	      
(defn acs-update-scenegraph
  "Update the scenegraph such that the graphs are isomorphic and
that the local transforms are proper.  Returns [scene-scenegraph-map scenegraph]"
  [scene-scenegraph-map scene scenegraph]
  ;;add/update first step
  ;;clean second step
  (let [[scene-scenegraph-map 
	 scenegraph] (reduce (fn [[scene-scenegraph-map scenegraph]
				  scene-id]
			       (maybe-update-scenegraph-entry
				scene-id 
				scene-scenegraph-map
				scene
				scenegraph))
			     [scene-scenegraph-map scenegraph]
			     (keys (scene :nodes)))
	[scene-scenegraph-map 
	 scenegraph] (reduce (fn [[scene-scenegraph-map scenegraph]
				  scene-id]
			       (let [scene-node (acs-get-node scene scene-id)]
				 (if scene-node
				   [scene-scenegraph-map scenegraph]
				   (let [entry (scene-scenegraph-map scene-id)
					 scenegraph-nodes (scenegraph :nodes)
					 scenegraph-nodes (dissoc 
							   scenegraph-nodes 
							   (entry :scenegraph-id))
					 scenegraph (assoc scenegraph
						      :nodes
						      scenegraph-nodes)
					 scene-scenegraph-map (dissoc scene-scenegraph-map
								      scene-id)]
				     [scene-scenegraph-map scenegraph]))))
			     [scene-scenegraph-map scenegraph]
			     (keys scene-scenegraph-map))]
    [scene-scenegraph-map scenegraph]))