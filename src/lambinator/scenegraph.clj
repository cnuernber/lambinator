(ns lambinator.scenegraph
  (:use lambinator.graphics-math
	lambinator.util))

;The scene graph, right now, is just a graph of 4x4 matrixes, addressed by id.
;a change to the graph increments a global counter so an outside entity can
;keep track of what is dirty easily.

(defstruct graph-node
  :id
  :local-transform
  :change-index ;starts at zero.  Increments if a change should affect the global transform
  :children
  :parent)

(defstruct scene-graph
  :next-change
  :next-id
  :nodes)

(defonce sg-empty-graph
  (struct-map scene-graph
    :next-change 1
    :next-id 1
    :nodes {}))

(defn sg-create-node
  "Create a new scene graph node.  Returns [new-graph id]"
  [sg-graph]
  (let [id (sg-graph :next-id)
	new-node (struct-map graph-node
		   :id id
		   :local-transform gm-identity-44
		   :change-index 0
		   :children []
		   :parent nil )
	next-id (inc id)
	nodes (assoc (sg-graph :nodes) id new-node)]
    [(assoc sg-graph :next-id next-id :nodes nodes) id]))


(defn sg-get-node
  "Return a node from the graph"
  [sg-graph id]
  ((sg-graph :nodes) id))

(defn- update-node
  [sg-graph node]
  (let [change (sg-graph :next-change)
	next-change (inc change)
	id (node :id)
	node (assoc node :change-index change)
	nodes (assoc (sg-graph :nodes) id node)]
    (assoc sg-graph :nodes nodes :next-change next-change)))
	
(defn sg-set-local-transform
  "Set the local transform (4x4 matrix) on a node.  Returns a new graph"
  [sg-graph node-id transform]
  (let [node (sg-get-node sg-graph node-id)]
    (if node
      (update-node sg-graph (assoc node :local-transform transform))
      sg-graph)))

(defn sg-remove-child
  "Remove this child from its parent if it has one.
This updates the node's change id but not the parent's
Returns a new graph"
  [sg-graph node-id]
  (let [node (sg-get-node sg-graph node-id)
	parent (when node
		 (sg-get-node sg-graph (node :parent)))]
    (if (and node parent)
      (let [node (assoc node :parent nil)
	    children (parent :children)
	    children (filter #(not (== % node-id)) children)
	    parent (assoc parent :children children)
	    nodes (sg-graph :nodes)
	    nodes (assoc nodes (parent :id) parent)
	    sg-graph (assoc sg-graph :nodes nodes)]
	(update-node sg-graph node))
      sg-graph)))

(defn sg-insert-child
  "Add a child to the given parent.  This will remove the child from an
existing parent if it has one.  Appends the child to the end if index
is nil.  Else attempts to insert the child"
  [sg-graph parent-id child-id index]
  (let [parent (sg-get-node sg-graph parent-id)
	child (sg-get-node sg-graph child-id)]
    (if (and parent child)
      (let [sg-graph (sg-remove-child sg-graph child-id)
	    children (parent :children)
	    child-count (count children)
	    index (if (not index)
		    child-count
		    (if (< index 0)
		      (+ child-count index)
		      index))
	    children (seq-insert children child-id index)
	    parent (assoc parent :children children)
	    child (assoc child :parent parent-id)
	    nodes (sg-graph :nodes)
	    nodes (assoc nodes (parent :id) parent)
	    sg-graph (assoc sg-graph :nodes nodes)]
	(update-node sg-graph child))
      sg-graph)))

(defn sg-remove-node
  "Remove a given node from the graph.  All children are orphaned.
Returns a new graph"
  [sg-graph node-id]
  (let [node (sg-get-node sg-graph node-id)]
    (if node
      (let [parent-id (node :parent-id)
	    children (node :children)
	    sg-graph (sg-remove-child sg-graph node-id)
	    sg-graph (if (first children)
		       (reduce #(sg-remove-child %1 %2) sg-graph children)
		       sg-graph)
	    nodes (sg-graph :nodes)
	    nodes (dissoc nodes node-id)]
	(assoc sg-graph :nodes nodes))
      sg-graph)))

(defn sg-roots
  "The roots of the scenegraph, returns a list of nodes"
  [sg-graph]
  (filter #(nil? (sg-get-node sg-graph (% :parent))) (vals (sg-graph :nodes)))) 

(defstruct global-transform-info
  :node-id          ;Id of the node
  :change-index     ;Change for which this is accurate
  :global-transform ;global transform for transforming positions
  :inverse-transpose ) ;for transforming direction vectors (normals, binormals, tangents)

(defn- perform-node-transform-update
  "Actually perform the update of the node"
  [info parent-global local-transform change-index node-id]
  (let [new-global (if parent-global
		     (gm-mm-44 parent-global local-transform)
		     local-transform)
	new-33 (gm-upper-33 new-global)
	new-33 (gm-gj-invert-33 new-33)
	new-33 (gm-transpose-33 new-33)]
    (if info
      [(assoc info :global-transform new-global :inverse-transpose new-33
	      :change-index change-index) true]
      [(struct-map global-transform-info
	 :node-id node-id
	 :change-index change-index
	 :global-transform new-global
	 :inverse-transpose new-33) true])))
  

(defn- update-node-transform
  "Update this node if necessary and recurse down to children."
  [last-trans sg-graph node-id parent-global perform-update]
  (let [node (sg-get-node sg-graph node-id)]
    ;If the node exists, perhaps perform update and recurse down the
    ;node hierarchy
    (if node
      (let [info (last-trans node-id)
	    change-index (node :change-index)
	    [info perform-update] (if (or perform-update
					  (not info)
					  (not (== change-index (info :change-index))))
				    (perform-node-transform-update info 
								   parent-global 
								   (node :local-transform)
								   change-index
								   node-id)
				    [info false])
	    last-trans (assoc last-trans node-id info)]
	(reduce #(update-node-transform %1 sg-graph %2 (info :global-transform) perform-update) 
		last-trans (node :children)))
	last-trans )))
  
(defn sg-update-global-transforms
  "Update the map of global transforms, performing multiplies where necessary.
Returns a new map"
  [last-trans sg-graph]
  (reduce #(update-node-transform %1 sg-graph (%2 :id) gm-identity-44 false)
	  last-trans 
	  (sg-roots sg-graph)))