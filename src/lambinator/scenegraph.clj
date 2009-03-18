(ns lambinator.scenegraph
  (:use lambinator.graphics-math
	lambinator.util
	lambinator.graph-util))

;The scene graph, right now, is just a graph of 4x4 matrixes, addressed by id.
;a change to the graph increments a global counter so an outside entity can
;keep track of what is dirty easily.

(defstruct sg-graph-node
  :id
  :local-transform
  :children
  :parent)

(defstruct sg-scene-graph
  :next-id
  :nodes)

(defonce sg-empty-graph
  (struct-map sg-scene-graph
    :next-id 1
    :nodes {}))

(defn sg-create-node
  "Create a new scene graph node.  Returns [new-graph id]"
  [sg-graph]
  (let [id (sg-graph :next-id)
	new-node (struct-map sg-graph-node
		   :id id
		   :local-transform gm-identity-44
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
  (let [id (node :id)
	nodes (assoc (sg-graph :nodes) id node)]
    (assoc sg-graph :nodes nodes)))

(defn sg-update-node
  [sg-graph node]
  (update-node sg-graph node))
	
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
      (let [sg-graph (gu-remove-child sg-graph parent node (parent :id) node-id)]
	(update-node sg-graph node))
      sg-graph)))

(defn sg-insert-child
  "Add a child to the given parent.  This will remove the child from an
existing parent if it has one.  Appends the child to the end if index
is nil.  Else attempts to insert the child.  Returns nil if either parent or
child cannot be found"
  [sg-graph parent-id child-id index]
  (let [parent (sg-get-node sg-graph parent-id)
	child (sg-get-node sg-graph child-id)]
    (when (and parent child)
      (let [sg-graph (sg-remove-child sg-graph child-id)
	    sg-graph (gu-insert-child sg-graph parent child parent-id child-id index)]
	(update-node sg-graph child)))))

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

(defstruct sg-global-transform-info
  :node-id          ;Id of the node
  :node             ;Change for which this is accurate
  :global-transform ;global transform for transforming positions
  :inverse-transpose ) ;for transforming direction vectors (normals, binormals, tangents)

(defn- perform-node-transform-update
  "Actually perform the update of the node"
  [info parent-global local-transform node-id node]
  (let [new-global (if parent-global
		     (gm-mm-44 parent-global local-transform)
		     local-transform)
	new-33 (gm-upper-33 new-global)
	new-33 (gm-gj-invert-33 new-33)
	new-33 (gm-transpose-33 new-33)]
    (if info
      [(assoc info :global-transform new-global :inverse-transpose new-33
	      :node node) true]
      [(struct-map sg-global-transform-info
	 :node-id node-id
	 :node node
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
	    [info perform-update] (if (or perform-update
					  (not info)
					  (not (identical? node (info :node))))
				    (perform-node-transform-update info 
								   parent-global 
								   (node :local-transform)
								   node-id
								   node)
				    [info false])
	    last-trans (assoc last-trans node-id info)]
	(reduce #(update-node-transform %1 sg-graph %2 (info :global-transform) perform-update) 
		last-trans (node :children)))
	last-trans )))
  
(defn sg-update-global-transforms
  "Update the map of global transforms, performing multiplies where necessary.
Returns a new map.  This design records the change index something is at so
that it doesn't calculate more information than necessary."
  [last-trans sg-graph]
  (let [last-trans (reduce #(update-node-transform %1 sg-graph (%2 :id) gm-identity-44 false)
			   last-trans 
			   (sg-roots sg-graph))]
    (reduce (fn [last-trans node-id]
	      (if (sg-get-node sg-graph node-id)
		last-trans
		(dissoc last-trans node-id)))
	    last-trans
	    (keys last-trans))))