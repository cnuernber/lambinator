(ns lambinator.graph-util
  (:use lambinator.util))

;Pretty specialized functions for manipulating graphs described
;by parent and children properties.  Relies
;one nodes having :parent and :children keywords and the graph
;containing a map of nodes named :nodes

(defn gu-remove-child
  [graph parent node parent-id node-id]
  (let [node (assoc node :parent nil)
	children (parent :children)
	children (filter #(not (== % node-id)) children)
	parent (assoc parent :children children)
	nodes (graph :nodes)
	nodes (assoc nodes parent-id parent node-id node)]
    graph (assoc graph :nodes nodes)))

(defn gu-insert-child
  [graph parent node parent-id node-id index]
  (let [children (parent :children)
	child-count (count children)
	index (if (not index)
		child-count
		(if (< index 0)
		  (+ child-count index)
		  index))
	children (seq-insert children node-id index)
	parent (assoc parent :children children)
	node (assoc node :parent parent-id)
	nodes (graph :nodes)
	nodes (assoc nodes parent-id parent node-id node)]
    (assoc graph :nodes nodes)))
