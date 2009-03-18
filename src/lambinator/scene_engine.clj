(ns lambinator.scene-engine
  (:use (lambinator scenegraph scene)))
	

;A scene engine manages keeping a reference to a scene
;and producing a list of scene items with global transform
;entries.

(defstruct sne-scene-engine
  :scene            ;ref to the scene
  :scene-graph-map  ;scene to scene graph map ref
  :scene-graph      ;scenegraph
  :global-transforms) ;scenegraph to global transforms ref

(def sne-empty-engine
  (struct-map sne-scene-engine
    :scene sn-empty-scene
    :scene-graph-map {}
    :scene-graph sg-empty-graph
    :global-transforms {}))

(defn sne-dirty?
  "Is this scene engine dirty"
  [engine scene]
  (not(identical? scene (engine :scene))))

(defn sne-scene-item-list
  "Returns a list of nested tuples.
- [node [global-xform inv-tpose]]
one for each node with a non-null item list"
  [scene scene-graph-map global-xforms]
  (let [item-nodes (filter (fn [[id node]]
			     (seq (node :items)))
			   (scene :nodes))
	item-node-xforms (map (fn [[id node]]
				(let [scene-id (node :id)
				      sg-map (scene-graph-map scene-id)
				      sg-id (when sg-map
					      (sg-map :scenegraph-id))
				      xform-entry (when sg-id
						    (global-xforms sg-id))
				      xform-invtpose (when xform-entry
						       [(xform-entry :global-transform)
							(xform-entry :inverse-transpose)])]
				  [node xform-invtpose]))
			      item-nodes)]
    ;return only nodes that a) have items and b) have a valid global transform
    (filter second item-node-xforms)))

(defn sne-update
  "Update the engine.  Performs scene -> global xform translation.
Returns a new engine"
  [engine scene]
  (let [scene-graph-map (engine :scene-graph-map)
	scenegraph (engine :scene-graph)
	global-xforms (engine :global-transforms)
	[scene-graph-map 
	 scenegraph] (sn-update-scenegraph scene-graph-map scene scenegraph)
	global-xforms (sg-update-global-transforms global-xforms scenegraph)]
    (assoc engine
      :scene-graph-map scene-graph-map
      :scene-graph scenegraph
      :global-transforms global-xforms
      :scene scene)))
