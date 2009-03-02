(ns lambinator.app-creator.scene-engine
  (:use (lambinator scenegraph)
	(lambinator.app-creator scene)))

;A scene engine manages keeping a reference to a scene
;and producing a list of scene items with global transform
;entries.

(defstruct scene-engine
  :scene-ref            ;ref to the scene
  :scene-graph-map-ref  ;scene to scene graph map ref
  :scene-graph-ref      ;scenegraph
  :global-transforms-ref) ;scenegraph to global transforms ref

(defn acse-create-engine
  "Create a new engine.  This creates a scene graph
and the associated datastructures"
  []
  (struct-map scene-engine
    :scene-ref (ref acs-empty-scene)
    :scene-graph-map-ref (ref {})
    :scene-graph-ref (ref sg-empty-graph)
    :global-transforms-ref (ref {})))

(defn acse-dirty?
  "Is this scene engine dirty"
  [engine scene]
  (not(identical? scene @(engine :scene-ref))))

(defn acse-scene-item-list
  "Returns a list of nested tuples.
- [node [global-xform inv-tpose]]
one for each node with a non-null item list"
  [scene scene-graph-map global-xforms]
  (let [item-nodes (filter (fn [[id node]]
			     (node :items))
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

(defn acse-update
  "Update the engine.  Performs scene -> global xform translation.
Returns nil; refs are updated"
  [engine scene]
  (let [scene-graph-map @(engine :scene-graph-map-ref)
	scenegraph @(engine :scene-graph-ref)
	global-xforms @(engine :global-transforms-ref)
	[scene-graph-map 
	 scenegraph] (acs-update-scenegraph scene-graph-map scene scenegraph)
	global-xforms (sg-update-global-transforms global-xforms scenegraph)]
    (dosync
     (ref-set (engine :scene-graph-map-ref) scene-graph-map)
     (ref-set (engine :scene-graph-ref) scenegraph)
     (ref-set (engine :global-transforms-ref) global-xforms)
     (ref-set (engine :scene-ref) scene))
    nil))