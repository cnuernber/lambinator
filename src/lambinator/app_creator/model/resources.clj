(ns lambinator.app-creator.model.resources
  (:use lambinator.app-creator.model))

(defn acmr-init
  "Given a model, initialize the resources object.
Returns a new model"
  [model]
  (let [root-node-id (model :root-node-id)
	[new-model resources-id] (acm-create-node model)
	[new-model _] (acm-set-node-property new-model root-node-id :resources resources-id)
	[new-model _] (acm-set-node-property new-model resources-id :images {} )
	[new-model _] (acm-set-node-parent new-model resources-id root-node-id)]
    new-model))


(defn acmr-get-resources-id
  "Return the id of the resource node"
  [model]
  ((acm-get-root-node model) :resources))

(defn- get-resource-images [model]
  (let [resource-id (acmr-get-resources-id model)
	resource-node (acm-get-node model resource-id)
	images (resource-node :images)]
    [resource-id images]))

(defn- get-resource-image [model full-path]
  (let [[resource-id images] (get-resource-image model)]
    [resource-id images (images full-path)]))

(defn acmr-add-image
  "Add a new resource image if it doesn't exist yet.  
Its id is the source path.  
Returns a model"
  [model full-canonical-source-path]
  (let [[resource-id images existing] (get-resource-images model full-canonical-source-path)]
    (if (nil? existing)
      (let [new-images (assoc images full-canonical-source-path true)]
	(first (acm-set-node-property model resource-id :images new-images)))
      model)))

(defn acmr-remove-image
  "Remove a resource image if it exists.
Returns a model"
  [model full-canonical-source-path]
  (let [[resource-id images existing] (get-resource-images model full-canonical-source-path)]
    (if existing
      (first (acm-set-node-property model resource-id :images (dissoc images full-canonical-source-path)))
      model)))

(defn acmr-get-images
  "Return a list of the images in the resources."
  [model]
  (keys (second (get-resource-images model))))
	
