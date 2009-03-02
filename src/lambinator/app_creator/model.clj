(ns lambinator.app-creator.model)

;The app-creator model is a large set of nodes.
;Nodes can refer to each other through their node id,
;which is a unique integer assigned to a node at creation
;time.  The only reason I am using node id's instead of
;direct references is to allow me to use functional datastructures
;to represent a dom where children do have pointers to their parents.

;Trying out a completely functional design

(defstruct model-container
  :next-id ;next id to be used for a new node
  :nodes   ;map of ids to nodes
  :root-node-id ;root node id
  :change-listeners) ;items to be notified when a property changes


(defonce acm-empty-model
  (struct-map model-container
    :next-id 2
    :nodes { 1 { :id 1 } }
    :root-node-id 1
    :change-listeners nil))
    

(defn acm-create-node 
  "Create a new node in the model.  
Returns [new-node-id model]"
  [model]
  (let [cur-id (model :next-id)
	new-node { :id cur-id }
	new-id (inc cur-id)
	new-nodes (assoc (model :nodes) new-id new-node) ]
    [(assoc model 
       :next-id new-id
       :nodes new-nodes) new-id]))


(defn acm-get-node
  "Returns the node registered under a given id"
  [model node-id]
  ((model :nodes) node-id))


(defn acm-destroy-node
  "Remove this node from the model.  What you do with it
after is your business.
Returns a tuple of the new model along with the old node"
  [model node-id]
  (let [nodes (model :nodes)
	existing (acm-get-node model node-id)
	new-nodes (dissoc nodes node-id)
	new-model (assoc model :nodes new-nodes)]
    [new-model existing]))


(defn acm-get-root-node
  "Get the root model node"
  [model]
  (acm-get-node model (model :root-node-id)))


(defn acm-add-change-listener 
  "A change listener is a function that takes four arguments,
node-id prop-name old-value new-value
Returns a new model"
  [model listener]
  (assoc model
    :change-listeners
    (conj (model :change-listeners) listener)))


(defn acm-remove-change-listener
  "Removes the listener if it is eq-able to the old listener.
Returns a new model"
  [model listener]
  (assoc model
    :change-listeners
    (filter (fn [list] (not (= list listener))) (model :change-listeners))))

(defn acm-set-node-property
  "Set a property on a model node
model - acm model
node-id - id of the node you want
prop-name - name of the property
prop-value - value of the property
Returns the [model, old-value] if the node exists, nil otherwise.
I wouldn't recommed setting the id"
  [model node-id prop-name prop-value]
  (let [node (acm-get-node model node-id)]
    (when node
      (let [old-value (node prop-name)
	    new-node (assoc node prop-name prop-value)
	    nodes (model :nodes)
	    new-nodes (assoc nodes node-id new-node)
	    new-model (assoc model :nodes new-nodes)]
	(doseq [listener (new-model :change-listeners)]
	  (listener node-id prop-name old-value prop-value))
	[new-model old-value]))))

(defn acm-get-node-property
  "Returns the value of the node property"
  [model node-id prop-name]
  (let [node (acm-get-node model node-id)]
    (when node
      (node prop-name))))


(defn acm-set-node-parent
  "Generic, pre-named parent property setter"
  [model node-id parent-id]
  (acm-set-node-property
   model
   node-id
   :parent
   parent-id))

(defn acm-get-node-parent
  "Generic, pre-named parent property getter"
  [model node-id]
   (acm-get-node-property model node-id :parent))	