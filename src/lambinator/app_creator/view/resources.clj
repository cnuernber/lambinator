(ns lambinator.app-creator.view.resources
  (:import (javax.swing JTree)
	   (javax.swing.tree DefaultMutableTreeNode))
  (:use lambinator.ui
	lambinator.app-creator.model.resources))

(defstruct acvr-data
  :resources ;resources jtree
  :images-node ;default mutable node for images
  :model-map ;map of model nodes to resources items
)

(defn acvr-create []
  (let [rootNode (DefaultMutableTreeNode. "Resources")
	imagesNode (DefaultMutableTreeNode. "Images" true)
	jtree (JTree. rootNode)]
    (.add rootNode imagesNode)
    ;(.setRootVisible jtree false) ;this is stupid, why show the root?
    (.setShowsRootHandles jtree true)
    (struct-map acvr-data
      :resources jtree
      :images-node imagesNode
      :model-map nil)))

(defn acvr-get-component[acvr-data]
  (acvr-data :resources))
      