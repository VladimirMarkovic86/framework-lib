(ns framework-lib.side-bar-menu
  (:require [js-lib.core :as md]
            [htmlcss-lib.core :refer [div]]))

(defn collapse-all-items
  "Collapses all open items"
  []
  (let [expand-elements (md/query-selector-all-on-element
                          ".root-div-menu"
                          ".expand")]
    (doseq [expand-element expand-elements]
      (md/remove-class
        expand-element
        "expand"))
   )
  (let [expand-elements (md/query-selector-all-on-element
                          ".root-div-menu"
                          ".is-selected")]
    (doseq [expand-element expand-elements]
      (md/remove-class
        expand-element
        "is-selected"))
   ))

(defn handle-selected-menu-item
  "Handles current menu item selection"
  [{evt-fn :evt-fn
    evt-p :evt-p}
   element
   event]
  (let [selected-element (md/query-selector-on-element
                           ".root-div-menu"
                           ".is-selected")]
    (md/remove-class
      selected-element
      "is-selected")
    (md/add-class
      element
      "is-selected"))
  (when (fn?
          evt-fn)
    (evt-fn
      evt-p
      element
      event))
 )

(defn expand-collapse-item-evt
  "Expand and collapse menu items"
  [{evt-fn :evt-fn
    evt-p :evt-p}
   element
   event]
  (let [siblings (md/get-next-siblings
                   element)
        is-expanded (md/contains-class
                      siblings
                      "expand")]
    (if is-expanded
      (let [is-sub-item (md/contains-class
                          element
                          "sub-item")
            el-parent (if is-sub-item
                        (.-parentElement
                          element)
                        ".root-div-menu")
            children (md/query-selector-all-on-element
                       el-parent
                       ".expand")]
        (md/remove-class
          children
          "expand"))
      (let [closest-expand (.closest
                             element
                             ".expand")]
        (if closest-expand
          (let [all-siblings (md/get-all-siblings
                               closest-expand)]
            (doseq [single-sibling all-siblings]
              (let [menu-items (md/query-selector-all-on-element
                                 single-sibling
                                 ".expand")]
                (md/remove-class
                  menu-items
                  "expand"))
             )
            (md/add-class
              siblings
              "expand"))
          (let [menu-items (md/query-selector-all
                             ".root-div-menu .expand")]
            (md/remove-class
              menu-items
              "expand")
            (md/add-class
              siblings
              "expand"))
         ))
     ))
  (when (fn?
          evt-fn)
    (evt-fn
      evt-p
      element
      event))
 )

(defn sample-evt
  "Sample event function"
  [evt-p
   element
   event]
  (.log
    js/console
    "sample-evt"))

(defn get-menu-vector
  "Menu vector sample"
  []
  [{:label "Project"
    :sub-menu [{:label "Create"
                :evt-fn sample-evt}
               {:label "Show all"
                :evt-fn sample-evt}]}
   {:label "Task"
    :sub-menu [{:label "Create"
                :evt-fn sample-evt}
               {:label "Show all"
                :evt-fn sample-evt}]}
   {:label "Working area"
    :sub-menu [{:label "Shell"
                :evt-fn sample-evt}
               {:label "File system"
                :evt-fn sample-evt}
               {:label "Leiningen"
                :evt-fn sample-evt}
               {:label "Git"
                :evt-fn sample-evt}
               {:label "IDE"
                :evt-fn sample-evt}]}
   {:label "Admin"
    :sub-menu [{:label "User"
                :sub-menu [{:label "Create"
                            :evt-fn sample-evt}
                           {:label "Show all"
                            :evt-fn sample-evt}]}
               {:label "Role"
                :sub-menu [{:label "Create"
                            :evt-fn sample-evt}
                           {:label "Show all"
                            :evt-fn sample-evt}]}
               {:label "Language"
                :sub-menu [{:label "Create"
                            :evt-fn sample-evt}
                           {:label "Show all"
                            :evt-fn sample-evt}]}]
    }])

(defn generate-menu
  "Generate html clojure maps of every menu item"
  [menu-vector
   index]
  (let [menu (atom [])]
    (doseq [menu-item menu-vector]
      (when menu-item
        (let [{label-txt :label
               evt-fn :evt-fn
               evt-p :evt-p
               sub-menu :sub-menu
               item-id :id} menu-item]
          (swap!
            menu
            conj
            (div
              (if sub-menu
                (let [sub-menu-vec (generate-menu
                                     sub-menu
                                     (inc
                                       index))]
                  (apply
                    conj
                    [(div
                       label-txt
                       {:id item-id
                        :class (str
                                 "menu-item-"
                                 index
                                 (when (not= index
                                             0)
                                   " sub-item"))}
                       {:onclick {:evt-fn handle-selected-menu-item
                                  :evt-p {:evt-fn expand-collapse-item-evt}}
                        })]
                    sub-menu-vec))
                (div
                  label-txt
                  {:id item-id
                   :class (str
                            "menu-item-"
                            index
                            (when (not= index
                                        0)
                              " sub-item"))}
                  {:onclick {:evt-fn handle-selected-menu-item
                             :evt-p {:evt-fn expand-collapse-item-evt
                                     :evt-p {:evt-fn evt-fn
                                             :evt-p evt-p}}
                             }}))
             ))
         ))
     )
    @menu))

(defn final-menu
  "Return html clojure maps of the side bar menu"
  [menu-vector]
  (let [html-maps-menu (generate-menu
                         menu-vector
                         0)]
    
    (div
      html-maps-menu
      {:class "root-div-menu"}))
 )

