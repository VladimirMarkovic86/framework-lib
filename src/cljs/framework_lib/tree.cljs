(ns framework-lib.tree
  (:require [htmlcss-lib.core :refer [gen div]]
            [js-lib.core :as md]
            [clojure.string :as cstring]))

(defn highlight-doc-name
  "Highlight document"
  [evt-p
   element
   event]
  (let [is-ctrl-pressed (aget
                          event
                          "ctrlKey")
        is-selected (cstring/index-of
                      (aget
                        (aget
                          event
                          "target")
                        "className")
                      "highlightDoc")]
    (when (and (not is-selected)
               (not is-ctrl-pressed))
      (when-let [highlighted-docs (md/query-selector-all-on-element
                                    ".tree"
                                    ".highlightDoc")]
        (doseq [highlighted-doc highlighted-docs]
          (md/remove-class
            highlighted-doc
            "highlightDoc"))
       ))
   )
  (md/add-class
    element
    "highlightDoc"))

(defn expand-dir
  "Expand content of directory"
  [{get-subdocs :get-subdocs
    get-subfile :get-subfile
    absolute-path :absolute-path
    context-menu-evt :context-menu-evt}
   sl-node
   event]
  (let [parent (md/get-parent
                 sl-node)
        sign-el (md/query-selector-on-element
                  parent
                  ".sign")
        sign (md/get-inner-html
               sign-el)
        doc-name (md/query-selector-on-element
                   parent
                   ".docName")
        doc-name (md/get-inner-html
                   doc-name)
        current-dir (str
                      absolute-path
                      "/"
                      doc-name)
        dir-event {:evt-fn expand-dir
                   :evt-p {:get-subdocs get-subdocs
                           :get-subfile get-subfile
                           :absolute-path current-dir
                           :context-menu-evt context-menu-evt}}
        highlight-event {:evt-fn highlight-doc-name}
        context-menu-event {:evt-fn context-menu-evt}]
    (when (= sign
             "+")
      (md/set-inner-html
        sign-el
        "-")
      (let [{sub-dirs :sub-dirs
             sub-files :sub-files} (get-subdocs
                                     current-dir)
            parent (md/get-parent
                     sl-node)
            parent (md/get-parent
                     parent)
            subarea (md/query-selector-on-element
                      parent
                      ".subareaEmpty")
            subarea-content (atom [])]
        (doseq [[is-changed
                 sub-dir] sub-dirs]
          (swap!
            subarea-content
            conj
            (div
              [(div
                 [(div
                    "+"
                    {:class "sign pointer"}
                    {:onclick dir-event})
                  (div
                    sub-dir
                    {:class
                      (str
                        "docName"
                        (when is-changed
                          " changed"))}
                    {:ondblclick dir-event
                     :onclick highlight-event
                     :oncontextmenu [highlight-event
                                     context-menu-event]}
                    {:dirPath (str
                                current-dir
                                "/"
                                sub-dir)})]
                 {:class "line"})
               (div
                 ""
                 {:class "subareaEmpty"})])
           ))
        (doseq [[is-changed
                 sub-file] sub-files]
          (swap!
            subarea-content
            conj
            (div
              [(div
                 ""
                 {:class "sign"})
               (div
                 sub-file
                 {:class
                   (str
                     "docName"
                     (when is-changed
                       " changed"))
                  }
                 {:ondblclick {:evt-fn get-subfile
                               :evt-p {:absolute-path current-dir
                                       :file-name sub-file}}
                  :onclick highlight-event
                  :oncontextmenu [highlight-event
                                  context-menu-event]}
                 {:dirPath current-dir
                  :filePath (str
                              current-dir
                              "/"
                              sub-file)})]
              {:class "line"})
           ))
        (let [subarea-congen (gen
                               @subarea-content)]
          (md/remove-class
            subarea
            "subareaEmpty")
          (md/add-class
            subarea
            "subarea")
          (md/append-element
            subarea
            subarea-congen))
       ))
    (when (= sign
             "-")
      (md/set-inner-html
        sign-el
        "+")
      (let [parent (md/get-parent
                     sl-node)
            parent (md/get-parent
                     parent)
            subarea (md/query-selector-on-element
                      parent
                      ".subarea")]
        (md/remove-class
          subarea
          "subarea")
        (md/add-class
          subarea
          "subareaEmpty")
        (md/set-inner-html
          subarea
          ""))
     ))
 )

(defn render-tree
  "Render tree of projects"
  [projects
   get-subdocs
   get-subfile
   context-menu-evt]
  (let [div-projects (atom [])]
    (doseq [{_id :_id
             group-id :group-id
             artifact-id :artifact-id
             version :version
             absolute-path :absolute-path
             language :language
             project-type :project-type
             changed :changed} projects]
      (let [path-vector (cstring/split
                          absolute-path
                          "/")
            parent-absolute-path (cstring/join
                                   "/"
                                   (pop
                                     path-vector))
            root-dir (last
                       path-vector)
            event {:evt-fn expand-dir
                   :evt-p {:get-subdocs get-subdocs
                           :get-subfile get-subfile
                           :absolute-path parent-absolute-path
                           :context-menu-evt context-menu-evt}}
            changedClass (when changed
                           "changed")]
        (swap!
          div-projects
          conj
          (div
            [(div
               [(div
                  "+"
                  {:class "sign pointer"}
                  {:onclick event})
                (div
                  root-dir
                  {:class (str
                            "docName rootDoc "
                            changedClass)}
                  {:ondblclick event
                   :onclick {:evt-fn highlight-doc-name}
                   :oncontextmenu [{:evt-fn highlight-doc-name}
                                   {:evt-fn context-menu-evt}]}
                  {:dirPath absolute-path
                   :ent-id _id})]
               {:class "line"})
             (div
               ""
               {:class "subareaEmpty"})]
           {:class "projectRoot"})
         ))
     )
    (gen
      (div
        @div-projects
        {:class "tree"}
        {:oncontextmenu {:evt-fn context-menu-evt}}))
   ))

