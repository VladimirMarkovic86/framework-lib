(ns framework-lib.tree-test
  (:require [clojure.test :refer-macros [deftest is testing]]
            [js-lib.core :as md]
            [htmlcss-lib.core :refer [gen div]]
            [framework-lib.tree :refer [highlight-doc-name expand-dir render-tree]]))

(deftest test-highlight-doc-name
  (testing "Test highlight doc name"
    
    (let [evt-p nil
          element (.createElement
                    js/document
                    "div")
          event (clj->js
                  {"ctrlKey" true
                   "target" {"className" ""}})]
      
      (is
        (not
          (md/contains-class
            element
            "highlightDoc"))
       )
      
      (highlight-doc-name
        evt-p
        element
        event)
      
      (is
        (md/contains-class
          element
          "highlightDoc")
       )
      
     )
    
   ))

(deftest test-expand-dir
  (testing "Test expand dir"
    
    (let [get-subdocs nil
          get-subfile nil
          absolute-path nil
          context-menu-evt nil
          evt-p {:get-subdocs get-subdocs
                 :get-subfile get-subfile
                 :absolute-path absolute-path
                 :context-menu-evt context-menu-evt}
          sl-node nil
          event nil
          result (expand-dir
                   evt-p
                   sl-node
                   event)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [tree-element (gen
                         (div
                           [(div
                              [(div
                                 [(div
                                    "+"
                                    {:class "sign pointer"})
                                  (div
                                    "test-absolute-path-1"
                                    {:class "docName rootDoc changed"})
                                  ]
                                 {:class "line"})
                               (div
                                 nil
                                 {:class "subareaEmpty"})
                               ]
                              {:class "projectRoot"})
                            (div
                              [(div
                                 [(div
                                    "+"
                                    {:class "sign pointer"})
                                  (div
                                    "test-absolute-path-2"
                                    {:class "docName rootDoc changed"})
                                  ]
                                 {:class "line"})
                               (div
                                 nil
                                 {:class "subareaEmpty"})
                               ]
                              {:class "projectRoot"})
                            ]
                           {:class "tree"}))
          void (md/append-element
                 "body"
                 tree-element)
          get-subdocs (fn [])
          get-subfile nil
          absolute-path nil
          context-menu-evt nil
          evt-p {:get-subdocs get-subdocs
                 :get-subfile get-subfile
                 :absolute-path absolute-path
                 :context-menu-evt context-menu-evt}
          sl-node (md/query-selector
                    ".docName")
          event nil
          result (expand-dir
                   evt-p
                   sl-node
                   event)]
      
      (is
        (= (md/get-outer-html
             ".tree")
           "<div class=\"tree\"><div class=\"projectRoot\"><div class=\"line\"><div class=\"sign pointer\">-</div><div class=\"docName rootDoc changed\">test-absolute-path-1</div></div><div class=\"subarea\"></div></div><div class=\"projectRoot\"><div class=\"line\"><div class=\"sign pointer\">+</div><div class=\"docName rootDoc changed\">test-absolute-path-2</div></div><div class=\"subareaEmpty\"></div></div></div>")
       )
      
      (md/remove-element
        ".tree")
      
     )
    
   ))

(deftest test-render-tree
  (testing "Test render tree"
    
    (let [projects nil
          get-subdocs nil
          get-subfile nil
          context-menu-evt nil
          result (render-tree
                   projects
                   get-subdocs
                   get-subfile
                   context-menu-evt)]
      
      (is
        (= (md/get-outer-html
             result)
           "<div class=\"tree\"></div>")
       )
      
     )
    
    (let [projects [{:_id "test-id-1"
                     :group-id "test-group-id-1"
                     :artifact-id "test-artifact-id-1"
                     :version "test-version-1"
                     :absolute-path "test-absolute-path-1"
                     :language "test-language-1"
                     :project-type "test-project-type-1"
                     :changed "test-changed-1"}
                    {:_id "test-id-2"
                     :group-id "test-group-id-2"
                     :artifact-id "test-artifact-id-2"
                     :version "test-version-2"
                     :absolute-path "test-absolute-path-2"
                     :language "test-language-2"
                     :project-type "test-project-type-2"
                     :changed "test-changed-2"}
                    ]
          get-subdocs nil
          get-subfile nil
          context-menu-evt nil
          result (render-tree
                   projects
                   get-subdocs
                   get-subfile
                   context-menu-evt)
          html-result (md/get-outer-html
                        result)]
      
      (is
        (= html-result
           "<div class=\"tree\"><div class=\"projectRoot\"><div class=\"line\"><div class=\"sign pointer\">+</div><div class=\"docName rootDoc changed\">test-absolute-path-1</div></div><div class=\"subareaEmpty\"></div></div><div class=\"projectRoot\"><div class=\"line\"><div class=\"sign pointer\">+</div><div class=\"docName rootDoc changed\">test-absolute-path-2</div></div><div class=\"subareaEmpty\"></div></div></div>")
       )
      
     )
    
   ))

