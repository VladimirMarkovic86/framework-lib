(ns framework-lib.side-bar-menu-test
  (:require [clojure.test :refer-macros [deftest is testing]]
            [htmlcss-lib.core :refer [gen div]]
            [js-lib.core :as md]
            [framework-lib.side-bar-menu :refer [collapse-all-items
                                                 handle-selected-menu-item
                                                 expand-collapse-item-evt
                                                 sample-evt
                                                 get-menu-vector
                                                 generate-menu
                                                 final-menu]]))

(deftest test-collapse-all-items
  (testing "Test collapse all items"
    
    (let [div-menu (gen
                     (div
                       (div
                         [(div
                            "Entity"
                            {:id "entity-nav-id"
                             :class "menu-item-0 is-selected"})
                          (div
                            (div
                              "Create"
                              {:id "entity-create-nav-id"
                               :class "menu-item-1 sub-item"})
                            {:class "expand"})
                          (div
                            (div
                              "Show all"
                              {:id "entity-show-all-nav-id"
                               :class "menu-item-1 sub-item"})
                            {:class "expand"})
                          ])
                       {:class "root-div-menu"}))
          void (md/append-element
                 "body"
                 div-menu)]
      
      (is
        (not
          (nil?
            (md/element-exists
              ".expand"))
         )
       )
      
      (is
        (not
          (nil?
            (md/element-exists
              ".is-selected"))
         )
       )
      
      (collapse-all-items)
      
      (is
        (nil?
          (md/element-exists
            ".expand"))
       )
      
      (is
        (nil?
          (md/element-exists
            ".is-selected"))
       )
      
      (md/remove-element
        ".root-div-menu")
      
     )
    
   ))

(deftest test-handle-selected-menu-item
  (testing "Test handle selected menu item"
    
    (let [div-menu (gen
                     (div
                       (div
                         [(div
                            "Entity"
                            {:id "entity-nav-id"
                             :class "menu-item-0 is-selected"})
                          (div
                            (div
                              "Create"
                              {:id "entity-create-nav-id"
                               :class "menu-item-1 sub-item"})
                            {:class "expand"})
                          (div
                            (div
                              "Show all"
                              {:id "entity-show-all-nav-id"
                               :class "menu-item-1 sub-item"})
                            {:class "expand"})
                          ])
                       {:class "root-div-menu"}))
          void (md/append-element
                 "body"
                 div-menu)
          selected-element (md/query-selector
                             ".is-selected")
          select-element (md/query-selector
                           "#entity-show-all-nav-id")
          evt-fn nil
          evt-p-ii nil
          evt-p {:evt-fn evt-fn
                 :evt-p evt-p-ii}
          element select-element
          event nil]
      
      (is
        (= (md/get-inner-html
             selected-element)
           "Entity")
       )
      
      (handle-selected-menu-item
        evt-p
        element
        event)
      
      (let [new-selected-element (md/query-selector
                                   ".is-selected")]
        
        (is
          (= (md/get-inner-html
               new-selected-element)
             "Show all")
         )
        
       )
      
      (md/remove-element
        ".root-div-menu")
      
     )
    
   ))

(deftest test-expand-collapse-item-evt
  (testing "Test expand collapse item evt"
    
    (let [div-menu (gen
                     (div
                       (div
                         [(div
                            "Entity"
                            {:id "entity-nav-id"
                             :class "menu-item-0 is-selected"})
                          (div
                            (div
                              "Create"
                              {:id "entity-create-nav-id"
                               :class "menu-item-1 sub-item"})
                            {:class "expand"})
                          (div
                            (div
                              "Show all"
                              {:id "entity-show-all-nav-id"
                               :class "menu-item-1 sub-item"})
                            {:class "expand"})
                          ])
                       {:class "root-div-menu"}))
          void (md/append-element
                 "body"
                 div-menu)
          selected-element (md/query-selector
                             ".is-selected")
          evt-fn nil
          evt-p-ii nil
          evt-p {:evt-fn evt-fn
                 :evt-p evt-p-ii}
          element selected-element
          event nil]
      
      (is
        (not
          (nil?
            (md/element-exists
              ".expand"))
         )
       )
      
      (expand-collapse-item-evt
        evt-p
        element
        event)
      
      (is
        (nil?
          (md/element-exists
            ".expand"))
       )
      
      (md/remove-element
        ".root-div-menu")
      
     )
    
   ))

(deftest test-sample-evt
  (testing "Test sample evt"
    
    (let [result (sample-evt)]
      
     )
    
   ))

(deftest test-get-menu-vector
  (testing "Test get menu vector"
    
    (let [result (get-menu-vector)]
      
      (is
        (= result
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
       )
      
     )
    
   ))

(deftest test-generate-menu
  (testing "Test generate menu"
    
    (let [menu-vector nil
          index nil
          result (generate-menu
                   menu-vector
                   index)]
      
      (is
        (vector?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [menu-vector [{:label "Project"
                        :sub-menu [{:label "Create"
                                    :evt-fn sample-evt}
                                   {:label "Show all"
                                    :evt-fn sample-evt}]}
                       ]
          index 0
          result (generate-menu
                   menu-vector
                   index)]
      
      (is
        (= result
           [{:el "div",
             :events nil,
             :attrs nil,
             :dynamic-attrs nil,
             :cont [{:el "div",
                     :events {:onclick {:evt-fn handle-selected-menu-item,
                                        :evt-p {:evt-fn expand-collapse-item-evt}}},
                     :attrs {:id nil, :class "menu-item-0"},
                     :dynamic-attrs nil,
                     :cont "Project"}
                    {:el "div",
                     :events nil,
                     :attrs nil,
                     :dynamic-attrs nil,
                     :cont {:el "div",
                            :events {:onclick {:evt-fn handle-selected-menu-item,
                                               :evt-p {:evt-fn expand-collapse-item-evt,
                                                       :evt-p {:evt-fn sample-evt,
                                                               :evt-p nil}}}},
                            :attrs {:id nil, :class "menu-item-1 sub-item"},
                            :dynamic-attrs nil,
                            :cont "Create"}}
                    {:el "div",
                     :events nil,
                     :attrs nil,
                     :dynamic-attrs nil,
                     :cont {:el "div",
                            :events {:onclick {:evt-fn handle-selected-menu-item,
                                               :evt-p {:evt-fn expand-collapse-item-evt,
                                                       :evt-p {:evt-fn sample-evt,
                                                               :evt-p nil}}}},
                            :attrs {:id nil, :class "menu-item-1 sub-item"},
                            :dynamic-attrs nil,
                            :cont "Show all"}}]}])
       )
      
     )
    
   ))

(deftest test-final-menu
  (testing "Test final menu"
    
    (let [menu-vector [{:label "Project"
                        :sub-menu [{:label "Create"
                                    :evt-fn sample-evt}
                                   {:label "Show all"
                                    :evt-fn sample-evt}]}
                       ]
          result (final-menu
                   menu-vector)]
      
      (is
        (= result
           {:el "div",
            :events nil,
            :attrs {:class "root-div-menu"},
            :dynamic-attrs nil,
            :cont [{:el "div",
                    :events nil,
                    :attrs nil,
                    :dynamic-attrs nil,
                    :cont [{:el "div",
                            :events {:onclick {:evt-fn handle-selected-menu-item,
                                               :evt-p {:evt-fn expand-collapse-item-evt}}},
                            :attrs {:id nil, :class "menu-item-0"},
                            :dynamic-attrs nil,
                            :cont "Project"}
                           {:el "div",
                            :events nil,
                            :attrs nil,
                            :dynamic-attrs nil,
                            :cont {:el "div",
                                   :events {:onclick {:evt-fn handle-selected-menu-item,
                                                      :evt-p {:evt-fn expand-collapse-item-evt,
                                                              :evt-p {:evt-fn sample-evt,
                                                                      :evt-p nil}}}},
                                   :attrs {:id nil,
                                           :class "menu-item-1 sub-item"},
                                   :dynamic-attrs nil,
                                   :cont "Create"}}
                           {:el "div",
                            :events nil,
                            :attrs nil,
                            :dynamic-attrs nil,
                            :cont {:el "div",
                                   :events {:onclick {:evt-fn handle-selected-menu-item,
                                                      :evt-p {:evt-fn expand-collapse-item-evt,
                                                              :evt-p {:evt-fn sample-evt,
                                                                      :evt-p nil}}}},
                                   :attrs {:id nil,
                                           :class "menu-item-1 sub-item"},
                                   :dynamic-attrs nil,
                                   :cont "Show all"}}]}]})
       )
      
     )
    
   ))

