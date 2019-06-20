(ns framework-lib.core-test-cljs
  (:require [clojure.test :refer-macros [deftest is testing]]
            [js-lib.core :as md]
            [htmlcss-lib.core :refer [gen div input]]
            [validator-lib.core :refer [validate-input]]
            [common-middle.session :as cms]
            [framework-lib.core :refer [render-img image-field select-field textarea-field
                                        find-input-fn generate-field close-popup
                                        popup-fn framework-default-error generate-ths
                                        handle-paging generate-pagination switch-view
                                        svg-table-icon svg-one-column-icon
                                        svg-two-column-icon svg-three-column-icon
                                        svg-four-column-icon svg-five-column-icon
                                        generate-row-number-dropdown-options
                                        generate-pagination-bar generate-thead
                                        handle-selected-menu-item format-date
                                        generate-tbody escape-html-tags
                                        generate-card-view cb-checked? vec-contains?
                                        checkbox-field insert-update-entity-success
                                        insert-update-entity generate-form-concrete
                                        generate-form entity-form insert-action
                                        create-entity update-action edit-entity
                                        edit-action entity-details
                                        entity-delete-success entity-delete
                                        search-entities-fn entity-table-success
                                        gen-table]]))

(deftest test-render-img
  (testing "Test render img"
    
    (let [file-id nil
          img-id nil
          result (render-img
                   {:file-id file-id
                    :img-id img-id})]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [file-el (.createElement
                    js/document
                    "input")
          void (.setAttribute
                 file-el
                 "type"
                 "file")
          void (.setAttribute
                 file-el
                 "id"
                 "file-id")
          img-el (.createElement
                   js/document
                   "img")
          void (.setAttribute
                 img-el
                 "id"
                 "img-id")
          void (md/append-element
                 "body"
                 file-el)
          void (md/append-element
                 "body"
                 img-el)
          file-id "file-id"
          img-id "img-id"
          result (render-img
                   {:file-id file-id
                    :img-id img-id})]
      
      (is
        (nil?
          result)
       )
      
     )
    
   ))

(deftest test-image-field
  (testing "Test image field"
    
    (let [content nil
          attrs nil
          evts nil
          dyn-attrs nil
          result (image-field
                   content
                   attrs
                   evts
                   dyn-attrs)]
      
      (is
        (= result
           [{:el "div",
             :events nil,
             :attrs nil,
             :dynamic-attrs nil,
             :cont [{:el "img",
                     :events nil,
                     :attrs {:id nil, :src nil},
                     :dynamic-attrs nil,
                     :cont ""}]}
            {:el "div",
             :events nil,
             :attrs nil,
             :dynamic-attrs nil,
             :cont [{:el "input",
                     :events {:onchange {:evt-fn render-img,
                                         :evt-p {:file-id "file", :img-id nil}}},
                     :attrs {:id "file", :type "file"},
                     :dynamic-attrs nil,
                     :cont ""}
                    {:el "span",
                     :events nil,
                     :attrs nil,
                     :dynamic-attrs nil,
                     :cont nil}]}])
       )
      
     )
    
    (let [content "Test"
          attrs {:value "1"}
          evts {}
          dyn-attrs {}
          result (image-field
                   content
                   attrs
                   evts
                   dyn-attrs)]
      
      (is
        (= result
           [{:el "div",
             :events nil,
             :attrs nil,
             :dynamic-attrs nil,
             :cont [{:el "img",
                     :events nil,
                     :attrs {:id nil, :src "1"},
                     :dynamic-attrs nil,
                     :cont ""}]}
            {:el "div",
             :events nil,
             :attrs nil,
             :dynamic-attrs nil,
             :cont [{:el "input",
                     :events {:onchange {:evt-fn render-img,
                                         :evt-p {:file-id "file", :img-id nil}}},
                     :attrs {:value "1", :id "file", :type "file"},
                     :dynamic-attrs nil,
                     :cont ""}
                    {:el "span",
                     :events nil,
                     :attrs nil,
                     :dynamic-attrs nil,
                     :cont nil}]}])
       )
      
     )
    
   ))

(deftest test-select-field
  (testing "Test select field"
    
    (let [content nil
          data nil
          attrs nil
          evts nil
          dyn-attrs nil
          result (select-field
                   content
                   data
                   attrs
                   evts
                   dyn-attrs)]
      
      (is
        (= result
           {:el "select",
            :events nil,
            :attrs nil,
            :dynamic-attrs nil,
            :cont [{:el "option",
                    :events nil,
                    :attrs {:value ""},
                    :dynamic-attrs nil,
                    :cont 33}]})
       )
      
     )
    
    (let [content ["test-value-1"
                   "test-value-2"]
          data "test-value"
          attrs {}
          evts {}
          dyn-attrs {}
          result (select-field
                   content
                   data
                   attrs
                   evts
                   dyn-attrs)]
      
      (is
        (= result
           {:el "select",
            :events {},
            :attrs {},
            :dynamic-attrs {},
            :cont [{:el "option",
                    :events nil,
                    :attrs {:value ""},
                    :dynamic-attrs nil,
                    :cont 33}
                   {:el "option",
                    :events nil,
                    :attrs {:value "test-value-1", :title "test-value-1"},
                    :dynamic-attrs nil,
                    :cont "test-value-1"}
                   {:el "option",
                    :events nil,
                    :attrs {:value "test-value-2", :title "test-value-2"},
                    :dynamic-attrs nil,
                    :cont "test-value-2"}]})
       )
      
     )
    
   ))

(deftest test-textarea-field
  (testing "Test textarea field"
    
    (let [content nil
          attrs nil
          evts nil
          dyn-attrs nil
          result (textarea-field
                   content
                   attrs
                   evts
                   dyn-attrs)]
      
      (is
        (= result
           {:el "textarea",
            :events nil,
            :attrs nil,
            :dynamic-attrs nil,
            :cont nil})
       )
      
     )
    
    (let [content "test"
          attrs {:id "test-id"}
          evts {}
          dyn-attrs {}
          result (textarea-field
                   content
                   attrs
                   evts
                   dyn-attrs)]
      
      (is
        (= result
           {:el "textarea",
            :events {},
            :attrs {:id "test-id"},
            :dynamic-attrs nil,
            :cont "test"})
       )
      
     )
    
   ))

(deftest test-find-input-fn
  (testing "Test find input fn"
    
    (let [field-type nil
          i nil
          result (find-input-fn
                   field-type
                   i)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [field-type "text"
          i 0
          result (find-input-fn
                   field-type
                   i)]
      
      (is
        (= result
           input)
       )
      
     )
    
    (let [field-type "textarea"
          i 0
          result (find-input-fn
                   field-type
                   i)]
      
      (is
        (= result
           textarea-field)
       )
      
     )
    
   ))

(deftest test-generate-field
  (testing "Test generate field"
    
    (let [id nil
          value nil
          field-type nil
          attrs nil
          evts nil
          options nil
          result (generate-field
                   id
                   value
                   field-type
                   attrs
                   evts
                   options)]
      
      (is
        (= result
           {:el "input",
            :events {:oninput {:evt-fn validate-input}},
            :attrs {:name nil, :type "text", :id nil},
            :dynamic-attrs {:value nil},
            :cont ""})
       )
      
     )
    
    (let [id "test-id"
          value "test-value"
          field-type "text"
          attrs {:test-attr "test-attr-value"}
          evts {}
          options nil
          result (generate-field
                   id
                   value
                   field-type
                   attrs
                   evts
                   options)]
      
      (is
        (= result
           {:el "input",
            :events {:oninput {:evt-fn validate-input}},
            :attrs {:name "test-id",
                    :type "text",
                    :test-attr "test-attr-value",
                    :id "test-id"},
            :dynamic-attrs {:value "test-value"},
            :cont ""})
       )
      
     )
    
    (let [id "test-id"
          value "test-value"
          field-type "select"
          attrs {:test-attr "test-attr-value"}
          evts {}
          options ["test-value-1"
                   "test-value-2"]
          result (generate-field
                   id
                   value
                   field-type
                   attrs
                   evts
                   options)]
      
      (is
        (= result
           {:el "select",
            :events {:onchange {:evt-fn validate-input}},
            :attrs {:name "test-id", :test-attr "test-attr-value", :id "test-id"},
            :dynamic-attrs nil,
            :cont [{:el "option",
                    :events nil,
                    :attrs {:value ""},
                    :dynamic-attrs nil,
                    :cont 33}
                   {:el "option",
                    :events nil,
                    :attrs {:value "test-value-1", :title "test-value-1"},
                    :dynamic-attrs nil,
                    :cont "test-value-1"}
                   {:el "option",
                    :events nil,
                    :attrs {:value "test-value-2", :title "test-value-2"},
                    :dynamic-attrs nil,
                    :cont "test-value-2"}]})
       )
      
     )
    
   ))

(deftest test-close-popup
  (testing "Test close popup"
    
    (let [result (close-popup)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [popup-el (.createElement
                     js/document
                     "div")
          void (.setAttribute
                 popup-el
                 "class"
                 "popup-modal")
          void (md/append-element
                 "body"
                 popup-el)]
      
      (is
        (not
          (nil?
            (md/element-exists
              ".popup-modal"))
         )
       )
      
      (close-popup)
      
      (is
        (nil?
          (md/element-exists
            ".popup-modal"))
       )
      
     )
    
   ))

(deftest test-popup-fn
  (testing "Test popup fn"
    
    (let [heading nil
          content nil]
      
      (is
        (nil?
          (md/element-exists
            ".popup-modal"))
       )
      
      (popup-fn
        {:content content
         :heading heading})
      
      (is
        (not
          (nil?
            (md/element-exists
              ".popup-modal"))
         )
       )
      
      (close-popup)
      
      (is
        (nil?
          (md/element-exists
            ".popup-modal"))
       )
       
     )
    
   ))

(deftest test-framework-default-error
  (testing "Test framework default error"
    
    (let [xhr nil
          result (framework-default-error
                   xhr)]
      
      (is
        (not
          (nil?
            (md/element-exists
              ".popup-modal"))
         )
       )
      
      (close-popup)
      
      (is
        (nil?
          (md/element-exists
            ".popup-modal"))
       )
      
     )
    
    (let [xhr (clj->js
                {"cljResponseType" "text/clojurescript"
                 "response" "{:message \"Error\", :status \"error\"}"})
          result (framework-default-error
                   xhr)]
      
      (is
        (not
          (nil?
            (md/element-exists
              ".popup-modal"))
         )
       )
      
      (is
        (= (md/get-inner-html
             ".popup-content")
           "Error")
       )
      
      (is
        (= (md/get-inner-html
             ".popup-heading")
           "error")
       )
      
      (close-popup)
      
      (is
        (nil?
          (md/element-exists
            ".popup-modal"))
       )
      
     )
    
   ))

(deftest test-generate-ths
  (testing "Test generate ths"
    
    (let [columns nil
          actions nil
          result (generate-ths
                   columns
                   actions)]
      
      (is
        (= result
           [])
       )
      
     )
    
    (let [columns {:projection [:property-1
                                :property-2
                                :property-3
                                ]
                   :style
                    {:property-1
                      {:content "Property 1"
                       :th {:style {:width "15%"}}
                       :td {:style {:width "15%"
                                    :text-align "left"}}
                       }
                     :property-2
                      {:content "Property 2"
                       :th {:style {:width "15%"}}
                       :td {:style {:width "15%"
                                    :text-align "left"}}
                       }
                     :property-3
                      {:content "Property 3"
                       :th {:style {:width "100px"}}
                       :td {:style {:width "100px"
                                    :text-align "left"}}
                       }}
                   }
          actions [{:label "Details"
                    :evt-fn nil
                    :evt-p nil
                    :class "details"
                    :menu-change nil}]
          result (generate-ths
                   columns
                   actions)]
      
      (is
        (= result
           [{:el "th",
             :events nil,
             :attrs {:style {:width "15%"}, :title "Property 1"},
             :dynamic-attrs nil,
             :cont "Property 1"}
            {:el "th",
             :events nil,
             :attrs {:style {:width "15%"}, :title "Property 2"},
             :dynamic-attrs nil,
             :cont "Property 2"}
            {:el "th",
             :events nil,
             :attrs {:style {:width "100px"}, :title "Property 3"},
             :dynamic-attrs nil,
             :cont "Property 3"}
            {:el "th",
             :events nil,
             :attrs {:colspan 1, :title 9, :style {:max-width "10%"}},
             :dynamic-attrs nil,
             :cont 9}])
       )
      
     )
    
   ))

(deftest test-handle-paging
  (testing "Test handle paging"
    
    (let [param-1 "first"
          query nil
          table-fn (fn [conf]
                     (let [div-el (.createElement
                                    js/document
                                    "div")
                           void (.setAttribute
                                  div-el
                                  "class"
                                  param-1)]
                       (md/append-element
                         "body"
                         div-el))
                    )
          conf {:query query
                :table-fn table-fn}
          pagination nil
          page param-1
          result (handle-paging
                   {:conf conf
                    :pagination pagination
                    :page page})]
      
      (is
        (not
          (nil?
            (md/element-exists
              (str
                "." param-1))
           ))
       )
      
      (md/remove-element
        (str
          "." param-1))
      
      (is
        (nil?
          (md/element-exists
            (str
              "." param-1))
         )
       )
      
     )
    
    (let [param-1 "previous"
          query nil
          table-fn (fn [conf]
                     (let [div-el (.createElement
                                    js/document
                                    "div")
                           void (.setAttribute
                                  div-el
                                  "class"
                                  param-1)]
                       (md/append-element
                         "body"
                         div-el))
                    )
          conf {:query query
                :table-fn table-fn}
          pagination nil
          page param-1
          result (handle-paging
                   {:conf conf
                    :pagination pagination
                    :page page})]
      
      (is
        (not
          (nil?
            (md/element-exists
              (str
                "." param-1))
           ))
       )
      
      (md/remove-element
        (str
          "." param-1))
      
      (is
        (nil?
          (md/element-exists
            (str
              "." param-1))
         )
       )
      
     )
    
    (let [param-1 "next"
          query nil
          table-fn (fn [conf]
                     (let [div-el (.createElement
                                    js/document
                                    "div")
                           void (.setAttribute
                                  div-el
                                  "class"
                                  param-1)]
                       (md/append-element
                         "body"
                         div-el))
                    )
          conf {:query query
                :table-fn table-fn}
          pagination nil
          page param-1
          result (handle-paging
                   {:conf conf
                    :pagination pagination
                    :page page})]
      
      (is
        (not
          (nil?
            (md/element-exists
              (str
                "." param-1))
           ))
       )
      
      (md/remove-element
        (str
          "." param-1))
      
      (is
        (nil?
          (md/element-exists
            (str
              "." param-1))
         )
       )
      
     )
    
    (let [param-1 "last"
          query nil
          table-fn (fn [conf]
                     (let [div-el (.createElement
                                    js/document
                                    "div")
                           void (.setAttribute
                                  div-el
                                  "class"
                                  param-1)]
                       (md/append-element
                         "body"
                         div-el))
                    )
          conf {:query query
                :table-fn table-fn}
          pagination nil
          page param-1
          result (handle-paging
                   {:conf conf
                    :pagination pagination
                    :page page})]
      
      (is
        (not
          (nil?
            (md/element-exists
              (str
                "." param-1))
           ))
       )
      
      (md/remove-element
        (str
          "." param-1))
      
      (is
        (nil?
          (md/element-exists
            (str
              "." param-1))
         )
       )
      
     )
    
    (let [param-1 "test"
          query nil
          table-fn (fn [conf]
                     (let [div-el (.createElement
                                    js/document
                                    "div")
                           void (.setAttribute
                                  div-el
                                  "class"
                                  param-1)]
                       (md/append-element
                         "body"
                         div-el))
                    )
          conf {:query query
                :table-fn table-fn}
          pagination nil
          page param-1
          result (handle-paging
                   {:conf conf
                    :pagination pagination
                    :page page})]
      
      (is
        (not
          (nil?
            (md/element-exists
              (str
                "." param-1))
           ))
       )
      
      (md/remove-element
        (str
          "." param-1))
      
      (is
        (nil?
          (md/element-exists
            (str
              "." param-1))
         )
       )
      
     )
    
   ))

(deftest test-generate-pagination
  (testing "Test generate pagination"
    
    (let [current-page nil
          number-of-pages nil
          show-link nil
          assoc-page nil
          result (generate-pagination
                   current-page
                   number-of-pages
                   show-link
                   assoc-page)]
      
      (is
        (= result
           [{:el "div", :events nil, :attrs nil, :dynamic-attrs nil, :cont nil}
            {:el "div", :events nil, :attrs nil, :dynamic-attrs nil, :cont nil}
            {:el "div",
             :events nil,
             :attrs {:class "current-page"},
             :dynamic-attrs nil,
             :cont 1}
            {:el "div", :events nil, :attrs nil, :dynamic-attrs nil, :cont nil}
            {:el "div", :events nil, :attrs nil, :dynamic-attrs nil, :cont nil}])
       )
      
     )
    
    (let [current-page 1
          number-of-pages 4
          show-link 3
          assoc-page (fn [] )
          result (generate-pagination
                   current-page
                   number-of-pages
                   show-link
                   assoc-page)]
      
      (is
        (= result
           [{:el "div", :events nil, :attrs nil, :dynamic-attrs nil, :cont 34}
            {:el "div", :events nil, :attrs nil, :dynamic-attrs nil, :cont 35}
            {:el "div", :events nil, :attrs nil, :dynamic-attrs nil, :cont 1}
            {:el "div",
             :events nil,
             :attrs {:class "current-page"},
             :dynamic-attrs nil,
             :cont 2}
            {:el "div", :events nil, :attrs nil, :dynamic-attrs nil, :cont 3}
            {:el "div", :events nil, :attrs nil, :dynamic-attrs nil, :cont 36}
            {:el "div", :events nil, :attrs nil, :dynamic-attrs nil, :cont 37}])
       )
      
     )
    
   ))

(deftest test-switch-view
  (testing "Test switch view"
    
    (let [card-columns-a (atom 0)
          table-rows-a (atom 10)
          preferences {:card-columns-a card-columns-a
                       :table-rows-a table-rows-a}
          table-fn (fn [] )
          query-fn (fn [] )
          conf {:table-fn table-fn
                :query-fn query-fn
                :preferences preferences}
          evt-p {:conf conf
                 :new-card-columns 3
                 :new-table-rows 3}
          element nil
          event nil]
      
      (switch-view
        evt-p
        element
        event)
      
      (is
        (= @card-columns-a
           3)
       )
      
      (is
        (= @table-rows-a
           3)
       )
      
     )
    
   ))

(deftest test-svg-table-icon
  (testing "Test svg table icon"
    
    (let [result (svg-table-icon)]
      
      (is
        (= result
           {:el "svg",
            :events nil,
            :attrs {:width "28", :height "28", :selected-value "0"},
            :dynamic-attrs nil,
            :cont [{:el "rect",
                    :events nil,
                    :attrs {:width "24", :height "6", :y "11", :x "2"},
                    :dynamic-attrs nil,
                    :cont nil}]})
       )
      
     )
    
   ))

(deftest test-svg-one-column-icon
  (testing "Test svg one column icon"
    
    (let [result (svg-one-column-icon)]
      
      (is
        (= result
           {:el "svg",
            :events nil,
            :attrs {:width "28", :height "28", :selected-value "1"},
            :dynamic-attrs nil,
            :cont [{:el "rect",
                    :events nil,
                    :attrs {:width "24", :height "24", :y "2", :x "2"},
                    :dynamic-attrs nil,
                    :cont nil}]})
       )
      
     )
    
   ))

(deftest test-svg-two-column-icon
  (testing "Test svg two column icon"
    
    (let [result (svg-two-column-icon)]
      
      (is
        (= result
           {:el "svg",
            :events nil,
            :attrs {:width "28", :height "28", :selected-value "2"},
            :dynamic-attrs nil,
            :cont [{:el "rect",
                    :events nil,
                    :attrs {:width "11", :height "24", :y "2", :x "2"},
                    :dynamic-attrs nil,
                    :cont nil}
                   {:el "rect",
                    :events nil,
                    :attrs {:width "11", :height "24", :y "2", :x "15"},
                    :dynamic-attrs nil,
                    :cont nil}]})
       )
      
     )
    
   ))

(deftest test-svg-three-column-icon
  (testing "Test svg three column icon"
    
    (let [result (svg-three-column-icon)]
      
      (is
        (= result
           {:el "svg",
            :events nil,
            :attrs {:width "28", :height "28", :selected-value "3"},
            :dynamic-attrs nil,
            :cont [{:el "rect",
                    :events nil,
                    :attrs {:width "6", :height "24", :y "2", :x "2"},
                    :dynamic-attrs nil,
                    :cont nil}
                   {:el "rect",
                    :events nil,
                    :attrs {:width "6", :height "24", :y "2", :x "11"},
                    :dynamic-attrs nil,
                    :cont nil}
                   {:el "rect",
                    :events nil,
                    :attrs {:width "6", :height "24", :y "2", :x "20"},
                    :dynamic-attrs nil,
                    :cont nil}]})
       )
      
     )
    
   ))

(deftest test-svg-four-column-icon
  (testing "Test svg four column icon"
    
    (let [result (svg-four-column-icon)]
      
      (is
        (= result
           {:el "svg",
            :events nil,
            :attrs {:width "28", :height "28", :selected-value "4"},
            :dynamic-attrs nil,
            :cont [{:el "rect",
                    :events nil,
                    :attrs {:width "5", :height "24", :y "2", :x "1"},
                    :dynamic-attrs nil,
                    :cont nil}
                   {:el "rect",
                    :events nil,
                    :attrs {:width "5", :height "24", :y "2", :x "8"},
                    :dynamic-attrs nil,
                    :cont nil}
                   {:el "rect",
                    :events nil,
                    :attrs {:width "5", :height "24", :y "2", :x "15"},
                    :dynamic-attrs nil,
                    :cont nil}
                   {:el "rect",
                    :events nil,
                    :attrs {:width "5", :height "24", :y "2", :x "22"},
                    :dynamic-attrs nil,
                    :cont nil}]})
       )
      
     )
    
   ))

(deftest test-svg-five-column-icon
  (testing "Test svg five column icon"
    
    (let [result (svg-five-column-icon)]
      
      (is
        (= result
           {:el "svg",
            :events nil,
            :attrs {:width "28", :height "28", :selected-value "5"},
            :dynamic-attrs nil,
            :cont [{:el "rect",
                    :events nil,
                    :attrs {:width "3", :height "24", :y "2", :x "2"},
                    :dynamic-attrs nil,
                    :cont nil}
                   {:el "rect",
                    :events nil,
                    :attrs {:width "3", :height "24", :y "2", :x "7"},
                    :dynamic-attrs nil,
                    :cont nil}
                   {:el "rect",
                    :events nil,
                    :attrs {:width "3", :height "24", :y "2", :x "12"},
                    :dynamic-attrs nil,
                    :cont nil}
                   {:el "rect",
                    :events nil,
                    :attrs {:width "3", :height "24", :y "2", :x "17"},
                    :dynamic-attrs nil,
                    :cont nil}
                   {:el "rect",
                    :events nil,
                    :attrs {:width "3", :height "24", :y "2", :x "22"},
                    :dynamic-attrs nil,
                    :cont nil}]})
       )
      
     )
    
   ))

(deftest test-generate-row-number-dropdown-options
  (testing "Test generate row number dropdown options"
    
    (let [conf nil
          result (generate-row-number-dropdown-options
                   conf)]
      
      (is
        (= result
           [{:el "div",
             :events {:onclick {:evt-fn switch-view,
                                :evt-p {:conf nil,
                                        :new-card-columns nil,
                                        :new-table-rows 1}}},
             :attrs nil,
             :dynamic-attrs nil,
             :cont "x1"}
            {:el "div",
             :events {:onclick {:evt-fn switch-view,
                                :evt-p {:conf nil,
                                        :new-card-columns nil,
                                        :new-table-rows 2}}},
             :attrs nil,
             :dynamic-attrs nil,
             :cont "x2"}
            {:el "div",
             :events {:onclick {:evt-fn switch-view,
                                :evt-p {:conf nil,
                                        :new-card-columns nil,
                                        :new-table-rows 3}}},
             :attrs nil,
             :dynamic-attrs nil,
             :cont "x3"}
            {:el "div",
             :events {:onclick {:evt-fn switch-view,
                                :evt-p {:conf nil,
                                        :new-card-columns nil,
                                        :new-table-rows 4}}},
             :attrs nil,
             :dynamic-attrs nil,
             :cont "x4"}
            {:el "div",
             :events {:onclick {:evt-fn switch-view,
                                :evt-p {:conf nil,
                                        :new-card-columns nil,
                                        :new-table-rows 5}}},
             :attrs nil,
             :dynamic-attrs nil,
             :cont "x5"}
            {:el "div",
             :events {:onclick {:evt-fn switch-view,
                                :evt-p {:conf nil,
                                        :new-card-columns nil,
                                        :new-table-rows 10}}},
             :attrs nil,
             :dynamic-attrs nil,
             :cont "x10"}
            {:el "div",
             :events {:onclick {:evt-fn switch-view,
                                :evt-p {:conf nil,
                                        :new-card-columns nil,
                                        :new-table-rows 20}}},
             :attrs nil,
             :dynamic-attrs nil,
             :cont "x20"}
            {:el "div",
             :events {:onclick {:evt-fn switch-view,
                                :evt-p {:conf nil,
                                        :new-card-columns nil,
                                        :new-table-rows 25}}},
             :attrs nil,
             :dynamic-attrs nil,
             :cont "x25"}
            {:el "div",
             :events {:onclick {:evt-fn switch-view,
                                :evt-p {:conf nil,
                                        :new-card-columns nil,
                                        :new-table-rows 50}}},
             :attrs nil,
             :dynamic-attrs nil,
             :cont "x50"}
            {:el "div",
             :events {:onclick {:evt-fn switch-view,
                                :evt-p {:conf nil,
                                        :new-card-columns nil,
                                        :new-table-rows 100}}},
             :attrs nil,
             :dynamic-attrs nil,
             :cont "x100"}])
       )
      
     )
    
   ))

(deftest test-generate-pagination-bar
  (testing "Test generate pagination bar"
    
    (let [current-page nil
          rows nil
          total-row-count nil
          pagination {:current-page current-page
                      :rows rows
                      :total-row-count total-row-count}
          conf {:preferences {:card-columns-a (atom 0)
                              :table-rows-a (atom 10)}}
          result (generate-pagination-bar
                   pagination
                   conf)
          [result-1
           result-2
           result-3] (:cont result)]
      
      (is
        (= (:attrs result-1)
           {:class "pagination-container"})
       )
      
      (is
        (= (:attrs result-2)
           {:class "dropdown-container dropdown-container-columns"})
       )
      
      (is
        (= (:attrs result-3)
           {:class "dropdown-container dropdown-container-rows"})
       )
      
     )
    
   ))

(deftest test-generate-thead
  (testing "Test generate thead"
    
    (let [table-class nil
          columns nil
          actions nil
          pagination nil
          conf nil
          result (generate-thead
                   table-class
                   columns
                   actions
                   pagination
                   conf)]
      
      (is
        (= result
           {:el "thead",
            :events nil,
            :attrs nil,
            :dynamic-attrs nil,
            :cont [{:el "tr",
                    :events nil,
                    :attrs nil,
                    :dynamic-attrs nil,
                    :cont []}
                   nil]})
       )
      
     )
    
    (let [table-class nil
          columns {:projection [:property-1
                                :property-2
                                :property-3
                                ]
                   :style
                    {:property-1
                      {:content "Property 1"
                       :th {:style {:width "15%"}}
                       :td {:style {:width "15%"
                                    :text-align "left"}}
                       }
                     :property-2
                      {:content "Property 2"
                       :th {:style {:width "15%"}}
                       :td {:style {:width "15%"
                                    :text-align "left"}}
                       }
                     :property-3
                      {:content "Property 3"
                       :th {:style {:width "100px"}}
                       :td {:style {:width "100px"
                                    :text-align "left"}}
                       }}
                   }
          actions [{:label "Details"
                    :evt-fn nil
                    :evt-p nil
                    :class "details"
                    :menu-change nil}]
          pagination nil
          conf nil
          result (generate-thead
                   table-class
                   columns
                   actions
                   pagination
                   conf)]
      
      (is
        (= result
           {:el "thead",
            :events nil,
            :attrs nil,
            :dynamic-attrs nil,
            :cont [{:el "tr",
                    :events nil,
                    :attrs nil,
                    :dynamic-attrs nil,
                    :cont [{:el "th",
                            :events nil,
                            :attrs {:style {:width "15%"}, :title "Property 1"},
                            :dynamic-attrs nil,
                            :cont "Property 1"}
                           {:el "th",
                            :events nil,
                            :attrs {:style {:width "15%"}, :title "Property 2"},
                            :dynamic-attrs nil,
                            :cont "Property 2"}
                           {:el "th",
                            :events nil,
                            :attrs {:style {:width "100px"}, :title "Property 3"},
                            :dynamic-attrs nil,
                            :cont "Property 3"}
                           {:el "th",
                            :events nil,
                            :attrs {:colspan 1,
                                    :title 9,
                                    :style {:max-width "10%"}},
                            :dynamic-attrs nil,
                            :cont 9}]}
                   nil]})
       )
      
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
                             :class "menu-item-0"})
                          (div
                            (div
                              "Create"
                              {:id "entity-create-nav-id"
                               :class "menu-item-1 sub-item is-selected"})
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
          evt-fn nil
          evt-p-ii nil
          menu-change "createToShowAll"
          evt-p {:evt-fn evt-fn
                 :evt-p evt-p-ii
                 :menu-change menu-change}
          element nil
          event nil
          result (handle-selected-menu-item
                   evt-p
                   element
                   event)]
      
      (is
        (= (md/get-inner-html
             ".is-selected")
           "Show all")
       )
      
      (md/remove-element
        ".root-div-menu")
      
      (is
        (nil?
          (md/element-exists
            ".is-selected"))
       )
      
     )
    
   ))

(deftest test-format-date
  (testing "Test format date"
    
    (reset!
      cms/selected-language
      "english")
    
    (let [date nil
          month nil
          year nil
          result (format-date
                   date
                   month
                   year)]
      
      (is
        (= result
           "//")
       )
      
     )
    
    (let [date 19
          month 6
          year 2019
          result (format-date
                   date
                   month
                   year)]
      
      (is
        (= result
           "6/19/2019")
       )
      
     )
    
    (reset!
      cms/selected-language
      "serbian")
    
    (let [date nil
          month nil
          year nil
          result (format-date
                   date
                   month
                   year)]
      
      (is
        (= result
           "..")
       )
      
     )
    
    (let [date 19
          month 6
          year 2019
          result (format-date
                   date
                   month
                   year)]
      
      (is
        (= result
           "19.6.2019")
       )
      
     )
    
   ))

(deftest test-generate-tbody
  (testing "Test generate tbody"
    
    (let [entities nil
          columns nil
          actions nil
          result (generate-tbody
                   entities
                   columns
                   actions)]
      
      (is
        (= result
           {:el "tbody", :events nil, :attrs nil, :dynamic-attrs nil, :cont []})
       )
      
     )
    
    (let [entities [{:property-1 "Property 1 entity 1 value"
                     :property-2 "Property 2 entity 1 value"
                     :property-3 "Property 3 entity 1 value"}
                    {:property-1 "Property 1 entity 2 value"
                     :property-2 "Property 2 entity 2 value"
                     :property-3 "Property 3 entity 2 value"}
                    {:property-1 "Property 1 entity 3 value"
                     :property-2 "Property 2 entity 3 value"
                     :property-3 "Property 3 entity 3 value"}]
          columns {:projection [:property-1
                                :property-2
                                :property-3
                                ]
                   :style
                    {:property-1
                      {:content "Property 1"
                       :th {:style {:width "15%"}}
                       :td {:style {:width "15%"
                                    :text-align "left"}}
                       }
                     :property-2
                      {:content "Property 2"
                       :th {:style {:width "15%"}}
                       :td {:style {:width "15%"
                                    :text-align "left"}}
                       }
                     :property-3
                      {:content "Property 3"
                       :th {:style {:width "100px"}}
                       :td {:style {:width "100px"
                                    :text-align "left"}}
                       }}
                   }
          actions [{:label "Custom action"
                    :evt-fn nil
                    :evt-p nil
                    :class "custom-action"
                    :menu-change nil}]
          result (generate-tbody
                   entities
                   columns
                   actions)]
      
      (is
        (= result
           {:el "tbody",
            :events nil,
            :attrs nil,
            :dynamic-attrs nil,
            :cont [{:el "tr",
                    :events nil,
                    :attrs nil,
                    :dynamic-attrs nil,
                    :cont [{:el "td",
                            :events nil,
                            :attrs {:style {:width "15%", :text-align "left"},
                                    :title "Property 1 entity 1 value"},
                            :dynamic-attrs nil,
                            :cont "Property 1 entity 1 value"}
                           {:el "td",
                            :events nil,
                            :attrs {:style {:width "15%", :text-align "left"},
                                    :title "Property 2 entity 1 value"},
                            :dynamic-attrs nil,
                            :cont "Property 2 entity 1 value"}
                           {:el "td",
                            :events nil,
                            :attrs {:style {:width "100px", :text-align "left"},
                                    :title "Property 3 entity 1 value"},
                            :dynamic-attrs nil,
                            :cont "Property 3 entity 1 value"}
                           {:el "td",
                            :events {:onclick {:evt-fn handle-selected-menu-item,
                                               :evt-p {:evt-fn nil,
                                                       :evt-p {:ent-id nil},
                                                       :menu-change nil}}},
                            :attrs {:class "action custom-action",
                                    :title "Custom action"},
                            :dynamic-attrs nil,
                            :cont "Custom action"}]}
                   {:el "tr",
                    :events nil,
                    :attrs nil,
                    :dynamic-attrs nil,
                    :cont [{:el "td",
                            :events nil,
                            :attrs {:style {:width "15%", :text-align "left"},
                                    :title "Property 1 entity 2 value"},
                            :dynamic-attrs nil,
                            :cont "Property 1 entity 2 value"}
                           {:el "td",
                            :events nil,
                            :attrs {:style {:width "15%", :text-align "left"},
                                    :title "Property 2 entity 2 value"},
                            :dynamic-attrs nil,
                            :cont "Property 2 entity 2 value"}
                           {:el "td",
                            :events nil,
                            :attrs {:style {:width "100px", :text-align "left"},
                                    :title "Property 3 entity 2 value"},
                            :dynamic-attrs nil,
                            :cont "Property 3 entity 2 value"}
                           {:el "td",
                            :events {:onclick {:evt-fn handle-selected-menu-item,
                                               :evt-p {:evt-fn nil,
                                                       :evt-p {:ent-id nil},
                                                       :menu-change nil}}},
                            :attrs {:class "action custom-action",
                                    :title "Custom action"},
                            :dynamic-attrs nil,
                            :cont "Custom action"}]}
                   {:el "tr",
                    :events nil,
                    :attrs nil,
                    :dynamic-attrs nil,
                    :cont [{:el "td",
                            :events nil,
                            :attrs {:style {:width "15%", :text-align "left"},
                                    :title "Property 1 entity 3 value"},
                            :dynamic-attrs nil,
                            :cont "Property 1 entity 3 value"}
                           {:el "td",
                            :events nil,
                            :attrs {:style {:width "15%", :text-align "left"},
                                    :title "Property 2 entity 3 value"},
                            :dynamic-attrs nil,
                            :cont "Property 2 entity 3 value"}
                           {:el "td",
                            :events nil,
                            :attrs {:style {:width "100px", :text-align "left"},
                                    :title "Property 3 entity 3 value"},
                            :dynamic-attrs nil,
                            :cont "Property 3 entity 3 value"}
                           {:el "td",
                            :events {:onclick {:evt-fn handle-selected-menu-item,
                                               :evt-p {:evt-fn nil,
                                                       :evt-p {:ent-id nil},
                                                       :menu-change nil}}},
                            :attrs {:class "action custom-action",
                                    :title "Custom action"},
                            :dynamic-attrs nil,
                            :cont "Custom action"}]}]})
       )
      
     )
    
   ))

(deftest test-escape-html-tags
  (testing "Test escape html tags"
    
    (let [value nil
          result (escape-html-tags
                   value)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [value "test"
          result (escape-html-tags
                   value)]
      
      (is
        (= result
           value)
       )
      
     )
    
    (let [value "<div></div>"
          result (escape-html-tags
                   value)]
      
      (is
        (= result
           "&lt;div&gt;&lt;/div&gt;")
       )
      
     )
    
   ))

(deftest test-generate-card-view
  (testing "Test generate card view"
    
    (let [current-page nil
          rows nil
          total-row-count nil
          pagination {:current-page current-page
                      :rows rows
                      :total-row-count total-row-count}
          conf {:preferences {:card-columns-a (atom 0)
                              :table-rows-a (atom 10)}}
          entities [{:property-1 "Property 1 entity 1 value"
                     :property-2 "Property 2 entity 1 value"
                     :property-3 "Property 3 entity 1 value"}
                    {:property-1 "Property 1 entity 2 value"
                     :property-2 "Property 2 entity 2 value"
                     :property-3 "Property 3 entity 2 value"}
                    {:property-1 "Property 1 entity 3 value"
                     :property-2 "Property 2 entity 3 value"
                     :property-3 "Property 3 entity 3 value"}]
          columns {:projection [:property-1
                                :property-2
                                :property-3
                                ]
                   :style
                    {:property-1
                      {:content "Property 1"
                       :th {:style {:width "15%"}}
                       :td {:style {:width "15%"
                                    :text-align "left"}}
                       }
                     :property-2
                      {:content "Property 2"
                       :th {:style {:width "15%"}}
                       :td {:style {:width "15%"
                                    :text-align "left"}}
                       }
                     :property-3
                      {:content "Property 3"
                       :th {:style {:width "100px"}}
                       :td {:style {:width "100px"
                                    :text-align "left"}}
                       }}
                   }
          actions [{:label "Custom action"
                    :evt-fn nil
                    :evt-p nil
                    :class "custom-action"
                    :menu-change nil}]
          result (generate-card-view
                   pagination
                   conf
                   entities
                   columns
                   actions)]
      
      (is
        (= (md/get-outer-html
             result)
           "<div class=\"card-view\"><div class=\"pagination-bar\"><div class=\"pagination-container\"><div class=\"pagination\"><div></div><div></div><div class=\"current-page\">1</div><div></div><div></div></div></div><div class=\"dropdown-container dropdown-container-columns\"><div class=\"dropdown-selection dropdown-selection-columns\"><svg width=\"28\" height=\"28\" selected-value=\"0\"><rect width=\"24\" height=\"6\" y=\"11\" x=\"2\"></rect></svg></div><div class=\"dropdown-menu dropdown-menu-columns\"><svg width=\"28\" height=\"28\" selected-value=\"0\"><rect width=\"24\" height=\"6\" y=\"11\" x=\"2\"></rect></svg><svg width=\"28\" height=\"28\" selected-value=\"1\"><rect width=\"24\" height=\"24\" y=\"2\" x=\"2\"></rect></svg><svg width=\"28\" height=\"28\" selected-value=\"2\"><rect width=\"11\" height=\"24\" y=\"2\" x=\"2\"></rect><rect width=\"11\" height=\"24\" y=\"2\" x=\"15\"></rect></svg><svg width=\"28\" height=\"28\" selected-value=\"3\"><rect width=\"6\" height=\"24\" y=\"2\" x=\"2\"></rect><rect width=\"6\" height=\"24\" y=\"2\" x=\"11\"></rect><rect width=\"6\" height=\"24\" y=\"2\" x=\"20\"></rect></svg><svg width=\"28\" height=\"28\" selected-value=\"4\"><rect width=\"5\" height=\"24\" y=\"2\" x=\"1\"></rect><rect width=\"5\" height=\"24\" y=\"2\" x=\"8\"></rect><rect width=\"5\" height=\"24\" y=\"2\" x=\"15\"></rect><rect width=\"5\" height=\"24\" y=\"2\" x=\"22\"></rect></svg><svg width=\"28\" height=\"28\" selected-value=\"5\"><rect width=\"3\" height=\"24\" y=\"2\" x=\"2\"></rect><rect width=\"3\" height=\"24\" y=\"2\" x=\"7\"></rect><rect width=\"3\" height=\"24\" y=\"2\" x=\"12\"></rect><rect width=\"3\" height=\"24\" y=\"2\" x=\"17\"></rect><rect width=\"3\" height=\"24\" y=\"2\" x=\"22\"></rect></svg></div></div><div class=\"dropdown-container dropdown-container-rows\"><div class=\"dropdown-selection dropdown-selection-rows\"><div>x10</div></div><div class=\"dropdown-menu dropdown-menu-rows\"><div>x1</div><div>x2</div><div>x3</div><div>x4</div><div>x5</div><div>x10</div><div>x20</div><div>x25</div><div>x50</div><div>x100</div></div></div></div><div class=\"card-container\"><div class=\"card-wrapper-3\"><div class=\"card\"><div class=\"card-heading\"></div><div class=\"card-content\"><div class=\"card-property\"><div class=\"card-property-name\" title=\"Property 1\">Property 1</div><div class=\"card-property-value\" title=\"Property 1 entity 1 value\">Property 1 entity 1 value</div></div><div class=\"card-property\"><div class=\"card-property-name\" title=\"Property 2\">Property 2</div><div class=\"card-property-value\" title=\"Property 2 entity 1 value\">Property 2 entity 1 value</div></div><div class=\"card-property\"><div class=\"card-property-name\" title=\"Property 3\">Property 3</div><div class=\"card-property-value\" title=\"Property 3 entity 1 value\">Property 3 entity 1 value</div></div><div class=\"card-actions\"><div class=\"card-action custom-action\" title=\"Custom action\">Custom action</div></div></div></div></div><div class=\"card-wrapper-3\"><div class=\"card\"><div class=\"card-heading\"></div><div class=\"card-content\"><div class=\"card-property\"><div class=\"card-property-name\" title=\"Property 1\">Property 1</div><div class=\"card-property-value\" title=\"Property 1 entity 2 value\">Property 1 entity 2 value</div></div><div class=\"card-property\"><div class=\"card-property-name\" title=\"Property 2\">Property 2</div><div class=\"card-property-value\" title=\"Property 2 entity 2 value\">Property 2 entity 2 value</div></div><div class=\"card-property\"><div class=\"card-property-name\" title=\"Property 3\">Property 3</div><div class=\"card-property-value\" title=\"Property 3 entity 2 value\">Property 3 entity 2 value</div></div><div class=\"card-actions\"><div class=\"card-action custom-action\" title=\"Custom action\">Custom action</div></div></div></div></div><div class=\"card-wrapper-3\"><div class=\"card\"><div class=\"card-heading\"></div><div class=\"card-content\"><div class=\"card-property\"><div class=\"card-property-name\" title=\"Property 1\">Property 1</div><div class=\"card-property-value\" title=\"Property 1 entity 3 value\">Property 1 entity 3 value</div></div><div class=\"card-property\"><div class=\"card-property-name\" title=\"Property 2\">Property 2</div><div class=\"card-property-value\" title=\"Property 2 entity 3 value\">Property 2 entity 3 value</div></div><div class=\"card-property\"><div class=\"card-property-name\" title=\"Property 3\">Property 3</div><div class=\"card-property-value\" title=\"Property 3 entity 3 value\">Property 3 entity 3 value</div></div><div class=\"card-actions\"><div class=\"card-action custom-action\" title=\"Custom action\">Custom action</div></div></div></div></div></div></div>")
       )
      
     )
    
   ))

(deftest test-cb-checked?
  (testing "Test cb checked?"
    
    (let [selected-cbs nil
          current-index nil
          option nil
          result (cb-checked?
                   selected-cbs
                   current-index
                   option)]
      
      (is
        (false?
          result)
       )
      
     )
    
    (let [selected-cbs ["not-this"
                        "or-this"
                        "selected-option"
                        "dont-check-me-out"]
          current-index 0
          option "selected-option"
          result (cb-checked?
                   selected-cbs
                   current-index
                   option)]
      
      (is
        (true?
          result)
       )
      
     )
    
   ))

(deftest test-vec-contains?
  (testing "Test vec contains?"
    
    (let [data nil
          el nil
          index nil
          result (vec-contains?
                   data
                   el
                   index)]
      
      (is
        (nil?
          result)
       )
      
     )
    
    (let [data ["not-this"
                "or-this"
                "selected-option"
                "dont-check-me-out"]
          el "selected-option"
          index 0
          result (vec-contains?
                   data
                   el
                   index)]
      
      (is
        (true?
          result)
       )
      
     )
    
   ))

(deftest test-checkbox-field
  (testing "Test checkbox field"
    
    (let [selected-cbs nil
          label-txt nil
          options nil
          disabled nil
          result (checkbox-field
                   selected-cbs
                   label-txt
                   options
                   disabled)]
      
      (is
        (vector?
          result)
       )
      
      (is
        (empty?
          result)
       )
      
     )
    
    (let [selected-cbs "selected-option"
          label-txt "Property-name"
          options ["not-this"
                   "nor-this"
                   "selected-option"
                   "dont-check-me-out"]
          disabled nil
          result (checkbox-field
                   selected-cbs
                   label-txt
                   options
                   disabled)]
      
      (is
        (= result
           [{:el "div",
             :events nil,
             :attrs nil,
             :dynamic-attrs nil,
             :cont [{:el "input",
                     :events nil,
                     :attrs {:id "cbProperty-namenot-this",
                             :name "cbProperty-name",
                             :type "checkbox",
                             :value "not-this"},
                     :dynamic-attrs nil,
                     :cont ""}
                    {:el "label",
                     :events nil,
                     :attrs {:id "lblcbProperty-namenot-this",
                             :for "cbProperty-namenot-this"},
                     :dynamic-attrs nil,
                     :cont "not-this"}]}
            {:el "div",
             :events nil,
             :attrs nil,
             :dynamic-attrs nil,
             :cont [{:el "input",
                     :events nil,
                     :attrs {:id "cbProperty-namenor-this",
                             :name "cbProperty-name",
                             :type "checkbox",
                             :value "nor-this"},
                     :dynamic-attrs nil,
                     :cont ""}
                    {:el "label",
                     :events nil,
                     :attrs {:id "lblcbProperty-namenor-this",
                             :for "cbProperty-namenor-this"},
                     :dynamic-attrs nil,
                     :cont "nor-this"}]}
            {:el "div",
             :events nil,
             :attrs nil,
             :dynamic-attrs nil,
             :cont [{:el "input",
                     :events nil,
                     :attrs {:id "cbProperty-nameselected-option",
                             :name "cbProperty-name",
                             :type "checkbox",
                             :value "selected-option"},
                     :dynamic-attrs nil,
                     :cont ""}
                    {:el "label",
                     :events nil,
                     :attrs {:id "lblcbProperty-nameselected-option",
                             :for "cbProperty-nameselected-option"},
                     :dynamic-attrs nil,
                     :cont "selected-option"}]}
            {:el "div",
             :events nil,
             :attrs nil,
             :dynamic-attrs nil,
             :cont [{:el "input",
                     :events nil,
                     :attrs {:id "cbProperty-namedont-check-me-out",
                             :name "cbProperty-name",
                             :type "checkbox",
                             :value "dont-check-me-out"},
                     :dynamic-attrs nil,
                     :cont ""}
                    {:el "label",
                     :events nil,
                     :attrs {:id "lblcbProperty-namedont-check-me-out",
                             :for "cbProperty-namedont-check-me-out"},
                     :dynamic-attrs nil,
                     :cont "dont-check-me-out"}]}])
       )
      
     )
    
   ))

(deftest test-insert-update-entity-success
  (testing "Test insert update entity success"
    
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
          xhr nil
          url nil
          table-fn (fn [] )
          ajax-params {:conf {:table-fn table-fn
                              :url url}}
          result (insert-update-entity-success
                   xhr
                   ajax-params)]
      
      (is
        (= (md/get-inner-html
             ".is-selected")
           "Show all")
       )
      
      (md/remove-element
        ".root-div-menu")
      
      (is
        (nil?
          (md/element-exists
            ".is-selected"))
       )
      
     )
    
   ))

(deftest test-insert-update-entity
  (testing "Test insert update entity"
    
    (let [conf nil
          result (insert-update-entity
                   conf)]
      
      (is
        (nil?
          result)
       )
      
     )
    
   ))

(deftest test-generate-form-concrete
  (testing "Test generate-form-concrete"
    
    (let [result (generate-form-concrete)]
      
      (is
        (= result
           [{:el "fieldset",
             :events nil,
             :attrs nil,
             :dynamic-attrs nil,
             :cont [{:el "legend",
                     :events nil,
                     :attrs nil,
                     :dynamic-attrs nil,
                     :cont " "}
                    {:el "input",
                     :events nil,
                     :attrs {:id "_id", :name "_id", :type "hidden", :value nil},
                     :dynamic-attrs nil,
                     :cont ""}]}
            {:el "div",
             :events nil,
             :attrs nil,
             :dynamic-attrs nil,
             :cont [{:el "input",
                     :events {:onclick {:evt-fn handle-selected-menu-item,
                                        :evt-p {:evt-fn nil,
                                                :evt-p {:disabled false,
                                                        :query {:entity-filter {}}},
                                                :menu-change "entityToShowAll"}}},
                     :attrs {:id "btnCancel",
                             :class "btn",
                             :type "button",
                             :value 12},
                     :dynamic-attrs nil,
                     :cont ""}
                    nil]}])
       )
      
     )
    
    (let [form-conf {:id :_id
                     :type entity-type
                     :entity-name "Entity name"
                     :fields {:property-1 {:label "Property 1"
                                           :input-el "text"
                                           :attrs {:placeholder "Property 1"
                                                   :required true}}
                              :property-2 {:label "Property 2"
                                          :input-el "text"
                                          :attrs {:placeholder "Property 2"
                                                  :required true}}
                              :property-3 {:label "Property 3"
                                           :input-el "email"
                                           :attrs {:placeholder "Property 3"
                                                   :required true}}
                              }
                     :fields-order [:property-1
                                    :property-2
                                    :property-3]}
          xhr nil
          ajax-params {:conf {:form-conf form-conf}}
          result (generate-form-concrete
                   xhr
                   ajax-params)
          [legend-el
           input-id-el
           property-1-el
           property-2-el
           property-3-el] (:cont (first
                                   result))]

         (is
           (= (first
                (get-in
                  property-1-el
                  [:cont
                   :cont]))
              "Property 1")
          )

         (is
           (= (first
                (get-in
                  property-2-el
                  [:cont
                   :cont]))
              "Property 2")
          )

         (is
           (= (first
                (get-in
                  property-3-el
                  [:cont
                   :cont]))
              "Property 3")
          )
      
     )
    
   ))

(deftest test-generate-form
  (testing "Test generate form"
    
    (let [div-content-el (.createElement
                           js/document
                           "div")
          void (.setAttribute
                 div-content-el
                 "class"
                 "content")
          void (md/append-element
                 "body"
                 div-content-el)
          xhr nil
          ajax-params nil
          result (generate-form
                   xhr
                   ajax-params)]
      
      (is
        (not
          (nil?
            (md/element-exists
              ".entity"))
         )
       )
      
      (md/remove-element
        ".content")
      
      (is
        (nil?
          (md/element-exists
            ".entity"))
       )
      
     )
    
   ))

(deftest test-entity-form
  (testing "Test entity form"
    
    (let [result (entity-form
                   conf)]
      
      (is
        (nil?
          result)
       )
      
     )
    
   ))

(deftest test-insert-action
  (testing "Test insert action"
    
    (let [conf nil
          result (insert-action
                   conf)]
      
      (is
        (= result
           {:form-type 4,
            :action :insert,
            :action-label 10,
            :action-fn insert-update-entity})
       )
      
     )
    
    (let [conf {:first-property "first-property-value"}
          result (insert-action
                   conf)]
      
      (is
        (= result
           {:first-property "first-property-value"
            :form-type 4,
            :action :insert,
            :action-label 10,
            :action-fn insert-update-entity})
       )
      
     )
    
   ))

(deftest test-create-entity
  (testing "Test create entity"
    
    (let [div-content-el (.createElement
                           js/document
                           "div")
          void (.setAttribute
                 div-content-el
                 "class"
                 "content")
          void (md/append-element
                 "body"
                 div-content-el)
          conf nil
          result (create-entity
                   conf)]
      
      (is
        (not
          (nil?
            (md/element-exists
              ".entity"))
         )
       )
      
      (md/remove-element
        ".content")
      
      (is
        (nil?
          (md/element-exists
            ".entity"))
       )
      
     )
    
   ))

(deftest test-update-action
  (testing "Test update action"
    
    (let [conf nil
          result (update-action
                   conf)]
      
      (is
        (= result
           {:form-type 7,
            :action :update,
            :action-label 11,
            :action-fn insert-update-entity})
       )
      
     )
    
    (let [conf {:test-property "test-property-value"}
          result (update-action
                   conf)]
      
      (is
        (= result
           {:test-property "test-property-value"
            :form-type 7,
            :action :update,
            :action-label 11,
            :action-fn insert-update-entity})
       )
      
     )
    
   ))

(deftest test-edit-entity
  (testing "Test edit entity"
    
    (let [conf nil
          result (edit-entity
                   conf)]
      
      (is
        (nil?
          result)
       )
      
     )
    
   ))

(deftest test-edit-action
  (testing "Test edit action"
    
    (let [conf nil
          result (edit-action
                   conf)]
      
      (is
        (= result
           {:form-type 6,
            :disabled true,
            :action :edit,
            :action-label 7,
            :action-fn edit-entity,
            :action-p {:form-type 7,
                       :action :update,
                       :action-label 11,
                       :action-fn insert-update-entity}})
       )
      
     )
    
    (let [conf {:test-property "test-property-value"}
          result (edit-action
                   conf)]
      
      (is
        (= result
           {:test-property "test-property-value"
            :form-type 6,
            :disabled true,
            :action :edit,
            :action-label 7,
            :action-fn edit-entity,
            :action-p {:test-property "test-property-value"
                       :form-type 7,
                       :action :update,
                       :action-label 11,
                       :action-fn insert-update-entity}})
       )
      
     )
    
   ))

(deftest test-entity-details
  (testing "Test entity details"
    
    (let [conf nil
          result (entity-details
                   conf)]
      
      (is
        (nil?
          result)
       )
      
     )
    
   ))

(deftest test-entity-delete-success
  (testing "Test entity delete success"
    
    (let [xhr nil
          table-fn (fn [])
          ajax-params {:conf {:table-fn table-fn}}
          result (entity-delete-success
                   xhr
                   ajax-params)]
      
      (is 
        (nil?
          result)
       )
      
     )
    
   ))

(deftest test-entity-delete
  (testing "Test entity delete"
    
    (let [conf nil
          result (entity-delete
                   conf)]
      
      (is
        (nil?
          result)
       )
      
     )
    
   ))

(deftest test-search-entities-fn
  (testing "Test search entities fn"
    
    (let [gen-table-fn (fn [])
          ajax-params {:gen-table-fn gen-table-fn}
          result (search-entities-fn
                   ajax-params)]
      
      (is
        (nil?
          result)
       )
      
     )
    
   ))

(deftest test-entity-table-success
  (testing "Test entity table success"
    
    (let [xhr nil
          ajax-params nil
          result (entity-table-success
                   xhr
                   ajax-params)]
      
      (is
        (nil?
          result)
       )
      
     )
    
   ))

(deftest test-gen-table
  (testing "Test gen table"
    
    (let [conf nil
          sl-node nil
          event nil
          search-call nil
          result (gen-table
                   conf
                   sl-node
                   event
                   search-call)]
      
      (is
        (nil?
          result)
       )
      
     )
    
   ))

