(ns framework-lib.core
  (:require [ajax-lib.core :refer [ajax get-response]]
            [js-lib.core :as md]
            [utils-lib.core :as utils]
            [htmlcss-lib.core :refer [gen stl anmtn slctr
                                      table thead tbody tr th td
                                      button label input div h3 h2
                                      textarea select option img a]]
            [cljs.reader :as reader]
            [clojure.string :as cstr]))

(def get-entities-url "/clojure/get-entities")

(def get-entity-url "/clojure/get-entity")

(def update-entity-url "/clojure/update-entity")

(def insert-entity-url "/clojure/insert-entity")

(def delete-entity-url "/clojure/delete-entity")

(def field-type-input "input")

(def field-type-radio "radio")

(def field-type-checkbox "checkbox")

(def field-type-textarea "textarea")

(def field-type-select "select")

(def sub-form "sub-form")

(def popup "popup")

(def field-type-image "image")

(def anim-time 100)

(defn- th-td-attrs
 ""
 [content
  attrs]
 (let [title (:title attrs)
       style (:style attrs)
       title (or title content)
       default-style (conj
                      {:width "auto"
                       :white-space "nowrap"
                       :text-align "center"
                       :text-overflow "ellipsis"
                       :overflow "hidden"
                       :padding "0 5px"}
                      style)]
  {:style default-style
   :title title}))

(defn- generate-ths
  "Generate th and append style for that th and td column"
  [columns-styles
   th-vector]
  (let [th-index (count @th-vector)]
   (if (< th-index (count columns-styles))
    (let [column-style (columns-styles th-index)
          content (:content column-style)
          th-attrs (:th column-style)
          header-style (th-td-attrs content
                                    th-attrs)
          attrs (:attrs column-style)]
     (swap! th-vector
            conj
            (th
                 (div
                      content
                      header-style)
                 attrs))
     (recur columns-styles
            th-vector))
    @th-vector))
  )

(defn- handle-paging
 "Handle click event on pagination link"
 [{conf :conf
   {table-conf :table-conf
    table-fn :table-fn} :conf
   pagination :pagination
   page :page}
  sl-node]
 (when (= page "first")
  (table-fn
   (assoc
     conf
     :table-conf (assoc table-conf
                        :current-page 0)))
  )
 (when (= page "previous")
  (table-fn
   (assoc
     conf
     :table-conf (assoc
                   table-conf
                   :current-page (dec (:current-page table-conf))
                  ))
   )
  )
 (when (= page "next")
  (table-fn
   (assoc
     conf
     :table-conf (assoc
                   table-conf
                   :current-page (inc (:current-page table-conf))
                  ))
   )
  )
 (when (= page "last")
  (table-fn
   (assoc
     conf
     :table-conf (assoc
                   table-conf
                   :current-page (dec (utils/round-up
                                       (:total-row-count pagination)
                                       (:rows pagination))
                                  ))
    ))
  )
 (when (and (not= page "first")
            (not= page "previous")
            (not= page "next")
            (not= page "last"))
  (table-fn
   (assoc
     conf
     :table-conf (assoc
                   table-conf
                   :current-page (dec (js/parseInt page))
                  ))
      ))
  )

(defn generate-pagination
  "Generate pagination row in thead"
  [current-page
   number-of-pages
   link-first
   link-previous
   link-next
   link-last
   assoc-page]
  (let [page-vector (atom [])]
   (swap! page-vector conj (if link-first
                            (div
                                 (a
                                      "first"
                                      {:page "first"}
                                      (assoc-page "first")))
                            (div))
    )
   (swap! page-vector conj (if link-previous
                            (div
                                 (a
                                      "previous"
                                      {:page "previous"}
                                      (assoc-page "previous"))
                                  )
                            (div))
    )
   (if (and (= current-page (dec number-of-pages))
            (< -1 (dec (dec current-page))
             ))
    (swap! page-vector conj (div
                                 (a
                                      (dec current-page)
                                      {:page (dec current-page)}
                                      (assoc-page (dec current-page))
                                  ))
     )
    nil)
   (if (< -1 (dec current-page))
    (swap! page-vector conj (div
                                 (a
                                      current-page
                                      {:page current-page}
                                      (assoc-page current-page))
                             ))
    nil)
   (swap! page-vector conj (div
                                (inc current-page)
                                {:class "current-page"})
    )
   (if (< (inc current-page) number-of-pages)
    (swap! page-vector conj (div
                                 (a
                                      (inc (inc current-page))
                                      {:page (inc (inc current-page))}
                                      (assoc-page (inc (inc current-page))
                                       ))
                             ))
    nil)
   (if (and (= current-page 0)
            (< (inc (inc current-page)) number-of-pages))
    (swap! page-vector conj (div
                                 (a
                                      (inc (inc (inc current-page))
                                       )
                                      {:page (inc (inc (inc current-page))
                                              )}
                                      (assoc-page (inc (inc (inc current-page))
                                                   ))
                                  ))
     )
    nil)
   (swap! page-vector conj (if link-next
                            (div
                                 (a
                                      "next"
                                      {:page "next"}
                                      (assoc-page "next"))
                             )
                            (div))
    )
   (swap! page-vector conj (if link-last
                            (div
                                 (a
                                      "last"
                                      {:page "last"}
                                      (assoc-page "last"))
                             )
                            (div))
    )
   @page-vector))

(defn- generate-thead
  "Generate thead for table"
  [columns-styles
   table-class
   actions
   pagination
   conf]
  (thead
       [(tr
             (if-not (empty? actions)
              (generate-ths (conj columns-styles
                                  {:content    "Actions"
                                   :attrs {:colspan (count actions)}})
                            (atom []))
              (generate-ths columns-styles
                            (atom [])))
         )
        (tr
             (th
                  (div
                       (let [current-page (:current-page pagination)
                             rows (:rows pagination)
                             total-row-count (:total-row-count pagination)
                             first-page-index 0
                             second-page-index 1
                             number-of-pages (utils/round-up total-row-count rows)
                             last-page-index (dec number-of-pages)
                             one-before-last (dec last-page-index)
                             assoc-page (fn [page]
                                         {:onclick
                                          {:evt-fn handle-paging
                                           :evt-p {:conf conf
                                                   :pagination pagination
                                                   :page page}}}
                                         )]
                        (if (< number-of-pages 4)
                         (generate-pagination current-page
                                              number-of-pages
                                              false
                                              false
                                              false
                                              false
                                              assoc-page)
                         (do (if (= current-page
                                    first-page-index)
                              (generate-pagination current-page
                                                   number-of-pages
                                                   false
                                                   false
                                                   true
                                                   true
                                                   assoc-page)
                              (if (= current-page
                                     last-page-index)
                               (generate-pagination current-page
                                                    number-of-pages
                                                    true
                                                    true
                                                    false
                                                    false
                                                    assoc-page)
                               (generate-pagination current-page
                                                    number-of-pages
                                                    true
                                                    true
                                                    true
                                                    true
                                                    assoc-page))
                              ))
                         ))
                       {:class "pagination"})
                  {:colspan (+ (count actions)
                               (count columns-styles))})
         )]))

(defn action-link
 ""
 [[content
   evt-fn
   evt-p
   ent-id]]
 (a
      content
      {:title content}
      {:onclick {:evt-fn evt-fn
                 :evt-p (assoc evt-p
                               :ent-id ent-id)}}
  ))

(defn- generate-tr
  "Generate tr elements for table body"
  [columns-styles
   data-vectors
   actions]
  (let [trs (atom [])]
   (doseq [data-vector data-vectors]
    (let [row-id (first data-vector)
          data-vector (utils/remove-index-from-vector
                       data-vector
                       0)]
     (swap! trs conj (tr
                          (let [tds (atom [])
                                td-index (atom 0)]
                           (doseq [data data-vector]
                            (let [column-style (get columns-styles @td-index)
                                  td-attrs (:td column-style)
                                  td (th-td-attrs data
                                                  td-attrs)]
                             (swap! tds conj (td
                                                  (div
                                                       data
                                                       td))
                              )
                             (swap! td-index inc))
                            )
                           (doseq [action actions]
                            (swap! tds conj (td
                                                 (div
                                                      (action-link (conj action
                                                                         row-id))
                                                  ))
                             ))
                           @tds))
      ))
    )
   @trs))

(defn- generate-tbody
 "Generate tbody for table"
 [columns-styles
  data-vectors
  actions]
 (tbody
      (generate-tr columns-styles
                   data-vectors
                   actions))
 )

(defn- input-field
  "Render input field"
  [data-type
   data
   label
   step
   disabled]
  (let [id (str "txt"
                 label)
        attrs {:id id
               :name id
               :type data-type
               :value data
               :required "required"}
        attrs (if step
               (assoc attrs
                      :step step)
               attrs)
        attrs (if disabled
               (assoc attrs
                      :disabled "disabled")
               attrs)]
   (input
        ""
        attrs))
 )

(defn- radio-field
  "Render radio field with different options"
  [data
   label
   options
   disabled]
  (let [rs (atom [])]
   (doseq [option options]
    (let [r-name (str "r"
                      label)
          id (str r-name
                  (md/replace-all option
                                  " "
                                  ""))
          r-attrs {:id id
                   :name r-name
                   :type "radio"
                   :value option
                   :required "required"}
          r-attrs (if (= data option)
                   (assoc r-attrs
                          :checked "checked")
                   r-attrs)
          r-attrs (if disabled
                   (assoc r-attrs
                          :disabled "disabled")
                   r-attrs)
          l-attrs {:id (str "lbl"
                            id)
                   :for id}]
     (swap! rs conj (div
                         [(input
                               ""
                               r-attrs)
                          (label
                               option
                               l-attrs)]))
     ))
   @rs))

(defn- cb-checked?
  "Query current option if it is checked"
  [selected-cbs
   current-index
   option]
  (if (< current-index (count selected-cbs))
   (if (= option (get selected-cbs current-index))
    true
    (recur selected-cbs
           (inc current-index)
           option))
   false))

(defn- vec-contains?
 ""
 [data
  el
  index]
 (when (< index (count data))
  (if (= el (get data index))
   true
   (recur data el (inc index))
   ))
 )

(defn- checkbox-field
  "Render checkbox fields with different options"
  [selected-cbs
   label
   options
   disabled]
  (let [cbs (atom [])]
   (doseq [option options]
    (let [cb-name (str "cb"
                       label)
          id (str cb-name
                  (md/replace-all option
                                  " "
                                  ""))
          cb-attrs {:id id
                    :name cb-name
                    :type "checkbox"
                    :value option}
          cb-attrs (if (vec-contains? selected-cbs option 0)
                    (assoc cb-attrs
                           :checked "checked")
                    cb-attrs)
          cb-attrs (if disabled
                    (assoc cb-attrs
                           :disabled "disabled")
                    cb-attrs)
          l-attrs {:id (str "lbl"
                            id)
                   :for id}]
     (swap! cbs conj (div
                          [(input
                                ""
                                cb-attrs)
                           (label
                                option
                                l-attrs)]))
     ))
   @cbs))

(defn- textarea-field
  "Render textarea field"
  [data
   label
   disabled]
  (let [id (str "ta"
                label)
        attrs {:id id
               :name id
               :required "required"}
        attrs (if disabled
               (assoc attrs
                      :disabled "disabled")
               attrs)]
   (textarea
        data
        attrs))
 )

(defn- select-field
  "Render select field"
  [option-vector
   label
   disabled]
  (let [id (str "sl"
                label)
        sl-attrs {:id id
                  :name id
                  :required "required"}
        sl-attrs (if disabled
                  (assoc sl-attrs
                         :disabled "disabled")
                  sl-attrs)]
   (select
        (let [options (atom [])]
         (doseq [[opt-val
                  opt-lbl] option-vector]
          (swap! options conj (option
                                   opt-lbl
                                   {:value opt-val}))
          )
         @options)
        sl-attrs))
 )

(defn- render-img
 ""
 []
 (let [file-field (md/query-selector "#txtImage")
       file-field-parent (md/get-parent-node file-field)
       file (aget (aget file-field "files") 0)
       img (md/query-selector "#imgImage")
       fileReader (js/FileReader.)
       onload (aset fileReader "onload"
               ((fn [aimg]
                (fn [e]
                 (aset aimg "src" (aget (aget e "target") "result"))))
                  img))
       dataURL (.readAsDataURL fileReader file)
       ]))

(defn- image-field
 ""
 [data
  label
  disabled]
 [(div
       (img
            ""
            {:id (str "img"
                      label)
             :name (str "img"
                        label)
             :style {:width "100px"
                     :height "100px"}
             :src data}))
  (div
       (let [id (str "txt"
                     label)
            attrs {:id id
                   :name id
                   :type "file"
                   :required "required"}
            attrs (if disabled
                   (assoc attrs
                          :disabled "disabled")
                   attrs)]
       (input
            ""
            attrs
            {:onchange {:evt-fn render-img}}))
       )
  ]
 )

(defn insert-update-entity-success
  "After successful entity insert or update display table again"
  [xhr
   {conf :conf
    {table-fn :table-fn} :conf}]
  (table-fn conf))

(defn insert-update-entity
 "Insert or update entity"
 [conf
  sl-node]
 (let [action               (:action conf)
       entity-conf          (:entity-conf conf)
       entity-type          (:entity-type entity-conf)
       entity-fields        (:entity-fields entity-conf)
       entity-keys          (vec (keys entity-fields))
       table-node           (md/query-selector ".entity")
       request-body         {:entity-type  entity-type}
       input-element-id     (md/query-selector-on-element table-node
                                                          "#txt_id")
       entity-id            (md/get-value input-element-id)
       entity               (atom {})
       specific-read-form   (:specific-read-form entity-conf)]
  (if specific-read-form
   (specific-read-form
    entity)
   (doseq [e-key entity-keys]
    (let [entity-field   (e-key entity-fields)
          label          (:label entity-field)
          field-type     (:field-type entity-field)
          data-type      (:data-type entity-field)
          id-prefix      (case field-type
                          "input"  "txt"
                          "radio"  "r"
                          "checkbox"  "cb"
                          "textarea"  "ta"
                          "")
          element-id (str id-prefix
                          (md/replace-all label
                                          " "
                                          ""))
          debug (.log js/console element-id)]
     (case field-type
      "radio"  (swap! entity conj {e-key (md/checked-value element-id)})
      "checkbox"  "cb"
      (let [input-element  (md/query-selector-on-element table-node (str "#" element-id))
            input-element-type  (md/get-type input-element)
            input-element-value  (md/get-value input-element)
            input-element-value  (if (= input-element-type
                                        "number")
                                  (reader/read-string input-element-value)
                                  input-element-value)]
       (swap! entity conj {e-key input-element-value}))
      ))
    ))
  (ajax
   {:url                  (if (= "Insert" action)
                           insert-entity-url
                           update-entity-url)
    :success-fn           insert-update-entity-success
    :entity               (assoc request-body :entity @entity :_id entity-id)
    :conf                 conf}))
 )

(defn popup-centered
 ""
 [el]
 (let [height (aget el "clientHeight")
       width (aget el "clientWidth")]
  (aset (aget el "style")
        "top"
        (str "calc("
             "50% - "
             (int (/ height
                     2))
             "px)"))
  (aset (aget el "style")
        "left"
        (str "calc("
             "50% - "
             (int (/ width
                     2))
             "px)"))
  ))

(defn close-popup
 ""
 []
 (md/remove-element "#popup-window")
 (md/remove-element "#popup-background"))

(defn popup-fn
 ""
 [{content :content
   heading :heading}]
 (md/append-element
  "body"
  (gen
   [(div
     ""
     {:id "popup-background"
      :style {:position "absolute"
              :width "100%"
              :height "100%"
              :opacity "0.2"
              :background-color "black"}})
    (div
     [(input ""
             {:style {:float "right"
                      :margin-top "10px"}
              :value "X"
              :type "button"}
             {:onclick {:evt-fn close-popup}})
      (div (h2 heading)
           {:style {:text-align "center"}})
      (div (content))]
     {:id "popup-window"
      :style {:position "absolute"
              :background-color "#90B4FE"
              :border "5px solid white"
              :border-radius "15px"
              :padding "0 15px 15px 15px"}})]))
 (popup-centered
  (md/query-selector "#popup-window"))
 )

(defn- generate-form-trs
 ""
 [xhr
  {conf :conf
   {table-fn :table-fn
    form-type :form-type
    action :action
    action-fn :action-fn
    action-fn-param :action-fn-param
    {entity-type :entity-type
     entity-fields :entity-fields
     entity-keys :fields-order} :entity-conf} :conf}]
 (let [response (if-not (nil? xhr)
                 (get-response xhr)
                 nil)
       entity-data (:data response)
       disabled (if (= form-type "Details")
                  true
                  false)
       trs (atom [])]
  (swap! trs conj (tr
                       (td
                            (h3
                                 (str form-type
                                      " "
                                      entity-type))
                            {:colspan 3})))
  (swap! trs conj (tr
                       (td
                            (input
                                 ""
                                 {:id "txt_id"
                                  :name "txt_id"
                                  :type "hidden"
                                  :value (:_id entity-data)})
                            {:colspan 3})))
  (doseq [e-key entity-keys]
    (let [field-conf       (e-key entity-fields)
          label-txt            (:label field-conf)
          label-no-spaces  (if (string? label-txt)
                            (md/replace-all label-txt " " "")
                            "lblDefault")
          field-type       (:field-type field-conf)
          data-type        (:data-type field-conf)
          step             (:step field-conf)
          disabled         (if (:disabled field-conf)
                            (:disabled field-conf)
                            disabled)
          options          (:options field-conf)
          data             (e-key entity-data)
          sub-form-trs (:sub-form-trs field-conf)
          popup-content (:popup field-conf)]
      (when (= field-type
               sub-form)
       (doseq [sub-form-tr (sub-form-trs entity-data disabled)]
        (swap! trs conj sub-form-tr)))
      (when (= field-type
               popup)
       (swap! trs conj (tr [(td (label label-txt))
                            (td (input ""
                                       {:id (str "btn"
                                                 label-no-spaces)
                                        :value "Change"
                                        :type "button"}
                                       {:onclick {:evt-fn popup-fn
                                                  :evt-p
                                                   {:content popup-content
                                                    :heading label-txt}}}))
                            (td ""
                                {:id (str "td"
                                          label-no-spaces)})]
                           )
        ))
      (when (and (not= field-type
                       sub-form)
                 (not= field-type
                       popup))
       (swap! trs conj
        (tr
         [(td (label label-txt
                     {:id (str "lbl"
                               label-no-spaces)
                      :for (str "txt"
                                label-no-spaces)}))
          
          (td (if (= field-type
                     field-type-input)
               (input-field data-type
                            data
                            label-no-spaces
                            step
                            disabled)
               (if (= field-type
                      field-type-radio)
                (radio-field data
                             label-no-spaces
                             options
                             disabled)
                (if (= field-type
                       field-type-checkbox)
                 (checkbox-field data
                                 label-no-spaces
                                 options
                                 disabled)
                 (if (= field-type
                        field-type-textarea)
                  (textarea-field data
                                  label-no-spaces
                                  disabled)
                  (if (= field-type
                         field-type-image)
                   (image-field data
                                label-no-spaces
                                disabled)
                   "")))
                ))
           )
          (td ""
              {:id (str "td"
                        label-no-spaces)})]
            ))
       )
      )
   )
  (swap! trs conj (tr
                       [(td
                             (input
                                  ""
                                  {:id "btnCancel"
                                   :type "button"
                                   :value "Cancel"
                                   :style {:float "right"}}
                                  {:onclick {:evt-fn table-fn
                                             :evt-p conf}}))
                        (td
                             (input
                                  ""
                                  {:id (str "btn"
                                            action)
                                   :type "button"
                                   :value action}
                                  {:onclick {:evt-fn action-fn
                                             :evt-p (if action-fn-param
                                                     action-fn-param
                                                     conf)}}
                              ))
                        (td)]))
  @trs))

(defn- generate-form
  "Generate entity form"
  [xhr
   ajax-params]
  (let [{{{entity-type :entity-type} :entity-conf} :conf} ajax-params
        table-node (gen
                    (div
                         (table
                              (generate-form-trs
                               xhr
                               ajax-params))
                         {:class "entity"}))]
   (md/fade-out-and-fade-in ".content"
                             anim-time
                             table-node))
 )

(defn- entity-form
  "Request data about particular entity for display, edit/update"
  [conf
   sl-node]
  (let [from-details (:from-details conf)
        conf (assoc conf :from-details nil)
        ent-id (:ent-id conf)
        entity (:entity-conf conf)
        ent-id-key (:entity-id entity)
        entity-type (:entity-type entity)
        entity-fields (:entity-fields entity)
        request-body {:entity-type  entity-type
                      :entity-filter  {ent-id-key ent-id}}]
   (ajax
    {:url                  get-entity-url
     :success-fn           generate-form
     :entity               request-body
     :conf                 conf}))
  )

(defn create-entity
  "Call generate-form function with create entity parameters"
  [conf]
  (generate-form nil
                {:conf (assoc conf
                              :form-type  "Create"
                              :action     "Insert"
                              :action-fn  insert-update-entity)}))

(defn- edit-entity-from-table
  "Call entity-form function from generated entities table with edit entity parameters"
  [conf
   sl-node]
  (entity-form (assoc conf
                      :form-type  "Edit"
                      :action     "Update"
                      :action-fn  insert-update-entity)
               sl-node))

(defn- edit-entity-from-details
  "Call entity-form function from generated details form with edit entity parameters"
  [conf
   sl-node]
  (entity-form (assoc conf
                      :form-type     "Edit"
                      :action        "Update"
                      :action-fn     insert-update-entity
                      :from-details  true)
               sl-node))

(defn- entity-details
  "Call entity-form function from generated entities table with details entity parameters"
  [conf
   sl-node]
  (entity-form (assoc conf
                      :form-type "Details"
                      :action "Edit"
                      :action-fn edit-entity-from-details
                      :action-fn-param (assoc conf
                                              :form-type  "Edit"
                                              :action     "Update"
                                              :action-fn  insert-update-entity))
               sl-node))

(defn- entity-delete-success
  "Entity delete success"
  [xhr
   {conf :conf
    {table-fn :table-fn} :conf}]
  (table-fn conf))

(defn- entity-delete
  "Request entity to be deleted from server"
  [conf
   sl-node]
  (let [entity (:entity-conf conf)
        ent-id (:ent-id conf)
        ent-id-key (:entity-id entity)
        entity-type (:entity-type entity)
        request-body {:entity-type  entity-type
                      :entity-filter  {ent-id-key ent-id}}]
   (ajax
    {:url delete-entity-url
     :request-method "DELETE"
     :success-fn entity-delete-success
     :entity request-body
     :conf conf}))
  )

(defn- entity-table-success
 "Generate entity table after retrieving entities"
 [xhr
  {conf :conf
   {table-fn :table-fn} :conf}]
 (let [table-class (or (:table-class conf) "entities")
       columns-styles (:columns-styles conf)
       response (get-response xhr)
       entities (:data response)
       pagination (:pagination response)
       render-in (:render-in conf)
       animation (:animation conf)
       animation-duration (:animation-duration conf)
       actions []
       actions (if (:details conf)
                (conj actions
                      ["details"
                       entity-details
                       conf])
                actions)
       actions (if (:edit conf)
                (conj actions
                      ["edit"
                       edit-entity-from-table
                       conf])
                actions)
       actions (if (:delete conf)
                (conj actions
                      ["delete"
                       entity-delete
                       conf])
                actions)]
  (if (empty? entities)
   (let [table-node (gen
                     (div
                          "No entities"
                          {:class table-class}))]
    (md/fade-out-and-fade-in
     render-in
     animation-duration
     table-node))
   (let [table-node (gen
                     (div
                      (table
                           [(generate-thead
                             columns-styles
                             table-class
                             actions
                             pagination
                             conf)
                            (generate-tbody
                             columns-styles
                             entities
                             actions)])
                      {:class table-class}))]
       (md/fade-out-and-fade-in
        render-in
        animation-duration
        table-node))
   ))
 )

(defn gen-table
 "Generate table with data
 
 cell-style     Represents vector of maps with attributes
                 [{:content    \"Name\"
                   :title      \"Name\"
                   :header     [[\"width\"      \"100px\"]
                                [\"text-align\" \"center\"]]
                   :column     [[\"width\"      \"100%\"]
                                [\"text-align\" \"left\"]]
                   :colspan    2}]
 data-vectors   Represents vector of vectors with n elements that represent
                 values of table columns
                  example of data format:
                   [[\"data of column 1 row 1\"
                     \"data of column 2 row 1\"
                     {:title \"title of column 3 row 1\"
                      :data  \"data of column 3 row 1\"}]
                    [\"data of column 1 row 2\"
                     \"data of column 2 row 2\"
                     {:title \"title of column 3 row 2\"
                      :data  \"data of column 3 row 2\"}]]
 table-class    Represents class of div that contains table that will be generated"
 [conf]
 (ajax
  {:url get-entities-url
   :success-fn entity-table-success
   :entity (:table-conf conf)
   :conf conf}))

