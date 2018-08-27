(ns framework-lib.core
  (:require [ajax-lib.core :refer [ajax get-response]]
            [js-lib.core :as md]
            [utils-lib.core :as utils]
            [htmlcss-lib.core :refer [gen stl anmtn slctr
                                      table thead tbody tr th td
                                      button label input div h3
                                      textarea select option img a]]
            [cljs.reader :as reader]
            [clojure.string :as cstr]
            [language-lib.core :refer [get-label]]))

(def get-entities-url
     "/clojure/get-entities")

(def get-entity-url
     "/clojure/get-entity")

(def update-entity-url
     "/clojure/update-entity")

(def insert-entity-url
     "/clojure/insert-entity")

(def delete-entity-url
     "/clojure/delete-entity")

(defn render-img
  "Render uploaded image"
  [{file-id :file-id
    img-id :img-id
    hidden-id :hidden-id}]
  (let [file-field (md/query-selector
                     (str
                       "#"
                       file-id))
        file-field-parent (md/get-parent
                            file-field)
        file (aget (aget file-field "files") 0)
        img (md/query-selector
              (str
                "#"
                img-id))
        hidden-input (md/query-selector
                       (str
                         "#"
                         hidden-id))
        fileReader (js/FileReader.)
        onload (aset fileReader "onload"
                 ((fn [aimg
                       ahidden-input]
                    (fn [e]
                      (aset aimg "src" (aget (aget e "target") "result"))
                      (aset ahidden-input "value" (aget (aget e "target") "result"))
                     ))
                    img
                    hidden-input))
        dataURL (.readAsDataURL fileReader file)]))

(defn image-field
  "Render img html field and hidden input field"
  [content
   attrs
   & [evts
      dyn-attrs]]
  (let [id (:id attrs)
        data (:value attrs)
        hidden-id (str
                    "hidden"
                    id)
        file-id (str
                  "file"
                  id)
        f-attrs (conj
                  attrs
                  {:id file-id
                   :name file-id
                   :type "file"})]
    [(div
       [(img
          ""
          {:id id
           :name id
           :style {:max-width "100px"
                   :max-height "100px"}
           :src data})
        (input
          ""
          {:id hidden-id
           :type "hidden"
           :value data})])
     (div
       (input
         ""
         f-attrs
         {:onchange {:evt-fn render-img
                     :evt-p {:file-id file-id
                             :img-id id
                             :hidden-id hidden-id}}
          }))]
   ))

(defn select-field
  "Render select field"
  [content
   attrs
   evts
   & [dyn-attrs]]
  (select
    (let [options (atom [])]
      (doseq [[opt-val
               opt-lbl] content]
        (swap!
          options
          conj
          (option
            opt-lbl
            {:value opt-val}))
       )
      @options)
    attrs))

(defn textarea-field
  "Render select field"
  [content
   attrs
   evts
   & [dyn-attrs]]
  (if (and (seqable? content)
           (empty? content))
    (let [value (:value attrs)]
      (textarea
        value
        attrs))
    (textarea
      content
      attrs))
 )

(def input-types
     [{:type #{"text"
               "date"
               "password"
               "number"
               "email"
               "radio"
               "checkbox"}
       :input-el input}
      {:type #{"textarea"}
       :input-el textarea-field}
      {:type #{"select"}
       :input-el select-field}
      {:type #{"img"}
       :input-el image-field}])

(defn find-input-fn
  ""
  [field-type
   i]
  (when (< i (count input-types))
    (let [{input-types :type
           input-el :input-el} (get input-types i)]
      (if (contains?
            input-types
            field-type)
        input-el
        (recur
          field-type
          (inc i))
       ))
   ))

(defn generate-field
  ""
  [id
   value
   field-type
   & [attrs
      evts
      options]]
  (let [input-fn (find-input-fn
                   field-type
                   0)
        attrs (conj
                {:name id
                 :type field-type}
                attrs)]
    (if-let [options options]
      (let [html-options (atom [])]
        (doseq [option options]
          (let [attrs (assoc
                        attrs
                        :value
                        option
                        :id
                        (str
                          id
                          option))
                attrs (if (or (= value
                                 option)
                              (some
                                #{option}
                                value))
                        (assoc
                          attrs
                          :checked
                          "checked")
                        attrs)]
            (swap!
              html-options
              conj
              (div
                [(input-fn
                   ""
                   attrs
                   evts)
                 (label
                   option)])
             )
           ))
        @html-options)
      (let [dyn-attrs (if (= field-type
                             "number")
                        {:valueAsNumber value}
                        (if (= field-type
                               "date")
                          {:valueAsDate value}
                          {:value value}))]
        (input-fn
          ""
          (assoc
            attrs
            :id
            id)
          evts
          dyn-attrs))
     ))
 )

(defn close-popup
  "Close popup"
  []
  (md/remove-element
    "#popup-window")
  (md/remove-element
    "#popup-background"))

(defn popup-fn
  "Append generated popup to body"
  [{content :content
    heading :heading}]
  (md/append-element
    "body"
    (gen
      (div
         [(div
            ""
            {:style {:position "absolute"
                     :width "100%"
                     :height "100%"
                     :opacity "0.2"
                     :background-color "black"}})
          (div
            [(input
               ""
               {:id "close-btn"
                :style {:float "right"
                        :margin-top "10px"
                        :border-radius "15px"
                        :padding "0px 4px"}
                :value "X"
                :type "button"}
               {:onclick {:evt-fn close-popup}})
             (div
               (h3
                 heading)
               {:id "popup-heading"
                :style {:text-align "center"}})
             (div
               content
               {:id "popup-content"})]
            {:id "popup-window"
             :style {:border "5px solid white"
                     :border-radius "15px"
                     :padding "0 15px 15px 15px"
                     :z-index "0"
                     :background-color "white"}})]
         {:id "popup-background"
          :style {:position "absolute"
                  :width "100%"
                  :height "100%"
                  :top "0"
                  :display "grid"
                  :justify-content "center"
                  :align-content "center"
                  :color "black"
                  :z-index 1}}))
   ))

(defn framework-default-error
  ""
  [xhr]
  (let [response (get-response xhr)
        message (:message response)
        status (:status response)]
    (popup-fn
      {:heading status
       :content message}))
 )

(def default-th-td-style
     {:width "auto"
      :white-space "nowrap"
      :text-align "center"
      :text-overflow "ellipsis"
      :overflow "hidden"
      :padding "0 5px"})

(defn- generate-ths
  "Generate th and append style for that th and td column"
  [columns
   actions]
  (let [ths (atom [])
        projection (:projection columns)
        style (:style columns)
        actions (count actions)]
    (doseq [column projection]
      (let [column-style (column style)
            content (:content column-style)
            th-column (:th column-style)
            th-style (conj
                       default-th-td-style
                       (:style th-column))
            th-attrs (dissoc th-column :style)
            th-attrs (when (not
                             (contains?
                               th-attrs
                               :title))
                       (assoc
                         th-attrs
                         :title
                         content))]
        (swap!
          ths
          conj
          (th
            (div
              content
              {:style th-style})
           th-attrs))
       ))
    (when (< 0 actions)
      (swap!
        ths
        conj
        (th
          (div
            (get-label 9))
          {:colspan actions}))
     )
    @ths))

(defn- handle-paging
  "Handle click event on pagination link"
  [{conf :conf
    {query :query
     table-fn :table-fn} :conf
    pagination :pagination
    page :page}]
  (when (= page "first")
    (table-fn
      (update-in
        conf
        [:query]
        assoc
        :current-page 0))
   )
  (when (= page "previous")
    (table-fn
      (update-in
        conf
        [:query]
        assoc
        :current-page (dec (:current-page query))
        ))
   )
  (when (= page "next")
    (table-fn
      (update-in
        conf
        [:query]
        assoc
        :current-page (inc (:current-page query))
        ))
   )
  (when (= page "last")
    (table-fn
      (update-in
        conf
        [:query]
        assoc
        :current-page
        (dec
          (utils/round-up
            (:total-row-count pagination)
            (:rows pagination))
         ))
     ))
  (when (and (not= page "first")
             (not= page "previous")
             (not= page "next")
             (not= page "last"))
   (table-fn
     (update-in
       conf
       [:query]
       assoc
       :current-page (dec (js/parseInt page))
       ))
   ))

(defn- generate-pagination
  "Generate pagination row in thead"
  [current-page
   number-of-pages
   show-link
   assoc-page]
  (let [page-vector (atom [])]
    (swap!
      page-vector
      (fn [atom-val seq] (apply conj atom-val seq))
      (if (or (= show-link
                 2)
              (= show-link
                 3))
        [(div
           (a
             "first"
             {:page "first"}
             (assoc-page "first")))
         (div
           (a
             "previous"
             {:page "previous"}
             (assoc-page "previous"))
          )]
        [(div) (div)]))
    (when (and (= current-page (dec number-of-pages))
               (< -1 (dec (dec current-page))
                ))
      (swap! page-vector conj (div
                                (a (dec current-page)
                                   {:page (dec current-page)}
                                   (assoc-page (dec current-page))
                                 ))
       ))
    (when (< -1 (dec current-page))
      (swap! page-vector conj (div
                               (a current-page
                                  {:page current-page}
                                  (assoc-page current-page))
                               ))
     )
    (swap! page-vector conj (div (inc current-page)
                                 {:class "current-page"}))
    (when (< (inc current-page) number-of-pages)
      (swap! page-vector conj (div
                               (a
                                    (inc (inc current-page))
                                    {:page (inc (inc current-page))}
                                    (assoc-page (inc (inc current-page))
                                     ))
                               ))
     )
    (when (and (= current-page 0)
               (< (inc (inc current-page)) number-of-pages))
      (swap! page-vector conj (div
                               (a (inc (inc (inc current-page))
                                   )
                                  {:page (inc (inc (inc current-page))
                                          )}
                                  (assoc-page (inc (inc (inc current-page))
                                               ))
                                ))
       ))
    (swap!
      page-vector
      (fn [atom-val seq] (apply conj atom-val seq))
      (if (or (= show-link
                 1)
              (= show-link
                 3))
       [(div
         (a "next"
            {:page "next"}
            (assoc-page "next"))
         )
        (div
         (a "last"
            {:page "last"}
            (assoc-page "last"))
         )]
       [(div) (div)]))
    @page-vector))

(defn- generate-thead
  "Generate thead for table"
  [table-class
   columns
   actions
   pagination
   conf]
  (thead
    [(tr
       (generate-ths
         columns
         actions))
     (when-let [pagination pagination]
       (tr
         (th
           (div
             (let [current-page (:current-page pagination)
                   rows (:rows pagination)
                   total-row-count (:total-row-count pagination)
                   first-page-index 0
                   second-page-index 1
                   number-of-pages (utils/round-up
                                     total-row-count
                                     rows)
                   last-page-index (dec number-of-pages)
                   one-before-last (dec last-page-index)
                   assoc-page (fn [page]
                               {:onclick
                                {:evt-fn handle-paging
                                 :evt-p {:conf conf
                                         :pagination pagination
                                         :page page}}})
                   condition-i (< number-of-pages 4)
                   condition-ii (= current-page
                                   first-page-index)
                   condition-iii (= current-page
                                    last-page-index)
                   pagination-row (atom nil)]
               (when condition-i
                 (reset!
                   pagination-row
                   (generate-pagination
                     current-page
                     number-of-pages
                     0 ; bez prikaza
                     assoc-page))
                )
               (when (and (not condition-i)
                          condition-ii)
                 (reset!
                   pagination-row
                   (generate-pagination
                     current-page
                     number-of-pages
                     1 ; zadnja dva se prikazuju
                     assoc-page))
                )
               (when (and (not condition-i)
                          (not condition-ii)
                          condition-iii)
                 (reset!
                   pagination-row
                   (generate-pagination
                     current-page
                     number-of-pages
                     2 ; prva dva se prikazuju
                     assoc-page))
                )
               (when (and (not condition-i)
                          (not condition-ii)
                          (not condition-iii))
                 (reset!
                   pagination-row
                   (generate-pagination
                     current-page
                     number-of-pages
                     3 ; svi se prikazuju
                     assoc-page))
                )
               @pagination-row)
            {:class "pagination"})
           {:colspan (+ (count actions)
                        (count (:projection columns))
                      )})
        ))]
   ))

(defn- generate-tbody
  "Generate tbody for table"
  [entities
   columns
   actions]
  (tbody
    (let [trs (atom [])
          projection (:projection columns)
          style (:style columns)]
      (doseq [entity entities]
        (let [row-id (:_id entity)]
          (swap!
            trs
            conj
            (tr
              (let [tds (atom [])]
                (doseq [column projection]
                  (let [column-style (column style)
                        content (column entity)
                        content (if (= (type
                                         content)
                                       js/Date)
                                  (.toLocaleString
                                    content)
                                  content)
                        td-column (:td column-style)
                        td-style (conj
                                   default-th-td-style
                                   (:style td-column))
                        td-attrs (dissoc td-column :style)
                        td-attrs (assoc td-attrs :title content)]
                   (swap!
                     tds
                     conj
                     (td
                       (div
                         content
                         {:style td-style})
                      td-attrs))
                   ))
                (doseq [{label :label
                         evt-fn :evt-fn
                         evt-p :evt-p} actions]
                  (swap!
                    tds
                    conj
                    (td
                      (div
                        (input
                          ""
                          {:title label
                           :type "button"
                           :value label}
                          {:onclick
                            {:evt-fn evt-fn
                             :evt-p (assoc
                                      evt-p
                                      :ent-id
                                      row-id)}})
                       ))
                   ))
                @tds))
           ))
       )
      @trs))
 )

(defn- cb-checked?
  "Query current option if it is checked"
  [selected-cbs
   current-index
   option]
  (if (< current-index
         (count selected-cbs))
    (if (= option
           (get
             selected-cbs
             current-index))
      true
      (recur
        selected-cbs
        (inc current-index)
        option))
    false))

(defn- vec-contains?
  "If vector contains element"
  [data
   el
   index]
  (when (< index
           (count data))
    (if (= (get data index)
           el)
      true
      (recur
        data
        el
        (inc index))
     ))
 )

(defn- checkbox-field
  "Render checkbox fields with different options"
  [selected-cbs
   label-txt
   options
   disabled]
  (let [cbs (atom [])]
    (doseq [option options]
      (let [cb-name (str
                      "cb"
                      label-txt)
            id (str cb-name
                    (md/replace-all
                      option
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
       (swap!
         cbs
         conj
         (div [(input
                 ""
                 cb-attrs)
               (label
                 option
                 l-attrs)])
        ))
     )
    @cbs))

(defn- insert-update-entity-success
  "After successful entity insert or update display table again"
  [xhr
   {conf :conf
    {table-fn :table-fn} :conf}]
  (table-fn conf))

(defn validate-field
  ""
  [input-element
   validations
   id]
  (let [validity (aget
                   input-element
                   "validity")
        valid (aget
                validity
                "valid")]
    (when (not valid)
      (swap!
        validations
        conj
        [(str
           "#td"
           id)
         (aget
           input-element
           "validationMessage")])
     ))
 )

(defn- insert-update-entity
  "Insert or update entity"
  [conf]
  (let [action (:action conf)
        form-conf (:form-conf conf)
        entity-type (:type form-conf)
        fields (:fields form-conf)
        entity-keys (keys fields)
        table-node (md/query-selector
                     ".entity")
        request-body {:entity-type entity-type}
        hidden-id (md/query-selector-on-element
                    table-node
                    "#_id")
        entity-id (md/get-value
                    hidden-id)
        entity (atom {})
        specific-read-form (:specific-read-form form-conf)
        validations (atom [])]
    (if specific-read-form
      (specific-read-form
        entity)
      (doseq [e-key entity-keys]
        (let [field (e-key fields)
              label-txt (:label field)
              input-el (:input-el field)
              id (name e-key)
              message-selector (str
                                 "#td"
                                 id)]
          (let [message-el (md/query-selector-on-element
                             ".entity"
                             message-selector)]
            (md/set-inner-html
              message-el
              ""))
          (when (= input-el
                   "radio")
            (swap!
              entity
              assoc
              e-key
              (md/checked-value
                id))
            (validate-field
              (md/query-selector-on-element
                ".entity"
                (str
                  "input[name='"
                  id
                  "']"))
              validations
              id))
          (when (= input-el
                   "checkbox")
            (swap!
              entity
              assoc
              e-key
              (md/cb-checked-values
                id))
           )
          (when-let [input-element (md/query-selector-on-element
                                     table-node
                                     (str
                                       "#"
                                       id))]
            (when (= input-el
                     "img")
              (swap!
                entity
                assoc
                e-key
                (md/get-src
                  input-element))
             )
            (when (= input-el
                     "number")
              (swap!
                entity
                assoc
                e-key
                (md/get-value-as-number
                  input-element))
              (validate-field
                input-element
                validations
                id))
            (when (= input-el
                     "date")
              (swap!
                entity
                assoc
                e-key
                (md/get-value-as-date
                  input-element))
              (validate-field
                input-element
                validations
                id))
            (when (not
                    (or (= input-el
                           "img")
                        (= input-el
                           "number")
                        (= input-el
                           "date"))
                   )
              (swap!
                entity
                assoc
                e-key
                (md/get-value
                  input-element))
              (validate-field
                input-element
                validations
                id))
           ))
       ))
    (if (empty? @validations)
      (ajax
        {:url (if (empty? entity-id)
                insert-entity-url
                update-entity-url)
         :success-fn insert-update-entity-success
         :error-fn framework-default-error
         :entity (assoc
                   request-body
                   :entity @entity
                   :_id entity-id)
         :conf conf})
      (doseq [[message-selector
               validation-message] @validations]
        (let [message-el (md/query-selector-on-element
                           ".entity"
                           message-selector)]
          (md/set-inner-html
            message-el
            validation-message))
       ))
   ))

(defn- generate-form-trs
  "Generate form fields"
  [xhr
   {conf :conf
    {table-fn :table-fn
     form-type :form-type
     disabled :disabled
     action :action
     action-fn :action-fn
     action-p :action-p
     {entity-type :type
      entity-name :entity-name
      fields :fields
      entity-keys :fields-order} :form-conf} :conf}]
  (let [entity-name (or entity-name
                        entity-type)
        response (when-not (nil? xhr)
                   (get-response xhr))
        entity-data (:data response)
        conf (assoc
               conf
               :disabled
               false)
        conf (update-in
               conf
               [:query]
               assoc
               :entity-filter
               {})
        trs (atom [])]
    (swap!
      trs
      conj
      (tr
        (td
          (h3
            (str
              form-type
              " "
              entity-name))
          {:colspan 3}))
     )
    (swap!
      trs
      conj
      (tr
        (td
          (input
            ""
            {:id "_id"
             :name "_id"
             :type "hidden"
             :value (:_id entity-data)})
          {:colspan 3}))
     )
    (doseq [e-key entity-keys]
      (let [field-conf (e-key fields)
            label-txt (:label field-conf)
            attrs (:attrs field-conf)
            attrs (if disabled
                    (assoc
                      attrs
                      :disabled
                      true)
                    attrs)
            evts (:evts field-conf)
            id (name e-key)
            input-el (:input-el field-conf)
            options (:options field-conf)
            data (e-key entity-data)
            sub-form-trs (:sub-form-trs field-conf)]
        (when-let [sub-form-trs sub-form-trs]
          (doseq [sub-form-tr (sub-form-trs
                                entity-data
                                attrs)]
            (swap!
              trs
              conj
              sub-form-tr))
         )
        (when (not sub-form-trs)
          (swap!
            trs
            conj
            (tr
              [(td
                 (label
                   label-txt
                   {:id (str
                          "lbl"
                          id)})
                )
               (td
                 (generate-field
                   (name e-key)
                   data
                   input-el
                   attrs
                   evts
                   options))
               (td
                 ""
                 {:id (str
                        "td"
                        id)
                  :class "validationMessage"})]
             ))
         ))
     )
    (swap!
      trs
      conj
      (tr [(td
             (input
               ""
               {:id "btnCancel"
                :type "button"
                :value (get-label 12)
                :style {:float "right"}}
               {:onclick {:evt-fn table-fn
                          :evt-p conf}}))
           (td
             (when true ; check if user is allowed particular action
               (input
                 ""
                 {:id (str "btn"
                           action)
                  :type "button"
                  :value action}
                 {:onclick {:evt-fn action-fn
                            :evt-p (if action-p
                                     action-p
                                     conf)}})
              ))
           (td)])
     )
    @trs))

(defn- generate-form
  "Generate entity form"
  [xhr
   ajax-params]
  (let [table-node (gen
                     (div
                       (table
                         (generate-form-trs
                           xhr
                           ajax-params))
                       {:class "entity"}))]
   (md/remove-element-content
     ".content")
   (md/append-element
     ".content"
     table-node))
 )

(defn- entity-form
  "Request data about particular entity for display, edit/update"
  [conf]
  (let [ent-id (:ent-id conf)
        entity (:form-conf conf)
        ent-id-key (:id entity)
        entity-type (:type entity)
        request-body {:entity-type entity-type
                      :entity-filter {ent-id-key ent-id}}]
    (ajax
      {:url get-entity-url
       :success-fn generate-form
       :error-fn framework-default-error
       :entity request-body
       :conf conf}))
 )

(defn insert-action
  ""
  [conf]
  (conj
    conf
    {:form-type (get-label 4)
     :action (get-label 10)
     :action-fn insert-update-entity}))

(defn create-entity
  "Call generate-form function with create entity parameters"
  [conf]
  (generate-form
    nil
    {:conf
      (insert-action
        conf)})
 )

(defn update-action
  ""
  [conf]
  (conj
    conf
    {:form-type (get-label 7)
     :action (get-label 11)
     :action-fn insert-update-entity}))

(defn edit-entity
  "Call entity-form function"
  [conf]
  (entity-form
    (update-action
      conf))
 )

(defn edit-action
  ""
  [conf]
  (conj
    conf
    {:form-type (get-label 6)
     :disabled true
     :action (get-label 7)
     :action-fn edit-entity
     :action-p (update-action
                 conf)})
 )

(defn entity-details
  "Call entity-form function from generated entities table with details entity parameters"
  [conf]
  (entity-form
    (edit-action
      conf))
 )

(defn entity-delete-success
  "Entity delete success"
  [xhr
   {conf :conf
    {table-fn :table-fn} :conf}]
  (table-fn conf))

(defn entity-delete
  "Request entity to be deleted from server"
  [conf]
  (let [entity (:form-conf conf)
        ent-id (:ent-id conf)
        ent-id-key (:id entity)
        entity-type (:type entity)
        request-body {:entity-type entity-type
                      :entity-filter {ent-id-key ent-id}}
        conf (update-in
               conf
               [:query]
               assoc
               :entity-filter
               {})]
   (ajax
     {:url delete-entity-url
      :request-method "DELETE"
      :success-fn entity-delete-success
      :error-fn framework-default-error
      :entity request-body
      :conf conf}))
 )

(defn search-entities-fn
  "Search entities by fields from configuration"
  [ajax-params]
  (let [gen-table-fn (:gen-table-fn ajax-params)
        conf (:conf ajax-params)
        search-value (md/get-value
                       "#txtSearchTable")
        conf (let [search-fields (:search-fields conf)
                    or-vector (atom [])]
               (doseq [search-field search-fields]
                 (swap!
                   or-vector
                   conj
                   {search-field {"$regex" search-value
                                  "$options" "i"}}))
               (update-in
                 conf
                 [:query]
                 assoc
                 :entity-filter
                 {"$or" @or-vector}))]
    (gen-table-fn
      conf
      nil
      nil
      true))
 )

(defn entity-table-success
  "Generate entity table after retrieving entities"
  [xhr
   ajax-params]
  (let [{conf :conf} ajax-params
        search-on (:search-on conf)
        search-call (:search-call ajax-params)
        table-class (or (:table-class conf)
                        "entities")
        table-selector (str
                         "."
                         table-class)
        columns (:columns conf)
        render-in (:render-in conf)
        response (get-response xhr)
        entities (:data response)
        pagination (:pagination response)
        default-actions {:details {:label (get-label 6)
                                   :evt-fn entity-details
                                   :evt-p conf}
                         :edit {:label (get-label 7)
                                :evt-fn edit-entity
                                :evt-p conf}
                         :delete {:label (get-label 8)
                                  :evt-fn entity-delete
                                  :evt-p conf}}
        actions-conf (:actions conf)
        actions (atom [])]
    (doseq [action actions-conf]
      (if-let [action (get
                        default-actions
                        action)]
        (swap!
          actions
          conj
          action)
        (swap!
          actions
          conj
          action))
     )
    (let [table-node (gen
                       [(when (and search-on
                                   (not search-call))
                          (div
                            (table
                              (tr
                                [(td
                                   (label
                                     (get-label 13))
                                  )
                                 (td
                                   (input
                                     ""
                                     {:id "txtSearchTable"}
                                     {:onkeyup {:evt-fn search-entities-fn
                                                :evt-p ajax-params}}))]
                               ))
                            {:class "search"})
                         )
                        (if (empty? entities)
                          (div
                            "No entities"
                            {:class table-class})
                          (div
                            (table
                              [(generate-thead
                                 table-class
                                 columns
                                 @actions
                                 pagination
                                 conf)
                               (generate-tbody
                                 entities
                                 columns
                                 @actions)])
                           {:class table-class}))]
                      )]
      (if search-call
        (md/remove-element
          table-selector)
        (md/remove-element-content
          render-in))
      (md/append-element
        render-in
        table-node))
   ))

(defn gen-table
  "Generate table with data"
  [conf
   & [sl-node
      event
      search-call]]
  (ajax
    {:url get-entities-url
     :success-fn entity-table-success
     :error-fn framework-default-error
     :entity (:query conf)
     :conf conf
     :gen-table-fn gen-table
     :search-call search-call}))

