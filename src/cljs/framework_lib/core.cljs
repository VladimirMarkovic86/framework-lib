(ns framework-lib.core
  (:require [ajax-lib.core :refer [ajax get-response base-url]]
            [js-lib.core :as md]
            [utils-lib.core :as utils]
            [htmlcss-lib.core :refer [gen table thead tbody tr th td
                                      label input div h3 textarea
                                      select option img a fieldset
                                      legend form span svg rect]]
            [cljs.reader :as reader]
            [clojure.string :as cstring]
            [clojure.set :as cset]
            [language-lib.core :refer [get-label]]
            [validator-lib.core :refer [validate-field validate-input]]
            [common-middle.request-urls :as rurls]
            [common-middle.session :as cms]
            [common-middle.functionalities :as fns]
            [framework-lib.side-bar-menu :as sbm]))

(def read-preferences-a-fn
     (atom nil))

(def save-preferences-a-fn
     (atom nil))

(def is-called-read-preferences-a
     (atom false))

(defn render-img
  "Render uploaded image"
  [{file-id :file-id
    img-id :img-id}]
  (let [file-field (md/query-selector
                     (str
                       "#"
                       file-id))
        file (aget
               (.-files
                 file-field)
               0)
        img (md/query-selector
              (str
                "#"
                img-id))
        fileReader (js/FileReader.)
        onload (aset
                 fileReader
                 "onload"
                 ((fn [aimg]
                    (fn [e]
                      (aset
                        aimg
                        "src"
                        (.-result
                          (.-target
                            e))
                       ))
                   )
                   img))
        dataURL (.readAsDataURL
                  fileReader
                  file)])
 )

(defn image-field
  "Render img html field and hidden input field"
  [content
   attrs
   & [evts
      dyn-attrs]]
  (let [id (:id attrs)
        data (or (:value attrs)
                 (:value dyn-attrs))
        file-id (str
                  "file"
                  id)
        f-attrs {:id file-id
                 :type "file"}
        f-attrs (if (map?
                      attrs)
                  (conj
                    attrs
                    f-attrs)
                  f-attrs)
        f-evts {:onchange {:evt-fn render-img
                           :evt-p {:file-id file-id
                                   :img-id id}}}
        f-evts (if (map?
                     evts)
                 (conj
                   evts
                   f-evts)
                 f-evts)]
    [(div
       [(img
          ""
          {:id id
           :src data})])
     (div
       [(input
          ""
          f-attrs
          f-evts)
        (span)])]
   ))

(defn select-field
  "Render select field"
  [content
   data
   attrs
   evts
   & [dyn-attrs]]
  (select
    (let [options (atom [])]
      (when-not (:multiple attrs)
        (swap!
          options
          conj
          (option
            (get-label 33)
            {:value ""}))
       )
      (doseq [option-el content]
        (let [[opt-lbl
               opt-val
               opt-title] (if (vector?
                              option-el)
                            option-el
                            [option-el
                             option-el
                             option-el])
              opt-attrs {:value opt-val
                         :title opt-title}
              opt-attrs (if (and data
                                 (or (and (string?
                                            data)
                                          (= (.toLowerCase
                                               data)
                                             (.toLowerCase
                                               opt-val))
                                      )
                                     (and (coll?
                                            data)
                                          (some
                                            #{opt-val}
                                            data))
                                  ))
                          (assoc
                            opt-attrs
                            :selected
                            true)
                          opt-attrs)]
          (swap!
            options
            conj
            (option
              opt-lbl
              opt-attrs))
         ))
      @options)
    attrs
    evts
    dyn-attrs))

(defn textarea-field
  "Render select field"
  [content
   attrs
   evts
   & [dyn-attrs]]
  (if (and (seqable?
             content)
           (empty?
             content))
    (let [value (:value dyn-attrs)]
      (textarea
        value
        attrs
        evts))
    (textarea
      content
      attrs
      evts))
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
  "Find fn that generates right input field type"
  [field-type
   i]
  (when (< i
           (count
             input-types))
    (let [{input-types :type
           input-el :input-el} (get
                                 input-types
                                 i)]
      (if (contains?
            input-types
            field-type)
        input-el
        (recur
          field-type
          (inc
            i))
       ))
   ))

(defn generate-field
  "Generate input field of particular type"
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
      (let [html-options (atom [])
            options-a (atom options)]
        (when (fn?
                options)
          (reset!
            options-a
            (options))
         )
        (if (= field-type
               "select")
          (let [attrs (assoc
                        attrs
                        :id
                        id)
                attrs (dissoc
                        attrs
                        :type)
                evts (assoc
                       evts
                       :onchange {:evt-fn validate-input})]
            (input-fn
              @options-a
              value
              attrs
              evts))
          (do
            (doseq [option @options-a]
              (let [[op-label
                     op-value] (if (vector?
                                     option)
                                 option
                                 [option
                                  option])
                    attrs (assoc
                            attrs
                            :value op-value
                            :id (str
                                  id
                                  (md/replace-all
                                    op-value
                                    " "
                                    ""))
                            :title op-label)
                    attrs (if (and value
                                   (or (and (string?
                                              value)
                                            (= (.toLowerCase
                                                 value)
                                               (.toLowerCase
                                                 op-value))
                                        )
                                       (and (coll?
                                              value)
                                            (some
                                              #{op-value}
                                              value))
                                    ))
                            (assoc
                              attrs
                              :checked
                              "checked")
                            attrs)
                    evts (assoc
                           evts
                           :onclick {:evt-fn validate-input})]
                (swap!
                  html-options
                  conj
                  (label
                    [op-label
                     (input-fn
                       ""
                       attrs
                       evts)])
                 ))
             )
            (div
              @html-options
              {:class "selection-items"}))
         ))
      (let [dyn-attrs (if (= field-type
                             "number")
                        {:valueAsNumber value}
                        (if (= field-type
                               "date")
                          {:valueAsDate value}
                          {:value value}))
            evts (assoc
                   evts
                   :oninput {:evt-fn validate-input})
            attrs (assoc
                    attrs
                    :id id)]
        (input-fn
          ""
          attrs
          evts
          dyn-attrs))
     ))
 )

(defn close-popup
  "Close popup"
  []
  (md/remove-element
    ".popup-modal"))

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
           {:class "popup-background"})
         (div
           [(div
              [(div
                 heading
                 {:class "popup-heading"
                  :title heading})
               (div
                 "X"
                 {:class "close-btn"}
                 {:onclick {:evt-fn close-popup}})]
              {:class "popup-top-bar"})
            (div
              content
              {:class "popup-content"})]
           {:class "popup-window"})]
        {:class "popup-modal"}))
   ))

(defn framework-default-error
  "Framework default error function"
  [xhr]
  (let [response (get-response xhr)
        message (:message response)
        status (:status response)
        message-code (:message-code response)
        status-code (:status-code response)
        message (if (and (utils/is-number?
                           message-code)
                         (pos?
                           message-code))
                  (get-label
                    message-code)
                  message)
        status (if (and (utils/is-number?
                          status-code)
                        (pos?
                          status-code))
                 (get-label
                   status-code)
                 status)]
    (popup-fn
      {:heading status
       :content message}))
 )

(defn generate-ths
  "Generate th and append style for that th and td column"
  [columns
   actions]
  (let [ths (atom [])
        projection (:projection columns)
        display-fields (:display-fields columns)
        projection (if display-fields
                     display-fields
                     projection)
        style (:style columns)
        actions (count
                  actions)]
    (doseq [column projection]
      (let [column-style (column
                           style)
            content (:content column-style)
            th-attrs (:th column-style)
            th-attrs (if-not (contains?
                               th-attrs
                               :title)
                       (assoc
                         th-attrs
                         :title
                         content)
                       th-attrs)]
        (swap!
          ths
          conj
          (th
            content
            th-attrs))
       ))
    (when (< 0
             actions)
      (swap!
        ths
        conj
        (th
          (get-label 9)
          {:colspan actions
           :title (get-label 9)
           :style {:max-width (str
                                (* actions
                                   10)
                                "%")}})
       ))
    @ths))

(defn handle-paging
  "Handle click event on pagination link"
  [{conf :conf
    {query :query
     table-fn :table-fn} :conf
    pagination :pagination
    page :page}]
  (when (= page
           "first")
    (table-fn
      (update-in
        conf
        [:query]
        assoc
        :current-page 0))
   )
  (when (= page
          "previous")
    (table-fn
      (update-in
        conf
        [:query]
        assoc
        :current-page (dec
                        (:current-page query))
       ))
   )
  (when (= page
           "next")
    (table-fn
      (update-in
        conf
        [:query]
        assoc
        :current-page (inc
                        (:current-page query))
       ))
   )
  (when (= page
           "last")
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
  (when (and (not= page
                   "first")
             (not= page
                   "previous")
             (not= page
                   "next")
             (not= page
                   "last"))
    (table-fn
      (update-in
        conf
        [:query]
        assoc
        :current-page (dec (js/parseInt page))
       ))
   ))

(defn generate-pagination
  "Generate pagination row in thead"
  [current-page
   number-of-pages
   show-link
   assoc-page]
  (let [page-vector (atom [])]
    (swap!
      page-vector
      (fn [atom-val
           coll]
        (apply
          conj
          atom-val
          coll))
      (if (or (= show-link
                 2)
              (= show-link
                 3))
        [(div
           (get-label 34)
           nil
           (assoc-page
             "first"))
         (div
           (get-label 35)
           nil
           (assoc-page
             "previous"))]
        [(div)
         (div)])
     )
    (when (and (= current-page
                 (dec
                   number-of-pages))
               (< -1
                  (- current-page
                     2))
           )
      (swap!
        page-vector
        conj
        (div
          (dec
            current-page)
          nil
          (assoc-page
            (dec
              current-page))
         ))
     )
    (when (< -1
             (dec
               current-page))
      (swap!
        page-vector
        conj
        (div
          current-page
          nil
          (assoc-page
            current-page))
       ))
    (swap!
      page-vector
      conj
      (div
        (inc
          current-page)
        {:class "current-page"}))
    (when (< (inc
               current-page)
             number-of-pages)
      (swap!
        page-vector
        conj
        (div
          (inc (inc current-page))
          nil
          (assoc-page
            (+ current-page
               2))
         ))
     )
    (when (and (= current-page
                  0)
               (< (+ current-page
                     2)
                  number-of-pages))
      (swap!
        page-vector
        conj
        (div
          (+ current-page
             3)
          nil
          (assoc-page
            (+ current-page
               3))
         ))
     )
    (swap!
      page-vector
      (fn [atom-val
           coll]
        (apply
          conj
          atom-val
          coll))
      (if (or (= show-link
                 1)
              (= show-link
                 3))
       [(div
          (get-label 36)
          nil
          (assoc-page
            "next"))
        (div
          (get-label 37)
          nil
          (assoc-page
            "last"))]
       [(div)
        (div)])
     )
    @page-vector))

(defn switch-view
  "Switches table view to card view"
  [evt-p
   element
   event]
  (let [{conf :conf
         new-card-columns :new-card-columns
         new-table-rows :new-table-rows} evt-p
        {table-fn :table-fn
         query-fn :query-fn
         {card-columns-a :card-columns-a
          table-rows-a :table-rows-a} :preferences} conf
        void (reset!
               card-columns-a
               (or new-card-columns
                   @card-columns-a))
        void (reset!
               table-rows-a
               (or new-table-rows
                   @table-rows-a))
        conf (assoc
               conf
               :query
               (query-fn))]
    (table-fn
      conf)
    (when (fn?
            @save-preferences-a-fn)
      (@save-preferences-a-fn))
   ))

(defn svg-table-icon
  "Creates clojure maps for svg table icon"
  [& [evt
      entity-name]]
  (svg
    [(rect
       nil
       {:width "24"
        :height "6"
        :y "11"
        :x "2"})]
    {:width "28"
     :height "28"
     :selected-value "0"}
    evt))

(defn svg-one-column-icon
  "Creates clojure maps for svg one column icon"
  [& [evt]]
  (svg
    [(rect
       nil
       {:width "24"
        :height "24"
        :y "2"
        :x "2"})]
    {:width "28"
     :height "28"
     :selected-value "1"}
    evt))

(defn svg-two-column-icon
  "Creates clojure maps for svg two columns icon"
  [& [evt]]
  (svg
    [(rect
       nil
       {:width "11"
        :height "24"
        :y "2"
        :x "2"})
     (rect
       nil
       {:width "11"
        :height "24"
        :y "2"
        :x "15"})]
    {:width "28"
     :height "28"
     :selected-value "2"}
    evt))

(defn svg-three-column-icon
  "Creates clojure maps for svg three columns icon"
  [& [evt]]
  (svg
    [(rect
       nil
       {:width "6"
        :height "24"
        :y "2"
        :x "2"})
     (rect
       nil
       {:width "6"
        :height "24"
        :y "2"
        :x "11"})
     (rect
       nil
       {:width "6"
        :height "24"
        :y "2"
        :x "20"})]
    {:width "28"
     :height "28"
     :selected-value "3"}
    evt))

(defn svg-four-column-icon
  "Creates clojure maps for svg four columns icon"
  [& [evt]]
  (svg
    [(rect
       nil
       {:width "5"
        :height "24"
        :y "2"
        :x "1"})
     (rect
       nil
       {:width "5"
        :height "24"
        :y "2"
        :x "8"})
     (rect
       nil
       {:width "5"
        :height "24"
        :y "2"
        :x "15"})
     (rect
       nil
       {:width "5"
        :height "24"
        :y "2"
        :x "22"})]
    {:width "28"
     :height "28"
     :selected-value "4"}
    evt))

(defn svg-five-column-icon
  "Creates clojure maps for svg five columns icon"
  [& [evt]]
  (svg
    [(rect
       nil
       {:width "3"
        :height "24"
        :y "2"
        :x "2"})
     (rect
       nil
       {:width "3"
        :height "24"
        :y "2"
        :x "7"})
     (rect
       nil
       {:width "3"
        :height "24"
        :y "2"
        :x "12"})
     (rect
       nil
       {:width "3"
        :height "24"
        :y "2"
        :x "17"})
     (rect
       nil
       {:width "3"
        :height "24"
        :y "2"
        :x "22"})]
    {:width "28"
     :height "28"
     :selected-value "5"}
    evt))

(defn generate-row-number-dropdown-options
  "Generates row number dropdown options"
  [conf]
  (let [options-vector [1 2 3 4 5 10 20 25 50 100]
        div-options-vector (atom [])]
    (doseq [option options-vector]
      (swap!
        div-options-vector
        conj
        (div
          (str
            "x" option)
          nil
          {:onclick {:evt-fn switch-view
                     :evt-p {:conf conf
                             :new-card-columns nil
                             :new-table-rows option}}
           }))
     )
    @div-options-vector))

(defn generate-pagination-bar
  "Generates clojure maps that represent pagination bar"
  [pagination
   conf]
  (div
    [(div
       (div
         (let [current-page (:current-page pagination)
               rows (:rows pagination)
               total-row-count (:total-row-count pagination)
               first-page-index 0
               second-page-index 1
               number-of-pages (utils/round-up
                                 total-row-count
                                 rows)
               last-page-index (dec
                                 number-of-pages)
               one-before-last (dec
                                 last-page-index)
               assoc-page (fn [page]
                            {:onclick
                              {:evt-fn handle-paging
                               :evt-p {:conf conf
                                       :pagination pagination
                                       :page page}}
                             })
               condition-i (< number-of-pages
                              4)
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
       {:class "pagination-container"})
     (div
       [(div
          (case (deref
                  (get-in
                    conf
                    [:preferences
                     :card-columns-a]))
            0 (svg-table-icon)
            1 (svg-one-column-icon)
            2 (svg-two-column-icon)
            3 (svg-three-column-icon)
            4 (svg-four-column-icon)
            5 (svg-five-column-icon)
            (svg-table-icon))
          {:class "dropdown-selection dropdown-selection-columns"})
        (div
          [(svg-table-icon
             {:onclick {:evt-fn switch-view
                        :evt-p {:conf conf
                                :new-card-columns 0
                                :new-table-rows nil}}
              })
           (svg-one-column-icon
             {:onclick {:evt-fn switch-view
                        :evt-p {:conf conf
                                :new-card-columns 1
                                :new-table-rows nil}}
              })
           (svg-two-column-icon
             {:onclick {:evt-fn switch-view
                        :evt-p {:conf conf
                                :new-card-columns 2
                                :new-table-rows nil}}
              })
           (svg-three-column-icon
             {:onclick {:evt-fn switch-view
                        :evt-p {:conf conf
                                :new-card-columns 3
                                :new-table-rows nil}}
              })
           (svg-four-column-icon
             {:onclick {:evt-fn switch-view
                        :evt-p {:conf conf
                                :new-card-columns 4
                                :new-table-rows nil}}
              })
           (svg-five-column-icon
             {:onclick {:evt-fn switch-view
                        :evt-p {:conf conf
                                :new-card-columns 5
                                :new-table-rows nil}}
              })
           ]
          {:class "dropdown-menu dropdown-menu-columns"})]
       {:class "dropdown-container dropdown-container-columns"})
     (div
       [(div
          (case (deref
                  (get-in
                    conf
                    [:preferences
                     :table-rows-a]))
            1 (div
                "x1")
            2 (div
                "x2")
            3 (div
                "x3")
            4 (div
                "x4")
            5 (div
                "x5")
            10 (div
                "x10")
            20 (div
                "x20")
            25 (div
                "x25")
            50 (div
                "x50")
            100 (div
                  "x100")
            (div
              "x?"))
          {:class "dropdown-selection dropdown-selection-rows"})
        (div
          (generate-row-number-dropdown-options
            conf)
          {:class "dropdown-menu dropdown-menu-rows"})]
       {:class "dropdown-container dropdown-container-rows"})
     ]
    {:class "pagination-bar"}))

(defn generate-thead
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
           (generate-pagination-bar
             pagination
             conf)
           {:colspan (+ (count
                          actions)
                        (count
                          (if (:display-fields columns)
                            (:display-fields columns)
                            (:projection columns))
                         ))}
          ))
      )])
 )

(def entity-to-show-all
     "entityToShowAll")

(def show-all-to-entity
     "showAllToEntity")

(def no-change
     "nochange")

(def create-to-show-all
     "createToShowAll")

(defn handle-selected-menu-item
  "Handle menu selected items for framework"
  [{evt-fn :evt-fn
    evt-p :evt-p
    menu-change :menu-change}
   & [element
      event]]
  (let [selected-element (md/query-selector-on-element
                           ".root-div-menu"
                           ".is-selected")
        active-element (atom nil)]
    (when (= menu-change
             entity-to-show-all)
      (let [create-menu-item (.-nextElementSibling
                               selected-element)
            show-all-menu-item (.-nextElementSibling
                                 create-menu-item)
            new-selected-element (md/query-selector-on-element
                                   show-all-menu-item
                                   ":first-child")]
        (if show-all-menu-item
          (reset!
            active-element
            new-selected-element)
          (reset!
            active-element
            (md/query-selector-on-element
              create-menu-item
              ":first-child"))
         ))
     )
    (when (= menu-change
             show-all-to-entity)
      (let [parent-element (.-parentElement
                             selected-element)
            create-menu-item (.-previousElementSibling
                               parent-element)
            entity-menu-item (.-previousElementSibling
                               create-menu-item)]
        (if entity-menu-item
          (reset!
            active-element
            entity-menu-item)
          (reset!
            active-element
            create-menu-item))
       ))
    (when (= menu-change
             no-change)
      (reset!
        active-element
        selected-element))
    (when (= menu-change
             create-to-show-all)
      (let [parent-element (.-parentElement
                             selected-element)
            create-menu-item (.-nextElementSibling
                               parent-element)
            new-selected-element (md/query-selector-on-element
                                   create-menu-item
                                   ":first-child")]
        (reset!
          active-element
          new-selected-element))
     )
    (md/remove-class
      selected-element
      "is-selected")
    (md/add-class
      @active-element
      "is-selected"))
  (when (fn?
          evt-fn)
    (evt-fn
      evt-p
      element
      event))
 )

(defn format-date
  "Formats date depending on selected language"
  [date
   month
   year]
  (let [result (atom "")]
    (when (= @cms/selected-language
             "english")
      (swap!
        result
        str
        month
        "/"
        date
        "/"
        year))
    (when (= @cms/selected-language
             "serbian")
      (swap!
        result
        str
        date
        "."
        month
        "."
        year))
    @result))

(defn generate-tbody
  "Generate tbody for table"
  [entities
   columns
   actions]
  (tbody
    (let [trs (atom [])
          projection (:projection columns)
          display-fields (:display-fields columns)
          projection (if display-fields
                       display-fields
                       projection)
          style (:style columns)]
      (doseq [entity entities]
        (let [row-id (:_id entity)]
          (swap!
            trs
            conj
            (tr
              (let [tds (atom [])]
                (doseq [column projection]
                  (let [column-style (column
                                       style)
                        content (column
                                  entity)
                        content (if (= column
                                       :label-code)
                                  (let [label-code (:label-code entity)
                                        original-field (:original-field column-style)]
                                    (if (and label-code
                                             (< 0
                                                label-code))
                                      (get-label
                                        label-code)
                                      (original-field entity))
                                   )
                                  content)
                        labels (:labels column-style)
                        content (if labels
                                  (let [selected-set (cset/select
                                                       (fn [[label-txt
                                                             value]]
                                                         (and content
                                                              (or
                                                                (and (string?
                                                                       content)
                                                                     (= (.toLowerCase
                                                                          content)
                                                                        (.toLowerCase
                                                                          value))
                                                                 )
                                                                (and (coll?
                                                                       content)
                                                                     (some
                                                                       #{value}
                                                                       content))
                                                               ))
                                                        )
                                                       labels)
                                        [label-txt
                                         value] (first
                                                  selected-set)]
                                    label-txt)
                                  content)
                        content (if (= (type
                                         content)
                                       js/Date)
                                  (format-date
                                    (.getDate
                                      content)
                                    (inc
                                      (.getMonth
                                        content))
                                    (.getFullYear
                                      content))
                                  content)
                        td-attrs (:td column-style)
                        td-attrs (assoc
                                   td-attrs
                                   :title
                                   content)]
                   (swap!
                     tds
                     conj
                     (td
                       content
                       td-attrs))
                   ))
                (doseq [{label-txt :label
                         clazz :class
                         evt-fn :evt-fn
                         evt-p :evt-p
                         menu-change :menu-change} actions]
                  (swap!
                    tds
                    conj
                    (td
                      label-txt
                      {:class (str
                                "action "
                                clazz)
                       :title label-txt}
                      {:onclick
                        {:evt-fn handle-selected-menu-item
                         :evt-p {:evt-fn evt-fn
                                 :evt-p (assoc
                                          evt-p
                                          :ent-id
                                          row-id)
                                 :menu-change menu-change}}
                       }))
                 )
                @tds))
           ))
       )
      @trs))
 )

(defn escape-html-tags
  "Escapes html tags from < and > into &lt; and &gt;"
  [value]
  (if (and value
           (string?
             value))
    (let [value (cstring/replace
                  value
                  "<"
                  "&lt;")
          value (cstring/replace
                  value
                  ">"
                  "&gt;")]
      value)
    value))

(defn generate-card-view
  "Generates card view from entities"
  [pagination
   conf
   entities
   columns
   actions]
  (let [card-columns-a (get-in
                         conf
                         [:preferences
                          :card-columns-a])
        card-columns (if (instance?
                           Atom
                           card-columns-a)
                       @card-columns-a
                       3)
        card-container-vector (atom [])]
    (doseq [entity entities]
      (let [projection (:projection columns)
            display-fields (:display-fields columns)
            projection (if display-fields
                         display-fields
                         projection)
            row-id (:_id entity)
            card-content-vector (atom [])
            card-action-vector (atom [])]
        (doseq [column projection]
          (let [style (:style columns)
                column-style (column
                               style)
                property-name (:content column-style)
                property-value (column
                                 entity)
                property-value (if (= column
                                      :label-code)
                                 (let [label-code (:label-code entity)
                                       original-field (:original-field column-style)]
                                   (if (and label-code
                                            (< 0
                                               label-code))
                                     (get-label
                                       label-code)
                                     (original-field entity))
                                  )
                                 property-value)
                labels (:labels column-style)
                property-value (if labels
                                 (let [selected-set (cset/select
                                                      (fn [[label-txt
                                                            value]]
                                                        (and property-value
                                                             (or
                                                               (and (string?
                                                                      property-value)
                                                                    (= (.toLowerCase
                                                                         property-value)
                                                                       (.toLowerCase
                                                                         value))
                                                                )
                                                               (and (coll?
                                                                      property-value)
                                                                    (some
                                                                      #{value}
                                                                      property-value))
                                                              ))
                                                       )
                                                      labels)
                                       [label-txt
                                        value] (first
                                                 selected-set)]
                                   label-txt)
                                 property-value)
                property-value (if (= (type
                                        property-value)
                                      js/Date)
                                 (format-date
                                   (.getDate
                                     property-value)
                                   (inc
                                     (.getMonth
                                       property-value))
                                   (.getFullYear
                                     property-value))
                                 property-value)]
            (swap!
              card-content-vector
              conj
              (div
                [(div
                   (escape-html-tags
                     property-name)
                   {:class "card-property-name"
                    :title property-name})
                 (div
                   (escape-html-tags
                     property-value)
                   {:class "card-property-value"
                    :title property-value})]
                {:class "card-property"}))
           ))
        (doseq [{label-txt :label
                 clazz :class
                 evt-fn :evt-fn
                 evt-p :evt-p
                 menu-change :menu-change} actions]
          (swap!
            card-action-vector
            conj
            (div
              label-txt
              {:class (str
                        "card-action "
                        clazz)
               :title label-txt}
              {:onclick
                {:evt-fn handle-selected-menu-item
                 :evt-p {:evt-fn evt-fn
                         :evt-p (assoc
                                  evt-p
                                  :ent-id
                                  row-id)
                         :menu-change menu-change}}
               }))
         )
        (swap!
          card-content-vector
          conj
          (div
            @card-action-vector
            {:class "card-actions"}))
        (swap!
          card-container-vector
          conj
          (div
            (div
              [(div
                 nil
                 {:class "card-heading"})
               (div
                 @card-content-vector
                 {:class "card-content"})]
              {:class "card"})
            {:class (case card-columns
                      1 "card-wrapper-1"
                      2 "card-wrapper-2"
                      3 "card-wrapper-3"
                      4 "card-wrapper-4"
                      5 "card-wrapper-5"
                      "card-wrapper-3")})
         ))
     )
    (gen
      (div
        [(generate-pagination-bar
           pagination
           conf)
         (div
           @card-container-vector
           {:class "card-container"})]
        {:class "card-view"}))
   ))

(defn cb-checked?
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

(defn vec-contains?
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

(defn checkbox-field
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
            cb-attrs (if (vec-contains?
                           selected-cbs
                           option
                           0)
                       (assoc
                         cb-attrs
                         :checked "checked")
                       cb-attrs)
            cb-attrs (if disabled
                       (assoc
                         cb-attrs
                         :disabled "disabled")
                       cb-attrs)
            l-attrs {:id (str
                           "lbl"
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

(defn insert-update-entity-success
  "After successful entity insert or update display table again"
  [xhr
   {url :url
    conf :conf
    {table-fn :table-fn} :conf}]
  (table-fn
    conf)
  (handle-selected-menu-item
    {:menu-change (if (= rurls/insert-entity-url
                         url)
                    create-to-show-all
                    entity-to-show-all)})
 )

(defn insert-update-entity
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
        is-valid (atom true)]
    (doseq [e-key entity-keys]
      (let [field (e-key fields)
            label-txt (:label field)
            input-el (:input-el field)
            sub-form-fieldset-read (:sub-form-fieldset-read field)
            sub-form-validation (:sub-form-validation field)
            id (name
                 e-key)]
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
            is-valid))
        (when (= input-el
                 "checkbox")
          (swap!
            entity
            assoc
            e-key
            (md/cb-checked-values
              id))
         )
        (when (= input-el
                 "sub-form")
          (swap!
            entity
            assoc
            e-key
            (sub-form-fieldset-read))
          (sub-form-validation
            validate-field
            is-valid))
        (when-let [input-element (md/query-selector-on-element
                                   table-node
                                   (str
                                     "#"
                                     id))]
          (when (= input-el
                   "img")
            (let [base64-image (md/get-src
                                 input-element)]
              (swap!
                entity
                assoc
                e-key
                base64-image)
              (when (and (string?
                           base64-image)
                         (or (empty?
                               base64-image)
                             (= base64-image
                                (.-baseURI
                                  input-element))
                          ))
                (let [file-input (md/query-selector-on-element
                                   table-node
                                   (str
                                     "#file"
                                     id))]
                  (validate-field
                    file-input
                    is-valid))
               ))
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
              is-valid))
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
              is-valid))
          (when (= input-el
                   "select")
            (let [selected-options-html (.-selectedOptions
                                          input-element)
                  selected-options (atom [])
                  range-end (.-length
                              selected-options-html)]
              (doseq [index (range
                              range-end)]
                (let [selected-option-html (aget
                                             selected-options-html
                                             index)]
                  (swap!
                    selected-options
                    conj
                    (.-value
                      selected-option-html))
                 ))
              (swap!
                entity
                assoc
                e-key
                @selected-options))
            (validate-field
              input-element
              is-valid))
          (when (not
                  (contains?
                    #{"img"
                      "number"
                      "date"
                      "radio"
                      "checkbox"
                      "select"
                      "sub-form"}
                    input-el))
            (swap!
              entity
              assoc
              e-key
              (md/get-value
                input-element))
            (validate-field
              input-element
              is-valid))
         ))
     )
    (when @is-valid
      (ajax
        {:url (if (empty?
                    entity-id)
                rurls/insert-entity-url
                rurls/update-entity-url)
         :success-fn insert-update-entity-success
         :error-fn framework-default-error
         :entity (assoc
                   request-body
                   :entity @entity
                   :_id entity-id)
         :conf conf}))
   ))

(defn generate-form-concrete
  "Generate form fields"
  [xhr
   {conf :conf
    {table-fn :table-fn
     form-type :form-type
     disabled :disabled
     action :action
     action-label :action-label
     action-fn :action-fn
     action-p :action-p
     allowed-actions :allowed-actions
     reports-on :reports-on
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
        fieldset-content (atom [])]
    (swap!
      fieldset-content
      conj
      (legend
        (str
          form-type
          " "
          entity-name))
     )
    (swap!
      fieldset-content
      conj
      (input
        ""
        {:id "_id"
         :name "_id"
         :type "hidden"
         :value (:_id entity-data)})
     )
    (when (and reports-on
               (= form-type
                  (get-label 6))
               (contains?
                 allowed-actions
                 fns/reports))
      (swap!
        fieldset-content
        conj
        (div
          nil
          {:style {:height "10px"}})
        (div
          (a
            (div
              nil
              {:class
                (str
                  @cms/selected-language
                  "-page-report report-icon")})
            {:target "_blank"
             :href (str
                     @base-url
                     rurls/reports-url
                     "?report=single"
                     "&entity=" entity-type
                     "&id=" (:_id entity-data)
                     "&language=" @cms/selected-language)})
          {:class "report-links"}))
     )
    (doseq [e-key entity-keys]
      (let [field-conf (e-key fields)
            label-txt (:label field-conf)
            attrs (:attrs field-conf)
            attrs (if (contains?
                        attrs
                        :title)
                    attrs
                    (assoc
                      attrs
                      :title
                      label-txt))
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
            sub-form-fieldset (:sub-form-fieldset field-conf)]
        (when-let [sub-form-fieldset sub-form-fieldset]
          (doseq [sub-form-field (sub-form-fieldset
                                   entity-data
                                   attrs)]
            (swap!
              fieldset-content
              conj
              sub-form-field))
         )
        (when (not
                sub-form-fieldset)
          (swap!
            fieldset-content
            conj
            (div
              (label
                [label-txt
                 (generate-field
                   (name
                     e-key)
                   data
                   input-el
                   attrs
                   evts
                   options)
                 (span)]))
           ))
       ))
    [(fieldset
       @fieldset-content)
     (div
       [(input
          ""
          {:id "btnCancel"
           :class "btn"
           :type "button"
           :value (get-label 12)}
          {:onclick {:evt-fn handle-selected-menu-item
                     :evt-p {:evt-fn table-fn
                             :evt-p conf
                             :menu-change (if (= action
                                                 :insert)
                                            create-to-show-all
                                            entity-to-show-all)}}
           })
        (when (or (and (= action
                          :insert)
                       (contains?
                         allowed-actions
                         (str
                           entity-type
                           "-create"))
                   )
                  (and (or (= action
                              :edit)
                           (= action
                              :update))
                       (contains?
                         allowed-actions
                         (str
                           entity-type
                           "-update"))
                   ))
          (input
            ""
            {:id (str
                   "btn"
                   (cstring/capitalize
                     (name
                       action))
                  )
             :class "btn btn-default"
             :type "submit"
             :value action-label}
            {:onclick {:evt-fn action-fn
                       :evt-p (if action-p
                                action-p
                                conf)}})
         )])]
   ))

(defn generate-form
  "Generate entity form"
  [xhr
   ajax-params]
  (let [table-node (gen
                     (form
                       (generate-form-concrete
                         xhr
                         ajax-params)
                       {:class "entity"
                        :onsubmit "return false"
                        :autocomplete "off"
                        :novalidate true}))
        action-type (get-in
                      ajax-params
                      [:conf
                       :action])]
    (when (= action-type
             :insert)
      (md/dispatch-event
        "reset"
        table-node))
    (md/remove-element-content
      ".content")
    (md/append-element
      ".content"
      table-node))
 )

(defn entity-form
  "Request data about particular entity for display, edit/update"
  [conf]
  (let [ent-id (:ent-id conf)
        entity (:form-conf conf)
        ent-id-key (:id entity)
        entity-type (:type entity)
        request-body {:entity-type entity-type
                      :entity-filter {ent-id-key ent-id}
                      :entity-projection (get-in
                                           conf
                                           [:form-conf
                                            :projection])
                      :projection-include (get-in
                                            conf
                                            [:form-conf
                                             :projection-include])}]
    (ajax
      {:url rurls/get-entity-url
       :success-fn generate-form
       :error-fn framework-default-error
       :entity request-body
       :conf conf}))
 )

(defn insert-action
  "Insert action configuration"
  [conf]
  (conj
    conf
    {:form-type (get-label 4)
     :action :insert
     :action-label (get-label 10)
     :action-fn insert-update-entity}))

(defn create-entity
  "Call generate-form function with create entity parameters"
  [conf]
  (let [conf (if (fn?
                   conf)
               (conf)
               conf)]
    (generate-form
      nil
      {:conf
        (insert-action
          conf)})
   ))

(defn update-action
  "Update action configuration"
  [conf]
  (conj
    conf
    {:form-type (get-label 7)
     :action :update 
     :action-label (get-label 11)
     :action-fn insert-update-entity}))

(defn edit-entity
  "Call entity-form function"
  [conf]
  (entity-form
    (update-action
      conf))
 )

(defn edit-action
  "Edit action configuration"
  [conf]
  (conj
    conf
    {:form-type (get-label 6)
     :disabled true
     :action :edit
     :action-label (get-label 7)
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
  (let [conf (assoc
               conf
               :after-delete
               true)]
    (table-fn
      conf))
 )

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
     {:url rurls/delete-entity-url
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
                       "#txtSearch")
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
                 {"$or" @or-vector}
                 :current-page
                 0))]
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
        after-delete (:after-delete conf)
        conf (dissoc
               conf
               :after-delete)
        ajax-params (assoc
                      ajax-params
                      :conf
                      conf)
        search-on (:search-on conf)
        search-call (:search-call ajax-params)
        table-class (or (:table-class conf)
                        "entities")
        table-selector (str
                         "."
                         table-class)
        reports-on (:reports-on conf)
        columns (:columns conf)
        render-in (:render-in conf)
        response (get-response xhr)
        entities (:data response)
        pagination (:pagination response)
        default-actions {:details {:label (get-label 6)
                                   :evt-fn entity-details
                                   :evt-p conf
                                   :class "details"
                                   :menu-change show-all-to-entity}
                         :edit {:label (get-label 7)
                                :evt-fn edit-entity
                                :evt-p conf
                                :class "edit"
                                :menu-change show-all-to-entity}
                         :delete {:label (get-label 8)
                                  :evt-fn entity-delete
                                  :evt-p conf
                                  :class "delete"
                                  :menu-change no-change}}
        card-columns-a (or (get-in
                             conf
                             [:preferences
                              :card-columns-a])
                           (atom false))
        actions-conf (atom
                       (:actions conf))
        allowed-actions (:allowed-actions conf)
        entity-type (get-in conf [:form-conf :type])
        actions (atom [])]
    (doseq [action @actions-conf]
      (if-let [action-map (get
                            default-actions
                            action)]
        (when (or (and (= action
                          :details)
                       (contains?
                         allowed-actions
                         (str
                           entity-type
                           "-read"))
                   )
                  (and (= action
                          :edit)
                       (contains?
                         allowed-actions
                         (str
                           entity-type
                           "-update"))
                   )
                  (and (= action
                          :delete)
                       (contains?
                         allowed-actions
                         (str
                           entity-type
                           "-delete"))
                   ))
          (swap!
            actions
            conj
            action-map))
        (when action
          (swap!
            actions
            conj
            action))
       ))
    (let [table-node (gen
                       [(when (and search-on
                                   (not search-call))
                          (fieldset
                            (label
                              [(get-label 13)
                               (input
                                 ""
                                 {:id "txtSearch"
                                  :placeholder (get-label 13)}
                                 {:onkeyup {:evt-fn search-entities-fn
                                            :evt-p ajax-params}})])
                            {:class "search"}))
                        (when (and reports-on
                                   (not search-call)
                                   (contains?
                                     allowed-actions
                                     fns/reports))
                          (div
                            [(a
                               (div
                                 nil
                                 {:class
                                   (str
                                     @cms/selected-language
                                     "-full-report report-icon")})
                               {:target "_blank"
                                :href (str
                                        @base-url
                                        rurls/reports-url
                                        "?report=table"
                                        "&entity=" entity-type
                                        "&page=-1"
                                        "&language=" @cms/selected-language)})
                             (a
                               (div
                                 nil
                                 {:class
                                   (str
                                     @cms/selected-language
                                     "-page-report report-icon")})
                               {:target "_blank"
                                :href (str
                                        @base-url
                                        rurls/reports-url
                                        "?report=table"
                                        "&entity=" entity-type
                                        "&page=" (:current-page pagination)
                                        "&language=" @cms/selected-language)})
                             ]
                            {:class "report-links"}))
                        (if (empty?
                              entities)
                          (div
                            [(div
                               (get-label 31)
                               {:class "no-results"})
                             (when after-delete
                               (div
                                 ""; Delete message should be here
                                 {:class "after-delete"}))]
                            {:class table-class})
                          (div
                            [(if (= @card-columns-a
                                    0)
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
                               (generate-card-view
                                 pagination
                                 conf
                                 entities
                                 columns
                                 @actions))
                             (when after-delete
                               (div
                                 ""; Delete message should be here
                                 {:class "after-delete"}))]
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
  (when-not @is-called-read-preferences-a
    (when (fn?
            @read-preferences-a-fn)
      (@read-preferences-a-fn)
      (reset!
        is-called-read-preferences-a
        true))
   )
  (let [conf (if (fn?
                   conf)
               (conf)
               conf)]
    (ajax
      {:url rurls/get-entities-url
       :success-fn entity-table-success
       :error-fn framework-default-error
       :entity (:query conf)
       :conf conf
       :gen-table-fn gen-table
       :search-call search-call}))
 )

