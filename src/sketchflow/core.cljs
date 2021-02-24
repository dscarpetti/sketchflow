(ns ^:figwheel-hooks sketchflow.core
  (:require
   [sketchflow.help :as help]
   [sketchflow.lang :as lang]
   [sketchflow.dot :as dot]
   [sketchflow.dotsvg :as dotsvg]
   [clojure.string :as str]
   [clojure.pprint :refer [pprint]]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [goog.dom :as gdom]))



;; define your app data so that it doesn't get over-written on reload
(defonce app-state (r/atom {:layout/drawer-open true
                            :layout/pretty-lines true
                            :sketch {:text nil
                                     :data nil}}))

(defn set-editor-value! [new-value]
  (let [data (lang/parse-nodes new-value)
        new-dot (dot/dot-string nil data)]
    (swap! app-state #(update % :sketch
                              merge {:data data
                                     :text new-value
                                     :dot new-dot}))

    (dotsvg/render-dot-string new-dot (fn [{:keys [status svg-uri error-message] :as render}]
                                        (.setItem js/localStorage "sketchflow-v0-auto" new-value)
                                        (swap! app-state assoc-in [:sketch :render] render)))))

(defn editor [pretty-lines {:keys [text data]}]
  [:div.editor
   [:div.editor-controls
    [:div.control
     [:button {:on-click (fn [e]
                           (try
                             (do
                               (println (lang/->string data))
                               (swap! app-state assoc-in [:sketch :text] (lang/->string data)))
                             (catch :default e
                               (println e))))}
                   "Format"]]
    [:div.control
     [:input {:type :checkbox :name "pretty"
              :checked (if pretty-lines "checked" false)
              :on-change (fn [e]
                           (swap! app-state assoc :layout/pretty-lines (-> e .-target .-checked)))}]
     [:label {:for "pretty"} "Pretty Lines"]]

    #_[:div.control [:button "Save"]]]
   [:div.editor-area
    (when pretty-lines
      [:div.editor-overlay
       #_(map #(vector :div.editor-line (if (str/blank? %) " " %)) (str/split text #"\n"))
       (map-indexed (fn [i line]
                      (cond
                        (str/blank? line)
                        [:div.editor-overlay-line {:key i}
                         [:div.line-depth]
                         [:div.line-content " "]]

                        (re-find #"^\s*[\{\!]" line)
                        [:div.editor-overlay-line {:key i}
                         [:div.line-depth "*"]
                         [:div.line-content line]]

                        :else
                        [:div.editor-overlay-line {:key i}
                         [:div.line-depth (count (second (re-find #"^(\s+)" line)))]
                         [:div.line-content line]]))
                    (str/split text #"\n"))

       ])
    [:div.editor-textarea

     [:textarea {:value text
                 :spell-check "false"
                 :on-change (fn [e]
                              (doto e (.preventDefault) (.stopPropagation))
                              (let [new-value (-> e .-target .-value)]
                                (set-editor-value! new-value)))}]]]

   ])

(defn file-manager [state]
  [:div "file manager"])

(defn help [state]
  help/rendered-help)

(def tabs
  [{:id :editor
    :name "Editor"
    :content #(editor (:layout/pretty-lines %) (:sketch %))}
   #_{:id :files
    :name "Files"
    :content file-manager}
   {:id :help
    :name "Help"
    :content help}
   #_{:id :dot
    :name "View Dot"
    :content #(str %)}])

(defn drawer [state]
  (let [active-tab-id (or (:layout/tab state) (:id (first tabs)))
        active-tab-content (:content (first (filter #(= (:id %) active-tab-id) tabs)))
        tab-selectors (doall (map (fn [{:keys [id name]}]
                                    [:div.tab {:key id
                                               :on-click #(swap! app-state assoc :layout/tab id)
                                               :class (when (= id active-tab-id) "active")}
                                     [:a {:href "#"} name]])
                                  tabs))]
    [:div.tabs
     [:div.tab-selectors
      tab-selectors]
     [:div.tab-content
      (active-tab-content state)]]))



(defn open-drawer! [] (swap! app-state assoc :layout/drawer-open true))
(defn close-drawer! [] (swap! app-state assoc :layout/drawer-open false))
(defn toggle-drawer! [] (swap! app-state update :layout/drawer-open not))
(defn layout [state header-content drawer-content workspace-content]
  [:div#layout
   [:div#header header-content]
   [:div#content
    [:div#drawer
     {:class (when (:layout/drawer-open state) "open-drawer")}
     drawer-content]
    [:div#workspace
     {:class (when (:layout/drawer-open state) "open-drawer")}
     workspace-content]
    ]])

(defn header [state]
  [:div.header-content
   [:div.header-left
    [:button {:on-click toggle-drawer!}
     (if (:layout/drawer-open state)
       "Close Drawer"
       "Open Drawer")]]
   [:div.header-center
    "SketchFlow"]
   [:div.header-right]])

(defn workspace [state]
  (when-let [uri (-> state :sketch :render :png-uri)]
    [:div {:on-click #(if (> 801 (.max js/Math
                                       (or (.-clientWidth (.-documentElement js/document)) 0)
                                       (or (.-innerWidth js/window) 0)))
                        (close-drawer!))

           :style {:position :absolute
                   :top "10px"
                   :bottom "10px"
                   :left "10px"
                   :right "10px"
                   :text-align "center"}}
        [:img {:src uri
               :style {:max-width "100%"
                       :max-height "100%"}}]]))

(defn shell []
  (let [state @app-state]
    (layout state
            (header state)
            (drawer state)
            (workspace state))



    #_[:div
     (editor (:sketch state))
     [:textarea {:disabled true
                 :rows 25
                 :cols 70
                 :value (lang/->string (-> state :sketch :data))}]
     ;[:div (str state)]
     (when-let [uri (-> state :sketch :render :png-uri)]
       [:div {:style {:padding-left "20em"
                      :position :fixed
                      :top 0
                      :bottom 0
                      :left 0
                      :right 0}}
        [:img {:src uri
               :style {:max-width "100%"
                       :max-height "50vh"}}]])]))

(def default-text
"!horizontal

SketchFlow
 Features
  Style Nodes { red box dashed }
   And Edges
   And Children { purple! hex! }
    Inherit Styles
    With !
  Use IDs #node-id
   For Complex Structure
    #node-id
  Create Ports
   1. X
   2. Y
   3. Z
")

(defn render []
  (rdom/render [shell] (gdom/getElement "app"))
  (let [text (or (-> @app-state :sketch :text) (.getItem js/localStorage "sketchflow-v0-auto"))
        text (if (str/blank? text) default-text text)]

   (set-editor-value! text)))



(render)


(defn ^:after-load on-reload [])
