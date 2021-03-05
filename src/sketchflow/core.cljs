(ns ^:figwheel-hooks sketchflow.core
  (:require
   [sketchflow.help :as help]
   [sketchflow.lang :as lang]
   [sketchflow.dot :as dot]
   [sketchflow.lang2 :as lang2]
   [sketchflow.dot2 :as dot2]

   [sketchflow.dotsvg :as dotsvg]
   [clojure.string :as str]
   [clojure.pprint :refer [pprint]]
   [reagent.core :as r]
   [reagent.dom :as rdom]
   [goog.dom :as gdom]))


(defonce app-state (r/atom {:layout/drawer-open true
                            :editor/pretty-lines true
                            :editor/depth-buttons true
                            :editor/rainbow-buttons true
                            :sketch {:text nil
                                     :data nil}}))

;; (defn add-undo-item! [undo-str]
;;   (swap! app-state (fn [state]
;;                      (let [undo-counter (inc (or (:undo/undo-counter state) -1))]
;;                        (.setItem js/localStorage "sketchflow-v0-undo-count" undo-counter)
;;                        (.setItem js/localStorage "sketchflow-v0-redo-count" "0")
;;                        (.setItem js/localStorage (str "sketchflow-v0-undo-state-" undo-counter) undo-str)
;;                        (assoc state
;;                               :undo/undo-counter undo-counter
;;                               :undo/redo-counter 0)))))

;; (defn undo! []
;;   (swap! app-state (fn [state]
;;                      (let [undo-counter (:undo/undo-counter state)
;;                            redo-counter (or (:undo/redo-counter state) 0)]
;;                        (if (and (number? undo-counter) (pos? undo-counter))
;;                          (let [undo-str (or (.getItem js/localStorage (str "sketchflow-v0-undo-state-" undo-counter)))
;;                                new-undo-counter (dec undo-counter)
;;                                new-redo-counter (inc redo-counter)]
;;                            (.setItem js/localStorage "sketchflow-v0-undo-count" new-undo-counter)
;;                            (.setItem js/localStorage "sketchflow-v0-redo-count" new-redo-counter)
;;                            (assoc state
;;                               :undo/undo-counter new-undo-counter
;;                               :undo/redo-counter new-redo-counter))
;;                          state)))))

;; (defn redo! []
;;   (swap! app-state (fn [state]
;;                      (let [undo-counter (:undo/undo-counter state)
;;                            redo-counter (or (:undo/redo-counter state) 0)]
;;                        (if (and (number? redo-counter) (pos? redo-counter))
;;                          (let [undo-str (or (.getItem js/localStorage (str "sketchflow-v0-undo-state-" redo-counter)))
;;                                new-undo-counter (inc undo-counter)
;;                                new-redo-counter (dec redo-counter)]
;;                            (.setItem js/localStorage "sketchflow-v0-undo-count" new-undo-counter)
;;                            (.setItem js/localStorage "sketchflow-v0-redo-count" new-redo-counter)
;;                            (assoc state
;;                                   :undo/undo-counter new-undo-counter
;;                                   :undo/redo-counter new-redo-counter))
;;                          state)))))

;; (defn load-undos! []
;;   (let [undo-counter (js/parseInt (or (.getItem js/localStorage "sketchflow-v0-undo-count") "0"))
;;         redo-counter (js/parseInt (or (.getItem js/localStorage "sketchflow-v0-redo-count") "0"))]
;;     (swap! app-state assoc
;;            :undo/undo-counter undo-counter
;;            :undo/redo-counter redo-counter)))

(defn set-editor-value! [new-value]
  (let [data (lang2/parse-nodes new-value)
        new-dot (dot2/dot-string nil data)]
    (swap! app-state #(update % :sketch
                              merge {:data data
                                     :text new-value
                                     :dot new-dot}))

    (dotsvg/render-dot-string new-dot (fn [{:keys [status svg-uri error-message] :as render}]
                                        (.setItem js/localStorage "sketchflow-v0-auto" new-value)
                                        ;;(add-undo-item! new-value)
                                        (swap! app-state assoc-in [:sketch :render] render)))))


(def ^:const +left-arrow+ "‹"#_"🢐")
(def ^:const +right-arrow+ "›"#_"🢒")

(defn indent-button [text line-number depth]
  [:button {:on-click (fn [_]
                        (let [[before rem] (split-at line-number (str/split text #"\n"))
                              line (first rem)
                              [children after] (split-with #(< depth (count (second (re-find #"^(\s+)" %)))) (rest rem) )
                              res (str/join "\n"
                                            (concat
                                             before
                                             [(str " " line)]
                                             (map #(if (str/blank? %) % (str " " %)) children)
                                             after))]
                          #_(println res)
                          #_(println before line children after)
                          (set-editor-value! res)))}

   +right-arrow+])

(defn deindent-button [text line-number depth]
  [:button {:on-click (fn [_]
                        (let [[before rem] (split-at line-number (str/split text #"\n"))
                              line (first rem)
                              [children after] (split-with #(< depth (count (second (re-find #"^(\s+)" %)))) (rest rem) )
                              res (str/join "\n"
                                            (concat
                                             before
                                             [(subs line 1)]
                                             (map #(if (str/blank? %) % (subs % 1)) children)
                                             after))]
                          #_(println res)
                          #_(println before line children after)
                          (set-editor-value! res)))}

   +left-arrow+])

(defn depth-control [text line-number depth rainbow]
  [:div.line-depth-controls {:class (when rainbow (str "depth" depth))
                             :style {:left (str (max 0 (dec depth)) "ch")}}
   (when (pos? depth)
     (deindent-button text line-number depth))
   (indent-button text line-number depth)])

(def smart-text-scroll-top (r/atom 0))
(defn smart-text-overlay [depth-buttons rainbow-buttons text]
  [:div.editor-overlay {:style {:top (str @smart-text-scroll-top "px")}}
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
                    (let [depth (count (second (re-find #"^(\s+)" line)))]
                      [:div.editor-overlay-line {:key i}
                       (if depth-buttons
                         (depth-control text i depth rainbow-buttons)
                         [:div.line-depth depth])
                       [:div.line-content line]])))
                (str/split text #"\n"))])

(defn smart-text-editor [text]
  (let [set-point-at (atom nil)
        el (atom nil)]
  (r/create-class
   {:component-did-update (fn [& args]
                            (when-let [pos @set-point-at]
                              (reset! set-point-at nil)
                              (set! (.-selectionStart @el) pos)
                              (set! (.-selectionEnd @el) pos)))
    :reagent-render
    (fn [text]
      [:textarea {:value text
                  :ref #(reset! el %)
                  :on-scroll (fn [e]
                               ;;(println "scroll" (- (.-scrollTop @el)))
                               (reset! smart-text-scroll-top (- (.-scrollTop @el))))
                  :spell-check "false"
                  :on-key-press (fn [e]
                                  (when (and (= (.-key e) "Enter")
                                             (not (.-shiftKey e)))
                                    (let [target (.-target e)
                                          sel-start (.-selectionStart target)
                                          sel-end (.-selectionEnd target)
                                          val (.-value target)]
                                      (when (= sel-start sel-end)
                                        (doto e (.preventDefault) (.stopPropagation))
                                        (let [part1 (subs val 0 sel-start)
                                              part2 (subs val sel-start)
                                              part1-last-line (or (last (str/split part1 #"\n")) "")
                                              space-count (count (second (re-find #"^(\s*)" part1-last-line )))
                                              spaces (str/join (repeat space-count " "))]
                                          (reset! set-point-at (+ (count part1) 1 space-count))
                                          (set-editor-value! (str part1 "\n" spaces part2)))))))
                  :on-change (fn [e]
                               (doto e (.preventDefault) (.stopPropagation))
                               (let [new-value (-> e .-target .-value)]
                                 (set-editor-value! new-value)))}])})))

(defn editor [{;;:undo/keys [undo-counter redo-counter]
               :editor/keys [pretty-lines depth-buttons rainbow-buttons]} {:keys [text data]}]
  [:div.editor
   [:div.editor-controls
    #_[:div.control
     [:button {:on-click undo!}
      "<"]
     (str undo-counter "/" redo-counter)
     [:button {:on-click redo!}
      ">"]


     ]

    [:div.control
     [:button {:on-click (fn [e]
                           (try
                             (do
                               ;;(println (lang/->string data))
                               #_(swap! app-state assoc-in [:sketch :text] (lang/->string data))
                               (set-editor-value! (lang2/->string data)))
                             (catch :default e
                               (println e))))}
                   "Format"]]
    [:div.control
     [:input {:type :checkbox :name "pretty"
              :checked (if pretty-lines "checked" false)
              :on-change (fn [e]
                           (swap! app-state assoc :editor/pretty-lines (-> e .-target .-checked)))}]
     [:label {:for "pretty"} "👓" #_"Pretty"]]
    [:div.control
     [:input {:type :checkbox :name "depth"
              :disabled (not pretty-lines)
              :checked (if depth-buttons "checked" false)
              :on-change (fn [e]
                           (swap! app-state assoc :editor/depth-buttons (-> e .-target .-checked)))}]
     [:label {:for "depth"} (str +left-arrow+ "/" +right-arrow+)]]
    [:div.control
     [:input {:type :checkbox :name "rainbow"
              :disabled (not (and pretty-lines depth-buttons))
              :checked (if rainbow-buttons "checked" false)
              :on-change (fn [e]
                             (swap! app-state assoc :editor/rainbow-buttons (-> e .-target .-checked)))}]
     [:label {:for "rainbow"} "🌈"]]


    ]


   [:div.editor-area
    (when pretty-lines
      (smart-text-overlay depth-buttons rainbow-buttons text))

    [:div.editor-textarea
     (if pretty-lines
       [smart-text-editor text]
       [:textarea {:value text
                   :spell-check "false"
                   :on-change (fn [e]
                                (doto e (.preventDefault) (.stopPropagation))
                                (let [new-value (-> e .-target .-value)]
                                  (set-editor-value! new-value)))}])

     ]]])

(defn file-manager [state]
  [:div "file manager"])

(defn help [state]
  help/rendered-help)

(defn alt-formats [{:keys [sketch]}]
  [:div.alt-formats
   [:section
    [:h2 "SVG"]
    [:div.svg-display
     {:dangerouslySetInnerHTML {:__html (:svg (:render sketch))}}
     ]]
   [:section.graphviz
    [:h2 "GraphViz"]
    [:div.dotstring
     (:dot sketch)]
    #_[:textarea
     {:value (:dot sketch)
      :rows 30
      :on-change #(doto % (.preventDefault) (.stopPropagation))}]]


   ])

(def tabs
  [{:id :editor
    :name "Editor"
    :content #(editor % (:sketch %))}
   #_{:id :files
    :name "Files"
      :content file-manager}
   {:id :formats
    :name "Formats"
    :content #(alt-formats %)}
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
            (workspace state))))


(def default-text
  "!title SketchFlow | Feature Demo
!horizontal

{ green }

{important: yellow folder}
{unimportant: grey note}

SketchFlow
 Features
  Style Nodes {red box dashed}
   And Edges
   And Children {purple! hex!}
    Inherit Styles
    With !
  Use IDs #node-id
   For Complex Structure
    #node-id
  Create Ports <4) Inbound Port {dashed}>
   1) X {important}
   2) Y {unimportant}
   3) Z {blue important!}
    inherit | importance
  Adjust Edges {naked!}
   A <like this {reverse}>
   B <or this {dashed}>
   C <or even this {dotted}>
   D <or go both ways {both}>
    Enjoy! {orange egg}
"
#_"!title SketchFlow Feature Demo
!horizontal

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
  Create Ports <4. Inbound Port>
   1. X
   2. Y
   3. Z
  Adjust Edges
   A <like this { reverse }>
   B <or this { dashed }>
   C <or even this { dotted }>
   D <& both ways { both }>
")


(defn render []
  (rdom/render [shell] (gdom/getElement "app"))
  (let [text (or (-> @app-state :sketch :text) (.getItem js/localStorage "sketchflow-v0-auto"))
        text (if (str/blank? text) default-text text)]

   (set-editor-value! text)))


;;(load-undos!)
(render)


(defn ^:after-load on-reload [])
