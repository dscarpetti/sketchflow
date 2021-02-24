(ns sketchflow.dotsvg
  (:require
   [clojure.string :as str]
   [viz.core :as viz]))


(def render-debounce (atom nil))



(defn svg->png-url [svg-str width height cb]
  ;;(println [width height])
  (let [canvas (.createElement js/document "canvas")
        ;;_ (set! (.-width canvas) (* 1 #_1.3333 (js/parseInt width)))
        ;;_ (set! (.-height canvas) (* 1 #_1.3333 (js/parseInt height)))
        ;;_ (set! (.-width canvas) (* 1.3333 (js/parseInt width)))
        ;;_ (set! (.-height canvas) (* 1.3333 (js/parseInt height)))
        _ (set! (.-width canvas) width)
        _ (set! (.-height canvas) height)
        
        ctx (.getContext canvas "2d")
        img (new js/Image)]
    (set! (.-onload img) (fn []
                           (.drawImage ctx img 0 0)
                           (cb (.toDataURL canvas))))
    (set! (.-src img) svg-str)))

(defn- gen-graph-svg [s cb]
  (try
    (let [svg (viz/image s)
          [_ width height] (re-find #"<svg width=\"(\d+)pt\" height=\"(\d+)pt\"" (if (string? svg) svg ""))
          width (if (str/blank? width)
                  0
                  (.ceil js/Math (* 1.3333 (js/parseInt width))))
          height (if (str/blank? height)
                   0
                   (.ceil js/Math (* 1.3333 (js/parseInt height))))
          svg-uri (str "data:image/svg+xml;base64," (js/btoa svg))]
      (svg->png-url svg-uri width height
                    (fn [png-uri]
                      (cb
                       {:status :success
                        :svg svg
                        :svg-uri svg-uri
                        :png-uri png-uri
                        :width width
                        :height height}))))
    (catch :default e
      (println e)
      {:status :error
       :error-message (.-message e)})))

(defn render-dot-string [dot-string cb]
  (when-let [timeout @render-debounce]
    (js/clearTimeout timeout))
  (reset! render-debounce (js/setTimeout #(gen-graph-svg dot-string cb) 1000)))







#_(defn- gen-graph-svg [s]
  (try
    (let [svg (viz/image s)
          [_ width height] (re-find #"<svg width=\"(\d+)pt\" height=\"(\d+)pt\"" (if (string? svg) svg ""))
          width (if (str/blank? width) 0 (* 1.3333 (js/parseInt width)))
          height (if (str/blank? height) 0 (* 1.3333 (js/parseInt height)))]
      {:status :success
       :svg svg
       :svg-uri (str "data:image/svg+xml;base64," (js/btoa svg))
       :width width
       :height height
       })
    (catch :default e
      (println e)
      {:status :error
       :error-message (.-message e)})))

#_(defn render-dot-string [dot-string cb]
  (when-let [timeout @render-debounce]
    (js/clearTimeout timeout))
  (reset! render-debounce (js/setTimeout #(cb (gen-graph-svg dot-string)) 1000)))
