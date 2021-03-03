(ns sketchflow.dot
  (:require
   [clojure.string :as str]))

(def colors
  {:red "red"
   :orange "orange"
   :yellow "gold"
   :green "darkgreen"
   :blue "blue"
   :purple "purple"
   :black "black"})


(def shapes
  {:box "box"
   :rect "box"
   :rectange "box"
   :square "square"
   :oval "oval"
   :star "star"
   :hex "hexagon"
   :hexagon "hexagon"
   :diamond "diamond"
   :circle "circle"})


(def edge-styles
  {:dot "dotted"
   :dotted "dotted"
   :dash "dashed"
   :dashed "dashed"
   :solid "solid"})

(def edge-directions
  {:reverse "back"
   :back "back"
   :backward "backward"
   :forward "forward"
   :fore "forward"
   :double "both"
   :both "both"})

(def edge-strengths
  {:weak "0"
   :normal "1"
   :strong "10"
   :ultra "20"})


(defn get-option [option-map option-seq]
  (first (remove nil? (map option-map option-seq))))


(defn- draw-nodes [s parent-options nodes]
  (reduce (fn [s {:keys [id ports name children options reference]}]
            (let [name (or name "?")

                  pid id

                  is-record (or (seq ports));;(and (not= type :wifi) (seq children)))

                  label (if is-record
                          (str/join "|"
                                    (cons
                                     (str "<___main___> " name)
                                     (map #(str "<" (first %) "> " (second %)) ports)))
                          name)


                  ;; shape (cond
                  ;;         is-record "record"
                  ;;         (= type :wifi) "hexagon"
                  ;;         (= type :group) "diamond"
                  ;;         :else "oval")

                  ;; color (or color (first (shuffle [:black :red :green :blue :orange :yellow :purple])))
                  ;; color (condp = color
                  ;;         :red "red"
                  ;;         :green "darkgreen"
                  ;;         :blue "blue"
                  ;;         :orange "orange"
                  ;;         :purple "purple"
                  ;;         :yellow "gold"
                  ;;         nil)
                  color (get-option colors options)
                  color-str (when color
                              (str ", color=\"" color "\""))
                  shape (if is-record "record" (get-option shapes options))
                  shape-str (when shape
                              (str ", shape=\""shape "\""))

                  node (when-not reference (str id " [label=\"" label  "\"" shape-str color-str "];"))

                  ;;;default-edge-type  (get-option edge-styles options)




                  edges (reduce (fn [es {:keys [id port edge-options edge-label]}]
                                  (let [edge-style (get-option edge-styles (concat edge-options options))
                                        edge-direction (get-option edge-directions (concat edge-options options))
                                        edge-strength (get-option edge-strengths (concat edge-options options))

                                        label (when edge-label (str " label=\"" edge-label "\""))
                                        style (when edge-style (str " style=\"" edge-style "\""))
                                        direction (when edge-direction (str " dir=\"" edge-direction "\""))
                                        strength (when edge-strength (str " weight=" edge-strength))
                                        components (remove nil? [label style direction strength])

                                        edge-str (when (seq components)
                                                   (str "[" (str/join "," components) "]"))
                                        #_(cond
                                                   (and edge-style edge-label)
                                                   (str " [ label=\" "edge-label"\", style=\""edge-style"\", dir=both ]")
                                                   edge-style
                                                   (str "[ style=\""edge-style"\" ]")
                                                   edge-label
                                                   (str " [ label=\" "edge-label"\" ]"))]

                                    (if is-record
                                      (let [port-id (or (first port) "___main___")]
                                        (str es "\n" pid ":" port-id " -> " id edge-str";"))
                                      (str es "\n" pid " -> " id edge-str ";"))))
                                ""
                                children)]
              (draw-nodes (str s "\n" node "\n" edges) options children)))
          s
          nodes))



(def ^:private +nodesep+ 0.75)
(def ^:private +ranksep+ 0.75)

(def splines
  {:ortho "ortho"
   :curved "curved"
   :curve "curve"})

(defn dot-string [_;;{:keys [direction nodesep ranksep splines] :or {nodesep +nodesep+ ranksep +ranksep+}}
                  {:keys [title nodes default-options config]}]
  ;;(pprint (parse-graph-string new-value))
  (let [rankdir (when (or (contains? config :horizontal)
                          (contains? config :hz))
                  "rankdir=LR\n")
        spline (get-option splines config)
        spline-str (when spline (str ", splines=" spline))


        color (get-option colors default-options)
        color-str (when color
                    (str "color=\"" color "\""))
        shape (get-option shapes default-options)
        shape-str (when shape
                    (str "shape=\"" shape "\""))

        edge-style (get-option edge-styles default-options)
        edge-str (when edge-style
                   (str "edge [ style=\""edge-style"\"]\n"))

        node-str (cond
                   (and color shape) (str "node [" color-str "," shape-str  "];\n")
                   color (str "node [" color-str "];\n")
                   shape (str "node [" shape-str "];\n"))

        title (when title (str "label=\""title"\"; labelloc=bottom; labeljust=center;\n\n"))

        ]
    (str "digraph SketchFlow {\n\n"
         title
         rankdir
         "graph [nodesep=\""+nodesep+"\", ranksep=\""+ranksep+"\"" spline-str  "]\n"
         node-str
         edge-str
         (draw-nodes "" nil nodes)
         "\n}")))
