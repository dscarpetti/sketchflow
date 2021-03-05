(ns sketchflow.dot2

  (:require
   [clojure.string :as str]))

(def colors
  {:red "red"
   :orange "orange"
   :yellow "gold"
   :green "darkgreen"
   :blue "blue"
   :purple "purple"
   :black "black"
   :grey "grey"
   :gray "gray"
   :brown "brown"})


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

   :oct "octagon"
   :octagon "octagon"
   :pent "pent"
   :pentagon "pentagon"
   :circle "circle"
   :doublecircle "doublecircle"
   :double-circle "doublecircle"
   :dcircle "doublecircle"
   :doubleoctagon "doubleoctagon"
   :double-octagon "doubleoctagon"
   :doctagon "doubleoctagon"
   :tripleoctagon "tripleoctagon"
   :triple-octagon "tripleoctagon"
   :toctagon "tripleoctagon"
   :sept "septagon"
   :septagon "septagon"
   :egg "egg"
   :house "house"
   :inv-triangle "invtriangle"
   :invtriangle "invtriangle"
   :itriangle "invtriangle"
   :triangle "triangle"
   :tri "triangle"
   :note "note"
   :tab "tab"
   :folder "folder"
   :component "component"
   :cds "cds"
   :cylinder "cylinder"
   :msquare "Msquare"
   :mdiamond "Mdiamond"
   :mcircle "Mcircle"
   :none "none"
   :signature "signature"
   :ul "underline"
   :underline "underline"
   :plain "plain"
   :plaintext "plaintext"
   :naked "plaintext"
   })


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


(defn- draw-nodes [s nodes]
  (reduce (fn [s {:keys [id ports label children display-options reference]}]
            (let [name (or name "?")

                  pid id

                  is-record (or (seq ports));;(and (not= type :wifi) (seq children)))

                  label (if is-record
                          (str/join "|"
                                    (cons
                                     (str "<___main___> " label)
                                     (map #(str "<" (first %) "> " (second %)) ports)))
                          label)

                  color (get-option colors display-options)
                  color-str (when color
                              (str ", color=\"" color "\""))
                  shape (if is-record "record" (get-option shapes display-options))
                  shape-str (when shape
                              (str ", shape=\""shape "\""))

                  node (when-not reference (str id " [label=\"" label  "\"" shape-str color-str "];"))

                  edges (reduce (fn [es {:keys [id parent-port edge-display-options edge-label edge-port]}]
                                  (let [edge-style (get-option edge-styles (concat edge-display-options display-options))
                                        edge-direction (get-option edge-directions (concat edge-display-options display-options))
                                        edge-strength (get-option edge-strengths (concat edge-display-options display-options))

                                        label (when edge-label (str " label=\"" edge-label "\""))
                                        style (when edge-style (str " style=\"" edge-style "\""))
                                        direction (when edge-direction (str " dir=\"" edge-direction "\""))
                                        strength (when edge-strength (str " weight=" edge-strength))
                                        components (remove nil? [label style direction strength])

                                        edge-port-id (when (first edge-port) (str ":" (first edge-port)))

                                        edge-str (when (seq components)
                                                   (str "[" (str/join "," components) "]"))]

                                    (if is-record
                                      (let [port-id (or (first parent-port) "___main___")]
                                        (str es "\n" pid ":" port-id " -> " id edge-port-id edge-str ";"))
                                      (str es "\n" pid " -> " id edge-port-id edge-str ";"))))
                                ""
                                children)]
              (draw-nodes (str s "\n" node "\n" edges) children)))
          s
          nodes))



(def ^:private +nodesep+ 0.6)
(def ^:private +ranksep+ 0.6)

(def splines
  {:ortho "ortho"
   :curved "curved"
   :curve "curve"})

(defn dot-string [_;;{:keys [direction nodesep ranksep splines] :or {nodesep +nodesep+ ranksep +ranksep+}}
                  {:keys [title tree default-options config ranksep nodesep]}]
  ;;(pprint (parse-graph-string new-value))
  (let [rankdir (when (or (contains? config :horizontal)
                          (contains? config :hz))
                  "rankdir=LR\n")

        ranksep (or ranksep +ranksep+)
        nodesep (or nodesep +nodesep+)

        spline (get-option splines config)
        spline-str (when spline (str ", splines=" spline))


        title (when title (str "label=\""title"\"; labelloc=bottom; labeljust=center;\n\n"))

        ]
    (str "digraph SketchFlow {\n\n"
         title
         rankdir
         "graph [nodesep=\""nodesep"\", ranksep=\""ranksep"\"" spline-str  "]\n"
         (draw-nodes "" tree)
         "\n}")))
