(ns sketchflow.lang
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.string :as str]))

(defn process-option [option-str]
  (let [raw (-> option-str str/trim str/lower-case)
        processed (str/replace raw #"[!>:]" "")]
    (when-not (str/blank? processed)
      (let [apply-to-children-only (re-find #"!" raw)
            apply-to-children (or apply-to-children-only (re-find #">" raw))]
        {:name (keyword processed)
         :heritable apply-to-children
         }))))

(defn- process-options [named-options options-str]
  (if (string? options-str)
    (let [[id values] (str/split options-str #"\:")
          [id values] (if values [id values] ["" id])
          id (str/replace id #"[>!:\s]" "")
          id (str/replace id #"\s+" "-")
          id (when-not (str/blank? id) (-> id str/trim str/lower-case keyword))
          processed-options (remove nil? (map process-option (str/split values #"\s+")))
          ;;named-options (reduce (fn [no x] (assoc no x [x])) named-options processed-options)
          new-named-options (if id
                              (assoc named-options id processed-options)
                              named-options)]
      [new-named-options processed-options])
    [named-options nil]))

(defn- parse-line [named-options node-table lines line]
  (let [ref-id (second (re-find #"\#([^\s]+)" line))
        ref-id (when ref-id (-> ref-id str/lower-case))
        id (-> (or ref-id
                   (str (gensym "node")))
               (str/replace #"[/\+\*\&\s\-\[\]\)\(]" "_"))

        line (str/replace line #"\#[^\s]+\s*" "")

        raw-options (second (re-find #"\{\s*([^\{\}]+)\s*\}" line))
        [named-options options] (process-options named-options raw-options)
        ;;options (map #(-> % str/lower-case str/trim keyword) (str/split options #"\s"))
        ;;options (if (empty? options) nil (set options))
        line (str/replace line #"\{[^\{\}]*\}" "")



        edge-label (second (re-find #"\<([^\<\>]+)\>" line))
        [_ edge-type edge-label] (when edge-label (re-matches #"\s*([\|\-\.]?)(.*)" edge-label))
        edge-type (condp = edge-type
                    "." :dotted
                    "-" :dashed
                    "|" :solid
                    nil)
        edge-label (when edge-label (str/trim edge-label))

        line (str/replace line #"\<[^\<\>]+\>" "")




        [_ indentation rest] (re-matches #"(\s*)(.*)" line)

        [_ port has-port rest] (re-matches #"([^\.]*)(\.?)([^\.]*)" (str/trim rest))
        [port-name rest] (if (str/blank? has-port)
                           [nil (str/trim port)]
                           [(str/trim port) (str/trim rest)])

        port-id (when port-name
                  (str "p_" (str/replace port-name #"[/\+\*\&\s\-\[\]\)\(]" "_")))

        name (if (str/blank? rest)
               nil
               (-> rest
                   (str/replace #"\"" "'")
                   (str/replace #"<" "&lt;")
                   (str/replace #">" "&gt;")
                   (str/replace #"\s*\|\s*" "\\n")))

        depth (count indentation)

        raw-options (when raw-options (first (str/split raw-options #"\s*:\s*")))

        existing-node (node-table id)

        node (if existing-node
               (assoc existing-node
                      :reference true
                      :depth depth
                      :edge-type edge-type
                      :edge-label edge-label
                      )
               {:id id
                :ref-id ref-id
                :name name
                :options options
                :raw-options (when raw-options (seq (map #(str/trim (str/lower-case %)) (str/split (str/replace raw-options #":" "") #"\s+"))))
                :port (when port-id [port-id port-name])
                :edge-type edge-type
                :edge-label edge-label
                :depth depth})]

    [named-options
     (assoc node-table id node)
     (conj lines node)]))

;(defn expand-options [named-options options]

(defn- create-tree [named-options parent-depth parent-options devices]
  (if (empty? devices)
    ()
    (let [[device & devices] devices

          options (reduce (fn [opts opt]
                            (if-let [o (named-options (:name opt))]
                              (if (:heritable opt)
                                (into opts (map #(assoc % :heritable true) o))
                                (into opts o))
                              (conj opts opt)))
                          []
                          (:options device))
          options (into options parent-options)

          split-depth (:depth device)
          [children rest] (split-with #(< split-depth (:depth %)) devices)
          depth (inc parent-depth)
          children (create-tree named-options depth (filter :heritable options) children)
          ports (->> children
                     (map :port)
                     (remove nil?))
          device (assoc device
                        :depth depth
                        :raw-options (:raw-options device)
                        :options (map :name options)
                        :children children
                        :ports (if (empty? ports) nil (into (sorted-set) ports)))]
      (into [device] (create-tree named-options parent-depth parent-options rest)))))

(defn- expand-options-helper [named-options seen options]
  (reduce (fn [[seen res] option]
            (if (seen option)
              [seen res]
              (if-let [named (named-options option)]
                (let [[seen sub-options] (expand-options-helper named-options (conj seen option) named)]
                  [seen (into res sub-options)])
                [(conj seen option) (conj res option)])))
          [seen []]
          options))

(defn- expand-options [named-options]
  (reduce-kv (fn [res id options-seq]
               (assoc res id (second (expand-options-helper named-options #{id} options-seq))))
             {} named-options))

(defn- line-type [l]
  (let [s (str/trim l)]
    (cond
      (str/starts-with? s "!") :config

      (str/starts-with? s "{") (if (re-find #":" s)
                                 :option
                                 :default-option)
      :else :node)))

(defn- parse-config [lines]
  (into (sorted-set) (map #(-> % (subs 1) str/trim str/lower-case keyword) lines)))

(defn parse-nodes [s]
  (let [lines (group-by
               line-type

               (remove str/blank? (str/split s #"\n")))

        option-lines (lines :option)
        node-lines (lines :node)
        config-flags (parse-config (lines :config))

        default-options (map #(-> % str/lower-case str/trim keyword)
                             (remove str/blank? (str/split (str/join " "
                                                                     (map #(str/replace % #"[\{\}]" "")
                                                                          (lines :default-option)))
                                                           #"\s+")))

        initial-named-options (reduce (fn [n-o line]
                                        (first (process-options n-o (second (re-find #"\{([^\{\}]+)\}" line)))))
                                      {}
                                      option-lines)
        [named-options _ lines] (reduce (fn [[named-options id-table lines] line]
                                          (parse-line named-options id-table lines line))
                                        [initial-named-options {} []]
                                        node-lines)
        named-options (expand-options named-options)]

    {:config config-flags
     :named-options named-options ;initial-named-options
     ;;:options named-options
     :default-options default-options
     :nodes (create-tree named-options -1 nil lines)}))

(def ^:private spaces "                                                          ")

(defn- node->string [s {:keys [depth port children name raw-options ref-id reference edge-type edge-label] :as node}]
  ;;(println reference ref-id name)
  (let [edge (when (or edge-type edge-label)
               (str " <"
                    (case edge-type
                      :dotted "."
                      :dashed "-"
                      :solid "|"
                      nil)
                    (when edge-type " ")
                    edge-label
                    ">"))
        node-str (if reference
                   (str s
                        (subs spaces 0 depth)
                        "#" ref-id
                        edge
                        "\n")
                   (str s
                      (subs spaces 0 depth)
                      (when port
                        (str (second port) ". "))
                      (str/replace name #"\\n" " | ")
                      (when ref-id
                        (str " #" ref-id))
                      edge

                      (when raw-options
                        (str " { " (str/join " " raw-options) " }"))
                      "\n"))]

    ;(if (empty? children)
    ;  (str node-str "\n")

      (reduce node->string
              node-str
              children)));)



(defn ->string [{:keys [config options default-options named-options nodes]}]
  (let [flags (str/join "\n" (map #(str "!" (name %)) config))
        named-options (str/join
                       "\n"
                       (remove nil?
                               (map (fn [[nick opts]]
                                      (when nick
                                        (str "{ " (name nick) ": "
                                             (str/join " " (map #(str (name (:name %)) (when (:heritable %) "!")) opts))
                                             " }")))
                                    named-options)))]
    (str
     flags
     (when-not (str/blank? flags) "\n\n")
     (when-not (empty? default-options)
       (str "{ " (str/join " " (map name default-options)) " }\n\n"))
     named-options
     (when-not (str/blank? named-options) "\n\n")

     (reduce node->string "" nodes))))
