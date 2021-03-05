(ns sketchflow.lang2
  (:require
   [clojure.pprint :refer [pprint cl-format]]
   [clojure.string :as str]))

;;;; Labels

(defn- clean-label [s]
  (when-not (str/blank? s)
    (-> s
        (str/trim)
        (str/replace #"&" "&amp;")
        (str/replace #"\"" "&quot;")
        (str/replace #"\\?<" "&lt;")
        (str/replace #"\\?>" "&gt;")
        (str/replace #"'" "&#39;")
        (str/replace #"\s*\|\s*" "\\n"))))

(defn- unclean-label [s]
  (when-not (str/blank? s)
    (-> s
        (str/trim)
        (str/replace #"&amp;" "&")
        (str/replace #"&quot;" "\"")
        (str/replace #"&lt;" "\\<")
        (str/replace #"&gt;" "\\>")
        (str/replace #"&#39" "'")
        (str/replace #"\\n" " | "))))

;;;; Options

(defn- prep-option-set-id [set-id]
  (-> set-id
      str/lower-case
      (str/replace #"[^a-z0-9_\-]" "")
      keyword))

(def ^:private extract-options-re #"\{([^\{\}]+)\}")

(defn- extract-options
  "extracts options from a string and returns a map with the keys
  :named-options
  :options
  :remaining-string"
  [named-options s]
  (let [options-str (or (second (re-find extract-options-re s)) "")
        [set-id options] (str/split options-str #"\:")
        [set-id options] (if (nil? options)
                           [nil options-str]
                           [(prep-option-set-id set-id) options])

        #_ (pprint {:s s
                   :set-id set-id
                   :options options
                   :options-str options-str})
        options (seq
                 (reduce (fn [options val-string]
                           (let [opt-id (-> val-string str/trim str/lower-case (str/replace #"!" ""))]
                             (if (str/blank? opt-id)
                               options
                               (conj options {:opt-id (keyword opt-id)
                                              :heritable (boolean (re-find #"!" val-string))}))))
                         []
                         (str/split options #"\s+")))]
    {:remaining-string (str/replace s extract-options-re "")
     :named-options (if (and set-id options)
                      (update named-options set-id concat options)
                      named-options)
     :options options}))

;;;; Ports

(defn- prep-port [port]
  (let [port-name (when port
                    (-> port
                        str/trim
                        (str/replace #"\s+" " ")))]
    (when-not (str/blank? port-name)
      (let [port-id (-> port-name
                        str/lower-case
                        (str/replace #"[-\s]" "_")
                        (str/replace #"[^a-z0-9_]" ""))
            port-id (when-not (str/blank? port-id)
                      #?(:cljs (try
                                 (let [n (js/parseInt port-id)]
                                   (if (= (str n) port-id) (cl-format nil "ip_~8,'0d" n) (str "p_" port-id)))
                                 (catch :default e (str "p_" port-id)))
                         :clj (try
                                (let [n (Long/parseLong port-id)]
                                  (if (= (str n) port-id) (cl-format nil "ip_~8,'0d" n) (str "p_" port-id)))
                                (catch Exception e (str "p_" port-id)))))]
        (when port-id
          [port-id (clean-label port-name)])))))



(def ^:private extract-port-re #"([^\.]*)(\.?)([^\.]*)")

(defn- extract-port [s]
  (let [[_ port has-port remaining-str] (re-matches extract-port-re s)
        [port remaining-str] (if (str/blank? has-port)
                               [nil s]
                               [port remaining-str])
        [port-id port-name :as port] (prep-port port)]

    {:remaining-string remaining-str
     :port port}))

(defn port-compare [[p1] [p2]]
  (compare p1 p2))

;;;; Edges

(def ^:private extract-edge-re #"\<([^\<\>]+)\>")

(defn- extract-edge [named-options s]
  (let [edge-str (or (second (re-find extract-edge-re s)) "")
        {:keys [named-options options remaining-string]} (extract-options named-options edge-str)
        {:keys [port remaining-string]} (extract-port remaining-string)]
    {:remaining-string (str/replace s extract-edge-re "")
     :named-options named-options
     :edge-label (clean-label remaining-string)
     :edge-options options
     :edge-port port}))

;;;; Nodes

(defn- parse-node-line [{:keys [named-options node-table nodes]} line]
  (let [raw-id (second (re-find #"\#([^\s]+)" line))
        id (if raw-id
             (str "id_" (-> raw-id
                            str/lower-case
                            (str/replace #"-" "_")
                            (str/replace #"[^a-zA-Z0-9_]" "")))
             (str "node_" (count node-table)))

        remaining-string (str/replace line #"\#[^\s]+" "")

        {:keys [edge-label edge-options edge-port named-options remaining-string]}
        (extract-edge named-options remaining-string)

        {:keys [named-options options remaining-string]}
        (extract-options named-options remaining-string)

        {:keys [port remaining-string]}
        (extract-port remaining-string)

        label (clean-label remaining-string)

        [_ indentation rest] (re-matches #"(\s*)(.*)" line)
        depth (count indentation)

        existing-node (get node-table id)
        node {:id id
              :raw-id raw-id
              :label (or (:label existing-node) label)
              :ports (conj (:ports existing-node) edge-port)
              :options (seq (concat (:options existing-node) options))}

        ]

    {:named-options named-options
     :node-table (assoc node-table id node)
     :nodes (conj nodes
                  {:id id
                   :reference (boolean existing-node)
                   :parent-port port
                   :edge-port edge-port
                   :edge-label edge-label
                   :edge-options edge-options
                   :depth depth})}))


(defn- expand-options
  ([named-options options]
   (second (expand-options named-options #{} options)))
  ([named-options seen-ids options]
   (reduce (fn [[seen-ids res] {:keys [opt-id heritable] :as option}]
             (if (seen-ids opt-id)
               [seen-ids res]
               (if-let [named-set (get named-options opt-id)]
                 (let [named-set (if heritable
                                   (map #(assoc % :heritable true) named-set)
                                   named-set)
                       [seen sub-options] (expand-options named-options (conj seen-ids opt-id) named-set)]
                   [seen (into res sub-options)])
                 [(conj seen-ids opt-id) (conj res option)])))
           [seen-ids []]
           options)))

(defn- parse-node-lines [default-options named-options lines]
  (let [{:keys [named-options node-table nodes]} (reduce parse-node-line {:named-options named-options
                                                                          :node-table {}
                                                                          :nodes []}
                                                         lines)
        node-table (reduce-kv
                    (fn [node-table node-id node-config]
                      (let [options (:options node-config)#_(if (:options node-config) (concat (:options node-config) default-options) default-options)
                            expanded-options (expand-options named-options options)
                            heritable-options (filter :heritable expanded-options)]
                        (update node-table node-id assoc
                                :expanded-options (map :opt-id expanded-options)
                                :heritable-options (map :opt-id heritable-options))))
                    node-table node-table)]
    (reduce (fn [nodes node]
              (let [node-config (node-table (:id node))
                    node (assoc node
                                :edge-options (seq (map :opt-id (:edge-options node)))
                                :edge-display-options (seq (map :opt-id (expand-options named-options (:edge-options node)))))]
                (conj nodes (merge node-config node))))
            [] nodes)))


;;;; Parse


(defn- create-tree [default-display-options parent [node & nodes]]
  (if (nil? node)
    ()
    (let [split-depth (:depth node)
          [children rest] (split-with #(< split-depth (:depth %)) nodes)

          node (assoc node
                      :display-options (concat (:expanded-options node) (:heritable-options parent) default-display-options)
                      :heritable-options (concat (:heritable-options node) (:heritable-options parent))
                      :depth (inc (or (:depth parent) -1)))

          children (create-tree default-display-options node children)

          ports (->> children
                     (map :parent-port)
                     (concat (:ports node))
                     (remove nil?))


          node (assoc node
                      :children children
                      :ports (when (seq ports) (into (sorted-set-by port-compare) ports)))]

      (into [node] (create-tree default-display-options parent rest)))))


(defn- line-type [l]
  (let [s (str/trim l)]
    (cond
      (str/starts-with? s "!") :config

      (str/starts-with? s "{") (if (re-find #":" s)
                                 :option
                                 :default-option)
      :else :node)))

#_(defn- parse-config [lines]
  (reduce (fn [[title config-set] line]
            (let [line (str/trim (subs (str/trim line) 1))
                  lcase (str/lower-case line)]
              (if (str/starts-with? lcase "title")
                [(str/trim (subs line 5)) config-set]
                [title (conj config-set (keyword lcase))])))
          [nil (sorted-set)]
          lines))

(defn- parse-float [x]
  #?(:clj (try (Float/parseFloat x) (catch Exception e nil))
     :cljs (try (js/parseFloat x) (catch :default e nil))))

(defn parse-config [lines]
  (reduce (fn [{:keys [flags title ranksep nodesep] :as config} line]
            (let [line (str/trim (subs (str/trim line) 1))
                  lcase (str/lower-case line)]
              (cond
                (str/starts-with? lcase "title") (assoc config :title (str/trim (subs line 5)))
                (str/starts-with? lcase "ranksep") (assoc config :ranksep (parse-float (subs line 7)))
                (str/starts-with? lcase "nodesep") (assoc config :nodesep (parse-float (subs line 7)))
                :else (update config :flags conj (keyword lcase)))))
          {:flags (sorted-set)}
          lines))

(defn parse [s]
  (let [lines (group-by line-type (remove str/blank? (str/split s #"\n")))

        {:keys [title ranksep nodesep flags]} (parse-config (:config lines))

        default-options (mapcat #(:options (extract-options nil %)) (:default-option lines))
        named-options (reduce #(:named-options (extract-options %1 %2)) {} (:option lines))

        default-display-options (map :opt-id (expand-options named-options default-options))

        nodes (parse-node-lines default-options named-options (:node lines))
        ]
    {:raw-title title
     :title (clean-label title)
     :config flags
     :ranksep ranksep
     :nodesep nodesep
     :default-options default-options
     :named-options named-options
     :tree (create-tree default-display-options nil nodes)}))

(def parse-nodes parse)


;;;; ->String

(def ^:private spaces "                                                          ")


(defn- option->string [opt]
  (str (name (:opt-id opt)) (when (:heritable opt) "!")))


(defn- node->string [s {:keys [depth parent-port children label options raw-id reference edge-options edge-label edge-port] :as node}]
  (let [edge (when (or edge-options edge-label)
               (str " <"

                    (when edge-port
                      (str (second edge-port) ". "))

                    (unclean-label edge-label)

                    (when (and edge-label edge-options) " ")
                    (when edge-options "{ ")
                    (when edge-options (str/join " " (map name edge-options)))
                    (when edge-options " }")
                    ">"))

        node-str (if reference
                   (str s
                        (subs spaces 0 depth)
                        "#" raw-id
                        edge
                        "\n")
                   (str s
                        (subs spaces 0 depth)
                      (when parent-port
                        (str (second parent-port) ". "))
                      (unclean-label label)

                      (when raw-id
                        (str " #" raw-id))
                      edge

                      (when options
                        (str " { " (str/join " " (map option->string options)) " }"))
                      "\n"))]

      (reduce node->string
              node-str
              children)))



(defn ->string [{:keys [raw-title config options default-options named-options tree]}]
  (let [title (when raw-title (str "!title " raw-title "\n"))
        flags (str/join "\n" (map #(str "!" (name %)) config))
        named-options (str/join
                       "\n"
                       (remove nil?
                               (map (fn [[nick opts]]
                                      (when nick
                                        (str "{ " (name nick) ": "
                                             (str/join " " (map option->string opts))
                                             " }")))
                                    named-options)))]
    (str
     title
     flags
     (when-not (str/blank? flags) "\n\n")
     (when-not (empty? default-options)
       (str "{ " (str/join " " (map option->string default-options)) " }\n\n"))
     named-options
     (when-not (str/blank? named-options) "\n\n")

     (reduce node->string "" tree))))
