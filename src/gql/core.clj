(ns gql.core
  (:require #_[clojure.core.match :refer [match]]
            [clojure.spec.alpha :as s]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn date? [v]
  (or (instance? java.util.Date v)
      (instance? java.time.Instant v)))

(defn boolean? [v]
  (instance? java.lang.Boolean v))

(defn uuid? [v]
  (instance? java.util.UUID v))

(defn primitive? [value]
  (or (number? value)
      (string? value)
      (boolean? value)
      (uuid? value)
      (date? value)))

(defn dispatch [v]
  (cond
    (keyword? v)    :keyword
    (primitive? v)  :primitive
    (vector? v)     :vector
    (set? v)        :set
    (map? v)        :map
    (symbol? v)     :symbol
    (var? v)        :var
    (sequential? v) :list
    :else           (throw (ex-info "Unsupported query" {:v     v
                                                         :class (class v)}))))

(def ^:dynamic *parser-state* nil)

(defmulti ->str (fn [v]
                  (#'dispatch v)))

(defmethod ->str :keyword [v] (name v))

(defmethod ->str :primitive [v] (str v))

(s/def ::compound-field
  (s/cat :field keyword?
         :alias (s/? keyword?)
         :args  (s/? map?)
         :subfields set?))

;; each field that has either subfields or aliases, or arguments
(defmethod ->str :vector [v]
  (let [parsed (s/conform ::compound-field v)]
    (if (= ::s/invalid parsed)
      (throw (ex-info "malformed" {:v v}))
      (str (when-let [alias  (:alias parsed)]
             (str (->str alias) ": "))
           (->str (:field parsed))
           (when-let [args (:args parsed)]
             (->str args))
           " "
           (->str (:subfields parsed)))
     )))

;; fields
(defmethod ->str :set [v]
  (str "{ " (str/join " " (map ->str v)) " }"))

;; arguments to resolver
(defmethod ->str :map [v]
  (str "(" (str/join ",  " (map (fn [[k v]]
                                  (format "%s: %s" (->str k) (->str v)))
                                v))
       ")"))

;; variable
(defmethod ->str :symbol [v]
  (swap! *parser-state* update :symbols conj v)
  (str "$" (name v)))

(defmethod ->str :var [v]
  (swap! *parser-state* update :fragments conj v)
  (str "..." (name (:name (meta v)))))

;; quoted symbols
(defmethod ->str :list [v]
  (->str
    (second v)))

(defn str-fragment [v]
  (let [[field type subfields] (var-get v)]
    (format "fragment %s on %s %s" (name field) (name type) (->str subfields))))

(defn collect-types [m]
  (first (filter #(get m %) [:string :int :float])))

(defn meta->type [s]
  (or
    (some-> (collect-types (meta s))
            name
            str/capitalize)
    "String"))

(defn edn->query
  "Transform an edn query into a string graphql query"
  ([query]
   (edn->query query nil))
  ([query query-name]
   (binding [*parser-state* (atom {:fragments #{}
                                   :symbols   #{}})]
     (let [out (str/trim
                (str
                  "query "
                  (when query-name
                    (str query-name
                         (when-not (empty? (:symbols @*parser-state*))
                           (->str (into {} (map (fn [s] [s (meta->type s)])  (:symbols @*parser-state*)))))
                         " "))
                  (->str query)
                  " "
                  (str/join " " (map str-fragment (:fragments @*parser-state*)))
                  ))
           ]
       (def -parser-state @*parser-state*)
       out))))

(def query-file (io/resource "persisted-queries.edn"))

(defn persisted-queries []
  (edn/read-string (slurp query-file)))

(defn get-persisted-query [name]
  (get (persisted-queries) name))

(defmacro defquery [n query]
  (let [hashed-name (format "%s-%s" n (Math/abs (hash query)))]
    (spit query-file (assoc (persisted-queries) hashed-name (edn->query query n)))
    `(def ~n
       {:id ~hashed-name
        :query ~query})))
