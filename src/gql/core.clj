(ns gql.core
  (:require #_[clojure.core.match :refer [match]]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

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
    (list? v)       :list
    (vector? v)     :vector
    (set? v)        :set
    (map? v)        :map
    (symbol? v)     :symbol
    (var? v)        :var
    :else           (throw (ex-info "Unsupported query" {:v     v
                                                         :class (class v)}))))

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
  (str "$" (name v)))

(defmethod ->str :var [v]
  (str "..." (name (:name (meta v)))))

(defn edn->str
  "Transform an edn query into a string graphql query"
  [query]
  (->str query))
