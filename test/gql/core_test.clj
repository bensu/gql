(ns gql.core-test
  (:require [clojure.test :refer :all]
            [com.walmartlabs.lacinia.util :refer [attach-resolvers]]
            [com.walmartlabs.lacinia :as lacina]
            [com.walmartlabs.lacinia.schema :as lacina-schema]
            [gql.core :refer :all]))

(def StarWarsSchema
  '{:enums
    {:episode
     {:description "The episodes of the original Star Wars trilogy."
      :values      [:NEWHOPE :EMPIRE :JEDI]}}

    :objects
    {:droid
     {:fields {:primary_functions {:type (list String)}
               :id                {:type Int}
               :name              {:type String}
               :appears_in        {:type (list :episode)}}}

     :human
     {:fields {:id          {:type Int}
               :name        {:type String}
               :home_planet {:type String}
               :appears_in  {:type (list :episode)}}}}

    :queries
    {:hero  {:type    (non-null :human)
             :args    {:episode {:type :episode}}
             :resolve :get-hero}
     :droid {:type    :droid
             :args    {:id {:type String :default-value "2001"}}
             :resolve :get-droid}}})

(defn get-hero [context arguments value]
  (let [{:keys [episode]} arguments]
    (if (= episode :NEWHOPE)
      {:id 1000
       :name "Luke"
       :home_planet "Tatooine"
       :appears_in ["NEWHOPE" "EMPIRE" "JEDI"]}
      {:id 2000
       :name "Lando Calrissian"
       :home_planet "Socorro"
       :appears_in ["EMPIRE" "JEDI"]})))

(def comparissonFields
  [:comparissonFields :Character #{:name :appearsIn}])

(def star-wars-schema
  (-> StarWarsSchema
      (attach-resolvers {:get-hero  get-hero
                         :get-droid (constantly {})})
      lacina-schema/compile))

(deftest string-queries
  (testing "basic string "
    (is (= {:data {:hero {:id 2000 :name "Lando Calrissian"}}}
           (let [query "{ hero { id name } }"]
             (lacina/execute star-wars-schema query nil nil))))))

(deftest edn-queries
  (testing "basic edn queries work"
    (is (= "query { hero { name id } }"
           (edn->query #{[:hero #{:id :name}]}))))
  (testing "aliasing works"
    (is (= "query { jedi: hero { name id } }"
           (edn->query #{[:hero :jedi #{:id :name}]}))))
  (testing "aliasing and arguments work"
    (is (= "query { jedi: hero(episode: EMPIRE) { name id } }"
           (edn->query #{[:hero :jedi {:episode "EMPIRE"} #{:id :name}]}))))
  (testing "can use variables"
    (is (= "query { hero(episode: $episode) { name id } }"
           (edn->query #{[:hero {:episode 'episode} #{:id :name}]}))))
  (testing "can use fragments"
    (is (= "query { hero { ...comparissonFields } } fragment comparissonFields on Character { name appearsIn }"
           (edn->query #{[:hero #{#'comparissonFields}]})))))

(defquery simpleQuery #{[:hero #{:id :name}]})

(defquery argsQuery #{[:hero {:episode ^:string 'empire} #{:id :name}]})

(deftest fetch-persisted-queries
  (testing "we can resolve a persisted query and use it"
    (is (= {:data {:hero {:id 2000 :name "Lando Calrissian"}}}
           (let [payload {:query (:id simpleQuery)
                          :args  {}}]
             (lacina/execute star-wars-schema
                             (get-persisted-query (:query payload))
                             (:args payload)
                             nil))))
    (is (= {:data {:hero {:id 2000 :name "Lando Calrissian"}}}
           (let [payload {:query (:id )
                          :args  {:episode :NEWHOPE}}]
             (lacina/execute star-wars-schema
                             (get-persisted-query (:query payload))
                             (:args payload)
                             nil))))))
