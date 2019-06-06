(ns venia.parser
  (:require [graphql-clj.parser :as parser]))


(declare reduce-arguments)


(defn argument-value
  [{:keys [tag value values fields]}]
  (cond
    (= tag :list-value)
    (mapv argument-value values)

    (= tag :object-value)
    (reduce-arguments fields)

    :default
    value))


(defn reduce-arguments
  [arguments]
  (reduce
    (fn [m {:keys [name value]}]
      (assoc m (keyword name) (argument-value value)))
    {}
    arguments))


(defn document-mapper
  [{:keys [tag selection-set arguments name]}]
  (cond
    (= tag :query-definition)
    (-> (mapv document-mapper selection-set)
        first)

    (= tag :selection-field)
    (let [fields   [(keyword name)
                    (when (some? arguments)
                      (reduce-arguments arguments))
                    (when (some? selection-set)
                      (mapv document-mapper selection-set))]
          fields' (filterv some? fields)]
      (if (= (count fields') 1)
        (first fields')
        fields'))))

(comment


  (reduce-arguments
    [{:tag :argument, :name "id", :value {:tag :string-value, :image "\"1002\"", :value "1002"}}])

  (def q "query {\n  human (id: {aa: 132}]) {\n    id\n    name\n    friends {\n      id\n      name\n      friends {\n        id\n      }\n    }\n  }\n}")

  (def q "query {\nposts (tag: \"science\", user: {username: \"john_doe\"}) {\nid,\ntitle,\nuser {\n    username,\n    first_name\n}\n}\n}")

  (->> (parser/parse-query-document q)
       (mapv document-mapper))
  )
