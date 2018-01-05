(ns att-test-assignment.matcher
  (:require [clojure.string :as s]
            [cemerick.url :refer [url]]))

(def ^:private key-regexp #"\?(.*?)(/|$)")

(defn new-pattern
  [pattern-string]
  (let [params (map rest (re-seq #"([A-z]+?)\((.*?)\);"
                                 pattern-string))]
    (apply merge-with concat
           (map (fn [a b]{a (list b)})
                (map (comp keyword first) params)
                (map second params)))))

(defn- parse-path
  [path-pattern path-regexp path]
  (let [path-keys (map (comp keyword second)
                       (re-seq key-regexp path-pattern))]
    (zipmap path-keys
            (rest (re-matches path-regexp path)))))

(defn- check-queryparams
  [param-patterns query]
  (let [keys (map (comp first #(s/split % #"=")) param-patterns)]
    (every? query keys)))

(defn- parse-queryparams
  [param-patterns query]
  (for [pattern param-patterns
        :let [[k nk] (s/split pattern #"=")]
        :when (and (contains? query k)
                   (s/starts-with? nk "?"))]
    [(keyword (subs nk 1)) (query k)]))

(defn recognize
  [pattern url-str]
  (let [url-obj (url url-str)
        [host] (:host pattern)
        [path-pattern] (:path pattern)
        queryparams (:queryparam pattern)
        path (subs (:path url-obj) 1)
        path-regexp (re-pattern (s/replace path-pattern
                                           key-regexp
                                           "(.*)$2"))]
    (when (and (or (not host)
                   (= host (:host url-obj)))
               (or (not queryparams)
                   (check-queryparams queryparams (:query url-obj)))
               (or (not path-pattern)
                   (re-matches path-regexp path)))
      (concat (parse-path path-pattern path-regexp path)
              (parse-queryparams queryparams (:query url-obj))))))
