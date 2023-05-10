(ns reitit.meta
  (:require [clojure.string :refer [starts-with? lower-case replace-first trim]]
            [clojure.tools.namespace.parse :refer [deps-from-ns-decl]]
            [clojure.tools.namespace.find :refer [find-ns-decls]]
            [clojure.java.classpath :refer [classpath]]))

(defmacro setroutes
  ([route] `(setroutes ~route {}))
  ([route meta] `(alter-meta! *ns* merge {:route ~route} ~meta)))

(defmacro defroute
  {:arglists '([name route attr-map? [params*] body])}
  [name route meta-or-params & body]
  (let [with-meta? (map? meta-or-params)
        meta   (if with-meta? meta-or-params {})
        params (if with-meta? (first body) meta-or-params)
        body   (if with-meta? (rest body) body)]
    `(def ~(with-meta name (assoc meta :route route)) (fn ~params ~@body))))

(defn ns-defs-ordered [ns]
  (->> (ns-publics ns) (vals) (sort-by #(:line (meta %)))))

(defn parse-starting-line [s]
  (let [verbs ["GET" "POST" "PATCH" "PUT" "DELETE"]
        verb (first (filter #(starts-with? s %) verbs))
        method (if (some? verb) (-> verb lower-case keyword) :get)
        uri (if (some? verb) (-> s (replace-first verb "") (trim)) s)]
    [method uri]))

(defn route [ns-meta meta handler]
  (let [route (:route meta)
        [method uri] (parse-starting-line route)
        base {:handler handler}]
    [uri {:name (keyword (str (ns-name (:ns meta)))
                         (str (:name meta)))
          method (merge ns-meta meta base)}]))

(defn sort-routes
  "Routes must be sorted in ascending order, i.e. specific first"
  [routes]
  (reverse (sort-by first routes)))

(defn merge-routes [routes]
  (seq
   (reduce
    (fn [res [path map]] (update res path #(merge % map))) {}
    (sort-routes routes))))

(defn scan-in-ns [ns]
  (merge-routes
   (let [ns-meta (meta ns)]
     (for [v (ns-defs-ordered ns)
           :let [handler (var-get v) meta (meta v)]
           :when (contains? meta :route)]
       (route ns-meta meta handler)))))

(defn scan-the-ns [ns]
  (let [url (-> ns meta :route (or "/"))
        routes (scan-in-ns ns)]
    (cond (some? routes) [url routes])))

(defn scan-ns [ns]
  (if (sequential? ns)
    (for [n ns
          :let [routes (scan-the-ns n)]
          :when (some? routes)]
      routes)
    (scan-the-ns ns)))

(defn ns-decl-import [decl]
  (let [name (second decl)]
    (require name)
    (find-ns name)))

(def the-ns-name (ns-name *ns*))

(defn scan-cp []
  (->> (classpath)
       (find-ns-decls)
       (filter #(contains? (deps-from-ns-decl %) the-ns-name))
       (map ns-decl-import)
       (scan-ns)
       (sort-routes)))
