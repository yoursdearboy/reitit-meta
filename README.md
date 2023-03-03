# Reitit routing using meta on handlers

```clj
(ns demo
  (:require [clojure.spec.alpha :as s]
            [reitit.coercion.spec :as coercion]
            [titier-meta :refer [defroute]]))

(defroute index "GET /"
  [request]
  {:body "Index page"})

(s/def ::id int?)

(defroute show "GET /:id"
  {:coercion coercion :parameters {:path {:id ::id}}}
  [request]
  {:body (->> request :parameters :path :id (str "Record number "))})
```

```clj
(ns app
  (:require [reitit.ring.middleware.parameters :refer [parameters-middleware]]
            [reitit.ring.coercion :refer [coerce-request-middleware]]
            [ring.adapter.jetty :refer [run-jetty]]
            [titier-meta]))

(def routes
  (ring/ring-handler
   (ring/router
    (titier-meta/scan-cp)
    {:data {:middleware [parameters-middleware coerce-request-middleware]}})))

(defn run [_]
  (run-jetty app {:port 3000 :join? false}))
```
