;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns app.http
  (:require
   [clojure.tools.logging :as log]
   [mount.core :as mount :refer [defstate]]
   [reitit.ring :as rring]
   [ring.adapter.jetty9 :as jetty]
   [app.config :as cfg]
   [app.http.auth :as auth]
   [app.http.auth.google :as google]
   [app.http.auth.ldap :as ldap]
   [app.http.debug :as debug]
   [app.http.errors :as errors]
   [app.http.handlers :as handlers]
   [app.http.middleware :as middleware]
   [app.http.session :as session]
   [app.http.ws :as ws]
   [app.metrics :as mtx]
   [app.services.notifications :as usn]))

(defn- create-router
  []
  (rring/router
   [["/metrics" {:get mtx/dump}]
    ["/api" {:middleware [[middleware/format-response-body]
                          [middleware/errors errors/handle]
                          [middleware/parse-request-body]
                          [middleware/params]
                          [middleware/multipart-params]
                          [middleware/keyword-params]
                          [middleware/cookies]]}

     ["/oauth"
      ["/google" {:post google/auth}]
      ["/google/callback" {:get google/callback}]]

     ["/echo" {:get handlers/echo-handler
               :post handlers/echo-handler}]

     ["/login" {:handler auth/login-handler
                :method :post}]
     ["/logout" {:handler auth/logout-handler
                 :method :post}]
     ["/login-ldap" {:handler ldap/auth
                     :method :post}]

     ["/w" {:middleware [session/middleware]}
      ["/query/:type" {:get handlers/query-handler}]
      ["/mutation/:type" {:post handlers/mutation-handler}]]]]))

(defn start-server
  []
  (let [wsockets {"/ws/notifications" ws/handler}
        options  {:port (:http-server-port cfg/config)
                  :h2c? true
                  :join? false
                  :allow-null-path-info true
                  :websockets wsockets}
        handler  (rring/ring-handler
                  (create-router)
                  (constantly {:status 404, :body ""})
                  {:middleware [[middleware/development-resources]
                                [middleware/development-cors]
                                [middleware/metrics]]})]
    (log/infof "Http server listening on http://localhost:%s/"
               (:http-server-port cfg/config))
    (jetty/run-jetty handler options)))

(defstate server
  :start (start-server)
  :stop (.stop server))
