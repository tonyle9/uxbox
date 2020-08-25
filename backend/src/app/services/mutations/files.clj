;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2019-2020 Andrey Antukh <niwi@niwi.nz>

(ns app.services.mutations.files
  (:require
   [clojure.spec.alpha :as s]
   [datoteka.core :as fs]
   [promesa.core :as p]
   [app.common.exceptions :as ex]
   [app.common.pages :as cp]
   [app.common.pages-migrations :as pmg]
   [app.common.spec :as us]
   [app.common.uuid :as uuid]
   [app.config :as cfg]
   [app.db :as db]
   [app.redis :as redis]
   [app.services.mutations :as sm]
   [app.services.mutations.projects :as proj]
   [app.services.queries.files :as files]
   [app.tasks :as tasks]
   [app.util.blob :as blob]
   [app.util.storage :as ust]
   [app.util.transit :as t]
   [app.util.time :as dt]))

;; --- Helpers & Specs

(s/def ::id ::us/uuid)
(s/def ::name ::us/string)
(s/def ::profile-id ::us/uuid)
(s/def ::project-id ::us/uuid)
(s/def ::url ::us/url)

;; --- Mutation: Create File

(declare create-file)

(s/def ::is-shared ::us/boolean)
(s/def ::create-file
  (s/keys :req-un [::profile-id ::name ::project-id]
          :opt-un [::id ::is-shared]))

(sm/defmutation ::create-file
  [{:keys [profile-id project-id] :as params}]
  (db/with-atomic [conn db/pool]
    (create-file conn params)))

(defn- create-file-profile
  [conn {:keys [profile-id file-id] :as params}]
  (db/insert! conn :file-profile-rel
              {:profile-id profile-id
               :file-id file-id
               :is-owner true
               :is-admin true
               :can-edit true}))

(defn create-file
  [conn {:keys [id profile-id name project-id is-shared]
         :or {is-shared false}
         :as params}]
  (let [id   (or id (uuid/next))
        data (cp/make-file-data)
        file (db/insert! conn :file
                         {:id id
                          :project-id project-id
                          :name name
                          :is-shared is-shared
                          :data (blob/encode data)})]
    (->> (assoc params :file-id id)
         (create-file-profile conn))
    (assoc file :data data)))


;; --- Mutation: Rename File

(declare rename-file)

(s/def ::rename-file
  (s/keys :req-un [::profile-id ::name ::id]))

(sm/defmutation ::rename-file
  [{:keys [id profile-id] :as params}]
  (db/with-atomic [conn db/pool]
    (files/check-edition-permissions! conn profile-id id)
    (rename-file conn params)))

(defn- rename-file
  [conn {:keys [id name] :as params}]
  (db/update! conn :file
              {:name name}
              {:id id}))


;; --- Mutation: Set File shared

(declare set-file-shared)

(s/def ::set-file-shared
  (s/keys :req-un [::profile-id ::id ::is-shared]))

(sm/defmutation ::set-file-shared
  [{:keys [id profile-id] :as params}]
  (db/with-atomic [conn db/pool]
    (files/check-edition-permissions! conn profile-id id)
    (set-file-shared conn params)))

(defn- set-file-shared
  [conn {:keys [id is-shared] :as params}]
  (db/update! conn :file
              {:is-shared is-shared}
              {:id id}))


;; --- Mutation: Delete File

(declare mark-file-deleted)

(s/def ::delete-file
  (s/keys :req-un [::id ::profile-id]))

(sm/defmutation ::delete-file
  [{:keys [id profile-id] :as params}]
  (db/with-atomic [conn db/pool]
    (files/check-edition-permissions! conn profile-id id)

    ;; Schedule object deletion
    (tasks/submit! conn {:name "delete-object"
                         :delay cfg/default-deletion-delay
                         :props {:id id :type :file}})

    (mark-file-deleted conn params)))

(defn mark-file-deleted
  [conn {:keys [id] :as params}]
  (db/update! conn :file
              {:deleted-at (dt/now)}
              {:id id})
  nil)


;; --- Mutation: Link file to library

(declare link-file-to-library)

(s/def ::link-file-to-library
  (s/keys :req-un [::profile-id ::file-id ::library-id]))

(sm/defmutation ::link-file-to-library
  [{:keys [profile-id file-id library-id] :as params}]
  (when (= file-id library-id)
    (ex/raise :type :validation
              :code :invalid-library
              :hint "A file cannot be linked to itself"))
  (db/with-atomic [conn db/pool]
    (files/check-edition-permissions! conn profile-id file-id)
    (link-file-to-library conn params)))

(defn- link-file-to-library
  [conn {:keys [file-id library-id] :as params}]
  (db/insert! conn :file-library-rel
              {:file-id file-id
               :library-file-id library-id}))


;; --- Mutation: Unlink file from library

(declare unlink-file-from-library)

(s/def ::unlink-file-from-library
  (s/keys :req-un [::profile-id ::file-id ::library-id]))

(sm/defmutation ::unlink-file-from-library
  [{:keys [profile-id file-id library-id] :as params}]
  (db/with-atomic [conn db/pool]
    (files/check-edition-permissions! conn profile-id file-id)
    (unlink-file-from-library conn params)))

(defn- unlink-file-from-library
  [conn {:keys [file-id library-id] :as params}]
  (db/delete! conn :file-library-rel
              {:file-id file-id
               :library-file-id library-id}))






;; A generic, Changes based (granular) file update method.

(s/def ::changes
  (s/coll-of map? :kind vector?))

(s/def ::session-id ::us/uuid)
(s/def ::revn ::us/integer)
(s/def ::update-file
  (s/keys :req-un [::id ::session-id ::profile-id ::revn ::changes]))

(declare update-file)
(declare retrieve-lagged-changes)
(declare insert-change)

(sm/defmutation ::update-file
  [{:keys [id profile-id] :as params}]
  (db/with-atomic [conn db/pool]
    (let [{:keys [id] :as file} (db/get-by-id conn :file id {:for-update true})]
      (files/check-edition-permissions! conn profile-id id)
      (update-file conn file params))))

(defn- update-file
  [conn file params]
  (when (> (:revn params)
           (:revn file))
    (ex/raise :type :validation
              :code :revn-conflict
              :hint "The incoming revision number is greater that stored version."
              :context {:incoming-revn (:revn params)
                        :stored-revn (:revn file)}))
  (let [sid      (:session-id params)
        changes  (:changes params)
        file     (-> file
                     (update :data blob/decode)
                     (update :data pmg/migrate-data)
                     (update :data cp/process-changes changes)
                     (update :data blob/encode)
                     (update :revn inc)
                     (assoc :changes (blob/encode changes)
                            :session-id sid))

        chng     (insert-change conn file)
        msg      {:type :file-change
                  :profile-id (:profile-id params)
                  :file-id (:id file)
                  :session-id sid
                  :revn (:revn file)
                  :changes changes}]

    @(redis/run! :publish {:channel (str (:id file))
                           :message (t/encode-str msg)})

    (db/update! conn :file
                {:revn (:revn file)
                 :data (:data file)}
                {:id (:id file)})

    (retrieve-lagged-changes conn chng params)))

(defn- insert-change
  [conn {:keys [revn data changes session-id] :as file}]
  (let [id      (uuid/next)
        file-id (:id file)]
    (db/insert! conn :file-change
                {:id id
                 :session-id session-id
                 :file-id file-id
                 :revn revn
                 :data data
                 :changes changes})))

(def ^:private
  sql:lagged-changes
  "select s.id, s.revn, s.file_id,
          s.session_id, s.changes
     from file_change as s
    where s.file_id = ?
      and s.revn > ?
    order by s.created_at asc")

(defn- retrieve-lagged-changes
  [conn snapshot params]
  (->> (db/exec! conn [sql:lagged-changes (:id params) (:revn params)])
       (mapv files/decode-row)))
