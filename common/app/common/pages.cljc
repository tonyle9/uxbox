;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; This Source Code Form is "Incompatible With Secondary Licenses", as
;; defined by the Mozilla Public License, v. 2.0.
;;
;; Copyright (c) 2020 UXBOX Labs SL

(ns app.common.pages
  "A common (clj/cljs) functions and specs for pages."
  (:require
   [clojure.spec.alpha :as s]
   [app.common.data :as d]
   [app.common.pages-helpers :as cph]
   [app.common.exceptions :as ex]
   [app.common.geom.shapes :as geom]
   [app.common.geom.matrix :as gmt]
   [app.common.geom.point :as gpt]
   [app.common.spec :as us]
   [app.common.uuid :as uuid]))

(def page-version 5)
(def file-version 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page Transformation Changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --- Specs

(s/def ::id uuid?)
(s/def ::shape-id uuid?)
(s/def ::session-id uuid?)
(s/def ::name string?)
(s/def ::parent-id uuid?)

;; Page Options
(s/def ::grid-x number?)
(s/def ::grid-y number?)
(s/def ::grid-color string?)

(s/def ::options
  (s/keys :opt-un [::grid-y
                   ::grid-x
                   ::grid-color]))

;; Interactions

(s/def ::event-type #{:click}) ; In the future we will have more options
(s/def ::action-type #{:navigate})
(s/def ::destination uuid?)

(s/def ::interaction
  (s/keys :req-un [::event-type
                   ::action-type
                   ::destination]))

(s/def ::interactions (s/coll-of ::interaction :kind vector?))

;; Page Data related
(s/def ::blocked boolean?)
(s/def ::collapsed boolean?)
(s/def ::content any?)
(s/def ::fill-color string?)
(s/def ::fill-opacity number?)
(s/def ::font-family string?)
(s/def ::font-size number?)
(s/def ::font-style string?)
(s/def ::font-weight string?)
(s/def ::hidden boolean?)
(s/def ::letter-spacing number?)
(s/def ::line-height number?)
(s/def ::locked boolean?)
(s/def ::page-id uuid?)
(s/def ::proportion number?)
(s/def ::proportion-lock boolean?)
(s/def ::rx number?)
(s/def ::ry number?)
(s/def ::stroke-color string?)
(s/def ::stroke-opacity number?)
(s/def ::stroke-style #{:solid :dotted :dashed :mixed :none})
(s/def ::stroke-width number?)
(s/def ::stroke-alignment #{:center :inner :outer})
(s/def ::text-align #{"left" "right" "center" "justify"})
(s/def ::type keyword?)
(s/def ::x number?)
(s/def ::y number?)
(s/def ::cx number?)
(s/def ::cy number?)
(s/def ::width number?)
(s/def ::height number?)
(s/def ::index integer?)
(s/def ::x1 number?)
(s/def ::y1 number?)
(s/def ::x2 number?)
(s/def ::y2 number?)

(s/def ::suffix string?)
(s/def ::scale number?)
(s/def ::export
  (s/keys :req-un [::type ::suffix ::scale]))

(s/def ::exports (s/coll-of ::export :kind vector?))


(s/def ::selrect (s/keys :req-un [::x
                                  ::y
                                  ::x1
                                  ::y1
                                  ::x2
                                  ::y2
                                  ::width
                                  ::height]))

(s/def ::point (s/keys :req-un [::x ::y]))
(s/def ::points (s/coll-of ::point :kind vector?))

(s/def ::shape-attrs
  (s/keys :opt-un [::blocked
                   ::collapsed
                   ::content
                   ::fill-color
                   ::fill-opacity
                   ::font-family
                   ::font-size
                   ::font-style
                   ::font-weight
                   ::hidden
                   ::letter-spacing
                   ::line-height
                   ::locked
                   ::proportion
                   ::proportion-lock
                   ::rx ::ry
                   ::cx ::cy
                   ::x ::y
                   ::exports
                   ::stroke-color
                   ::stroke-opacity
                   ::stroke-style
                   ::stroke-width
                   ::stroke-alignment
                   ::text-align
                   ::width ::height
                   ::interactions
                   ::selrect
                   ::points]))

(s/def ::minimal-shape
  (s/keys :req-un [::type ::name]
          :opt-un [::id]))

(s/def ::shape
  (s/and ::minimal-shape ::shape-attrs
         (s/keys :opt-un [::id])))

(s/def ::shapes (s/coll-of uuid? :kind vector?))
(s/def ::canvas (s/coll-of uuid? :kind vector?))

(s/def ::objects
  (s/map-of uuid? ::shape))

(s/def ::page
  (s/keys :req-un [::id
                   ::name
                   ::options
                   ::objects]))

(s/def ::pages (s/coll-of ::us/uuid :kind vector?))
(s/def ::pages-index (s/map-of ::us/uuid ::page))

;; TODO: missing colors and components
(s/def ::data
  (s/keys :req-un [::pages-index ::pages]))

(s/def ::ids (s/coll-of ::us/uuid))
(s/def ::attr keyword?)
(s/def ::val any?)
(s/def ::frame-id uuid?)

(defmulti operation-spec-impl :type)

(defmethod operation-spec-impl :set [_]
  (s/keys :req-un [::attr ::val]))

(defmulti change-spec-impl :type)

(s/def :internal.changes.set-option/option any?)
(s/def :internal.changes.set-option/value any?)

(defmethod change-spec-impl :set-option [_]
  (s/keys :req-un [:internal.changes.set-option/option
                   :internal.changes.set-option/value]))

(defmethod change-spec-impl :add-obj [_]
  (s/keys :req-un [::id ::page-id ::frame-id ::obj]
          :opt-un [::parent-id]))


(s/def ::operation (s/multi-spec operation-spec-impl :type))
(s/def ::operations (s/coll-of ::operation))

(defmethod change-spec-impl :mod-obj [_]
  (s/keys :req-un [::id ::page-id ::operations]))

(defmethod change-spec-impl :del-obj [_]
  (s/keys :req-un [::id ::page-id]))

(defmethod change-spec-impl :reg-objects [_]
  (s/keys :req-un [::page-id ::shapes]))

(defmethod change-spec-impl :mov-objects [_]
  (s/keys :req-un [::page-id ::parent-id ::shapes]
          :opt-un [::index]))

(defmethod change-spec-impl :add-page [_]
  (s/or :empty (s/keys :req-un [::id ::name])
        :complete (s/keys :req-un [::page])))

(defmethod change-spec-impl :mod-page [_]
  (s/keys :req-un [::id ::name]))

(defmethod change-spec-impl :del-page [_]
  (s/keys :req-un [::id]))



(s/def ::change (s/multi-spec change-spec-impl :type))
(s/def ::changes (s/coll-of ::change))

(def root uuid/zero)

;; TODO: pending to be removed
(def default-page-data
  "A reference value of the empty page data."
  {:version page-version
   :options {}
   :objects
   {root
    {:id root
     :type :frame
     :name "root"
     :shapes []}}})

(def empty-page-data
  {:options {}
   :name "Page"
   :objects
   {root
    {:id root
     :type :frame
     :name "Root Frame"}}})

(def empty-file-data
  {:version file-version
   :pages []
   :pages-index {}})

(def default-color "#b1b2b5") ;; $color-gray-20
(def default-shape-attrs
  {:fill-color default-color
   :fill-opacity 1})

(def default-frame-attrs
  {:frame-id uuid/zero
   :fill-color "#ffffff"
   :fill-opacity 1
   :shapes []})

(def ^:private minimal-shapes
  [{:type :rect
    :name "Rect"
    :fill-color default-color
    :fill-opacity 1
    :stroke-style :none
    :stroke-alignment :center
    :stroke-width 0
    :stroke-color "#000000"
    :stroke-opacity 0
    :rx 0
    :ry 0}

   {:type :image}

   {:type :icon}

   {:type :circle
    :name "Circle"
    :fill-color default-color
    :fill-opacity 1
    :stroke-style :none
    :stroke-alignment :center
    :stroke-width 0
    :stroke-color "#000000"
    :stroke-opacity 0}

   {:type :path
    :name "Path"
    :fill-color "#000000"
    :fill-opacity 0
    :stroke-style :solid
    :stroke-alignment :center
    :stroke-width 2
    :stroke-color "#000000"
    :stroke-opacity 1
    :segments []}

   {:type :frame
    :name "Artboard"
    :fill-color "#ffffff"
    :fill-opacity 1
    :stroke-style :none
    :stroke-alignment :center
    :stroke-width 0
    :stroke-color "#000000"
    :stroke-opacity 0}

   {:type :curve
    :name "Path"
    :fill-color "#000000"
    :fill-opacity 0
    :stroke-style :solid
    :stroke-alignment :center
    :stroke-width 2
    :stroke-color "#000000"
    :stroke-opacity 1
    :segments []}

   {:type :text
    :name "Text"
    :content nil}])

(defn make-minimal-shape
  [type]
  (let [shape (d/seek #(= type (:type %)) minimal-shapes)]
    (when-not shape
      (ex/raise :type :assertion
                :code :shape-type-not-implemented
                :context {:type type}))
    (assoc shape
           :id (uuid/next)
           :x 0
           :y 0
           :width 1
           :height 1
           :selrect {:x 0
                     :x1 0
                     :x2 1
                     :y 0
                     :y1 0
                     :y2 1
                     :width 1
                     :height 1}
           :points []
           :segments [])))

(defn make-file-data
  []
  (let [id (uuid/next)
        pd (assoc empty-page-data
                  :id id
                  :name "Page-1")]
    (-> empty-file-data
        (update :pages conj id)
        (update :pages-index assoc id pd))))

;; --- Changes Processing Impl

(defmulti process-change (fn [data change] (:type change)))
(defmulti process-operation (fn [_ op] (:type op)))

(defn process-changes
  [data items]
  (->> (us/verify ::changes items)
       (reduce #(do
                  ;; (prn "process-change" (:type %2) (:id %2))
                  (or (process-change %1 %2) %1))
               data)))

(defmethod process-change :set-option
  [data {:keys [page-id option value]}]
  (d/update-in-when data [:pages-index page-id]
                    (fn [data]
                      (let [path (if (seqable? option) option [option])]
                        (if value
                          (assoc-in data (into [:options] path) value)
                          (assoc data :options (d/dissoc-in (:options data) path)))))))

(defmethod process-change :add-obj
  [data {:keys [id obj page-id frame-id parent-id index] :as change}]
  (d/update-in-when data [:pages-index page-id]
                    (fn [data]
                      (let [parent-id (or parent-id frame-id)
                            objects (:objects data)]
                        (when (and (contains? objects parent-id)
                                   (contains? objects frame-id))
                          (let [obj (assoc obj
                                           :frame-id frame-id
                                           :parent-id parent-id
                                           :id id)]
                            (-> data
                                (update :objects assoc id obj)
                                (update-in [:objects parent-id :shapes]
                                           (fn [shapes]
                                             (let [shapes (or shapes [])]
                                               (cond
                                                 (some #{id} shapes) shapes
                                                 (nil? index) (conj shapes id)
                                                 :else (cph/insert-at-index shapes index [id]))))))))))))

(defmethod process-change :mod-obj
  [data {:keys [id page-id operations] :as change}]
  (d/update-in-when data [:pages-index page-id :objects]
                    (fn [objects]
                      (if-let [obj (get objects id)]
                        (assoc objects id (reduce process-operation obj operations))
                        objects))))

(defmethod process-change :del-obj
  [data {:keys [page-id id] :as change}]
  (letfn [(delete-object [objects id]
            (if-let [target (get objects id)]
              (let [parent-id (cph/get-parent id objects)
                    frame-id  (:frame-id target)
                    parent    (get objects parent-id)
                    objects   (dissoc objects id)]
                (cond-> objects
                  (and (not= parent-id frame-id)
                       (= :group (:type parent)))
                  (update-in [parent-id :shapes] (fn [s] (filterv #(not= % id) s)))

                  (contains? objects frame-id)
                  (update-in [frame-id :shapes] (fn [s] (filterv #(not= % id) s)))

                  (seq (:shapes target))   ; Recursive delete all
                                           ; dependend objects
                  (as-> $ (reduce delete-object $ (:shapes target)))))
              objects))]
    (d/update-in-when data [:pages-index page-id :objects] delete-object id)))

(defn rotation-modifiers
  [center shape angle]
  (let [displacement (let [shape-center (geom/center shape)]
                       (-> (gmt/matrix)
                           (gmt/rotate angle center)
                           (gmt/rotate (- angle) shape-center)))]
    {:rotation angle
     :displacement displacement}))

(defmethod process-change :reg-objects
  [data {:keys [page-id shapes]}]
  (letfn [(reg-objects [objects]
            (reduce #(update %1 %2 update-group %1) objects
                    (sequence (comp
                               (mapcat #(cons % (cph/get-parents % objects)))
                               (map #(get objects %))
                               (filter #(= (:type %) :group))
                               (map :id)
                               (distinct))
                              shapes)))
          (update-group [group objects]
            (let [gcenter (geom/center group)
                  gxfm    (comp
                           (map #(get objects %))
                           (map #(-> %
                                     (assoc :modifiers
                                            (rotation-modifiers gcenter % (- (:rotation group 0))))
                                     (geom/transform-shape))))
                  selrect (-> (into [] gxfm (:shapes group))
                              (geom/selection-rect))]

              ;; Rotate the group shape change the data and rotate back again
              (-> group
                  (assoc-in [:modifiers :rotation] (- (:rotation group)))
                  (geom/transform-shape)
                  (merge (select-keys selrect [:x :y :width :height]))
                  (assoc-in [:modifiers :rotation] (:rotation group))
                  (geom/transform-shape))))]

    (d/update-in-when data [:pages-index page-id :objects] reg-objects)))


(defmethod process-change :mov-objects
  [data {:keys [parent-id shapes index page-id] :as change}]
  (letfn [(is-valid-move? [objects shape-id]
            (let [invalid-targets (cph/calculate-invalid-targets shape-id objects)]
              (and (not (invalid-targets parent-id))
                   (cph/valid-frame-target shape-id parent-id objects))))

          (insert-items [prev-shapes index shapes]
            (let [prev-shapes (or prev-shapes [])]
              (if index
                (cph/insert-at-index prev-shapes index shapes)
                (reduce (fn [acc id]
                          (if (some #{id} acc)
                            acc
                            (conj acc id)))
                        prev-shapes
                        shapes))))

          (strip-id [coll id]
            (filterv #(not= % id) coll))

        (remove-from-old-parent [cpindex objects shape-id]
          (let [prev-parent-id (get cpindex shape-id)]
            ;; Do nothing if the parent id of the shape is the same as
            ;; the new destination target parent id.
            (if (= prev-parent-id parent-id)
              objects
              (loop [sid shape-id
                     pid prev-parent-id
                     objects objects]
                (let [obj (get objects pid)]
                  (if (and (= 1 (count (:shapes obj)))
                           (= sid (first (:shapes obj)))
                           (= :group (:type obj)))
                    (recur pid
                           (:parent-id obj)
                           (dissoc objects pid))
                    (update-in objects [pid :shapes] strip-id sid)))))))


          (update-parent-id [objects id]
            (update objects id assoc :parent-id parent-id))

          ;; Updates the frame-id references that might be outdated
          (update-frame-ids [frame-id objects id]
            (let [data (assoc-in objects [id :frame-id] frame-id)
                  obj  (get objects id)]
              (cond-> objects
                (not= :frame (:type obj))
                (as-> $$ (reduce update-frame-ids $$ (:shapes obj))))))

          (move-objects [objects]
            (let [valid?  (every? (partial is-valid-move? objects) shapes)
                  cpindex (reduce (fn [index id]
                                    (let [obj (get objects id)]
                                      (assoc! index id (:parent-id obj))))
                                  (transient {})
                                  (keys objects))
                  cpindex (persistent! cpindex)

                  parent  (get-in data [:objects parent-id])
                  parent  (get objects parent-id)
                  frame   (if (= :frame (:type parent))
                            parent
                            (get objects (:frame-id parent)))

                  frm-id  (:id frame)]

              (if valid?
                (as-> objects $
                  (update-in $ [parent-id :shapes] insert-items index shapes)
                  (reduce update-parent-id $ shapes)
                  (reduce (partial remove-from-old-parent cpindex) $ shapes)
                  (reduce (partial update-frame-ids frm-id) $ (get-in $ [parent-id :shapes])))
                objects)))]

    (d/update-in-when data [:pages-index page-id :objects] move-objects)))

(defmethod process-change :add-page
  [data {:keys [id name page]}]
  (cond
    (and (string? name) (uuid? id))
    (let [page (assoc empty-page-data
                      :id id
                      :name name)]
      (-> data
          (update :pages conj id)
          (update :pages-index assoc id page)))

    (map? page)
    (->> data
         (update :pages conj (:id page)
                 (update :pages-index assoc (:id page))))

    :else
    (ex/raise :type :conflict
              :hint "name or page should be provided, never both")))



(defmethod process-change :mod-page
  [data {:keys [id name]}]
  (d/update-in-when data [:pages-index id] assoc :name name))

(defmethod process-change :del-page
  [data {:keys [id]}]
  (-> data
      (update :pages (fn [pages] (filterv #(not= % id) pages)))
      (update :pages-index dissoc id)))


(defmethod process-operation :set
  [shape op]
  (let [attr (:attr op)
        val  (:val op)]
    (if (nil? val)
      (dissoc shape attr)
      (assoc shape attr val))))

(defmethod process-operation :default
  [shape op]
  (ex/raise :type :not-implemented
            :code :operation-not-implemented
            :context {:type (:type op)}))


;; (defn testfn [a b c] (+ a b c))
;; (simple-benchmark [xfn #(testfn 1 2 %)] (xfn 3) 100000000)
;; (simple-benchmark [xfn (partial testfn 1 2)] (xfn 3) 100000000)

