(ns app.main.ui.dashboard.grid
  (:require
   [cuerdas.core :as str]
   [beicon.core :as rx]
   [rumext.alpha :as mf]
   [app.main.ui.icons :as i]
   [app.main.data.dashboard :as dsh]
   [app.main.store :as st]
   [app.main.ui.modal :as modal]
   [app.main.ui.keyboard :as kbd]
   [app.main.ui.confirm :refer [confirm-dialog]]
   [app.main.ui.components.context-menu :refer [context-menu]]
   [app.main.worker :as wrk]
   [app.main.fonts :as fonts]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [t tr]]
   [app.util.router :as rt]
   [app.util.timers :as ts]
   [app.util.time :as dt]))

;; --- Grid Item Thumbnail

(mf/defc grid-item-thumbnail
  {::mf/wrap [mf/memo]}
  [{:keys [file] :as props}]
  (let [container (mf/use-ref)]
    (mf/use-effect
     (mf/deps (:id file))
     (fn []
       (->> (wrk/ask! {:cmd :thumbnails/generate
                       :id (first (:pages file))})
            (rx/subs (fn [{:keys [svg fonts]}]
                       (run! fonts/ensure-loaded! fonts)
                       (when-let [node (mf/ref-val container)]
                         (set! (.-innerHTML ^js node) svg)))))))
    [:div.grid-item-th {:style {:background-color (get-in file [:data :options :background])}
                        :ref container}]))

;; --- Grid Item

(mf/defc grid-item-metadata
  [{:keys [modified-at]}]
  (let [locale (mf/deref i18n/locale)
        time   (dt/timeago modified-at {:locale locale})]
    (str (t locale "ds.updated-at" time))))

(mf/defc grid-item
  {:wrap [mf/memo]}
  [{:keys [id file] :as props}]
  (let [local  (mf/use-state {:menu-open false :edition false})
        locale (mf/deref i18n/locale)

        delete     (mf/use-callback (mf/deps id) #(st/emit! nil (dsh/delete-file id)))
        add-shared (mf/use-callback (mf/deps id) #(st/emit! (dsh/set-file-shared id true)))
        del-shared (mf/use-callback (mf/deps id) #(st/emit! (dsh/set-file-shared id false)))
        on-close   (mf/use-callback #(swap! local assoc :menu-open false))

        on-delete
        (mf/use-callback
         (mf/deps id)
         (fn [event]
           (dom/stop-propagation event)
           (modal/show! confirm-dialog {:on-accept delete})))

        on-navigate
        (mf/use-callback
         (mf/deps id)
         (fn []
           (let [pparams {:project-id (:project-id file)
                          :file-id (:id file)}
                 qparams {:page-id (first (get-in file [:data :pages]))}]
             (st/emit! (rt/nav :workspace pparams qparams)))))

        on-add-shared
        (mf/use-callback
         (mf/deps id)
         (fn [event]
           (dom/stop-propagation event)
           (modal/show! confirm-dialog
                        {:message (t locale "dashboard.grid.add-shared-message" (:name file))
                         :hint (t locale "dashboard.grid.add-shared-hint")
                         :accept-text (t locale "dashboard.grid.add-shared-accept")
                         :not-danger? true
                         :on-accept add-shared})))

        on-edit
        (mf/use-callback
         (mf/deps id)
         (fn [event]
           (dom/stop-propagation event)
           (swap! local assoc :edition true)))

        on-del-shared
        (mf/use-callback
         (mf/deps id)
         (fn [event]
           (dom/stop-propagation event)
           (modal/show! confirm-dialog
                        {:message (t locale "dashboard.grid.remove-shared-message" (:name file))
                         :hint (t locale "dashboard.grid.remove-shared-hint")
                         :accept-text (t locale "dashboard.grid.remove-shared-accept")
                         :not-danger? false
                         :on-accept del-shared})))

        on-menu-click
        (mf/use-callback
         (mf/deps id)
         (fn [event]
           (dom/stop-propagation event)
           (swap! local assoc :menu-open true)))

        on-blur
        (mf/use-callback
         (mf/deps id)
         (fn [event]
           (let [name (-> event dom/get-target dom/get-value)]
             (st/emit! (dsh/rename-file id name))
             (swap! local assoc :edition false))))

        on-key-down
        (mf/use-callback
         #(cond
            (kbd/enter? %) (on-blur %)
            (kbd/esc? %) (swap! local assoc :edition false)))

        ]
    [:div.grid-item.project-th {:on-click on-navigate}
     [:div.overlay]
     [:& grid-item-thumbnail {:file file}]
     (when (:is-shared file)
       [:div.item-badge
         i/library])
     [:div.item-info
      (if (:edition @local)
        [:input.element-name {:type "text"
                              :auto-focus true
                              :on-key-down on-key-down
                              :on-blur on-blur
                              :default-value (:name file)}]
        [:h3 (:name file)])
      [:& grid-item-metadata {:modified-at (:modified-at file)}]]
     [:div.project-th-actions {:class (dom/classnames
                                       :force-display (:menu-open @local))}
      [:div.project-th-icon.menu
       {:on-click on-menu-click}
       i/actions]
      [:& context-menu {:on-close on-close
                        :show (:menu-open @local)
                        :options [[(t locale "dashboard.grid.rename") on-edit]
                                  [(t locale "dashboard.grid.delete") on-delete]
                                  (if (:is-shared file)
                                     [(t locale "dashboard.grid.remove-shared") on-del-shared]
                                     [(t locale "dashboard.grid.add-shared") on-add-shared])]}]]]))

;; --- Grid

(mf/defc grid
  [{:keys [id opts files hide-new?] :as props}]
  (let [locale (mf/deref i18n/locale)
        click  #(st/emit! (dsh/create-file id))]
    [:section.dashboard-grid
     (cond
       (pos? (count files))
       [:div.dashboard-grid-row
        (when (not hide-new?)
          [:div.grid-item.add-file {:on-click click}
           [:span (t locale "ds.new-file")]])

        (for [item files]
          [:& grid-item
           {:id (:id item)
            :file item
            :key (:id item)}])]

       (zero? (count files))
       [:div.grid-files-empty
        [:div.grid-files-desc (t locale "dashboard.grid.empty-files")]
        [:div.grid-files-link
         [:a.btn-secondary.btn-small {:on-click click} (t locale "ds.new-file")]]])]))
