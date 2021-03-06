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
     (mf/deps file)
     (fn []
       (-> (wrk/ask! {:cmd :thumbnails/generate
                      :id (first (:pages file))
                      })
           (rx/subscribe (fn [{:keys [svg fonts]}]
                           (run! fonts/ensure-loaded! fonts)
                           (when-let [node (mf/ref-val container)]
                             (set! (.-innerHTML ^js node) svg)))))))
    [:div.grid-item-th {:style {:background-color (get-in file [:data :options :background])}
                        :ref container}]))

;; --- Grid Item

(mf/defc grid-item-metadata
  [{:keys [modified-at]}]
  (let [locale (i18n/use-locale)
        time (dt/timeago modified-at {:locale locale})]
    (str (t locale "ds.updated-at" time))))

(mf/defc grid-item
  {:wrap [mf/memo]}
  [{:keys [file] :as props}]
  (let [local (mf/use-state {:menu-open false
                             :edition false})
        locale (i18n/use-locale)
        on-navigate #(st/emit! (rt/nav :workspace
                                       {:project-id (:project-id file)
                                        :file-id (:id file)}
                                       {:page-id (first (:pages file))}))
        delete-fn #(st/emit! nil (dsh/delete-file (:id file)))
        on-delete #(do
                     (dom/stop-propagation %)
                     (modal/show! confirm-dialog {:on-accept delete-fn}))

        add-shared-fn #(st/emit! nil (dsh/set-file-shared (:id file) true))
        on-add-shared
        #(do
           (dom/stop-propagation %)
           (modal/show! confirm-dialog
                        {:message (t locale "dashboard.grid.add-shared-message" (:name file))
                         :hint (t locale "dashboard.grid.add-shared-hint")
                         :accept-text (t locale "dashboard.grid.add-shared-accept")
                         :not-danger? true
                         :on-accept add-shared-fn}))

        remove-shared-fn #(st/emit! nil (dsh/set-file-shared (:id file) false))
        on-remove-shared
        #(do
           (dom/stop-propagation %)
           (modal/show! confirm-dialog
                        {:message (t locale "dashboard.grid.remove-shared-message" (:name file))
                         :hint (t locale "dashboard.grid.remove-shared-hint")
                         :accept-text (t locale "dashboard.grid.remove-shared-accept")
                         :not-danger? false
                         :on-accept remove-shared-fn}))

        on-blur #(let [name (-> % dom/get-target dom/get-value)]
                   (st/emit! (dsh/rename-file (:id file) name))
                   (swap! local assoc :edition false))

        on-key-down #(cond
                       (kbd/enter? %) (on-blur %)
                       (kbd/esc? %) (swap! local assoc :edition false))
        on-menu-click #(do
                         (dom/stop-propagation %)
                         (swap! local assoc :menu-open true))
        on-menu-close #(swap! local assoc :menu-open false)
        on-edit #(do
                   (dom/stop-propagation %)
                   (swap! local assoc :edition true))]
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
      ;; [:div.project-th-icon.pages
      ;;  i/page
      ;;  #_[:span (:total-pages project)]]
      ;; [:div.project-th-icon.comments
      ;;  i/chat
      ;;  [:span "0"]]
      [:div.project-th-icon.menu
       {:on-click on-menu-click}
       i/actions]
      [:& context-menu {:on-close on-menu-close
                        :show (:menu-open @local)
                        :options [[(t locale "dashboard.grid.rename") on-edit]
                                  [(t locale "dashboard.grid.delete") on-delete]
                                  (if (:is-shared file)
                                     [(t locale "dashboard.grid.remove-shared") on-remove-shared]
                                     [(t locale "dashboard.grid.add-shared") on-add-shared])]}]]]))

;; --- Grid

(mf/defc grid
  [{:keys [id opts files hide-new?] :as props}]
  (let [locale (i18n/use-locale)
        order (:order opts :modified)
        filter (:filter opts "")
        on-click #(do
                    (dom/prevent-default %)
                    (st/emit! (dsh/create-file id)))]
    [:section.dashboard-grid
      (if (> (count files) 0)
        [:div.dashboard-grid-row
         (when (not hide-new?)
           [:div.grid-item.add-file {:on-click on-click}
            [:span (tr "ds.new-file")]])
         (for [item files]
           [:& grid-item {:file item :key (:id item)}])]
        [:div.grid-files-empty
         [:div.grid-files-desc (t locale "dashboard.grid.empty-files")]
         [:div.grid-files-link
          [:a.btn-secondary.btn-small {:on-click on-click} (t locale "ds.new-file")]]])]))
