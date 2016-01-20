(ns lastfm.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]
              [ajax.core :refer  [ajax-request url-request-format json-response-format]]
              [cemerick.url :as url]))

(def ^:private as-url-prefix "http://ws.audioscrobbler.com/2.0/?method=")
(def ^:private as-url-suffix "api_key=4d9d38032cb68351994d53a6622d5db7&format=json")
(def ^:private user-methods
  {:artist-tracks "user.getartisttracks"
   :friends "user.getfriends" 
   :info "user.getinfo" 
   :loved-tracks "user.getlovedtracks" 
   :personal-tags "user.getpersonaltags" 
   :recent-tracks "user.getrecenttracks" 
   :top-albums "user.gettopalbums" 
   :top-artists "user.gettopartists" 
   :top-tags "user.gettoptags" 
   :top-tracks "user.gettoptracks" 
   :weekly-album-chart "user.getweeklyalbumchart" 
   :weekly-artist-chart "user.getweeklyartistchart" 
   :weekly-chart-list "user.getweeklychartlist" 
   :weekly-track-chart "user.getweeklytrackchart" 
   })

(def ^:private lepton (atom {:method :artist-tracks
                             :username ""
                             :artist ""
                             :start-time "" :start-timestamp 0
                             :end-time "" :end-timestamp 0}))

;; -------------------------
;; Views

(defn- gen-input [key]
  (letfn [(change! [e] (swap! lepton assoc key (-> e .-target .-value)))]
    [:input.form-control {:field :text
                          :on-change change!
                          :placeholder (name key)
                          :value (get @lepton key)}]))

(defn- date-input [key]
  (let [key-stamp (keyword (str (name key) "stamp"))]
    (letfn [(change! [e]
              (let [v (-> e .-target .-value)
                    dia (re-seq #"(\d\d\d\d)[-/](\d+)[-/](\d+)" v)]
                (swap! lepton assoc key v)
                (if (nil? dia)
                  (swap! lepton assoc key-stamp 0)
                  (swap! lepton assoc key-stamp (-> (clojure.string/join "/" (drop 1 (first dia)))
                                                    (js/Date.)
                                                    (.getTime)
                                                    (/ 1000)
                                                    (Math/floor))))))]
      [:input.form-control {:field :text :on-change change! :placeholder (str (name key) " yyyy/mm/dd") :value (get @lepton key)}])))

(defn- method-select []
  (letfn [(opts [] (map (fn [key]
                          (let [hm (hash-map :key key :value value)]
                            (vector :option (if (= :bastard (:method @lepton))
                                              (assoc hm :selected true)
                                              hm) (name key))))
                        (keys user-methods)))
          (ms-change! [e] (swap! lepton assoc :method (-> e .-target .-value)))]
    (let [elegir (vec (cons :select.form-control
                            (cons {:field :list :on-change ms-change! :default-value (:method @lepton)} (opts))))]
      elegir)))

(defn- sraz [key]
  [:div.datepacker {:field :datepicker :id key :date-format "yyyy/mm/dd" :inline true}])

(defn home-page []
  (fn []
    [:div
      [:div.page-header [:h3 "I need to water that plant in the corner"]]
      [:div.col-md-3
        [:div.row (method-select)]
        [:div.row
          (gen-input :username)]
        [:div.row (gen-input :artist)]
        [:div.row (date-input :start-time)]
        [:div.row (date-input :end-time)]
        [:div.row [:a {:href "/about"} (str "go to about page: " (:username @lepton))]]]
      [:div.col-md-6
        (str "I've been bitten! " (:start-timestamp @lepton) "  " (:end-timestamp @lepton))]]))

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!)
  (accountant/dispatch-current!)
  (mount-root))