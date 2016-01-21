(ns lastfm.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]
              [ajax.core :refer  [ajax-request url-request-format json-response-format]]
              [cemerick.url :as url]
              [clojure.string :refer [join trim split capitalize lower-case]]))

(def ^:private date-artist-name-album
  (fn [track]
    [:div.track
    [:div.date (:#text (:date track))]
    [:div.artist (:#text (:artist track))]
    [:div.name (:name track)]
    [:div.album (:#text (:album track))]]))

(def ^:private top-tracks-fn
  (fn [track]
    [:div.track
     [:div.artist (:name (:artist track))]
     [:div.name (:name track)]
     [:div.playcount (str (:playcount track) " times")]]))

(def ^:private top-artists-fn
  (fn [track]
    [:div.track
     [:div.name (:name track)]
     [:div.playcount (str (:playcount track) " times")]]))

(def ^:private as-url-prefix "http://ws.audioscrobbler.com/2.0/?method=")
(def ^:private as-url-suffix "&api_key=4d9d38032cb68351994d53a6622d5db7&format=json")
(def ^:private user-methods (dissoc
  {:artist-tracks {:method "user.getartisttracks" :f date-artist-name-album :uri [:user :artist :start-timestamp :end-timestamp] :k :track}
   :friends {:method "user.getfriends"} 
   :info {:method "user.getinfo"} 
   :loved-tracks {:method "user.getlovedtracks" :f date-artist-name-album :uri [:user :limit] :k :track} 
   :personal-tags {:method "user.getpersonaltags"} 
   :recent-tracks {:method "user.getrecenttracks" :f date-artist-name-album :uri [:user :limit] :k :track} 
   :top-albums {:method "user.gettopalbums" :f top-tracks-fn :uri [:user :limit :period] :k :album} 
   :top-artists {:method "user.gettopartists" :f top-artists-fn :uri [:user :limit :period] :k :artist} 
   :top-tags {:method "user.gettoptags"} 
   :top-tracks {:method "user.gettoptracks" :f top-tracks-fn :uri [:user :limit :period] :k :track} 
   :weekly-album-chart {:method "user.getweeklyalbumchart"} 
   :weekly-artist-chart {:method "user.getweeklyartistchart"} 
   :weekly-chart-list {:method "user.getweeklychartlist"} 
   :weekly-track-chart {:method "user.getweeklytrackchart"} 
   } :friends :info :personal-tags :top-tags :weekly-album-chart :weekly-chart-list :weekly-artist-chart :weekly-track-chart))

(def ^:private period-options ["overall" "7day" "1month" "3month" "6month" "12month"])

(def ^:private lepton (atom {:method :artist-tracks
                             :user ""
                             :artist ""
                             :start-time "" :start-timestamp 0
                             :end-time "" :end-timestamp 0
                             :limit 200
                             :period (first period-options)
                             :response "No hay nada"
                             :error []
                             :err-str [:p "Nada"]}))

(def ^:private lepton-errors
  {:user {:f #(empty? (trim (:user @lepton))) :e "Add a username, ya leper"}
   :artist {:f #(empty? (trim (:artist @lepton))) :e "An artist is required, ya mullosk"}
   :start-timestamp {:f #(= 0 (:start-timestamp @lepton)) :e "A valid start date is recommended, muon-boy"}
   :end-timestamp {:f #(= 0 (:end-timestamp @lepton)) :e "A decent end date is your friend"}
   :limit {:f #(= 0 (:limit @lepton)) :e "A limit of zero shaves no yak"}
   :period {:f #(empty? (:period @lepton)) :e "This never happens"}})

(defn- to-camel-case  [s] 
  (let  [[p & r]  (split s #"[-_]")] 
    (str p (join "" (map capitalize r)))))

(defn- make-uri [method]
  (let [keys (get-in user-methods [method :uri])]
    ;; (js/alert (str method " -- " keys))
    (str 
      (join "&" (cons (str as-url-prefix (get-in user-methods [method :method]))
                      (map #(str (to-camel-case (name %)) "=" (url/url-encode (get @lepton %))) keys)))
      as-url-suffix)))

(defn format-response [res]
  (let [dauntabel (keyword ((comp lower-case to-camel-case) (name (:method @lepton))))
        shaven (get res dauntabel)
        tracks (get shaven (get-in user-methods [(:method @lepton) :k]))]
    (vec (cons :div.tracks
               (map (get-in user-methods [(:method @lepton) :f]) tracks)))))

(defn- lastfm-request []
  (let [uri (make-uri (:method @lepton))]
    ;; (js/alert uri)
    (swap! lepton assoc :response "Pondering ...")
    (ajax-request {:uri uri
                   :method :get
                   :response-format (json-response-format {:keywords? true})
                   :handler (fn [[ok res]]
                              (if ok
                                (swap! lepton assoc :response (format-response res))
                                (swap! lepton assoc :response (str "Something went awry: " (:status-text res)))))})))

(defn- error-check []
  (swap! lepton assoc :error
         (reduce (fn [es k]
                   (if ((get-in lepton-errors [k :f]))
                     (conj es (get-in lepton-errors [k :e]))
                     es))
                 [] (get-in user-methods [(:method @lepton) :uri]))))

;; -------------------------
;; Views

(defn- thurk-errors []
  (let [errors (:error @lepton)]
    (if (empty? errors)
      [:p "All is quiet in the northern wastes."]
      (vec (cons :p (interpose [:br] (map #(vector :span %) errors)))))))

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
          (ms-change! [e] (swap! lepton assoc :method (keyword (-> e .-target .-value))))]
    (let [elegir (vec (cons :select.form-control
                            (cons {:field :list :on-change ms-change! :default-value (:method @lepton)} (opts))))]
      elegir)))

(defn- period-select []
  (letfn [(ps-change! [e] (swap! lepton assoc :period (-> e .-target .-value)))]
    (vec 
      (cons :select.form-control
            (cons {:field :list
                   :on-change ps-change!
                   :default-value (:period @lepton)}
                  (map #(vector :option {:key % :value %} %) period-options))))))

(defn- sraz [key]
  [:div.datepacker {:field :datepicker :id key :date-format "yyyy/mm/dd" :inline true}])

(defn- submit []
  (letfn [(submit! []
            (do (error-check)
                (when (empty? (:error @lepton))
                  (lastfm-request))))]
    [:button.btn.btn-success {:on-click submit!} "Oouh!"]))

(defn home-page []
  (do
    (fn []
      [:div
        [:div.page-header [:h3 "I need to water that plant in the corner"]]
        [:div.col-md-3
          [:div.row (method-select)]
          [:div.row
            (gen-input :user)]
          [:div.row (gen-input :artist)]
          [:div.row (date-input :start-time)]
          [:div.row (date-input :end-time)]
          [:div.row (period-select)]
          [:div.row (submit)]
          [:div#errors.row (thurk-errors)]
          [:div.row (str "status: " (:method @lepton))]]
        [:div.col-md-9
         (:response @lepton)]])))

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
