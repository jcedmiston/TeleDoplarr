(ns teledoplarr.backends.overseerr
  (:require
   [clojure.core.async :as a]
   [teledoplarr.backends.overseerr.impl :as impl]
   [teledoplarr.state :as state]
   [teledoplarr.utils :as utils]))

(defn search [term media-type]
  (let [type (impl/media-type media-type)]
    (utils/request-and-process-body
     impl/GET
     (partial impl/process-search-result type)
     (str "/search?query=" term))))

; In Overseerr, the only additional option we'll need is which season,
; if the request type is a series
(defn additional-options [result media-type]
  (a/go
    (let [details (a/<! (impl/details (:id result) media-type))
          {:keys [partial-seasons]} @state/config]
      (when (= media-type :series)
        (let [seasons (impl/seasons-list details)
              backend-partial-seasons? (a/<! (impl/partial-seasons?))]
          {:season (cond
                     (= 2 (count seasons)) (:id (first seasons))
                     (false? partial-seasons) -1
                     (false? backend-partial-seasons?) -1
                     :else (impl/seasons-list details))
           :season-count (dec (count seasons))})))))

(defn details [id media-type & {:keys [is-4k? season]}]
  (a/go
    (let [details (a/<! (impl/details id media-type))]
      {:overview (:overview details)
       :poster (if (nil? (:poster-path details)) "https://critics.io/img/movies/poster-placeholder.png" (str impl/poster-path (:poster-path details)))
       :status (impl/media-status details media-type :is-4k? is-4k? :season season)
       :plex-url (-> details :media-info :plex-url)})))

(defn request-embed [{:keys [title id season]} media-type]
  (a/go
    (let [fourk (a/<! (impl/backend-4k? media-type))
          details (a/<! (impl/details id media-type))]
      {:title title
       :overview (:overview details)
       :poster (str impl/poster-path (:poster-path details))
       :media-type media-type
       :request-formats (cond-> [""] fourk (conj "4K"))
       :season season})))

(defn request [payload media-type]
  (a/go
    (let [{:keys [format id season season-count telegram-id]} payload
          {:overseerr/keys [default-id]} @state/config
          details (a/<! (impl/details id media-type))
          ovsr-id ((a/<! (impl/telegram-users)) telegram-id)
          status (impl/media-status details media-type
                                    :is-4k? (= format :4K)
                                    :season season)
          body (cond-> {:mediaType (impl/media-type media-type)
                        :mediaId id
                        :is4k (= format :4K)}
                 (= :series media-type)
                 (assoc :seasons
                        (if (= -1 season)
                          (into [] (range 1 (inc season-count)))
                          [season])))]
      (cond
        (contains? #{:unauthorized :pending :processing :available} status) status
        (and (nil? ovsr-id) (nil? default-id)) :unauthorized
        :else (a/<! (impl/POST "/request" {:form-params body
                                           :content-type :json
                                           :headers {"X-API-User" (str (or ovsr-id default-id))}}))))))
