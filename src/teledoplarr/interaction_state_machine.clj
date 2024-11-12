(ns teledoplarr.interaction-state-machine
  (:require
   [clojure.core.async :as a]
   [clojure.string :as str]
   [teledoplarr.telegram :as telegram]
   [teledoplarr.state :as state]
   [teledoplarr.utils :as utils :refer [log-on-error]]
   [fmnoise.flow :refer [else then]]
   [taoensso.timbre :refer [fatal info]]
   [telegrambot-lib.core :as t]))

(def channel-timeout 600000)

(defn system-interaction! [interaction msg]
  (a/go
    (let [chat-id (:chat-id interaction)
          {:keys [bot]} @state/telegram]
      (t/send-message bot chat-id msg))))

(defn start-interaction! [interaction]
  (a/go
    (let [uuid (str (java.util.UUID/randomUUID))
          {:keys [chat-id media-type msg-text]} interaction
          {:keys [bot]} @state/telegram]
      ;; Search for results
      (if (not (str/blank? msg-text))
        ((info "Performing search for" (name media-type) msg-text)
         (let [results (->> (log-on-error
                             (a/<! ((utils/media-fn media-type "search") msg-text media-type))
                             "Exception from search")
                            (then #(->> (take (:max-results @state/config telegram/MAX-OPTIONS) %)
                                        (into []))))
               result (first results)
               results-count (count results)
               {:keys [poster status tmdb-url plex-url]} (a/<! ((utils/media-fn media-type "details") (-> result :id) media-type))]
        ;; Setup ttl cache entry
           (swap! state/cache assoc uuid {:results results
                                          :media-type media-type
                                          :last-modified (System/currentTimeMillis)})
           (if (empty? results)
             (->> (utils/check-response (t/send-message bot chat-id (str "Search result returned no hits for " msg-text)))
                  (else #(fatal % "Error in creating no result response")))
          ;; Create dropdown for search results
             (->> (utils/check-response (t/send-photo bot
                                                      chat-id
                                                      poster
                                                      {:caption (telegram/caption result status 1 results-count)
                                                       :reply_markup {:inline_keyboard (telegram/result-reply-markup uuid 0 results-count status tmdb-url plex-url)}}))
                  (else #(fatal % "Error in creating search responses"))))))
        (t/send-message bot chat-id (str "Please provide the name of a " (name media-type) ".\nEx: '/" (name media-type) " The Example " (str/capitalize (name media-type)) "'"))))))

(defmulti process-event! (fn [event _ _ _] event))

(defn query-for-option-or-request [pending-opts interaction uuid]
  (a/go
    (let [{:keys [chat-id msg-id]} interaction
          {:keys [bot]} @state/telegram
          {:keys [media-type payload]} (get @state/cache uuid)]
      (if (empty? pending-opts)
        (let [embed (log-on-error
                     (a/<! ((utils/media-fn media-type "request-embed") payload media-type))
                     "Exception from request-embed")]
          (swap! state/cache assoc-in [uuid :embed] embed)
          ((process-event! "request" interaction uuid embed)
           (else #(fatal % "Error in sending request embed"))))
        (let [[op options] (first pending-opts)]
          (->> (utils/check-response (t/edit-message-caption bot
                                                             chat-id
                                                             msg-id (str (:title payload) " ("
                                                                         (:year payload) ")\n\n Which "
                                                                         (utils/canonical-option-name op) "?")
                                                             {:reply_markup (telegram/option-reply-markup op options uuid)}))
               (else #(fatal % "Error in creating option dropdown"))))))))

(defmethod process-event! "result-select" [_ interaction uuid option]
  (a/go
    (let [{:keys [results media-type]} (get @state/cache uuid)
          result (nth results (Integer/parseInt option))
          add-opts (log-on-error
                    (a/<! ((utils/media-fn media-type "additional-options") result media-type))
                    "Exception thrown from additional-options")
          pending-opts (->> add-opts
                            (filter #(seq? (second %)))
                            (into {}))
          ready-opts (apply (partial dissoc add-opts) (keys pending-opts))]
      ; Start setting up the payload
      (swap! state/cache assoc-in [uuid :payload] result)
      (swap! state/cache assoc-in [uuid :pending-opts] pending-opts)
      ; Merge in the opts that are already satisfied
      (swap! state/cache update-in [uuid :payload] merge ready-opts)
      (query-for-option-or-request pending-opts interaction uuid))))

(defmethod process-event! "change-result" [_ interaction uuid option]
  (a/go
    (let [{:keys [chat-id msg-id]} interaction
          {:keys [bot]} @state/telegram
          {:keys [results media-type]} (get @state/cache uuid)
          [prev_index direction] (str/split option #"/")
          index (+ (Integer/parseInt prev_index) (Integer/parseInt direction))
          results-count (count results)
          result (nth results index)
          {:keys [poster status tmdb-url plex-url]} (a/<! ((utils/media-fn media-type "details") (-> result :id) media-type))]
      (->> (utils/check-response
            (t/edit-message-media bot
                                  chat-id
                                  msg-id
                                  {:type "photo"
                                   :media poster
                                   :caption (telegram/caption result status (inc index) results-count)}
                                  {:reply_markup {:inline_keyboard (telegram/result-reply-markup uuid index results-count status tmdb-url plex-url)}}))
           (else #(fatal % "Error in message response"))))))

(defmethod process-event! "cancel" [_ interaction _ _]
  (a/go
    (let [{:keys [bot]} @state/telegram
          {:keys [chat-id msg-id]} interaction]
      (->> (utils/check-response
            (t/delete-message bot chat-id msg-id))
           (else #(fatal % "Error in message response")))
      (->> (utils/check-response
            (t/send-message bot chat-id "Canceled"))
           (else #(fatal % "Error in message response"))))))

(defmethod process-event! "cancel-no-response" [_ interaction _ _]
  (a/go
    (let [{:keys [bot]} @state/telegram
          {:keys [chat-id msg-id]} interaction]
      (->> (utils/check-response
            (t/delete-message bot chat-id msg-id))
           (else #(fatal % "Error in message response"))))))

(defmethod process-event! "option-select" [_ interaction uuid option]
  (let [[opt selection] (str/split option #"/")
        cache-val (swap! state/cache update-in [uuid :pending-opts] #(dissoc % (keyword opt)))]
    (swap! state/cache assoc-in [uuid :payload (keyword opt)] (Integer/parseInt selection))
    (query-for-option-or-request (get-in cache-val [uuid :pending-opts]) interaction uuid)))

(defmethod process-event! "request" [_ interaction uuid format]
  (let [{:keys [chat-id msg-id user-id]} interaction
        {:keys [bot]} @state/telegram
        {:keys [payload media-type]} (get @state/cache uuid)]
    (letfn [(msg-resp [msg]
              (->> (utils/check-response
                    (t/edit-message-caption bot chat-id msg-id msg))
                   (else #(fatal % "Error in message response"))))]
      (->> (log-on-error
            (let [telegram-id (utils/zp user-id 19)]
              (a/<!! ((utils/media-fn media-type "request")
                      (assoc payload :format (keyword format) :telegram-id telegram-id)
                      media-type)))
            "Exception from request")
           (then (fn [status]
                   (case status
                     :unauthorized (msg-resp "You are unauthorized to perform this request. Please contact your system administrator for more help.")
                     :pending (msg-resp "This has already been requested and the request is pending")
                     :processing (msg-resp "This is currently processing and should be available soon!")
                     :available (msg-resp "This selection is already available!")
                     (do
                       (info "Adding request for" (-> payload :title))
                       (let [length (count (-> interaction :msg :callback_query :from :username))
                             user (-> interaction :msg :callback_query :from)]
                         (case (:telegram/requested-msg-style @state/config)
                           :none nil
                           :embed (t/edit-message-caption bot chat-id msg-id (telegram/request-performed-caption payload media-type (-> user :username)) {:entities [{:type "mention" :offset 0 :length (inc length) :user user}]})
                           :plain ((t/delete-message bot chat-id msg-id)
                                   (t/send-message bot chat-id (telegram/request-performed-caption payload media-type (-> user :username)) {:entities [{:type "mention" :offset 0 :length (inc length) :user user}]}))))))))
           (else (fn [e]
                   (let [{:keys [status body] :as data} (ex-data e)]
                     (if (= status 403)
                       (->> @(t/edit-message-caption bot chat-id msg-id (body "message")))
                       (else #(fatal % "Error in sending request failure response")))
                     (->> @(t/edit-message-text bot chat-id msg-id "Unspecified error on request, check logs")
                          (then #(fatal "Non 403 error on request" % data))
                          (else #(fatal % "Error in sending error response"))))))))))

(defn continue-interaction! [interaction]
  (let [{:keys [chat-id msg-id]} interaction
        {:keys [bot]} @state/telegram
        [event uuid option] (str/split (-> interaction :msg :callback_query :data) #":")
        callback-id (-> interaction :msg :callback_query :id)
        now (System/currentTimeMillis)]
    ; Send the ack
    (->> (utils/check-response
          (t/answer-callback-query bot callback-id))
         (else #(fatal % "Error sending response ack")))
    ; Check last modified
    (if-let [{:keys [last-modified]} (get @state/cache uuid)]
      (if (> (- now last-modified) channel-timeout)
        ; Update interaction with timeout message
        (->> (utils/check-response
              (t/edit-message-text bot chat-id msg-id "Request timed out, please try again"))
             (else #(fatal % "Error in sending timeout response")))
        ; Move through the state machine to update cache side effecting new components
        (do
          (swap! state/cache assoc-in [uuid :last-modified] now)
          (process-event! event interaction uuid option)))
      (->> (utils/check-response
            (t/edit-message-text bot chat-id msg-id "Request timed out, please try again"))
           (else #(fatal % "Error in sending timeout response"))))))
