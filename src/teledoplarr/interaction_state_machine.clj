(ns teledoplarr.interaction-state-machine
  (:require
   [clojure.core.async :as a]
   [clojure.string :as str]
   [teledoplarr.telegram :as discord]
   [teledoplarr.state :as state]
   [teledoplarr.utils :as utils :refer [log-on-error]]
   [fmnoise.flow :refer [else then]]
   [taoensso.timbre :refer [fatal info]]
   [telegrambot-lib.core :as t]))

(def channel-timeout 600000)

(defn system-interaction! [interaction msg]
  (a/go
    (let [chat-id (:chat-id interaction)
          {:keys [bot]} @state/discord]
      (t/send-message bot chat-id msg))))

(defn start-interaction! [interaction]
  (a/go
    (let [uuid (str (java.util.UUID/randomUUID))
          {:keys [chat-id media-type]} interaction
          query (-> interaction :msg-text)
          {:keys [bot]} @state/discord]
                                        ; Search for results
      (info "Performing search for" (name media-type) query)
      (let [results (->> (log-on-error
                          (a/<! ((utils/media-fn media-type "search") query media-type))
                          "Exception from search")
                         (then #(->> (take (:max-results @state/config discord/MAX-OPTIONS) %)
                                     (into []))))
            result (first results)
            result-count (count results)]
        ; Setup ttl cache entry
        (swap! state/cache assoc uuid {:results results
                                       :media-type media-type
                                       :last-modified (System/currentTimeMillis)})
        (if (empty? results)
          (t/send-message bot chat-id (str "Search result returned no hits for " query))
          ; Create dropdown for search results
          ((t/send-photo bot 
                            chat-id 
                            (str "https://image.tmdb.org/t/p/w500" (:poster-path result)) 
                            {:caption (str (:title result) " (" 
                                           (:year result) ")\n\n" 
                                           (:overview result) "\n" 1 " / " 
                                           (count results)) 
                             :reply_markup {:inline_keyboard (discord/result_reply_markup uuid 0 result-count)}}
                            )
           (else #(fatal % "Error in creating search responses"))))))))

(defmulti process-event! (fn [event _ _ _] event))

(defn query-for-option-or-request [pending-opts interaction uuid]
  (a/go
    (let [{:keys [chat-id msg-id]} interaction
          {:keys [bot]} @state/discord
          {:keys [media-type payload]} (get @state/cache uuid)]
      (if (empty? pending-opts)
        (let [embed (log-on-error
                     (a/<! ((utils/media-fn media-type "request-embed") payload media-type))
                     "Exception from request-embed")]
          (swap! state/cache assoc-in [uuid :embed] embed)
          ((process-event! "request" interaction uuid embed)
           (else #(fatal % "Error in sending request embed"))))
        (let [[op options] (first pending-opts)]
          ((t/edit-message-caption bot chat-id msg-id (str (:title payload) " (" (:year payload) ")\n\n Which " (utils/canonical-option-name op) "?") {:reply_markup (discord/option-reply-markup op options uuid 0)})
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

(defmethod process-event! "next-result" [_ interaction uuid option]
  (a/go
    (let [{:keys [chat-id msg-id]} interaction
          {:keys [bot]} @state/discord
          {:keys [results]} (get @state/cache uuid)
          index (Integer/parseInt option)
          result-count (count results)
          result (nth results index)]
      (t/edit-message-media bot chat-id msg-id {:type "photo" :media (str "https://image.tmdb.org/t/p/w500" (:poster-path result)) :caption (str (:title result) " (" (:year result) ")\n\n" (:overview result) "\n" (inc index) " / " (count results))} {:reply_markup {:inline_keyboard (discord/result_reply_markup uuid index result-count)}}))))

(defmethod process-event! "prev-result" [_ interaction uuid option]
  (a/go
    (let [{:keys [chat-id msg-id]} interaction
          {:keys [bot]} @state/discord
          {:keys [results]} (get @state/cache uuid)
          index (Integer/parseInt option)
          result-count (count results)
          result (nth results index)]
      (t/edit-message-media bot chat-id msg-id {:type "photo" :media (str "https://image.tmdb.org/t/p/w500" (:poster-path result)) :caption (str (:title result) " (" (:year result) ")\n\n" (:overview result) "\n" (inc index) " / " (count results))} {:reply_markup {:inline_keyboard (discord/result_reply_markup uuid index result-count)}}))))

(defmethod process-event! "cancel" [_ interaction _ _]
  (a/go
    (let [{:keys [bot]} @state/discord
          {:keys [chat-id msg-id]} interaction]
      (t/delete-message bot chat-id msg-id)
      (t/send-message bot chat-id "Canceled"))))

(defmethod process-event! "option-page" [_ interaction uuid option]
  (let [{:keys [chat-id msg-id]} interaction
        {:keys [bot]} @state/discord
        {:keys [pending-opts]} (get @state/cache uuid)
        [opt page] (str/split option #"-")
        op (keyword opt)
        page (Long/parseLong page)
        options (op pending-opts)]
    (t/edit-message-reply-markup bot chat-id msg-id (discord/option-dropdown op options uuid page))
    (else #(fatal % "Error in updating option dropdown"))))

(defmethod process-event! "option-select" [_ interaction uuid option]
  (let [[opt selection] (str/split option #"-")
        cache-val (swap! state/cache update-in [uuid :pending-opts] #(dissoc % (keyword opt)))]
    (swap! state/cache assoc-in [uuid :payload (keyword opt)] (Integer/parseInt selection))
    (query-for-option-or-request (get-in cache-val [uuid :pending-opts]) interaction uuid)))

(defmethod process-event! "request" [_ interaction uuid format]
  (let [{:keys [chat-id msg-id]} interaction
        {:keys [bot]} @state/discord
        {:keys [payload media-type embed]} (get @state/cache uuid)]
    (letfn [(msg-resp [msg] (t/edit-message-caption bot chat-id msg-id msg)
              (else #(fatal % "Error in message response")))]
      (->> (log-on-error
            (a/<!! ((utils/media-fn media-type "request")
                    (assoc payload :format (keyword format) :discord-id "")
                    media-type))
            "Exception from request")
           (then (fn [status]
                   (case status
                     :unauthorized (msg-resp "You are unauthorized to perform this request in the configured backend")
                     :pending (msg-resp "This has already been requested and the request is pending")
                     :processing (msg-resp "This is currently processing and should be available soon!")
                     :available (msg-resp "This selection is already available!")
                     (do
                       (info "Performing request for " payload)
                       (case (:discord/requested-msg-style @state/config)
                         :none nil
                         :embed (t/edit-message-caption bot chat-id msg-id (discord/request-performed-embed embed chat-id))
                         ((t/delete-message bot chat-id msg-id)
                          (let [length (count (-> interaction :msg :callback_query :from :username))
                                user (-> interaction :msg :callback_query :from)]
                            (t/send-message bot chat-id (discord/request-performed-plain payload media-type (-> user :username)) {:entities [{:type "mention" :offset 0 :length (inc length) :user user}]}))))))))
           (else (fn [e]
                   (let [{:keys [status body] :as data} (ex-data e)]
                     (if (= status 403)
                       (->> @(t/edit-message-caption bot chat-id msg-id (body "message")))
                       (else #(fatal % "Error in sending request failure response")))
                     (->> @(t/edit-message-text bot chat-id msg-id "Unspecified error on request, check logs")
                          (then #(fatal "Non 403 error on request" % data))
                          (else #(fatal % "Error in sending error response"))))))))))

(defn continue-interaction! [interaction]
  (let [[event uuid option] (str/split (-> interaction :msg :callback_query :data) #":")
        now (System/currentTimeMillis)
        chat-id (:chat-id interaction)
        msg-id (:msg-id interaction)
        callback-id (-> interaction :msg :callback_query :id)
        {:keys [bot]} @state/discord]
    ; Send the ack
    ((t/answer-callback-query bot callback-id)
     (else #(fatal % "Error sending response ack")))
    ; Check last modified
    (if-let [{:keys [last-modified]} (get @state/cache uuid)]
      (if (> (- now last-modified) channel-timeout)
        ; Update interaction with timeout message
        ((t/edit-message-text bot chat-id msg-id "Request timed out, please try again")
         (else #(fatal % "Error in sending timeout response")))
        ; Move through the state machine to update cache side effecting new components
        (do
          (swap! state/cache assoc-in [uuid :last-modified] now)
          (process-event! event interaction uuid option)))
      ((t/edit-message-text bot chat-id msg-id "Request timed out, please try again")
       (else #(fatal % "Error in sending timeout response"))))))
