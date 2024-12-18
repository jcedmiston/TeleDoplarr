(ns teledoplarr.telegram
  (:require
   [clojure.string :as str]
   [cheshire.core :as ches]
   [telegrambot-lib.core :as t]))

(def MAX-OPTIONS 25)
(def MAX-CHARACTERS 100)

(defn interaction-data [interaction media-type]
  {:id (:update_id interaction)
   :chat-id (get-in interaction [:callback_query :message :chat :id] (-> interaction :message :chat :id))
   :media-type media-type
   :user-id (get-in interaction [:callback_query :from :id] (-> interaction :message :from :id))
   :msg-id (get-in interaction [:callback_query :message :message_id] (-> interaction :message :message_id))
   :msg-text (if (contains? interaction :message) (str/replace (-> interaction :message :text) #"^\/[a-z_]+[ ]*" "") nil)
   :msg interaction})

(defn result-reply-action-button [status uuid index plex-url?]
  (case status
    :available [{:text "Open in Plex" :url plex-url?} {:text "Done" :callback_data (str "cancel:" uuid ":cancel")}]
    :partially-available [{:text "Request More" :callback_data (str "result-select:" uuid ":" index)} {:text "Open in Plex" :url plex-url?}]
    :pending [{:text "Done" :callback_data (str "cancel:" uuid ":cancel")}]
    :processing [{:text "Done" :callback_data (str "cancel:" uuid ":cancel")}]
    :unknown [{:text "Request" :callback_data (str "result-select:" uuid ":" index)} {:text "Done" :callback_data (str "cancel:" uuid ":cancel")}]
    (nil) [{:text "Request" :callback_data (str "result-select:" uuid ":" index)} {:text "Done" :callback_data (str "cancel:" uuid ":cancel")}]))

(defn result-reply-markup [uuid index count status tmdb-url plex-url?]
  (let [prev (if (= index 0) nil {:text "< Prev" :callback_data (str "change-result:" uuid ":" index "/-1")})
        next (if (= index (dec count)) nil {:text "Next >" :callback_data (str "change-result:" uuid ":" index "/+1")})]
    (map (partial remove nil?) [[prev {:text "TMDB" :url (or tmdb-url "https://tmdb.org")} next]
                                (result-reply-action-button status uuid index plex-url?)])))

(defn status-dot [status]
  (case status
    :available "🟢"
    :partially-available "🟡"
    :pending "🟡"
    :processing "🟡"
    ""))

(defn select-option [uuid option-name option]
  {:text (apply str (take MAX-CHARACTERS (or (:title option) (str (:name option) " " (status-dot (:status option))))))
   :callback_data (str "season-select:" uuid ":" (name option-name) "/" (:id option))})

(defn option-reply-markup [option options uuid]
  (let [all-season-option (first options)
        all-season-button [{:text (apply str (take MAX-CHARACTERS (or (:title all-season-option) (str (:name all-season-option) " " (status-dot (:status all-season-option))))))
                            :callback_data (str "option-select:" uuid ":" (name option) "/" (:id all-season-option))}]
        options-array (partition-all 3 (map (partial select-option uuid option) (rest options)))
        cancel-button [{:text "Cancel" :callback_data (str "cancel:" uuid ":cancel")}]
        done-button [{:text "Submit" :callback_data (str "submit-seasons:" uuid ":season")}]]
    (ches/generate-string {:inline_keyboard (conj options-array all-season-button cancel-button done-button)})))

(defn register-commands [bot media-types]
  (let [commands (ches/generate-string (concat [{:command "start" :description "Check if the bot is ready to respond"}
                                                {:command "help" :description "Provides some help for commands"}]
                                               (for [media media-types]
                                                 {:command (name media)
                                                  :description (str "Request a " (name media))})))]
    (t/set-my-commands bot commands)))

(defn status-pill [status]
  (case status
    :available "🟢 Available Now"
    :partially-available "🟡 Partially Available"
    :pending "🟡 Proccessing Request"
    :processing "🟡 Proccessing Request"
    :unknown "🔴 Not Yet Available"
    nil "🔴 Not Yet Available"))

(defn request-performed-caption [payload media-type username]
  (str "@" username " your request for the "
       (name media-type) " `" (:title payload) " (" (:year payload) ")"
       "` has been received!"))

(defn caption [result status index results-count]
  (str (:title result) " ("
       (:year result) ")\n"
       (status-pill status) "\n\n"
       (:overview result) "\n\n" index " of " results-count " results"))
