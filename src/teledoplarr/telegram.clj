(ns teledoplarr.telegram
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [cheshire.core :refer :all]
   [com.rpl.specter :as s]
   [teledoplarr.utils :as utils]
   [fmnoise.flow :as flow :refer [else]]
   [taoensso.timbre :refer [fatal info]]
   [telegrambot-lib.core :as t]))

(def MAX-OPTIONS 25)
(def MAX-CHARACTERS 100)

(def request-thumbnail
  {:series "https://thetvdb.com/images/logo.png"
   :movie "https://i.imgur.com/44ueTES.png"})

(defn interaction-data [interaction media-type]
  {:id (-> interaction :update_id)
   :chat-id (if (contains? interaction :callback_query) (-> interaction :callback_query :message :chat :id) (-> interaction :message :chat :id))
   :media-type media-type
   :user-id (if (contains? interaction :callback_query) (-> interaction :callback_query :from :id) (-> interaction :message :from :id))
   :msg-id (if (contains? interaction :callback_query) (-> interaction :callback_query :message :message_id) (-> interaction :message :message_id))
   :msg-text (if (contains? interaction :callback_query) (-> interaction :callback_query :message :text) (str/replace (-> interaction :message :text) #"^\/[a-z_]+[ ]*" ""))
   :msg interaction})

(defn page-button [uuid option page label]
  {:text (apply str (take MAX-CHARACTERS label))
   :callback_data (str "option-page:" uuid ":" option "-" page)})

(defn result-reply-action-button [status uuid button-data]
  (case status
    :available [{:text "Open in Plex" :url button-data} {:text "Done" :callback_data (str "cancel:" uuid ":cancel")}]
    :pending [{:text "Done" :callback_data (str "cancel:" uuid ":cancel")}]
    :processing [{:text "Done" :callback_data (str "cancel:" uuid ":cancel")}]
    :unknown [{:text "Request" :callback_data (str "result-select:" uuid ":" button-data)} {:text "Cancel" :callback_data (str "cancel:" uuid ":cancel")}]
    (nil) [{:text "Request" :callback_data (str "result-select:" uuid ":" button-data)} {:text "Cancel" :callback_data (str "cancel:" uuid ":cancel")}]))

(defn result-reply-markup [uuid index count status plex-url?]
  (let [action-button-data (if (nil? plex-url?) index plex-url?)
        prev (if (= index 0) nil {:text "< Prev" :callback_data (str "change-result:" uuid ":" index "/-1")})
        next (if (= index (dec count)) nil {:text "Next >" :callback_data (str "change-result:" uuid ":" index "/+1")})]
    (map (partial remove nil?) [[prev {:text "TMDB" :url "https://tmdb.org"} next]
                                (result-reply-action-button status uuid action-button-data)])))

(defn select-option [uuid option-name option]
  (let [id (-> option :id)]
  [{:text (apply str (take MAX-CHARACTERS (or (:title option) (:name option))))
    :callback_data (str "option-select:" uuid ":" (name option-name) "/" id)}]))

(defn option-reply-markup [option options uuid]
  (generate-string {:inline_keyboard (map (partial select-option uuid option) options)}))

(defn request-embed [{:keys [media-type title overview poster season quality-profile language-profile rootfolder]}]
  {:title title
   :description overview
   :image {:url poster}
   :thumbnail {:url (media-type request-thumbnail)}
   :fields (filterv
            identity
            ; Some overrides to make things pretty
            [(when quality-profile
               {:name "Profile"
                :value quality-profile})
             (when language-profile
               {:name "Language Profile"
                :value language-profile})
             (when season
               {:name "Season"
                :value (if (= season -1) "All" season)})
             (when rootfolder
               {:name "Root Folder"
                :value rootfolder})])})

(defn request-performed-plain [payload media-type username]
  (str "@" username " your request for the "
       (name media-type) " `" (:title payload) " (" (:year payload) ")"
       "` has been received!"))

(defn request-performed-embed [payload med]
  (str))

(defn request-commands [media-types]
  (generate-string (concat [{:command "start" :description "check if the bot is ready to respond."}
                            {:command "help" :description "help"}]
                           (for [media media-types]
                             {:command (name media)
                              :description (str "Request a " (name media))}))))

(defn register-commands [bot media-types]
  (let [commands (request-commands media-types)]
    (t/set-my-commands bot commands)))

(defn status-pill [status]
  (case status
    :available "🟢 Available Now"
    :partially-available "🟡 Partially Available"
    :pending "🟡 Proccessing Request"
    :processing "🟡 Proccessing Request"
    :unknown "🔴 Not Yet Available"
    nil "🔴 Not Yet Available"))

(defn caption [result status index results-count]
  (str (:title result) " ("
       (:year result) ")\n"
       (status-pill status) "\n\n"
       (:overview result) "\n\n" index " of " results-count " results"))
