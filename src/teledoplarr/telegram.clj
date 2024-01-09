(ns teledoplarr.telegram
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [cheshire.core :refer :all]
   [com.rpl.specter :as s]
   [teledoplarr.utils :as utils]
   [fmnoise.flow :as flow :refer [else]]
   [taoensso.timbre :refer [fatal info]]
   [telegrambot-lib.core :as tbot]))

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

(defn result_reply_markup [uuid index count]
  (let [prev (if (= index 0) nil {:text "< Prev" :callback_data (str "prev-result:" uuid ":" (dec index))})
        next (if (= index (dec count)) nil {:text "Next >" :callback_data (str "next-result:" uuid ":" (inc index))})]
    (map (partial remove nil?) [[prev {:text "TMDB" :url "https://tmdb.org"} next]
                                [{:text "Request" :callback_data (str "result-select:" uuid ":" index)} {:text "Cancel" :callback_data (str "cancel:" uuid ":cancel")}]])))

(defn select-option [uuid option-name index option]
  [{:text (apply str (take MAX-CHARACTERS (or (:title option) (:name option))))
    :callback_data (str "option-select:" uuid ":" (name option-name) "-" index)}])

(defn option-reply-markup [option options uuid page]
  (generate-string {:inline_keyboard (map-indexed (partial select-option uuid option) options)}))

(defn option-dropdown [option options uuid page]
  (info option)
  (info options)
  (info page)
  (let [all-options (map #(set/rename-keys % {:name :label :id :value}) options)
        chunked (partition-all MAX-OPTIONS all-options)
        ddown ((str "Which " (utils/canonical-option-name option) "?") (str "option-select:" uuid ":" (name option)))]
    (info all-options)
    (cond-> ddown
      ; Create the action row if we have more than 1 chunk
      (> (count chunked) 1) (update-in [:components] conj {:type 1 :components []})
      ; More chunks exist
      (< page (dec (count chunked))) (update-in [:components 1 :components] conj (page-button uuid (name option) (inc page) "More"))
      ; Past chunk 1
      (> page 0) (update-in [:components 1 :components] conj (page-button uuid (name option) (dec page) "Less")))))

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
  (str ))

(defn request-commands [media-types]
  (generate-string (concat [{:command "start" :description "check if the bot is ready to respond."}
                            {:command "help" :description "help"}]
                           (for [media media-types]
                             {:command (name media)
                              :description (str "Request a " (name media))}))))

(defn register-commands [bot media-types]
  (let [commands (request-commands media-types)]
    (tbot/set-my-commands bot commands)))
