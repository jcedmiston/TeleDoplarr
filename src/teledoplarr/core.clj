(ns teledoplarr.core
  (:require
   [clojure.core.async :as a]
   [clojure.string :as str]
   [cheshire.core :refer :all]
   [config.core :refer [load-env]]
   [fmnoise.flow :refer [else then]]
   [telegrambot-lib.core :as t]
   [teledoplarr.config :as config]
   [teledoplarr.telegram :as discord]
   [teledoplarr.interaction-state-machine :as ism]
   [teledoplarr.state :as state]
   [teledoplarr.utils :as utils :refer [log-on-error]]
   [teledoplarr.status :as status]
   [taoensso.timbre :refer [fatal info] :as timbre]
   [taoensso.timbre.tools.logging :as tlog])
  (:gen-class))

; Pipe tools.logging to timbre
(tlog/use-timbre)

(defn poll-updates
  "Long poll for recent chat messages from Telegram."
  ([bot]
   (poll-updates bot nil))

  ([bot offset]
   (try
     (t/get-updates bot {:offset offset
                            :timeout 10})

     (catch Exception e
       (fatal "tbot/get-updates exception:" e)))))

(defn handle-msg
  "Check the message text for command or string matches and handle the
   message appropriately."
  [msg]
  (let [text (-> msg :message :text)]
    (info (str "New msg received. " text))
    (cond
      (not (= nil text))
      (cond
        (str/starts-with? text "/start") (ism/system-interaction! (discord/interaction-data msg nil) "I'm up and ready to accept requests!")
        (str/starts-with? text "/help") (ism/system-interaction! (discord/interaction-data msg nil) "Use '/request_' commands to submit requests for new media. If you are using Overserr please make sure your Telegram ID has been added in your account settings.")
        (str/starts-with? text "/movie") (ism/start-interaction! (discord/interaction-data msg :movie))
        (str/starts-with? text "/series") (ism/start-interaction! (discord/interaction-data msg :series)))
      :else (ism/continue-interaction! (discord/interaction-data msg nil)))))

(defn app
  "Retrieve and process chat messages."
  [bot]
  (info "Request Media bot service started.")

  (loop []
    (let [updates (poll-updates bot @status/update-id)
          messages (-> updates :result)]

      ;; Check all messages, if any, for commands/keywords.
      (doseq [msg messages]
        (handle-msg msg)
        ;; Increment the next update-id to process.
        (let [update_id (-> msg :update_id)]
          (status/set-id! (inc update_id))))

      ;; Wait a while before checking for updates again.
      (Thread/sleep 100))
    (recur)))

(defn setup-config! []
  (reset! state/config (config/valid-config (load-env)))
  (timbre/merge-config! {:min-level [[#{"*"} (:log-level @state/config :info)]]
                         :output-fn (partial timbre/default-output-fn {:stacktrace-fonts {}})}))

(defn startup! []
  (setup-config!)
  (let [token (:telegram/token @state/config)
        bot (t/create token)
        media-types (config/available-media @state/config)
        init-state {:bot bot}]
    (reset! state/discord init-state)
    (discord/register-commands bot media-types)
    (app bot)))

; Program Entry Point
(defn -main
  [& _]
  (startup!)
  (shutdown-agents))
