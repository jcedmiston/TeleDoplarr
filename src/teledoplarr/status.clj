(ns teledoplarr.status
  "Functions to keep track of the status of which update ids have been
   processed."
  (:gen-class))

(defonce update-id (atom nil))

(defn set-id!
  "Sets the update id to process next as the the passed in `id`."
  [id]
  (reset! update-id id))
