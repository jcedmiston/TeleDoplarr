(ns build
  (:require
   [org.corfield.build :as bb]))

(defn uber [_]
  (bb/clean nil)
  (bb/uber {:uber-file "target/teledoplarr.jar"
            :main 'teledoplarr.core}))
