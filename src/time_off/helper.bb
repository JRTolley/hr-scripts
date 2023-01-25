(ns time-off.helper
  (:require [clojure.set :refer [rename-keys]]))

(def is-weekend-day? (comp some? #(#{"SATURDAY" "SUNDAY"} (str (. (java.time.LocalDate/parse %) getDayOfWeek)))))
(def partition-weekdays #(rename-keys
                          (group-by is-weekend-day? %)
                          {true :weekend
                           false :weekday}))

(defn print-weekend-and-discard
  [{:keys [weekend weekday]}]
  (println "Weekend days Removed: " weekend)
  weekday)