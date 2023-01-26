(ns time-off.helper
  (:require [clojure.set :refer [rename-keys] :as set]
            [clojure.string :as str]))

(def is-weekend-day? (comp some? #(#{"SATURDAY" "SUNDAY"} (str (. (java.time.LocalDate/parse %) getDayOfWeek)))))
(def partition-weekdays #(rename-keys
                          (group-by is-weekend-day? %)
                          {true :weekend
                           false :weekday}))

(defn print-weekend-and-discard
  [{:keys [weekend weekday]}]
  (println "Weekend days removed: " (sort weekend))
  weekday)

(def is-already-booked? (fn [m d]
                          (>= (get-in m [(keyword d) :totalHours]) 8)))
(defn partition-already-booked
  [current-timesheet dates]
  (rename-keys 
    (group-by (partial is-already-booked? current-timesheet) dates)
   {true :booked
    false :free}))

(defn print-already-booked-and-discard
  [{:keys [booked free]}]
  (println "Dates already with 8 hours removed: " (sort booked))
  free)

(def current-day-of-month (. (java.time.LocalDate/now) getDayOfMonth))

(def over-current-date? #(>= (Integer/parseInt (second (re-matches #"^.*(\d{2})$" %))) current-day-of-month))

(def partition-over-current-date #(rename-keys
                          (group-by over-current-date? %)
                          {true :over-current-date
                           false :acceptable-date}))

(defn print-over-current-date-and-discard
  [{:keys [over-current-date acceptable-date]}]
  (println "Days in the future removed: " (sort over-current-date))
  acceptable-date)

(defn input-group->day-range-set
  [input]
  (let [[_ day-start day-end] (re-matches #"^(\d{1,2})\D*(\d{0,2})$" (str/trim input))
        day-end (if (empty? day-end) day-start day-end)
        [day-start, day-end] [(Integer/parseInt day-start) (Integer/parseInt day-end)]
        day-start (min day-start day-end)]
    (into #{} (range day-start (inc day-end)))))

(defn generate-day-range
  [s]
  (let [input-groups (str/split s #",")
        day-range-sets (map input-group->day-range-set input-groups)]
    (->> day-range-sets
         (apply set/union)
         (map #(format "%02d" %)))))