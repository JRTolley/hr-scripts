
(require '[babashka.curl :as curl]
         '[clojure.data.csv :refer [read-csv]]
         '[clojure.string :as str]
         '[cheshire.core :as json]
         '[babashka.tasks :refer [shell]])
(require  '[time-off.helper :as h])

;; STATE
(def URL "https://juxtpro.bamboohr.com")
(def headers (atom {:authority "juxtpro.bamboohr.com"
                    :accept "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9"
                    :cache-control "max-age=0"
                    :referer "https://juxtpro.bamboohr.com/login.php"
                    :user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36"}))
(def data (atom {}))

;; Helper functions 
(defn extract-projects
  [projects]
  (->> projects
       :byId
       (reduce
        (fn [acc [_ v]]
          (if (empty? (get-in v [:tasks :allIds]))
            (conj acc (select-keys v [:id :name]))
            (concat acc (map (fn [[_ {:keys [id name]}]]
                               {:id (:id v)
                                :task-id id
                                :name name})
                             (get-in v [:tasks :byId]))))) 
        [])
       (map #(update % :name str/replace #" " "_"))))

(defn ->entry
  [date project-id task-id note]
  {:id nil
   :dailyEntryId 1
   :date date
   :employeeId (:employee-id @data)
   :hours 8
   :note note
   :projectId project-id
   :taskId task-id})

(defn extract-current-month
  [timesheet]
  (->> timesheet
       :timesheet
       :dailyDetails
       vals
       first
       :date
       (re-matches #"(\d{4}-\d{2}).*")
       second))



(defn get-upload-headers
  []
  (-> @headers
      (assoc :content-type "content-type: application/json;charset=UTF-8"
             :accept "application/json, text/plain, */*")))

;; UI
(defn header [msg]
  (shell (format "gum style --padding 1 --foreground 212 '%s'" msg)))

(defn input [& {:keys [value placeholder] :or {value "" placeholder ""}}]
  (-> (shell {:out :string}
             (format "gum input --placeholder '%s' --value '%s'" placeholder value))
      :out
      str/trim))

(defn write [value placeholder]
  (-> (shell {:out :string}
             (format "gum write --show-line-numbers --placeholder '%s' --value '%s'" placeholder value))
      :out
      str/trim))

(defn table [csv]
  (let [data (read-csv csv)
        headers (->> data
                     first
                     (map str/upper-case))
        num-headers (count headers)
        width (int (/ 100.0 num-headers))
        cmd (format "gum table --widths %s" (str/join "," (repeat num-headers width)))]
    (shell {:in csv :out :string} cmd)))

(defn confirm [msg]
  (-> (shell {:continue true}
             (format "gum confirm '%s'" msg))
      :exit
      zero?))

(defn choose
  [opts & {:keys [no-limit limit]}]
  (let [opts (str/join " " opts)
        limit (str "--limit " (or limit 1))
        no-limit (if no-limit (str "--no-limit") "")
        cmd (format "gum choose %s %s %s" limit no-limit opts)]
    (-> (shell {:out :string} cmd)
        :out
        str/trim
        str/split-lines)))

;; Requests
(defn get-home-body
  []
  (:body (curl/get (str URL "/home")
                 {:headers @headers})))

(defn retrieve-csrf-token!
  [home-body]
  (->> home-body
       (re-find #"CSRF_TOKEN = \"(.*)\".*;")
       second
       (swap! headers assoc :x-csrf-token)))

(defn retrieve-employee-id!
  [home-body]
  (->> home-body
       (re-find #"\"employeeId\":\"(\d*)\"")
       second
       (swap! data assoc :employee-id)))

(defn retrieve-time-tracking-id!
  [home-body]
  (->> home-body
       (re-find #"window.time_tracking.*\"id\":(\d*)")
       second
       (swap! data assoc :time-tracking-id)))

(defn retrieve-projects! 
  [home-body]
  (-> home-body
      (->> (re-find #"\"projectsWithTasks\":(.*),\"recentProjectsAndTasks\""))
      second
      (json/parse-string true)
      extract-projects
      (->> (swap! data assoc :projects))))

(defn init-data
  []
  (let [home-body (get-home-body)]
    (retrieve-csrf-token! home-body)
    (retrieve-employee-id! home-body)
    (retrieve-time-tracking-id! home-body)
    (retrieve-projects! home-body)))

(defn retrieve-time-sheet! []
  (-> (curl/get (str URL "/timesheet/" (:time-tracking-id @data))
                 {:headers @headers})
      :body
       (json/parse-string true)
      (->> (swap! data assoc :timesheet))))

(defn set-entry! [entries]
  (println (json/generate-string {:hours (vec entries)}))
  (curl/post (str URL "/timesheet/hour/entries")
             {:headers (get-upload-headers)
              :raw-args ["--data-raw" (json/generate-string {:hours (vec entries)})]}))

;; Sub Programs
(defn view-time-tracking
  []
  (->> (:timesheet @data)
       :timesheet
       :dailyDetails
       vals
       (map #(select-keys % [:date :totalHours :projectName :timeOffHours :paidHolidayHours]))
       (sort-by first)
       (map #(str/join "," (vals %)))
       (str/join "\n")
       (str "Date, Hours, Project Name, TimeOffHours, HolidayHours\n")
       table))

(defn generate-time-tracking-entries
  []
  (let [day-range-string (input {:value "" :placeholder "Enter day range(s) (14-19, 25, 28-31) [inclusive, no am-pm yet]"})
        day-range (h/generate-day-range day-range-string)
        project (first (choose (map :name (:projects @data))))
        [project-id task-id] (some #(when (= (:name %) project) [(:id %) (:task-id %)]) (:projects @data)) 
        note (input {:value "" :placeholder "Enter a note"})]
    (->> day-range
         (map #(str (:current-month @data) "-" %))
         h/partition-over-current-date
         h/print-over-current-date-and-discard
         h/partition-weekdays
         h/print-weekend-and-discard
         (h/partition-already-booked  (get-in @data [:timesheet :timesheet :dailyDetails]))
         h/print-already-booked-and-discard
         (map #(->entry % project-id task-id note)))))

(defn add-time-tracking
  []
  (let [entries (generate-time-tracking-entries)]
    
    (if-not (empty? entries) 
      (do
        (header "Confirm Entries?")
        (table (str "Date, ProjectId, TaskId, Hours, Note \n"
                    (str/join "\n"
                              (map #(str/join "," (vals (select-keys % [:date :projectId :taskId :hours :note]))) entries))))
        (when (confirm "Push entries to bamboohr?")
          (set-entry! entries)))
      (println "No dates to set"))))

;; Main Program
(header "Init")
(def cookie (input {:placeholder "Please insert cookie: "}))
(swap! headers assoc :cookie cookie)
(init-data)

(loop []
  (retrieve-time-sheet!)
  (swap! data assoc :current-month (extract-current-month (:timesheet @data)))
  (header "What would you like to do?")

  (def option (choose ["View-Current-TimeSheet"
                       "Add-To-Current-TimeSheet"]))

  (case option
    ["View-Current-TimeSheet"] (view-time-tracking)
    ["Add-To-Current-TimeSheet"] (add-time-tracking))
  (recur))
