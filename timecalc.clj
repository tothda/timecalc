#!/usr/bin/env boot

(require '[clojure.pprint :refer [pprint print-table]])
(require '[clojure.walk :refer [postwalk]])

(defn sec->hms
  "Converts `sec' seconds to a hour, min, sec three element vector."
  [sec]
  (let [h (quot sec 3600)
        m (quot (- sec (* h 3600)) 60)
        s (- sec (* h 3600) (* m 60))]
    [h m s]))

(defn hms->sec
  "Converts a hms vector back to seconds."
  [[h m s]]
  (+ (* h 3600) (* m 60) s))

(defn format-hms [[h m s]]
  (if (pos? h)
    (format "%1d:%02d:%02d" (int h) (int m) (int s))
    (format "%02d:%02d" (int m) (int s))))

(defn pace-in-sec
  [split1 split2]
  (let [[d1 & time1] split1
        [d2 & time2] split2
        d (- d2 d1)
        sec1 (hms->sec time1)
        sec2 (hms->sec time2)
        s (- sec2 sec1)]
    (int (/ s d))))

(defn pace
  [split1 split2]
  (sec->hms (pace-in-sec split1 split2)))

(defn split-time
  [msmt1 msmt2]
  (let [[_ & time1] msmt1
        [_ & time2] msmt2]
    (sec->hms (- (hms->sec time2)
                 (hms->sec time1)))))

(defn estimated-finish
  [p n]
  (let [[d & t] n
        dist-left (- 42.19 d)
        pace-sec (pace-in-sec p n)
        sec-left (Math/round (* pace-sec dist-left))
        sec-passed (hms->sec t)
        estimated-sec (+ sec-left sec-passed)]
    (sec->hms estimated-sec)))

(defn calc-split-data
  [f [p n]]
  (let []
    {::split-start-dist (first p)
     ::split-end-dist (first n)
     ::split-pace (pace p n)
     ::split-time (split-time p n)
     ::overall-pace (pace f n)
     ::overall-time (split-time f n)
     ::estimated-finish (estimated-finish p n)}))

(defn calc-data
  [msmts]
  (let [msmts (partition 4 4 msmts)
       msmt-first (first msmts)
       calc-split-data (partial calc-split-data msmt-first)
       splits (partition 2 1 msmts)]
   (map calc-split-data splits)))

(defn hms?
  [x]
  (and (vector? x)
       (= (count x) 3)
       (every? int? x)))

(def labels
  {::split-dist "résztáv"
   ::split-pace "tempó"
   ::split-time "részidő"
   ::overall-pace "telj. tempó"
   ::overall-time "telj. idő"
   ::estimated-finish "becslés"})

(defn format-calc-data
  [data]
  (let [add-dist (fn [{start ::split-start-dist
                       end ::split-end-dist
                       :as split-data}]
                   (-> split-data
                       (assoc ::split-dist (format "%s - %s" start end))
                       (dissoc ::split-start-dist)
                       (dissoc ::split-end-dist)))
        change-labels (fn [split-data]
                        (into {}
                              (for [[k v] split-data]
                                [(get labels k k)
                                 (if (hms? v) (format-hms v) v)])))]
      (map (comp change-labels add-dist) data)))

(defn print-stats
  [id]
  (let [raw-data (->> (read-string (slurp "race-data.edn"))
                      (filter #(= id (:id %)))
                      (first))
        print-data (partial print-table (vals labels))]
    (println (format "\n%s" (:title raw-data)))
    (-> raw-data
        :splits
        calc-data
        format-calc-data
        print-data)))

(defn hms-diff [hms1 hms2]
  (sec->hms (- (hms->sec hms2) (hms->sec hms1))))

(defn -main [& args]
  (doseq [id args]
    (print-stats (keyword id))))

