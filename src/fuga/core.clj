(ns fuga.core
  (:require [clojure.java.io :as io])
  (:import (javax.sound.midi MidiSystem Sequencer)))

(def command-map
  {\8 :OFF
   \9 :ON
   \a :AFTERTOUCH
   \b :CONTROL
   \c :PATCH
   \d :PRESSURE
   \e :PITCHBEND
   \f :SYSTEM})

;; reading and playing sequences -------------------

(defn read-midi
  [midi]
  (MidiSystem/getSequence midi))

(defn open-fugue
  [book number]
  (let [book-prefix (if (= book 2) "ii")]
    (io/resource (str "bach/r-pf" book-prefix (format "%02d" number) ".mid"))))

(defn fugue-sequence
  [book number]
  (read-midi (open-fugue book number)))

(defn open-sequence
  [midi]
  (let [sequencer (MidiSystem/getSequencer)]
    (.setSequence sequencer midi)
    (.open sequencer)
    sequencer))

(defn start-sequence
  [sequencer]
  (.start sequencer))

(defn stop-sequence
  [sequencer]
  (.stop sequencer))

(defn close-sequence
  [sequencer]
  (.close sequencer))

;; extracting note data -------------------------------------

(defn extract-data
  [event]
  (let [tick (.getTick event)
        message (-> event .getMessage .getMessage)
        code (format "%02x" (first message))
        command (get command-map (first code) :UNKNOWN)
        channel (Long/decode (str "0x" (last code)))
        data (map int (rest message))]
    {:tick tick :command command :channel channel :data data}))

(defn extract-events
  [track]
  (let [num (.size track)
        index (take num (iterate inc 0))]
    (map #(extract-data (.get track %)) index)))

(defn extract-tracks
  [midi]
  (map extract-events (.getTracks midi)))

(defn filter-notes
  [tracks]
  (let [notes (last tracks)]
    (filter #(or (= (:command %) :ON)
                 (= (:command %) :OFF))
            notes)))

(defn make-processor
  []
  {:on {} :notes []})

(defn on-key
  [note]
  (keyword (str (-> note :data first) "|" (:channel note))))

(defn add-note
  [notes tick off]
  (if tick
    (conj notes {:begin tick
                 :end (:tick off)
                 :note (-> off :data first)})
    notes))

(defn dissoc-in
  [data keys]
  (update-in data (butlast keys) #(dissoc % (last keys))))

(defn process-note
  [process note]
  (let [note-key (on-key note)]
    (condp = (:command note)
      :ON (assoc-in process [:on note-key] (:tick note))
      :OFF (let [added (update-in process [:notes] #(add-note % (-> process :on note-key) note))]
             (dissoc-in added [:on note-key]))
      process)))

(defn process-notes
  [notes]
  (let [process (reduce process-note (make-processor) notes)]
    (sort-by :begin (get process :notes))))

(defn pull-notes
  "Given a midi sequence, extract and process notes into the form
  {:begin A :end B :note C}"
  [midi]
  (let [tracks (extract-tracks midi)
        notes (filter-notes tracks)]
    (process-notes notes)))

(defn fugue-notes
  [book number]
  (pull-notes (fugue-sequence book number)))

(defn- update-diff-pile
  [pile n]
  (let [diff (- n (:previous pile))
        added (update-in pile [:diffs] #(conj % diff))]
    (assoc added :previous n)))

(defn map-differences
  [events]
  (let [basis {:previous (first events) :diffs '()}
        intervals (reduce update-diff-pile basis events)]
    (reverse (:diffs intervals))))

(defn assoc-diff
  [notes]
  (let [diffs (map-differences (map :begin notes))
        ids (take (count diffs) (iterate inc 1))]
    (map (fn [note diff id] (assoc note :diff diff :id id)) notes diffs ids)))

(defn split-every
  [pred s]
  (let [reduction
        (reduce
         (fn [col item]
           (if (pred item)
             (let [updated (update-in col [:split] #(conj % (reverse (:pool col))))]
               (assoc updated :pool (list item)))
             (update-in col [:pool] #(conj % item))))
         {:split '() :pool []} s)]
    (reverse (conj (:split reduction) (reverse (:pool reduction))))))

(defn group-notes
  [notes]
  (split-every #(> (:diff %) 10) notes))

(defn split-prelude-fugue
  [notes]
  (let [diffs (map-differences (map :begin notes))
        max-diff (apply max diffs)
        index (.indexOf diffs max-diff)
        [prelude fugue] (split-at index notes)]
    {:prelude (assoc-diff prelude) :fugue (assoc-diff fugue)}))

(defn read-fugue
  [book number]
  (split-prelude-fugue (fugue-notes book number)))

(defn within?
  [note n]
  (and (>= n (:begin note))
       (<= n (:end note))))

(defn coincide?
  [pre post]
  (or (within? pre (:begin post))
      (within? pre (:end post))
      (within? post (:begin pre))
      (within? post (:end pre))))

(defn bicleave
  [pred s]
  (reduce
   (fn [[yes no] item]
     (if (pred item)
       [(conj yes item) no]
       [yes (conj no item)]))
   [[] []] s))

(defn still-playing?
  [note new-note]
  (>= (:end note) (:begin new-note)))

(defn filter-playing
  [notes new-note]
  (bicleave #(still-playing? % new-note) notes))

(defn find-min
  "Given comparator c and projection f, find the minimum value for f in s."
  [c f s]
  (if-not (empty? s)
    (let [head (first s)
          reduction (reduce
                     (fn [closest el]
                       (let [value (f el)]
                         (if (c value (:value closest))
                           {:value value :el el}
                           closest)))
                     {:value (f head) :el head}
                     (rest s))]
      (:el reduction))))
     
(defn surrounding-notes
  [note preceding continuing peers]
  (let [closest (find-min <
                 #(Math/abs (- (:note note) (:note %)))
                 (concat preceding continuing))]
    (assoc note
      :preceding (map :id preceding)
      :during (map :id (concat continuing peers))
      :relative (if closest
                  (- (:note note) (:note closest))
                  0))))

(defn update-coincidents
  [co note-group]
  (let [[during just-preceding] (filter-playing (:continuing co) (last note-group))
        updated (reduce
                 (fn [co note]
                   (let [preceding (concat (:preceding co) just-preceding)
                         peers (remove #(= (:id %) (:id note)) note-group)
                         surrounding (surrounding-notes note preceding during peers)]
                     (update-in co [:notes] #(conj % surrounding))))
                 co note-group)]
    (assoc updated
      :preceding note-group
      :continuing (concat during note-group))))

(defn find-coincidents
  [notes]
  (let [basis {:preceding [] :continuing [] :notes []}
        groups (group-notes notes)]
    (:notes (reduce update-coincidents basis groups))))

