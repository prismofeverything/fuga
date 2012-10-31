(ns fuga.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]
            [occam.core :as occam]
            [occam.cluster :as cluster])
  (:import (javax.sound.midi MidiSystem MidiEvent Sequence Sequencer ShortMessage)))

(defn invert-map
  [m]
  (reduce
   (fn [m [k v]]
     (assoc m v k)) {} m))

(def command-map
  {\8 :OFF
   \9 :ON
   \a :AFTERTOUCH
   \b :CONTROL
   \c :PATCH
   \d :PRESSURE
   \e :PITCHBEND
   \f :SYSTEM})

(def byte-commands
  {:OFF ShortMessage/NOTE_OFF
   :ON ShortMessage/NOTE_ON
   :AFTERTOUCH ShortMessage/POLY_PRESSURE
   :CONTROL ShortMessage/CONTROL_CHANGE
   :PATCH ShortMessage/PROGRAM_CHANGE
   :PRESSURE ShortMessage/CHANNEL_PRESSURE
   :PITCHBEND ShortMessage/PITCH_BEND
   :START (int 250)
   :STOP (int 252)})

;; (def byte-commands (invert-map command-map))
(def tick-buffer 10)

(def occam-header
  {:nominal
   [{:long-name "preceding-major-seventh-below", :degrees "2", :role "1", :short-name "pjvb"}
    {:long-name "preceding-minor-seventh-below", :degrees "2", :role "1", :short-name "pnvb"}
    {:long-name "preceding-major-sixth-below", :degrees "2", :role "1", :short-name "pjxb"}
    {:long-name "preceding-minor-sixth-below", :degrees "2", :role "1", :short-name "pnxb"}
    {:long-name "preceding-fifth-below", :degrees "2", :role "1", :short-name "ppfb"}
    {:long-name "preceding-tritone-below", :degrees "2", :role "1", :short-name "pttb"}
    {:long-name "preceding-fourth-below", :degrees "2", :role "1", :short-name "ppob"}
    {:long-name "preceding-major-third-below", :degrees "2", :role "1", :short-name "pjtb"}
    {:long-name "preceding-minor-third-below", :degrees "2", :role "1", :short-name "pntb"}
    {:long-name "preceding-major-second-below", :degrees "2", :role "1", :short-name "pjsb"}
    {:long-name "preceding-minor-second-below", :degrees "2", :role "1", :short-name "pnsb"}
    {:long-name "preceding-unison", :degrees "2", :role "1", :short-name "puuu"}
    {:long-name "preceding-minor-second-above", :degrees "2", :role "1", :short-name "pnsa"}
    {:long-name "preceding-major-second-above", :degrees "2", :role "1", :short-name "pjsa"}
    {:long-name "preceding-minor-third-above", :degrees "2", :role "1", :short-name "pnta"}
    {:long-name "preceding-major-third-above", :degrees "2", :role "1", :short-name "pjta"}
    {:long-name "preceding-fourth-above", :degrees "2", :role "1", :short-name "ppua"}
    {:long-name "preceding-tritone-above", :degrees "2", :role "1", :short-name "ptta"}
    {:long-name "preceding-fifth-above", :degrees "2", :role "1", :short-name "ppfa"}
    {:long-name "preceding-minor-sixth-above", :degrees "2", :role "1", :short-name "pnxa"}
    {:long-name "preceding-major-sixth-above", :degrees "2", :role "1", :short-name "pjxa"}
    {:long-name "preceding-minor-seventh-above", :degrees "2", :role "1", :short-name "pnva"}
    {:long-name "preceding-major-seventh-above", :degrees "2", :role "1", :short-name "pjva"}
    {:long-name "during-major-seventh-below", :degrees "2", :role "1", :short-name "djvb"}
    {:long-name "during-minor-seventh-below", :degrees "2", :role "1", :short-name "dnvb"}
    {:long-name "during-major-sixth-below", :degrees "2", :role "1", :short-name "djxb"}
    {:long-name "during-minor-sixth-below", :degrees "2", :role "1", :short-name "dnxb"}
    {:long-name "during-fifth-below", :degrees "2", :role "1", :short-name "dpfb"}
    {:long-name "during-tritone-below", :degrees "2", :role "1", :short-name "dttb"}
    {:long-name "during-fourth-below", :degrees "2", :role "1", :short-name "dpob"}
    {:long-name "during-major-third-below", :degrees "2", :role "1", :short-name "djtb"}
    {:long-name "during-minor-third-below", :degrees "2", :role "1", :short-name "dntb"}
    {:long-name "during-major-second-below", :degrees "2", :role "1", :short-name "djsb"}
    {:long-name "during-minor-second-below", :degrees "2", :role "1", :short-name "dnsb"}
    {:long-name "during-unison", :degrees "2", :role "1", :short-name "duuu"}
    {:long-name "during-minor-second-above", :degrees "2", :role "1", :short-name "dnsa"}
    {:long-name "during-major-second-above", :degrees "2", :role "1", :short-name "djsa"}
    {:long-name "during-minor-third-above", :degrees "2", :role "1", :short-name "dnta"}
    {:long-name "during-major-third-above", :degrees "2", :role "1", :short-name "djta"}
    {:long-name "during-fourth-above", :degrees "2", :role "1", :short-name "dpua"}
    {:long-name "during-tritone-above", :degrees "2", :role "1", :short-name "dtta"}
    {:long-name "during-fifth-above", :degrees "2", :role "1", :short-name "dpfa"}
    {:long-name "during-minor-sixth-above", :degrees "2", :role "1", :short-name "dnxa"}
    {:long-name "during-major-sixth-above", :degrees "2", :role "1", :short-name "djxa"}
    {:long-name "during-minor-seventh-above", :degrees "2", :role "1", :short-name "dnva"}
    {:long-name "during-major-seventh-above", :degrees "2", :role "1", :short-name "djva"}
    {:long-name "next-note", :degrees "23", :role "2", :short-name "nn"}]
   :data [] :test nil})

;; reading and playing sequences from midi files -------------------

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

;; extracting note data from midi files -------------------------------------

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

(defn midi-event
  [event]
  (let [command (get byte-commands (:command event))
        [data1 data2] (map byte (:data event))
        message (ShortMessage.)]
    (.setMessage message command (:channel event) data1 data2)
    (MidiEvent. message (:tick event))))

(defn midi-sequence
  [events]
  (let [sequence (Sequence. 0 240)
        track (.createTrack sequence)]
    (doseq [event events]
      (.add track (midi-event event)))
    sequence))

;; processing midi events into notes ---------------------------------

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
                 :note (-> off :data first)
                 :duration (- (:tick off) tick)})
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

;; process notes into basic harmonic relations -----------------------

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
  (split-every #(> (:diff %) tick-buffer) notes))

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
  (>= (- (:end note) tick-buffer) (:begin new-note)))

(defn filter-playing
  [notes new-note]
  (bicleave #(still-playing? % new-note) notes))

(defn find-min
  "Given comparator c and projection f, find the minimum value for f in sequence s."
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
     
(defn note-difference
  [k a b]
  (if (and a b)
    (- (get a k) (get b k))
    0))

(defn surrounding-notes
  [note preceding continuing peers]
  (let [closest (find-min <
                 #(Math/abs (- (:note note) (:note %)))
                 (concat preceding continuing))]
    (assoc note
      :preceding (map :id preceding)
      :during (map :id (concat continuing peers))
      :wait (note-difference :begin note closest)
      :relative (note-difference :note note closest))))

(defn update-coincidents
  [co note-group]
  (let [[during just-preceding] (filter-playing (:continuing co) (last note-group))
        updated (reduce
                 (fn [co note]
                   (let [preceding (set/union (:preceding co) (set just-preceding))
                         peers (remove #(= (:id %) (:id note)) note-group)
                         surrounding (surrounding-notes note preceding during peers)]
                     (update-in co [:notes] #(conj % surrounding))))
                 co note-group)]
    (assoc updated
      :preceding (set note-group)
      :continuing (concat during note-group))))

(defn find-coincidents
  [notes]
  (let [basis {:preceding #{} :continuing [] :notes []}
        groups (group-notes notes)]
    (:notes (reduce update-coincidents basis groups))))

(def relative-keys [:relative :note])

(defn reference-pool
  [notes note]
  (fn [references]
    (map (fn [reference]
           (let [referred (select-keys (nth notes (dec reference)) relative-keys)]
             (assoc referred
               :from (- (:note referred) (:note note))
               :before (- (:begin note) (:begin referred)))))
         references)))

(defn find-note-references
  [notes note]
  (let [pool (reference-pool notes note)]
    (-> note
        (update-in [:preceding] pool)
        (update-in [:during] pool))))

(defn apply-note-references
  [notes coincidents]
  (map (partial find-note-references notes) coincidents))

(defn fugue-interrelation
  [book number]
  (let [fugue (:fugue (read-fugue book number))
        coincidents (find-coincidents fugue)]
    (apply-note-references coincidents coincidents)))

;; occam translation --------------------------------------

(defn within-n
  [n]
  (fn [x]
    (loop [x x]
      (cond
       (>= x n) (recur (- x n))
       (<= x (- n)) (recur (+ x n))
       :else x))))

(def within-12 (within-n 12))

(defn shift-relative
  [relative]
  (+ 12 (within-12 relative)))

(def bin-index (take 23 (iterate inc 1)))

(defn relative-bins
  [relatives]
  (let [mapping (reduce (fn [m relative] (assoc m relative true)) {} relatives)]
    (map (fn [x] (if (get mapping x) 1 0)) bin-index)))

(defn translate-relatives
  [relatives]
  (let [mapping (map #(shift-relative (:from %)) relatives)]
    (relative-bins mapping)))

(defn translate-note
  [note]
  (let [relative (shift-relative (:relative note))
        preceding (translate-relatives (:preceding note))
        during (translate-relatives (:during note))
        relatives (vec (concat preceding during))]
    (conj relatives relative)))

(defn histogram
  [s]
  (reduce
   (fn [hist item]
     (assoc hist item (if-let [total (get hist item)] (inc total) 1)))
   {} s))

(defn occam-data
  [notes]
  (let [translations (map translate-note notes)
        frequency-map (histogram translations)
        occam-rows (map
                    (fn [[r f]]
                      (conj (vec r) f))
                    frequency-map)]
    (assoc occam-header :data occam-rows)))

(defn occamize-fugue
  [book number]
  (let [relations (fugue-interrelation book number)
        data (occam-data relations)
        filename (str "occam/occam-book-" book "-fugue-" number ".in")]
    (occam/write-occam filename data)))

;; TODO: break duration down into bins
;; TODO: translate notes back into midi
;; TODO: markov chain generator based on note data