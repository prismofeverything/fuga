(ns fuga.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]
            [occam.csv :as csv]
            [occam.core :as occam]
            [occam.cluster :as cluster]
            [occam.fit :as fit])
            ;; [fuga.tones :as tones])
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
(def tick-buffer 15)

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

(defn make-header
  [data]
  (let [in-count (dec (count (first data)))
        in-names (take in-count (csv/symbol-tower "abcdefghijklmnopqrstuvwxy"))
        ins (map
             (fn [in-name]
               {:degrees 50 :role 1 :short-name in-name :long-name in-name})
             in-names)
        out {:long-name "next-note" :short-name "z" :degrees 50 :role 2}
        nominal (concat ins (list out))
        gram (occam/bind-frequencies data)]
    {:nominal nominal :data gram :test nil}))

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

(defn play-sequence
  [midi]
  (let [sequencer (open-sequence midi)]
    (start-sequence sequencer)
    #(close-sequence sequencer)))

(defn write-sequence
  [midi filename]
  (let [output (io/file filename)]
    (MidiSystem/write midi 1 output)))

(defn get-synthesizer
  []
  (MidiSystem/getSynthesizer))

(defn play-midi-file
  [path]
  (let [data (io/file path)
        midi (read-midi data)]
    (play-sequence midi)))

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

(defn patch-event
  [instrument channel]
  (println instrument channel)
  {:command :PATCH
   :channel channel
   :tick 0
   :data [instrument 0]})

(defn midi-sequence
  [instrument events]
  (let [sequence (Sequence. 0 240)
        track (.createTrack sequence)
        programs (map (partial patch-event instrument) (range 16))
        score (concat programs events)]
    (doseq [event score]
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

;; turn notes back into a midi sequence ---------------------------

(def velocity-default 72)

(defn note-events
  [note channel]
  (let [velocity (or (:velocity note) velocity-default)
        on {:command :ON :tick (:begin note) :data [(:note note) velocity] :channel channel}
        off {:command :OFF :tick (:end note) :data [(:note note) 0] :channel channel}]
    [on off]))

(defn separate
  [p s]
  (let [{hits true misses false} (group-by p s)]
    [hits misses]))

(defn midi-notes
  [notes]
  (loop [notes (sort-by :begin notes)
         channels {}
         channel 0
         midi nil]
    (if-not (empty? notes)
      (let [note (first notes)
            [remaining over] (separate
                              (fn [[_ end]]
                                (>= end (:begin note)))
                              channels)
            open-channel (first (sort (map first over)))
            channel (if (and open-channel (< open-channel channel))
                      open-channel
                      channel)]
        (recur
         (rest notes)
         (assoc (into {} remaining) channel (:end note))
         (inc channel)
         (concat midi (note-events note channel))))
      midi)))

(defn note-sequence
  [instrument notes]
  ((comp (partial midi-sequence instrument) midi-notes) notes))
  ;; (comp midi-sequence midi-notes))

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
        ids (take (count diffs) (iterate inc 0))]
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

(def relative-cutoff 11)

(defn surrounding-notes
  [note preceding continuing peers]
  (let [distance (comp #(Math/abs %) (partial note-difference :note note))
        closest (find-min < distance
                 (concat preceding continuing))
        between (distance closest)]
    (assoc note
      :closest (if (< between relative-cutoff) [(:id closest)])
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

(defn note-metric
  [note]
  (Math/sqrt (:duration note)))

(defn cluster-duration
  [notes]
  (let [cluster (cluster/cluster-by notes note-metric 7 :dur)]
    (update-in cluster [:restored] (partial sort-by :begin))))

(defn fugue-coincidents
  [book number]
  (let [fugue (:fugue (read-fugue book number))]
    (cluster-duration (find-coincidents fugue))))

;; extracting note chains -------------------------

(defn build-chain
  [web]
  (if-let [head (first (sort > (keys web)))]
    (loop [head head
           web web
           chain (list head)]
      (if-let [tail (get web head)]
        (let [web (dissoc web head)
              chain (cons tail chain)]
          (recur tail web chain))
        [chain web]))
    [nil nil]))

(defn note-web
  [notes]
  (reduce
   (fn [web note]
     (if-let [closest (-> note :closest first)]
       (assoc web
         (:id note) closest)
       web))
   {} notes))

(defn build-chains
  [notes]
  (loop [web (note-web notes)
         chains nil]
    (if (empty? web)
      chains
      (let [[chain web] (build-chain web)]
        (if (empty? chain)
          chains
          (recur web (cons chain chains)))))))

;; unfolding references -------------------------------------------

(def relative-keys [:relative :note :begin :duration])

(defn reference-pool
  [notes note]
  (fn [references]
    (map
     (fn [reference]
       (if reference
         (let [referred (select-keys (nth notes reference) relative-keys)]
           (assoc referred
             :from (- (:note referred) (:note note))
             :before (- (:begin note) (:begin referred))))))
     references)))

(defn find-note-references
  [notes note]
  (let [pool (reference-pool notes note)]
    (reduce
     (fn [note key]
       (update-in note [key] pool))
     note
     [:preceding :during :closest])))

(defn apply-note-references
  [notes coincidents]
  (map
   (partial find-note-references notes)
   coincidents))

;; note gestures ------------------------------------

(defn queue
  [l]
  (let [q (clojure.lang.PersistentQueue/EMPTY)]
    (apply conj (cons q l))))

(defn shift-window
  [capture landscape window]
  (loop [view (queue (take window landscape))
         landscape (drop window landscape)
         snapshots nil]
    (let [snapshot (capture view)
          snapshots (cons snapshot snapshots)]
      (if (empty? landscape)
        (reverse snapshots)
        (let [opening (first landscape)
              view (-> view pop (conj opening))]
          (recur view (rest landscape) snapshots))))))

(defn extract-gesture
  [notes chain history key]
  (let [note-chain (map #(get (nth notes %) key) chain)]
    (shift-window
     (fn [snapshot]
       (let [pivot (nth snapshot history)
             relative (map #(- % pivot) snapshot)]
         relative))
     note-chain
     (+ history 2))))

(defn extract-gestures
  [notes chains history key]
  (mapcat
   #(extract-gesture notes % history key)
   (filter #(> (count %) history) chains)))

(defn fugue-interrelation
  [book number history]
  (let [{fugue :restored durs :averages} (fugue-coincidents book number)
        chains (build-chains fugue)
        note-gestures (extract-gestures fugue chains history :note)
        dur-gestures (extract-gestures fugue chains history :dur)
        referents (apply-note-references fugue fugue)]
    {:book book
     :number number
     :history history
     :chains chains
     :durs durs
     :gestures {:note note-gestures :dur dur-gestures}
     :relations referents}))

;; old occam translation --------------------------------------

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

(defn shift-back
  [occam]
  (- 12 occam))

(def bin-index
  (take 23 (iterate inc 1)))

(defn relative-bins
  [relatives]
  (let [mapping
        (reduce
         (fn [m relative]
           (assoc m relative true))
         {} relatives)]
    (map
     (fn [x]
       (if (get mapping x) 1 0))
     bin-index)))

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

(defn occam-data
  [notes]
  (let [translations (map translate-note notes)
        frequency-map (occam/histogram translations)
        occam-rows (map
                    (fn [[r f]]
                      (conj (vec r) f))
                    frequency-map)]
    (assoc occam-header :data occam-rows)))

(defn occamize-fugue
  [book number history]
  (let [relations (fugue-interrelation book number history)
        data (occam-data (:relations relations))
        filename (str "occam/occam-book-" book "-fugue-" number ".in")]
    (occam/write-occam filename data)))

;; chain occam translation ------------------------------------------------

(defn chain-path
  [book number history key phase]
  (let [key (name key)
        phase (name phase)]
    (str "occam/" phase
         "/book-" book
         "-fugue-" number
         "-history-" history
         "-" key
         "." phase)))

(defn nested-greatest
  [c f z]
  (f (cluster/greatest c f z)))

(defn occam-interrelation
  [interrelation key]
  (let [gesture (-> interrelation :gestures key)
        minimum-independent (nested-greatest < (partial cluster/greatest < identity) gesture)
        minimum-dependent (nested-greatest < last gesture)
        independent-count (- (count (first gesture)) 2)
        independent (map
                     (fn [datum]
                       (map
                        #(- % minimum-independent)
                        (take independent-count datum)))
                     gesture)
        dependent (map (comp list #(- % minimum-dependent) last) gesture)
        data (map concat independent dependent)]
    [minimum-independent
     minimum-dependent
     (make-header data)]))

(defn occamize-chains
  [book number history key]
  (let [relations (fugue-interrelation book number history)
        [ifloor dfloor occam] (occam-interrelation relations key)
        path (chain-path book number history key :in)]
    (occam/write-occam path occam)
    (assoc relations
      :occam occam
      :key key
      :independent-floor ifloor
      :dependent-floor dfloor)))

(defn occamize-history
  [book number history key]
  (map 
   (fn [slice]
     (occamize-chains book number slice key))
   (range 1 (inc history))))

;; note generation --------------------------------------

(defn signature
  [phase relation]
  (let [phase (name phase)
        signify (juxt :book :number :history :key (fn [_] phase))]
    (signify relation)))

(defn merge-fit-mapping
  [history]
  (map
   (fn [relation]
     (let [sigil (signature :fit relation)
           path (apply chain-path sigil)
           mapping (fit/read-fit-mapping path)]
       (assoc relation
         :mapping mapping)))
   history))

(def note-keys [:note :dur])
(defn map-note-keys
  [keyf]
  (into
   {}
   (map
    (fn [key]
      [key (keyf key)])
    note-keys)))

(defn note-predictions
  [book number history]
  (map-note-keys
   (fn [key]
     (let [occam (occamize-history book number history key)]
       (merge-fit-mapping occam)))))

(defn predict-state
  [predictions key states]
  (loop [states states]
    (if (empty? states)
      0
      (let [index (-> states count dec)
            portal (-> predictions key (nth index))
            floor (:independent-floor portal)
            shifted (map #(- % floor) states)
            prediction (fit/produce-state (:mapping portal) shifted)]
        (if prediction
          (+ prediction (:dependent-floor portal))
          (recur (rest states)))))))

(defn predict-note
  [predictions states]
  (map-note-keys
   (fn [key]
     (predict-state predictions key (get states key)))))

(defn pivot-trail
  [trail]
  (let [pivot (first trail)]
    (map #(- % pivot) trail)))

(defn align-trail
  [trail]
  (map-note-keys
   (fn [key]
     (let [align (comp reverse pivot-trail (partial map key))]
       (align trail)))))

(def note-bounds 12)
(def octave 12)

(defn bound-note
  [note seed]
  (cond
   (> note (+ seed note-bounds)) (- note octave)
   (< note (- seed note-bounds)) (+ note octave)
   :else note))

(defn reflect
  [n base]
  (let [modulus (mod (Math/abs n) base)
        reflect? (odd? (quot n base))]
    (if reflect?
      (- base modulus)
      modulus)))

(defn evolve-note
  [child parent seed]
  (let [{note :note dur :dur}
        (map-note-keys
         (fn [key]
           (+ (get child key) (get parent key))))
        note (bound-note note (:note (first seed)))
        dur (reflect dur 6)]
        ;; dur (mod dur 7)]
    {:note note :dur dur}))

(defn note-generator
  [predictions history seed]
  (iterate
   (fn [notes]
     (let [trail (take history notes)
           present (first trail)
           states (align-trail trail)
           prediction (predict-note predictions states)
           note (evolve-note prediction present seed)]
       (cons note notes)))
   seed))

(defn voice-generator
  [book number history seed]
  (let [predictions (note-predictions book number history)]
    {:predictions predictions
     :generator (note-generator predictions history seed)}))

(defn linear-durs
  [pulse]
  (iterate #(+ % pulse) pulse))

(defn power-durs
  [base]
  (iterate #(* % 2) base))

(def pulse 25)

(defn render-notes
  [voice predictions]
  (loop [voice voice
         notes nil
         time 0]
    (if (empty? voice)
      (reverse notes)
      ;; (reverse notes)
      (let [{tone :note dur :dur} (first voice)
            ;; durs [70 70 140 140 140 280 280]
            durs [50 50 50 100 100 200 400]
            ;; durs (take 7 (power-durs pulse)) ;; (linear-durs pulse))
            ;; durs (-> predictions :dur first :durs)
            duration (nth durs dur)
            span (+ time duration)
            velocity (+ 40 (* (inc dur) 5))
            note {:note tone :begin time :end span :velocity velocity}]
        (recur (rest voice) (cons note notes) span)))))

(defn generate-notes
  [generator num]
  (let [generate (comp reverse last (partial take num) :generator)
        voice (generate generator)
        notes (render-notes voice (:predictions generator))]
    (println voice)
    {:notes notes :generator (update-in generator [:generator] (partial drop num))}))

(defn shift-note
  [note {tone :note begin :begin}]
  (let [time-shift (partial + begin)]
    (-> note
        (update-in [:note] (partial + tone))
        (update-in [:begin] time-shift)
        (update-in [:end] time-shift))))

(defn shift-notes
  [notes shift]
  (map #(shift-note % shift) notes))

(defn generate-sequence
  [generator num instrument]
  (let [notes (:notes (generate-notes generator num))]
    (note-sequence instrument notes)))

(def play-voice (comp play-sequence generate-sequence))
(def play-notes (comp play-sequence note-sequence))

(defn merge-notes
  [groups]
  (sort-by :begin (apply concat groups)))

(defn layer-notes
  [notes layers]
  (let [layering (map (partial shift-notes notes) layers)
        complete (cons notes layering)]
    (merge-notes complete)))

(defn regular-layering
  [bases delay]
  (let [[layers begin]
        (reduce
         (fn [[layers begin] base]
           [(cons {:note base :begin begin} layers) (+ begin delay)])
         [nil delay]
         bases)]
    #(layer-notes % layers)))

(defn write-notes
  [notes filename]
  (spit filename notes))

(defn read-notes
  [filename]
  (read-string (slurp filename)))

(defn full-circle
  [& {:keys [book fugue num seed layering tonality speed midi? instrument inst]
    :or {book 1 fugue 4 num 333 speed 2 instrument 21 midi? true
         seed [{:note 61 :dur 5}]
         layering [-14 14 -28]
         tonality nil
         inst nil}}]
         ;; inst (tones/b3)
         ;; tonality tones/tonal}}]
  (let [voice (voice-generator book fugue 3 seed)
        layer (regular-layering layering 3200)
        generator (generate-notes voice num)
        notes (-> generator :notes layer)
        sequence (note-sequence instrument notes)]
    ;; (if midi?
    (write-sequence sequence (str "midi/book-" book "-fugue-" fugue "-" (rand-int 1000000) ".mid"))
    (assoc generator :stop (play-sequence sequence)))) ;; instrument notes))))
      ;; (do
      ;;   (tones/play-gates notes :inst inst :tonality tonality :speed speed)
      ;;   generator))))

(def good-sounds [21 48 69 70 72 73 91])

(defn operator-staircase
  []
  (iterate
   (fn [f]
     (fn [a b]
       (loop [bottom b
              result a]
         (if (>= 0 bottom)
           result
           (recur
            (dec bottom)
            (f a result))))))
   (fn [a b]
     (inc b))))
