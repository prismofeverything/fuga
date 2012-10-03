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
  [filename]
  (MidiSystem/getSequence (io/file filename)))

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