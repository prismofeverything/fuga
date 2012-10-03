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

(defn process-notes
  [notes]
  )