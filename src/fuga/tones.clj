(ns fuga.tones
  (:use [overtone.core]))

(def dull-partials
  [0.56
   0.92
   1.19
   1.71
   2
   2.74
   3
   3.76
   4.07])

(def partials
  [0.5
   1
   3
   4.2
   5.4
   6.8])

(defcgen bell-partials
  [freq {:default 440}
   dur  {:default 1.0}
   partials {:default [0.5 1 2 4]}]
  "Generates a series of progressively shorter and quieter enveloped sine waves
  for each of the partials specified. The length of the envelope is proportional
  to dur and the fundamental frequency is specified with freq."
  (:ar
   (apply
    +
    (map
     (fn [partial proportion]
       (let [env      (env-gen (perc 0.01 (* dur proportion)))
             vol      (/ proportion 2)
             overtone (* partial freq)]
         (* env vol (sin-osc overtone))))
     partials
     (iterate #(/ % 2) 1.0)))))

(definst dull-bell [freq 220 dur 1.0 vol 1.0]
  (let [snd (* vol (bell-partials freq dur dull-partials))]
    (detect-silence snd :action FREE)
    snd))

(definst pretty-bell [freq 220 dur 1.0 vol 1.0]
  (let [snd (* vol (bell-partials freq dur partials))]
    (detect-silence snd :action FREE)
    snd))

(def bell-metro  (metronome 400))
(def kije-troika-intervals
  (let [_ nil]
    [[ :i++ :v++ ]
     [ :i :i ]
     [_     _    _     _    _     _   _   _
      _     _    _     _    _     _  :v   _
      :i+  :vii  :vi  :vii  :i+   _  :vi  _
      :v    _     :vi  _   :iii   _  :v   _
      :vi  :v     :iv  _   :i+   _   :vii :i+
      :v   _      _    _   _     _   :iv  :iii
      :ii  _      :vi  _  :v     _   :iv  _   :v :iv
      :iii :iv    :v   _  :i+   :vi :iv  _   :iii  :iv :v _ :v _ :i ]]))

(def troika-hz
  "Map all nested kije troika intervals to hz using the major scale with root C5"
  (let [scale [:major :C5]]
    (letfn [(intervals->hz [intervals]
              (map #(when % (midi->hz %)) (apply degrees->pitches intervals scale)))]
      (map intervals->hz kije-troika-intervals))))

;; Plays the tune endlessly
(defn play-bells
  "Recursion through time over an sequence of infinite sequences of hz notes
  (or nils representing rests) to play with the pretty bell at the specific
  time indicated by the metronome"
  [beat notes]
  (let [next-beat     (inc beat)
        notes-to-play (remove nil? (map first notes))]
    (at (bell-metro beat)
        (dorun
         (map #(pretty-bell % :vol 0.5) notes-to-play)))
    (apply-at (bell-metro next-beat) #'play-bells [next-beat (map rest notes)])))

;; Start the bells ringing...
(defn runner
  "Start up the play-bells recursion with a repeating troika melody and bassline"
  []
  (play-bells (bell-metro) (map cycle troika-hz)))

;; (pretty-bell 440) ;; sounds a bit woodblock
;; (pretty-bell 2000 7.00) ;; diiiiiiiiinnng
;; (dull-bell 600 5.0) ;;  ddddddonnnngg
;; (runner) ;; happy xmas
;; (stop)
