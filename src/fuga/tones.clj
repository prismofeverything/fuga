(ns fuga.tones
  (:use [overtone.core])
  (:require [tonality.tonality :as tonality]))

;; (boot-server)

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

;; (definst bell
;;   [freq 220
;;    dur 1.0
;;    vol 1.0
;;    partials [0.5 1 2 4]]
;;   (let [snd (* vol (bell-partials freq dur partials))]
;;     (detect-silence snd :action FREE)
;;     snd))

(def twelve
  (tonality/tonality
   (tonality/equal-temperament 12)
   20.0 0))

(def tonal
  (tonality/tonality
   [1/1 16/15 9/8 9/8 5/4 5/4 4/3 4/3 3/2 8/5 5/3 5/3 15/8 15/8]
   15.0 0))

(definst sinon
  [freq 400 amp 0.8 gate 1.0 a 0.01 d 3 s 1 r 0.01]
  (let [psi (sin-osc freq)
        env (env-gen (adsr a d s r) gate :action FREE)]
    (* amp env psi)))

(definst b3
  [freq 60 amp 0.8 gate 1.0 a 0.01 d 3 s 1 r 0.01]
  (let [waves (sin-osc [(* 0.5 freq)
                        freq
                        (* (/ 3 2) freq)
                        (* 2 freq)
                        (* freq 2 (/ 3 2))
                        (* freq 2 2)
                        (* freq 2 2 (/ 5 4))
                        (* freq 2 2 (/ 3 2))
                        (* freq 2 2 (/ 7 4))
                        (* freq 2 2 2)])
        snd (apply + waves)
        env (env-gen (adsr a d s r) gate :action FREE)]
    (* amp env snd 0.1)))

(defn play-moments
  "The notes must already have been partitioned into groups which all share :begin"
  [f moments start speed]
  (if-not (empty? moments)
    (let [moment (first moments)
          future (rest moments)
          begin (-> moment first :begin (* speed))
          next (-> future first first :begin (* speed))]
      (at
       (+ start begin)
       (dorun
        (map f moment)))
      (if next
        (apply-at
         (+ start next)
         #'play-moments
         [f future start speed])))))

(defn play-tones
  [tones & {:keys [inst tonality speed]
            :or {inst dull-bell
                 tonality twelve
                 speed 1}}]
  (let [start (now)
        moments (partition-by :begin tones)]
    (play-moments
     (fn [tone]
       (let [begin (:begin tone)
             end (:end tone)
             dur (* speed 0.001 (- end begin))
             freq (-> tone :note tonality)
             vol (-> tone :velocity (* 0.01))]
         (inst freq dur vol)))
     moments start speed)))
        
(defn play-gates
  [tones & {:keys [inst tonality speed]
            :or {inst b3
                 tonality twelve
                 speed 1}}]
  (let [start (now)
        moments (partition-by :begin tones)]
    (play-moments
     (fn [tone]
       (let [end (:end tone)
             freq (-> tone :note (tonality (:begin tone)))
             amp (-> tone :velocity (* 0.01))
             synth (inst :freq freq :amp amp)]
         (at
          (+ start (* end speed))
          (ctl synth :gate 0))))
     moments start speed)))