(ns sequencer.core
  (:use [overtone.live]))

(defonce root-trg-bus (control-bus)) ;; global metronome pulse
(defonce root-cnt-bus (control-bus)) ;; global metronome count
(defonce beat-trg-bus (control-bus)) ;; beat pulse (fraction of root)
(defonce beat-cnt-bus (control-bus)) ;; beat count

(def DEFAULT-BPM 120)
(def BPS (/ DEFAULT-BPM 60))
(def BEAT-FRACTION "Number of global pulses per beat" 96)

(defsynth root-trg [rate (* BPS BEAT-FRACTION)]
  (out:kr root-trg-bus (impulse:kr rate)))

(defsynth root-cnt []
  (out:kr root-cnt-bus (pulse-count:kr (in:kr root-trg-bus))))

(defsynth beat-trg [div BEAT-FRACTION]
  (out:kr beat-trg-bus (pulse-divider (in:kr root-trg-bus) div)))

(defsynth beat-cnt []
  (out:kr beat-cnt-bus (pulse-count (in:kr beat-trg-bus))))

(defsynth mono-sequencer
  "Plays a single channel audio buffer."
  [buf 0 rate 1 out-bus 0 beat-num 0 sequencer 0 amp 1]
  (let [cnt      (in:kr beat-cnt-bus)
        beat-trg (in:kr beat-trg-bus)
        bar-trg  (and (buf-rd:kr 1 sequencer cnt)
                      (= beat-num (mod cnt 8))
                      beat-trg)
        vol      (set-reset-ff bar-trg)]
    (out
     out-bus (* vol
                amp
                (pan2
                 (rlpf
                  (scaled-play-buf 1 buf rate bar-trg)
                  (demand bar-trg 0 (dbrown 200 20000 50 INF))
                  (lin-lin:kr (lf-tri:kr 0.01) -1 1 0.1 0.9)))))))

(def kick-s (freesound 777))
(def click-s (freesound 406))
(def boom-s (freesound 33637))
(def subby-s (freesound 25649))

(defonce buf-0 (buffer 8))
(defonce buf-1 (buffer 8))
(defonce buf-2 (buffer 8))
(defonce buf-3 (buffer 8))

(do
  (def r-trg (root-trg))
  (def r-cnt (root-cnt [:after r-trg]))
  (def b-trg (beat-trg [:after r-trg]))
  (def b-cnt (beat-cnt [:after b-trg]))


  (def kicks (doall
              (for [x (range 8)]
                (mono-sequencer :buf kick-s :beat-num x :sequencer buf-0))))

  (def clicks (doall
               (for [x (range 8)]
                 (mono-sequencer :buf click-s :beat-num x :sequencer buf-1))))

  (def booms (doall
              (for [x (range 8)]
                (mono-sequencer :buf boom-s :beat-num x :sequencer buf-2))))

  (def subbies (doall
                (for [x (range 8)]
                  (mono-sequencer :buf subby-s :beat-num x :sequencer buf-3)))))

;; An empty palatte to play with:
(do
  (buffer-write! buf-0 [1 0 1 1 0 0 1 0])  ;; kick
  (buffer-write! buf-1 [0 0 0 0 1 0 0 0])  ;; click
  (buffer-write! buf-2 [0 0 0 0 0 0 1 0])  ;; boom
  (buffer-write! buf-3 [0 0 0 0 0 0 0 0])) ;; subby

;; try mixing up the sequences. Evaluate this a few times:
;; (do
;;   (buffer-write! buf-0 (repeatedly 8 #(choose [0 1])))
;;   (buffer-write! buf-1 (repeatedly 8 #(choose [0 1])))
;;   (buffer-write! buf-2 (repeatedly 8 #(choose [0 1])))
;;   (buffer-write! buf-3 (repeatedly 8 #(choose [0 1]))))

;; and then to something interesting
;; (do
;;   (buffer-write! buf-0 [1 1 1 1 1 1 1 1])
;;   (buffer-write! buf-1 [1 0 1 0 0 1 1 0])
;;   (buffer-write! buf-2 [1 1 0 1 0 1 1 0])
;;   (buffer-write! buf-3 [1 0 0 0 0 0 1 0]))

;; try changing the rate of the global pulse (everything else will
;; follow suit):
;; (ctl r-trg :rate 75)
;; (ctl r-trg :rate 300)
;; (ctl r-trg :rate 150)
(stop)
