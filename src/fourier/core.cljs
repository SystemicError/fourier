(ns fourier.core
  (:require [clojure.string :as str]))

(println "Program started.")

; mathematical necessities

(defn complex [a b]
  {:real a :imag b})

(defn complex+ [& args]
  (reduce #(complex (+ (:real %1) (:real %2))
                    (+ (:imag %1) (:imag %2))) args))

(defn complex* [& args]
  (reduce #(complex (- (* (:real %1) (:real %2)) (* (:imag %1) (:imag %2)))
                    (+ (* (:real %1) (:imag %2)) (* (:imag %1) (:real %2)))) args))

(defn cis [t]
  (complex (Math/cos t) (Math/sin t)))

(defn dft [zs]
  (for [n (range (count zs))]
    (complex*
      (reduce complex+
              (map complex*
                   zs
                   (for [t (range (count zs))]
                     (cis (/ (* -2.0 Math/PI n t) (count zs))))))
      (complex (/ (count zs)) 0))))

(defn inv-dft [bins]
  (for [n (range (count bins))]
    (reduce complex+
            (map complex*
                 bins
                 (for [t (range (count bins))]
                   (cis (/ (* 2.0 Math/PI n t) (count bins))))))))

; client necessities

(defn read-coords []
  "Returns a list of all segments"
  (let [coords (.-value (.getElementById js/document "coords"))
        lines (str/split-lines coords)
        dummy (js/console.log lines)]
    (for [line lines]
      (let [fields (str/split (str/trim line) #"\t")
            x (js/parseInt (nth fields 0))
            y (js/parseInt (nth fields 1))]
        {:real x
         :imag y}))))

(defn draw-path [ctx zs]
  "Given draw input path."
  (if (> (count zs) 1)
    (do
      (.beginPath ctx)
      (.moveTo ctx (:real (first zs)) (:imag (first zs)))
      (.lineTo ctx (:real (nth zs 1)) (:imag (nth zs 1)))
      (.stroke ctx)
      (recur ctx (rest zs)))))

(defn update-draw-in []
  "Update input canvas."
  (let [draw-in-canvas (.getElementById js/document "draw-in")
        ctx (.getContext draw-in-canvas "2d")
        zs (read-coords)]
    (do
      (.clearRect ctx 0 0 (.-width draw-in-canvas) (.-height draw-in-canvas))
      (draw-path ctx zs))))

(defn update-draw-out [bins]
  "Update output canvas"
  (let [draw-out-canvas (.getElementById js/document "draw-out")
        ctx (.getContext draw-out-canvas "2d")]
    (do
      (.clearRect ctx 0 0 (.-width draw-out-canvas) (.-height draw-out-canvas))
      (draw-path ctx (inv-dft (take 5 bins))))))

(defn on-mouse-move [event]
  (let [coords (.getElementById js/document "coords")
        draw-in (.getElementById js/document "draw-in")
        x (- (.-clientX event) (.-offsetLeft draw-in))
        y (- (.-clientY event) (.-offsetTop draw-in))
        buttons (.-buttons event)]
    (if (= buttons 1)
      (do
        (set! (.-value coords) (str (.-value coords) (if (= "" (.-value coords)) "" "\n") x "\t" y))
        (update-draw-in)))))


(defn on-mouse-down [event]
  (let [coords (.getElementById js/document "coords")]
    (set! (.-value coords) "")))

(defn on-mouse-up [event]
  (let [bins (dft (read-coords))]
    (set! (.-value (.getElementById js/document "bins")) (str bins))
    (update-draw-out bins)))

; event hooks

(set! (.-onmousemove (.getElementById js/document "draw-in")) on-mouse-move)
(set! (.-onmousedown (.getElementById js/document "draw-in")) on-mouse-down)
(set! (.-onmouseup (.getElementById js/document "draw-in")) on-mouse-up)
