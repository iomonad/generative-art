(ns animation.mandelbrot
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [complex.core :as c]))

(defn scale-pixel-to-complex [[x y] center width window-dim]
  (c/complex
   (+ (c/real-part center) (* width (/ (- x (/ window-dim 2)) window-dim)))
   (+ (c/imaginary-part center) (* width (/ (- y (/ window-dim 2)) window-dim)))))

(defn escape-to-color [en maxiter]
  (let [rat (/ (en 0) maxiter)]
    (q/color (* 100 rat) 100 (* 100  (- 1 rat)))))

(defn escape-number-loop [c maxiter]
  (loop [z (c/complex 0 0)
         iter 0]
    (if (or (> iter maxiter)
            (or (> (c/real-part z) 2)
                (> (c/imaginary-part z) 2)))
      [iter z]
      (recur (c/+ (c/* z z) c) (inc iter)))))

(defn setup []
  (q/frame-rate 30)
  (q/color-mode :hsb 100)
  {:center (c/complex -0.6 0.6)
   :width 1.0})

(defn update-state [state]
  state)

(defn draw-state [state]
  (let [w (q/width)
        h (q/height)
        pxl (q/pixels)]
    (doseq [x (range w)
            y (range h)]
      (let [c (scale-pixel-to-complex [x y] (:center state) (:width state) w)
            mycolor (escape-to-color (escape-number-loop c 100) 100)]
        (aset-int pxl (+ x (* y w)) mycolor)))
    (q/update-pixels)))

(q/defsketch mandelbrot-clj
  :size [1000 1000]
  :setup setup
  :update update-state
  :draw draw-state
  :renderer :p2d
  :features [:keep-on-top]
  :middleware [m/fun-mode])
