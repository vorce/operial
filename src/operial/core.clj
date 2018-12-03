(ns operial.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  ; (q/no-loop)
  (q/smooth)
  (q/frame-rate 1)
  ; (q/color-mode :hsb)
  {:start-x (q/random 800),
   :start-y (q/random 600)})

(def space 10)

(defn coords-based-on [width height [x y label]]
  (let [half-width (/ width 2)
        quarter-height (/ height 4)
        three-quarters-height (* 3 quarter-height)]
      (case label
        :top [(+ x (/ width 2) (/ space 2)) (- y (* 3 (/ height 4)) space)]
        :top-right [(+ x (/ width 2) space) (- y (/ height 4))]
        :bottom-right [(+ x (/ space 2)) (+ y space)]
        :bottom [(- x (/ width 2) (/ space 2)) y]
        :bottom-left [(- x (/ width 2) space) (- y (* 3 (/ height 4)))]
        :top-left [(- x (/ space 2)) (- y height space)]
      )
    )
  )

(defn hexagon-points [width height xpos ypos]
  (let [half-width (/ width 2)
        quarter-height (/ height 4)
        three-quarter-height (* quarter-height 3)]
    [[xpos ypos :top]
     [(+ xpos half-width) (+ ypos quarter-height) :top-right]

     [(+ xpos half-width) (+ ypos quarter-height) :top-right]
     [(+ xpos half-width) (+ ypos three-quarter-height) :bottom-right]

     [(+ xpos half-width) (+ ypos three-quarter-height) :bottom-right]
     [xpos (+ ypos height) :bottom]

     [xpos (+ ypos height) :bottom]
     [(- xpos half-width) (+ ypos three-quarter-height) :bottom-left]

     [(- xpos half-width) (+ ypos three-quarter-height) :bottom-left]
     [(- xpos half-width) (+ ypos quarter-height) :top-left]

     [(- xpos half-width) (+ ypos quarter-height) :top-left]
     [xpos ypos :top]]
    )
  )

(defn draw-shape [vertices]
  (q/stroke-cap :project)
  (q/begin-shape :lines)
  (doseq [v vertices]
    (let [x (v 0)
          y (v 1)]
      (q/vertex x y)))
  (q/end-shape)
)

(defn hexagon [width height]
  (q/stroke-cap :project)
  (q/begin-shape :lines)
  (let [half-width (/ width 2)
        quarter-height (/ height 4)
        three-quarter-height (* quarter-height 3)]
        (q/vertex 0 0) ; \
        (q/vertex half-width quarter-height)

        (q/vertex half-width quarter-height) ; |
        (q/vertex half-width three-quarter-height)

        (q/vertex half-width three-quarter-height) ; /
        (q/vertex 0 height)

        (q/vertex 0 height) ; \
        (q/vertex (- half-width) three-quarter-height)

        (q/vertex (- half-width) three-quarter-height) ; |
        (q/vertex (- half-width) quarter-height)

        (q/vertex (- half-width) quarter-height) ; /
        (q/vertex 0 0)
    )
  (q/end-shape))

(defn hex-cluster [width height points count]
  (if (zero? count) nil
    (let [[next-x next-y] (coords-based-on width height (rand-nth points))
          second (hexagon-points width height next-x next-y)]
      (q/blend-mode :add)
      (q/stroke-weight (rand-nth [1 2 3 4 5 6]))
      (draw-shape second)
      (recur width height second (dec count)))))

(defn draw-state [state]
  (q/background 0)
  (q/stroke-weight 1)
  (q/stroke 70 80 110 128)
  (q/fill 170 180 235)
  (q/ellipse 400 300 100 100)
  (q/with-translation [400 300]
    (hexagon 100 110))
  (let [first-hex (hexagon-points 36 40 (state :start-x) (state :start-y))]
    (hex-cluster 36 40 first-hex 120)))


(q/defsketch operial
  :title "Operial"
  :size [800 600]
  :setup setup
  :draw draw-state
  ; :features [:keep-on-top]
  :middleware [m/fun-mode])
