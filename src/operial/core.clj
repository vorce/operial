(ns operial.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn setup []
  ;(q/no-loop)
  (q/smooth)
  (q/frame-rate 1)
  ; (q/color-mode :hsb)
  {:start-x (q/random 800),
   :start-y (q/random 600)
   :hex-size (rand-nth [[18 20] [27 30] [36 40] [54 60] [72 80]])})

(def space 10)

(defn check [w h c1 c2]
  (let [c2-tl (c2 9)
        c1-top (c1 0)
        c1-br (c1 3)
        c2-bot (c2 5)]
    {:c2-tl (coords-based-on w h c2-tl)
     :c1-top (coords-based-on w h c1-top)
     :c1-br (coords-based-on w h c1-br)
     :c2-bot (coords-based-on w h c2-bot)}
  ))

(defn coords-based-on [width height [x y label]]
  (let [half-width (/ width 2)
        quarter-height (/ height 4)
        three-quarters-height (* 3 quarter-height)
        half-space (/ space 2)]
      (case label
        :top [(+ x half-width half-space) (- y three-quarters-height space)]
        :top-right [(+ x half-width space) (- y (/ height 4))] ; (- y (/ height 4))
        :bottom-right [(+ x half-space) (+ y space)]
        :bottom [(- x half-width half-space) (- y (- (/ height 4) space))]
        :bottom-left [(- x half-width space) (- y three-quarters-height)]
        :top-left [(- x half-space) (- y height space)]
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
  (q/stroke-cap :round)
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

(defn debug [w h]
  (let [first-hex (hexagon-points w h 400 300)
  [tx ty] (coords-based-on w h (first-hex 0))
  top (hexagon-points w h tx ty)
  [tr-x tr-y] (coords-based-on w h (first-hex 1))
  top-right (hexagon-points w h tr-x tr-y)
  [br-x br-y] (coords-based-on w h (first-hex 3))
  bot-right (hexagon-points w h br-x br-y)
  [bx by] (coords-based-on w h (first-hex 5))
  bot (hexagon-points w h bx by)
  [bl-x bl-y] (coords-based-on w h (first-hex 7))
  bot-left (hexagon-points w h bl-x bl-y)
  [tl-x tl-y] (coords-based-on w h (first-hex 9))
  top-left (hexagon-points w h tl-x tl-y)]
  (q/stroke 255 255 255)
  (draw-shape first-hex)
  (q/stroke 255 0 0)
  (draw-shape top)
  (q/stroke 0 255 0)
  (draw-shape top-right)
  (q/stroke 0 0 255)
  (draw-shape bot-right)
  (q/stroke 255 255 0)
  (draw-shape bot)
  (q/stroke 255 0 255)
  (draw-shape bot-left)
  (q/stroke 0 255 255)
  (draw-shape top-left)
  ))

(defn hex-cluster [width height points count]
  (if (zero? count) nil
    (let [[next-x next-y] (coords-based-on width height (rand-nth points))
          second (hexagon-points width height next-x next-y)]
      (q/blend-mode :add)
      (q/stroke-weight (rand-nth [1 2 3 4 5 6 7]))
      (draw-shape second)
      (recur width height second (dec count)))))

(defn draw-state [state]
  (q/background 0)
  (q/stroke-weight 1)
  (q/stroke 70 80 110 128)
  (q/fill 235 180 170 200)
  (q/ellipse 400 300 150 150)
  ;(q/with-translation [400 300]
  ;  (hexagon 100 110))
  (let [hex-size (state :hex-size)
        hex-w (hex-size 0)
        hex-h (hex-size 1)
        first-hex (hexagon-points hex-w hex-h (state :start-x) (state :start-y))
        ;bigger-hex (hexagon-points 72 80 (q/random 800) (q/random 600))
        ]
    (hex-cluster hex-w hex-h first-hex 250)
    ;(hex-cluster 72 80 bigger-hex 15)
    ))

(q/defsketch operial
  :title "Operial"
  :size [800 600]
  :setup setup
  :draw draw-state
  ; :features [:keep-on-top]
  :middleware [m/fun-mode])
