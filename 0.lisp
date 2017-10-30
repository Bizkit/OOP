;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; flags.lisp - příklad ke kapitole 5: Dědičnost
;;;;

#|

DOKUMENTACE
-----------

(Jde o uživatelskou dokumentaci. U tříd dokumentujeme vlastnosti a zprávy, 
které jsou určeny uživateli. Interní části tříd nedokumentujeme.)


TŘÍDA FLAG-RECTANGLE (PICTURE)
------------------------------

Pomocná třída. Instance jsou černě zarámované obdélníky o poměru stran 2:3, 
které slouží jako podklad vlajky. Vlastnost color udává barvu výplně.

UPRAVENÉ ZDĚDĚNÉ VLASTNOSTI

color:    určuje barvu výplně
items:    jen ke čtení


TŘÍDA JP-FLAG (PICTURE)
-----------------------

Japonská vlajka.

UPRAVENÉ ZDĚDĚNÉ VLASTNOSTI

items:    jen ke čtení


TŘÍDA CZ-FLAG (PICTURE)
-----------------------

Česká vlajka.

UPRAVENÉ ZDĚDĚNÉ VLASTNOSTI

items:    jen ke čtení


TŘÍDA EU-FLAG (PICTURE)
-----------------------

Vlajka EU.

NOVÁ VLASTNOST

black-and-white-p:   Zda je vlajka v černobílé verzi. Default: nil.

UPRAVENÉ ZDĚDĚNÉ VLASTNOSTI

items:    jen ke čtení


Projděte si testovací kód na konci souboru.
|#

;;;
;;; Třída flag-rectangle: podklad vlajky 2:3
;;;

(defclass flag-rectangle (picture) ())

(defun flag-rect-points ()
  (mapcar (lambda (coords)
            (apply #'move (make-instance 'point) coords))
          '((20 20) (200 20) (200 140) (20 140))))

(defmethod initialize-instance ((fr flag-rectangle) &rest initargs)
  (call-next-method)
  (let ((rect1 (make-instance 'polygon))
        (rect2 (make-instance 'polygon)))
    (set-items rect1 (flag-rect-points))
    (set-items rect2 (flag-rect-points))
    (set-filledp rect2 t)
    (set-color rect2 :white)
    (set-items fr (list rect1 rect2)))
  fr)

(defmethod set-color ((fr flag-rectangle) value)
  (call-next-method)
  (set-color (second (items fr)) value)
  fr)

;;;
;;; jp-flag
;;;

(defun jp-flag-items ()
    (list (set-color 
           (set-radius 
            (set-filledp
             (move (make-instance 'circle) 110 80)
             t) 
            36)
           (color:make-rgb (/ #xce 255.0)
                           (/ #x11 255.0)
                           (/ #x26 255.0)))
          (make-instance 'flag-rectangle)))

(defclass jp-flag (picture)
  ())

(defmethod initialize-instance ((flag jp-flag) &rest args)
  (call-next-method)
  (set-items flag (jp-flag-items))
  flag)

;;;
;;; cz-flag
;;;

;; Dvě užitečné pomocné funkce:

(defun make-point (x y)
  (move (make-instance 'point) x y))

(defun make-polygon (coord-list filledp closedp color)
  (set-closedp (set-filledp
                (set-color
                 (set-items (make-instance 'polygon)
                            (mapcar (lambda (pair)
                                      (apply #'make-point pair))
                                    coord-list))
                 color)
                filledp)
               closedp))

(defun cz-flag-items ()
  (list (make-polygon '((20 20) (200 20) (200 140) (20 140)) nil t :black)
        (make-polygon '((20 20) (110 80) (20 140)) t t :blue)
        (make-polygon '((20 20) (200 20) (200 80) (110 80)) t t :white)
        (make-polygon '((110 80) (200 80) (200 140) (20 140)) t t :red)))

(defclass cz-flag (picture)
  ())

(defmethod initialize-instance ((flag cz-flag) &rest args)
  (call-next-method)
  (set-items flag (cz-flag-items)))

;;;
;;; eu-flag
;;;

;;Pomocné třídy star a eu-stars

(defclass star (polygon) ())

(defun star-points ()
  (let ((result '())
        (origin (make-instance 'point)))
    (dotimes (i 5)
      (let ((pt1 (make-instance 'point))
            (pt2 (make-instance 'point)))
        (move pt1 0 -1)
        (rotate pt1 (* pi 2/5 i) origin)
        (move pt2 0 (- 1 0.618))
        (rotate pt2 (* pi 2/5 (+ i 2)) origin)
        (setf result (cons pt1 (cons pt2 result)))))
    result))

(defmethod initialize-instance ((star star) &rest initargs)
  (call-next-method)
  (set-items star (star-points))
  (set-closedp star t))

(defclass eu-stars (picture)
  ())

(defun eu-star (index center)
  (let ((position (make-instance 'point))
        (result (make-instance 'star)))
    (set-filledp result t)
    (move position (x center) (y center))
    (move position 0 -40)
    (rotate position (* index (/ pi 6)) center)
    (move result (x position) (y position))
    (scale result (* 1/18 120) position)
    result))

(defmethod initialize-instance ((stars eu-stars) &rest initargs)
  (call-next-method)
  (let ((center (move (make-instance 'point) 110 80))
        (items '()))
    (dotimes (i 12)
      (setf items (cons (eu-star i center) items)))
    (set-items stars items)
    stars))

(defmethod set-color ((stars eu-stars) color)
  (call-next-method)
  (send-to-items stars 'set-color color)
  stars)
    
;;Vlastní třída eu-flag

(defclass eu-flag (picture)
  (black-and-white-p))

(defun eu-flag-background (f)
  (second (items f)))

(defun eu-flag-stars (f)
  (first (items f)))

(defmethod black-and-white-p ((f eu-flag))
  (slot-value f 'black-and-white-p))

(defmethod set-black-and-white-p ((f eu-flag) value)
  (setf (slot-value f 'black-and-white-p) value)
  (cond (value (set-color (eu-flag-background f) :white)
               (set-color (eu-flag-stars f) :black))
        (t (set-color (eu-flag-background f) 
                      (color:make-rgb 0.0 (/ 51.0 255) (/ 153.0 255)))
           (set-color (eu-flag-stars f) (color:make-rgb 1.0 (/ 204.0 255) 0.0)))))

(defmethod initialize-instance ((flag eu-flag) &rest initargs)
  (call-next-method)
  (set-items flag (list (make-instance 'eu-stars)
                        (make-instance 'flag-rectangle)))
  (set-black-and-white-p flag nil)
  flag)


;; Testy (postupně vyhodnocujte pomocí F8)

#|
;; flag-rectangle:

(setf rect (make-instance 'flag-rectangle))
(setf w (make-instance 'window))
(set-shape w rect)
(redraw w)
(set-color rect :yellow)
(redraw w)
|#

#|
;; jp-flag:

(setf w (make-instance 'window))
(set-shape w (make-instance 'jp-flag))
(redraw w)
(set-background w :skyblue)
(move (shape w) 35 20)
(redraw w)
|#

#|
;; cz-flag:

(setf w (make-instance 'window))
(set-shape w (make-instance 'cz-flag))
(redraw w)
(set-background w :skyblue)
(redraw w)
|#

#|
;; Test pomocné třídy star (nezdokumentované):

(setf star (make-instance 'star))
(setf w (make-instance 'window))
(set-shape w star)
(redraw w)
(scale star 100 (make-instance 'point))
(redraw w)
(move star 110 110)
(redraw w)
(set-filledp star t)
(redraw w)
|#

#|
;; Test pomocné třídy eu-stars (nezdokumentované):

(setf stars (make-instance 'eu-stars))
(setf w (make-instance 'window))
(set-shape w stars)
(redraw w)
(set-color stars :yellow)
(redraw w)
|#

#|
;; eu-flag:

(setf w (make-instance 'window))
(setf f (make-instance 'eu-flag))
(set-shape w f)
(redraw w)
(set-black-and-white-p f t)
(redraw w)
(set-black-and-white-p f nil)
(redraw w)
|#
