;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Zdrojový soubor k učebnímu textu M. Krupka: Objektové programování
;;;;
;;;; Kapitola 5, Dědičnost
;;;;

#| 
Před načtením souboru načtěte knihovnu micro-graphics
Pokud při načítání (kompilaci) dojde k chybě 
"Reader cannot find package MG",
znamená to, že knihovna micro-graphics není načtená.
|#

;;;
;;; Třída shape
;;;

(defclass shape ()
  ((color :initform :black)
   (thickness :initform 1)
   (filledp :initform nil)
   (window :initform nil)))

(defmethod window ((shape shape)) 
  (slot-value shape 'window))

(defmethod set-window ((shape shape) value) 
  (setf (slot-value shape 'window) value)
  shape)

(defmethod shape-mg-window ((shape shape))
  (when (window shape)
    (mg-window (window shape))))

(defmethod color ((shape shape)) 
  (slot-value shape 'color))

(defmethod set-color ((shape shape) value) 
  (setf (slot-value shape 'color) value)
  shape)

(defmethod thickness ((shape shape)) 
  (slot-value shape 'thickness)) 

(defmethod set-thickness ((shape shape) value) 
  (setf (slot-value shape 'thickness) value)
  shape) 

(defmethod filledp ((shape shape))
  (slot-value shape 'filledp))

(defmethod set-filledp ((shape shape) value)
  (setf (slot-value shape 'filledp) value)
  shape)

(defmethod move ((shape shape) dx dy)
  shape)

(defmethod rotate ((shape shape) angle center)
  shape)

(defmethod scale ((shape shape) coeff center)
  shape)

(defmethod set-mg-params ((shape shape)) 
  (let ((mgw (shape-mg-window shape)))
    (mg:set-param mgw :foreground (color shape)) 
    (mg:set-param mgw :filledp (filledp shape))
    (mg:set-param mgw :thickness (thickness shape)))
  shape)

(defmethod do-draw ((shape shape)) 
  shape)

(defmethod draw ((shape shape))
  (set-mg-params shape)
  (do-draw shape))


;;;
;;; Třída point
;;;

(defclass point (shape) 
  ((x :initform 0) 
   (y :initform 0)))

(defmethod x ((point point))
  (slot-value point 'x))

(defmethod y ((point point))
  (slot-value point 'y))

(defmethod set-x ((point point) value)
  (unless (typep value 'number)
    (error "x coordinate of a point should be a number"))
  (setf (slot-value point 'x) value)
  point)

(defmethod set-y ((point point) value)
  (unless (typep value 'number)
    (error "y coordinate of a point should be a number"))
  (setf (slot-value point 'y) value)
  point)

(defmethod r ((point point)) 
  (let ((x (slot-value point 'x)) 
        (y (slot-value point 'y))) 
    (sqrt (+ (* x x) (* y y)))))

(defmethod phi ((point point)) 
  (let ((x (slot-value point 'x)) 
        (y (slot-value point 'y))) 
    (cond ((> x 0) (atan (/ y x))) 
          ((< x 0) (+ pi (atan (/ y x)))) 
          (t (* (signum y) (/ pi 2))))))

(defmethod set-r-phi ((point point) r phi) 
  (set-x point (* r (cos phi)))
  (set-y point (* r (sin phi)))
  point)

(defmethod set-r ((point point) value) 
  (set-r-phi point value (phi point)))

(defmethod set-phi ((point point) value) 
  (set-r-phi point (r point) value))

(defmethod set-mg-params ((pt point))
  (call-next-method)
  (mg:set-param (shape-mg-window pt) :filledp t)
  pt)

(defmethod do-draw ((pt point)) 
  (mg:draw-circle (shape-mg-window pt) 
                  (x pt) 
                  (y pt) 
                  (thickness pt))
  pt)

(defmethod move ((pt point) dx dy)
  (set-x pt (+ (x pt) dx))
  (set-y pt (+ (y pt) dy))
  pt)

(defmethod rotate ((pt point) angle center)
  (let ((cx (x center))
        (cy (y center)))
    (move pt (- cx) (- cy))
    (set-phi pt (+ (phi pt) angle))
    (move pt cx cy)
    pt))

(defmethod scale ((pt point) coeff center)
  (let ((cx (x center))
        (cy (y center)))
    (move pt (- cx) (- cy))
    (set-r pt (* (r pt) coeff))
    (move pt cx cy)
    pt))


;;;
;;; Třída circle
;;;

(defclass circle (shape) 
  ((center :initform (make-instance 'point)) 
   (radius :initform 1)))

(defmethod radius ((c circle))
  (slot-value c 'radius))

(defmethod set-radius ((c circle) value)
  (when (< value 0)
    (error "Circle radius should be a non-negative number"))
  (setf (slot-value c 'radius) value)
  c)

(defmethod center ((c circle))
  (slot-value c 'center))

(defmethod do-draw ((c circle))
  (mg:draw-circle (shape-mg-window c)
                  (x (center c))
                  (y (center c))
                  (radius c))
  c)

(defmethod move ((c circle) dx dy)
  (move (center c) dx dy)
  c)

(defmethod rotate ((c circle) angle center)
  (rotate (center c) angle center)
  c)

(defmethod scale ((c circle) coeff center)
  (scale (center c) coeff center)
  (set-radius c (* (radius c) coeff))
  c)


;;;
;;; Třída compound-shape
;;;

(defclass compound-shape (shape)
  ((items :initform '())))

(defmethod items ((shape compound-shape)) 
  (copy-list (slot-value shape 'items)))

(defmethod send-to-items ((shape compound-shape) 
			  message
			  &rest arguments)
  (dolist (item (items shape))
    (apply message item arguments))
  shape)

(defmethod check-item ((shape compound-shape) item)
  (error "Method should be rewritten."))

(defmethod check-items ((shape compound-shape) item-list)
  (dolist (item item-list)
    (check-item shape item))
  shape)

(defmethod set-items ((shape compound-shape) value)
  (check-items shape value)
  (setf (slot-value shape 'items) (copy-list value))
  shape)

(defmethod move ((shape compound-shape) dx dy)
  (send-to-items shape 'move dx dy)
  shape)

(defmethod rotate ((shape compound-shape) angle center)
  (send-to-items shape 'rotate angle center)
  shape)

(defmethod scale ((shape compound-shape) coeff center)
  (send-to-items shape 'scale coeff center)
  shape)


;;;
;;; Třída picture
;;;

(defclass picture (compound-shape)
  ())

(defmethod check-item ((pic picture) item)
  (unless (typep item 'shape)
    (error "Invalid picture element type."))
  pic)

(defmethod set-items ((pic picture) items)
  (call-next-method)
  (send-to-items pic 'set-window (window pic)))

(defmethod draw ((pic picture))
  (dolist (item (reverse (items pic)))
    (draw item))
  pic)

(defmethod set-window ((shape picture) value)
  (send-to-items shape 'set-window value)
  (call-next-method))


;;;
;;; Třída polygon
;;;

(defclass polygon (compound-shape)
  ((closedp :initform t)))

(defmethod check-item ((poly polygon) item)
  (unless (typep item 'point) 
    (error "Items of polygon should be points."))
  poly)

(defmethod closedp ((p polygon))
  (slot-value p 'closedp))

(defmethod set-closedp ((p polygon) value)
  (setf (slot-value p 'closedp) value)
  p)

(defmethod set-mg-params ((poly polygon)) 
  (call-next-method)
  (mg:set-param (shape-mg-window poly) 
                :closedp
                (closedp poly))
  poly)

(defmethod do-draw ((poly polygon)) 
  (let (coordinates)
    (dolist (point (reverse (items poly)))
      (setf coordinates (cons (y point) coordinates)
            coordinates (cons (x point) coordinates)))
    (mg:draw-polygon (shape-mg-window poly) 
                     coordinates))
  poly)


;;;
;;; Třída window
;;;

(defclass window ()
  ((mg-window :initform (mg:display-window))
   (shape :initform nil)
   (background :initform :white)))

(defmethod mg-window ((window window))
  (slot-value window 'mg-window))

(defmethod shape ((w window))
  (slot-value w 'shape))

(defmethod set-shape ((w window) shape)
  (when shape
    (set-window shape w))
  (setf (slot-value w 'shape) shape)
  w)

(defmethod background ((w window))
  (slot-value w 'background))

(defmethod set-background ((w window) color)
  (setf (slot-value w 'background) color)
  w)

(defmethod redraw ((window window))
  (let ((mgw (mg-window window)))
    (mg:set-param mgw :background (background window))
    (mg:clear mgw)
    (when (shape window)
      (draw (shape window))))
  window)
