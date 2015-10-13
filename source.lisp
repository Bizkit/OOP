;;; kod z minule prednasky

;; class POINT

(defclass point ()
  ((x :initform 0)
   (y :initform 0)))

;reading polar coordinates

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

; setting polar coordinates

(defmethod set-r-phi ((point point) r phi)
  (setf (slot-value point 'x) (* r (cos phi))
        (slot-value point 'y) (* r (sin phi)))
  point)

(defmethod set-r ((point point) r)
  (set-r-phi point r (phi point)))

(defmethod set-phi ((point point) phi)
  (set-r-phi point (r point) phi))


;; class CIRCLE

(defclass circle ()
  ((center :initform (make-instance 'point))
   (radius :initform 1)))


;;; nove - motivace k zapouzdreni
 
;; 1

(defvar *point* (make-instance 'point))
(setf (slot-value *point* 'x) 10)
(setf (slot-value *point* 'y) nil)
(r *point*)

(setf (slot-value *point* 'y) 0)

(set-phi *point* nil)

;; 2

(defvar *circle* (make-instance 'circle))
(setf (slot-value *circle* 'radius) -1)

;; 3 

(setf (slot-value *point* 'x) 3)
(setf (slot-value *point* 'y) 4)

(set-r-phi *point* 5 (/ pi 2))

;; 4 - zmena vnitrni reprezentace

(defclass point ()
  ((r :initform 0)
   (phi :initform 0)))



;;; prepracovane tridy

;; class POINT

(defclass point ()
  ((x :initform 0)
   (y :initform 0)))

; reading and setting slot values

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

; polar coordinates - no changes

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
  (setf (slot-value point 'x) (* r (cos phi))
        (slot-value point 'y) (* r (sin phi)))
  point)

(defmethod set-r ((point point) r)
  (set-r-phi point r (phi point)))

(defmethod set-phi ((point point) phi)
  (set-r-phi point (r point) phi))



;; class CIRCLE

(defclass circle ()
  ((center :initform (make-instance 'point))
   (radius :initform 1)))

; reading and setting slot values

(defmethod radius ((c circle))
  (slot-value c 'radius))

(defmethod set-radius ((c circle) value)
  (when (< value 0)
    (error "Circle radius should be a non-negative number"))
  (setf (slot-value c 'radius) value)
  c)

(defmethod center ((c circle))
  (slot-value c 'center))



;; priklad nastaveni stredu

(defvar *circ* (make-instance 'circle))
(set-x (center *circ*) 10)


;; class PICTURE

(defclass picture ()
  ((items :initform '())))

(defmethod items ((pic picture))
  (slot-value pic 'items))

(defmethod set-items ((pic picture) value)
  (unless (every (lambda (elem)
                   (or (typep elem 'point)
                       (typep elem 'circle)
                       (typep elem 'picture)))
                 value)
    (error "Picture elements are not of the desired type."))
  (setf (slot-value pic 'items) value)
  pic)



;; priklad - mozne problemy

(defvar *list* (list 0 0 0))
(defvar *pic* (make-instance 'picture))
(set-items *pic* *list*)

(setf *list* (list (make-instance 'point) (make-instance 'circle)))
(set-items *pic* *list*)
(items *pic*)

(setf (first *list*) 0)
(items *pic*)



;; reseni - uprava metod

(defmethod items ((pic picture))
  (copy-list (slot-value pic 'items)))

(defmethod set-items ((pic picture) value)
  (unless (every (lambda (elem)
                   (or (typep elem 'point)
                       (typep elem 'circle)
                       (typep elem 'picture)))
                 value)
    (error "Picture elements are not of desired type."))
  (setf (slot-value pic 'items) (copy-list value))
  pic)

;; test

(setf *list* (list (make-instance 'point) (make-instance 'circle)))
(set-items *pic* *list*)
(items *pic*)

(setf (first *list*) 0)
(items *pic*)
*list*



;; snadnejsi vytvareni a naplneni instanci

(defun make-point (x y)
  (set-x (set-y (make-instance 'point) y) x))

(make-point 4 3)

;; prehlednejsi varianta
(defun make-point (x y)
  (let ((point (make-instance 'point)))
    (set-x point x)
    (set-y point y)
    point))

(make-point 4 3)

;;--------------------------------------------;;
;;; ukazky pouziti knihovny micro-graphics

(defvar *win*) 
(setf *win* (mg:display-window))

(mg:get-param *win* :background)

(mapcar
 (lambda (p)
   (list p (mg:get-param *win* p)))
 '(:thickness :foreground :background :filledp :closedp))

(mg:set-param *win* :background :red)

(mg:clear *win*)

(mg:draw-circle *win* 50 50 30)

(mg:set-param *win* :thickness 10)
(mg:set-param *win* :foreground :yellow)
(mg:draw-circle *win* 100 50 20)

(mg:set-param *win* :filledp t)
(mg:set-param *win* :closedp t)
(mg:set-param *win* :foreground :lightblue)
(mg:draw-polygon *win* '(10 10 10 150 50 150))

;;; Objektova nastavba

;; auxiliary functions

(defun colorp (color)
  (if (find color (color:get-all-color-names)) t nil))

;(colorp :white)
;(colorp :whit)



;; class WINDOW

(defclass window ()
  ((mg-window :initform (mg:display-window))
   (shape :initform nil)
   (background :initform :white)))

; reading and setting slot values

(defmethod mg-window ((window window))
  (slot-value window 'mg-window))

(defmethod shape ((w window))
  (slot-value w 'shape))

(defmethod set-shape ((w window) shape)
  (unless (or (typep shape 'point)
              (typep shape 'circle)
              (typep shape 'picture)
              (typep shape 'polygon))
    (error "The value shape is not of desired type."))
  (set-window shape w)
  (setf (slot-value w 'shape) shape)
  w)

(defmethod background ((w window))
  (slot-value w 'background))

(defmethod set-background ((w window) color)
  (unless (colorp color)
    (error "The value color is not a correct representation of a color. "))
  (setf (slot-value w 'background) color)
  w)

; drawing methods

(defmethod redraw ((window window))
  (let ((mgw (slot-value window 'mg-window)))
    (mg:set-param mgw :background (background window))
    (mg:clear mgw)
    (when (shape window)
      (draw (shape window))))
  window)

;; class POINT

(defclass point () 
  ((x :initform 0) 
   (y :initform 0)
   (color :initform :black) 
   (thickness :initform 1) 
   (window :initform nil)))

; reading and setting slot values

(defmethod x ((point point))
  (slot-value point 'x))

(defmethod y ((point point))
  (slot-value point 'y))

(defmethod set-x ((point point) value)
  (unless (typep value 'number)
    (error "x coordinate of a point should be a number. "))
  (setf (slot-value point 'x) value)
  point)

(defmethod set-y ((point point) value)
  (unless (typep value 'number)
    (error "y coordinate of a point should be a number. "))
  (setf (slot-value point 'y) value)
  point)

; polar coordinates

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
  (setf (slot-value point 'x) (* r (cos phi))
        (slot-value point 'y) (* r (sin phi)))
  point)

(defmethod set-r ((point point) r)
  (set-r-phi point r (phi point)))

(defmethod set-phi ((point point) phi)
  (set-r-phi point (r point) phi))

; function for easy construction

(defun make-point (x y)
  (let ((point (make-instance 'point)))
    (set-x point x)
    (set-y point y)
    point))

; reading and setting drawing parametr slots

(defmethod color ((pt point)) 
  (slot-value pt 'color)) 

(defmethod set-color ((pt point) value) 
  (unless (colorp value)
    (error "The value is not a correct representation of a color. "))
  (setf (slot-value pt 'color) value)
  pt) 

(defmethod thickness ((pt point)) 
  (slot-value pt 'thickness)) 

(defmethod set-thickness ((pt point) value) 
  (unless (and (typep value 'integer) (< 0 value))
    (error "The value should be a positive integer. "))
  (setf (slot-value pt 'thickness) value)
  pt) 

(defmethod window ((pt point)) 
  (slot-value pt 'window))

(defmethod set-window ((pt point) value) 
  (unless (typep value 'window)
    (error "The value should be a window. "))
  (setf (slot-value pt 'window) value)
  pt)

(defmethod shape-mg-window ((pt point))
  (when (window pt)
    (mg-window (window pt))))

; drawing methods

(defmethod set-mg-params ((pt point)) 
  (let ((mgw (shape-mg-window pt)))
    (mg:set-param mgw :foreground (color pt)) 
    (mg:set-param mgw :filledp t))
  pt)

(defmethod do-draw ((pt point)) 
  (mg:draw-circle (shape-mg-window pt) 
                  (x pt) 
                  (y pt) 
                  (thickness pt))
  pt)

(defmethod draw ((pt point))
  (set-mg-params pt)
  (do-draw pt))



;; class CIRCLE

(defclass circle () 
  ((center :initform (make-instance 'point)) 
   (radius :initform 1)
   (color :initform :black)
   (thickness :initform 1)
   (filledp :initform nil)
   (window :initform nil)))

; reading and setting slot values

(defmethod radius ((c circle))
  (slot-value c 'radius))

(defmethod set-radius ((c circle) value)
  (when (< value 0)
    (error "Circle radius should be a non-negative number"))
  (setf (slot-value c 'radius) value)
  c)

(defmethod center ((c circle))
  (slot-value c 'center))

; reading and setting drawing parametr slots

(defmethod color ((c circle)) 
  (slot-value c 'color)) 

(defmethod set-color ((c circle) value) 
  (unless (colorp value)
    (error "The value is not a correct representation of a color. "))
  (setf (slot-value c 'color) value)
  c) 

(defmethod thickness ((c circle)) 
  (slot-value c 'thickness)) 

(defmethod set-thickness ((c circle) value) 
  (unless (and (typep value 'integer) (< 0 value))
    (error "The value should be a positive integer. "))
  (setf (slot-value c 'thickness) value)
  c) 

(defmethod filledp ((c circle))
  (slot-value c 'filledp))

(defmethod set-filledp ((c circle) value)
  (setf (slot-value c 'filledp) value)
  c)

(defmethod window ((c circle)) 
  (slot-value c 'window))

(defmethod set-window ((c circle) value) 
  (unless (typep value 'window)
    (error "The value should be a window. "))
  (setf (slot-value c 'window) value)
  c)

(defmethod shape-mg-window ((c circle))
  (when (window c)
    (mg-window (window c))))

; drawing methods 

(defmethod set-mg-params ((c circle))
  (let ((mgw (shape-mg-window c)))
    (mg:set-param mgw :foreground (color c))
    (mg:set-param mgw :thickness (thickness c))
    (mg:set-param mgw :filledp (filledp c)))
  c)

(defmethod do-draw ((c circle))
  (mg:draw-circle (shape-mg-window c)
                  (x (center c))
                  (y (center c))
                  (radius c))
  c)

(defmethod draw ((c circle))
  (set-mg-params c)
  (do-draw c))



;; class PICTURE

(defclass picture ()
  ((items :initform '())
   (window :initform nil)))

; reading and setting slot values

(defmethod items ((pic picture))
  (copy-list (slot-value pic 'items)))

(defmethod set-items ((pic picture) value)
  (unless (every (lambda (elem)
                   (or (typep elem 'point)
                       (typep elem 'circle)
                       (typep elem 'picture)
                       (typep elem 'polygon)))
                 value)
    (error "Picture elements are not of desired type."))
  (setf (slot-value pic 'items) (copy-list value))
  (when (window pic) (set-window pic (window pic)))
  pic)

; reading and setting drawing parametr slots

(defmethod window ((shape picture))
  (slot-value shape 'window))
 
(defmethod set-window ((shape picture) value)
  (unless (typep value 'window)
    (error "The value should be a window. "))
  (dolist (item (items shape))
    (set-window item value))
  (setf (slot-value shape 'window) value)
  shape)

(defmethod shape-mg-window ((shape picture))
  (when (window shape)
    (mg-window (window shape))))

; drawing method

(defmethod draw ((pic picture))
  (dolist (item (reverse (items pic)))
    (draw item))
  pic)

;; class POLYGON

(defclass polygon ()
  ((items :initform '())
   (color :initform :black)
   (thickness :initform 1)
   (filledp :initform nil)
   (closedp :initform t)
   (window :initform nil)))

; reading and setting slot values

(defmethod items ((poly polygon)) 
  (copy-list (slot-value poly 'items)))

(defmethod set-items ((poly polygon) value) 
  (unless (every (lambda (elem) 
                   (typep elem 'point)) 
                 value)
    (error "Items (value) should be a list of points. "))
  (setf (slot-value poly 'items) (copy-list value))
  poly)

; reading and setting drawing parametr slots

(defmethod color ((p polygon))
  (slot-value p 'color))

(defmethod set-color ((p polygon) value)
  (unless (colorp value)
    (error "The value is not a correct representation of a color. "))
  (setf (slot-value p 'color) value)
  p)

(defmethod thickness ((p polygon))
  (slot-value p 'thickness))

(defmethod set-thickness ((p polygon) value)
  (unless (and (typep value 'integer) (< 0 value))
    (error "The value should be a positive integer. "))
  (setf (slot-value p 'thickness) value)
  p)

(defmethod closedp ((p polygon))
  (slot-value p 'closedp))

(defmethod set-closedp ((p polygon) value)
  (setf (slot-value p 'closedp) value)
  p)

(defmethod filledp ((p polygon))
  (slot-value p 'filledp))

(defmethod set-filledp ((p polygon) value)
  (setf (slot-value p 'filledp) value)
  p)

(defmethod window ((p polygon))
  (slot-value p 'window))

(defmethod set-window ((p polygon) value)
  (unless (typep value 'window)
    (error "The value should be a window. "))
  (setf (slot-value p 'window) value)
  p)

(defmethod shape-mg-window ((shape polygon))
  (when (window shape)
    (mg-window (window shape))))

; drawing methods

(defmethod set-mg-params ((poly polygon)) 
  (let ((mgw (shape-mg-window poly)))
    (mg:set-param mgw :foreground (color poly)) 
    (mg:set-param mgw :thickness (thickness poly)) 
    (mg:set-param mgw :filledp (filledp poly))
    (mg:set-param mgw :closedp (closedp poly)))
  poly)

(defmethod do-draw ((poly polygon)) 
  (let (coordinates)
    (dolist (point (reverse (items poly)))
      (setf coordinates (cons (y point) coordinates))
      (setf coordinates (cons (x point) coordinates)))
    (mg:draw-polygon (shape-mg-window poly) 
                     coordinates))
  poly)

(defmethod draw ((poly polygon)) 
  (set-mg-params poly) 
  (do-draw poly))

;; Pozor - musi se upravit set-items u picture a set-shape u window.
;; Pridani moznosti (typep elem 'polygon)


;;; Transformations

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

(defmethod move ((pic picture) dx dy)
  (dolist (it (items pic))
    (move it dx dy))
  pic)

(defmethod rotate ((pic picture) angle center)
  (dolist (it (items pic))
    (rotate it angle center))
  pic)

(defmethod scale ((pic picture) coeff center)
  (dolist (item (items pic))
    (scale item coeff center))
  pic)

(defmethod move ((poly polygon) dx dy)
  (dolist (it (items poly))
    (move it dx dy))
  poly)

(defmethod rotate ((poly polygon) angle center)
  (dolist (it (items poly))
    (rotate it angle center))
  poly)

(defmethod scale ((poly polygon) coeff center)
  (dolist (item (items poly))
    (scale item coeff center))
  poly)



; TESTS

(setf *win* (make-instance 'window))
(set-background *win* :red)
(redraw *win*)

(defvar *circ*)
(setf *circ* (make-instance 'circle))
(set-x (center *circ*) 50)
(set-y (center *circ*) 50)
(set-radius *circ* 30)

(set-shape *win* *circ*)
(redraw *win*)

(set-thickness *circ* 10)
(set-color *circ* :yellow)
(redraw *win*)

(scale *circ* 2 (center *circ*))
(move *circ* 100 100)
(redraw *win*)

(defvar *pol*)
(setf *pol* (make-instance 'polygon))
(set-color *pol* :green)
(set-filledp *pol* t)
(set-closedp *pol* t)
(set-color *pol* :lightblue)
(set-items *pol* (list (make-point 10 10)
                      (make-point 10 150)
                      (make-point 150 150)))
(set-shape *win* *pol*)
(redraw *win*)

(defvar *pic*)
(setf *pic* (make-instance 'picture))
(set-items *pic* (list *circ* *pol*))
(set-shape *win* *pic*)
(redraw *win*)

;;-----------------------------------------;;


