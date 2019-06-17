;;;-----------------------------------------------------------------------------
;;; Vector Utilities
;;; Not exactly high-performance stuff. Would suggest a switch to BLAS/LAPACK
;;; bindings in the future as I begin to require more power out of this thing.
;;;
;;; This works for prototyping purposes though. It's kind of fun too.
;;;-----------------------------------------------------------------------------
;; Disassembly Sizes:
;; 589 bytes
;; 648 bytes
;; 652 bytes
;; 232 bytes
;; Only decreased when compiler was declaimed. Otherwise more declarations just
;; added more type-checking
(defun vectorize! (fn matrix)
  (let* ((dim (array-dimensions matrix))
         (rows (car dim))
         (cols (cadr dim)))
    (declare (function fn)
             (type (simple-array (single-float) (* *)) matrix)
             (fixnum rows)
             (fixnum cols))
    (loop for x below rows
       do (loop for y below cols
             do (let ((ref (aref matrix x y)))
                  (setf ref (funcall fn ref)))))))


(defun vectorize (fn matrix)
  (let* ((dim (array-dimensions matrix))
         (rows (car dim))
         (cols (cadr dim))
         (ref (make-array dim :element-type 'single-float :adjustable nil)))
    (declare (function fn)
             (type (simple-array (single-float) (* *)) matrix ref)
             (fixnum rows)
             (fixnum cols))
    (loop for x below rows
       do (loop for y below cols
             do (setf (aref ref x y) (funcall fn (aref matrix x y)))))
    ref))

;; No need to extend to N matrices yet. Only adding two things at once.
;; Doesn't mutate out arrays anymore though, so that's pretty neat.
(defun m+ (m1 m2)
  "Add two 2D arrays m1 and m2"
  (if (not (equal (array-dimensions m1) (array-dimensions m2)))
      (error "Array dimensions not equal")
      (let* ((dim (array-dimensions m1))
             (rows (car dim))
             (cols (cadr dim))
             (ref (make-array dim :element-type 'single-float :adjustable nil)))
        (loop for x below rows
           do (loop for y below cols
                 do (setf (aref ref x y) (+ (aref m1 x y)
                                            (aref m2 x y)))))
        ref)))

(defun <*> (m1 m2)
  "Multiply two 2D arrays m1 and m2. Haddamard multiply."
  (if (not (equal (array-dimensions m1) (array-dimensions m2)))
      (error "Array dimensions not equal")
      (let* ((dim (array-dimensions m1))
             (rows (car dim))
             (cols (cadr dim))
             (ref (make-array dim :element-type 'single-float :adjustable nil)))
        (loop for x below rows
           do (loop for y below cols
                 do (setf (aref ref x y) (* (aref m1 x y)
                                            (aref m2 x y)))))
        ref)))

      

(defun m* (m1 m2)
  "Get the dot products of each row of m1, and each column of m2"
  (let* ((rows (car (array-dimensions m1)))
         (cols (cadr (array-dimensions m2)))
         (ref (make-array (list rows cols))))
    (loop for x below rows
       do (loop for y below cols
             do (setf (aref ref x y) (dot (row m1 x) (col m2 y)))))
    ref))

(defun row (matrix x)
  (make-array (cadr (array-dimensions matrix))
              :initial-contents
              (loop for i below (cadr (array-dimensions matrix))
                 collect (aref matrix x i))))

(defun col (matrix y)
  (make-array (car (array-dimensions matrix))
              :initial-contents
              (loop for i below (car (array-dimensions matrix))
                 collect (aref matrix i y))))

(defun dot (v1 v2)
  (reduce #'+ (map 'vector #'* v1 v2)))


(defun transpose (matrix)
  (let ((rows (car (array-dimensions matrix)))
        (cols (cadr (array-dimensions matrix))))
    (make-array (list cols rows)
                :initial-contents
                (loop for x from 0 to (1- cols)
                   collect (loop for y from 0 to (1- rows)
                              collect (aref matrix y x))))))
((x1 x2 x3)  ((z1)   ((x o z)
 (y1 y2 y3))  (z2) =  (y o z))
              (z3))
;;;-----------------------------------------------------------------------------
;;;Most of this crap has to do with networks
;;;-----------------------------------------------------------------------------


(defun sigmoid (x)
  (declare (number x))
  (/ 1.0 (+ 1 (exp (- x)))))

(defun sigmoid-prime (x)
  (declare (number x))
  (* (sigmoid x) (- 1 (sigmoid x))))


(defun network (sizes)
  "Basically creatse a network of randomly distributed weights. The SIZES input of '(2 3 1) signifies 2 input nodes, 3 hidden nodes, and 1 output node. This extends to additional hidden nodes if needed."
  (loop for i below (1- (length sizes))
     collect (make-array (list (nth i sizes) (nth (1+ i) sizes))
                         :element-type 'single-float
                         :initial-contents  (rand-dist (nth i sizes)
                                                       (nth (1+ i) sizes)))))
;; Given '(2 3 1), return ((x1 x2 x3) (y1))
(defun biases (sizes)
  "Gathers an array to be applied to the result of the previous outputs. Each node has one bias, so the arrays repeat per node (easier to add matrices that way). Biases are not set for input layers: they are only ever applied to outputs from later layers."
  (loop for i below (- (length sizes) 1)
     collect (make-array (nth (1+ i) sizes)
                         :element-type 'single-float
                         :initial-contents (car (rand-dist 1 (nth (1+ i) sizes))))))

(defun weight-aref (layer from to table)
  "Bear in mind this is backwards from most linear algebra notation"
  (aref (nth layer table) from to))

(defun feed-forward (activation-inputs weights biases &optional (fn #'sigmoid))
  "Multiply the previous activation matrix by the next set of weights, and their biases.
Then pass this value through an activation function (sigmoid, probably)"
  (vectorize fn (m+ (m* activation-inputs weights) biases)))
  
(defun forward-propogate (sizes inputs)
  (let ((weights (network sizes))
        (biases '(#2A((-0.4479947 0.6072321 0.5293317)) #2A((-0.6961334))))
        (activations inputs))
    (loop for i below (1- (length sizes))
         ;;; ???
       collect (let ((res (feed-forward activations (nth i weights) (nth i biases))))
                 (setf activations res)
                 res))))
(defun rand-dist (x y)
  (loop for i below x
     collect (loop for j below y
                  collect (1- (random 2.0)))))

(defun n-lists (n list)
  (loop for i below n
       collect list))

;;; Across biases and weights, multiply weight by activation and add bias.
;;; Go to next matrix and do the same thing.

   o
o  
   o  o
o  
   o
[[1 2 3]         [[1]
 [4 5 6]]         [2]
                  [3]]

;;; 1) Produce a set of activation inputs.
;;; 2) Pass them through w*a + b.
;;; 3) Use the result as your new set of activation inputs.
;;; 4) Repeat for the next layer.
