;;;; Neuron
;;;; Fires when weighted sum of inputs meets an activation threshold.
;;;; Lets program an AND neuron.
;;;;
;;;;
;;;; WOOOHOOOO, FIRST WORKING PERCEPTRON MODEL THAT I BUILT!!!!!
;;;;
;;;; Now how can I streamline this into a library?
;;;; I want to be able to stick perceptrons together like legos.
;;;; As evidenced by the second part of this, things get too messy when
;;;; building these things by hand.
;;;;
;;;; Process signals between 23.8 to 24GHz range?

(setf w1 (- (random 2.0) 1))
(setf w2 (- (random 2.0) 1))
(setf wb (- (random 2.0) 1))
(setf bias 1)

(defun activate (sum)
  (if (> sum 0)
      1
      -1))

(defun sum (x y)
  (+ (* x w1)
     (* y w2)
     (* bias wb)))

;; Y = X for now, but this lets us have more advanced functions later
(defun y-line (x)
  (* x x)) ; It's a parabola now. Can it still figure it out?

(defun expected (x y)
  "Is the point (x,y) above or below the line y = x?"
  (let ((y-line (y-line x))) ; The resultand y-value. If y is above this, the point is above the line.
  (cond
    ((eq y-line y) 0)  ; (x y) is on the line
    ((> y y-line) -1)  ; (x y) is below the line
    ((< y y-line) 1)))) ; (x y) is above the line

(defun error? (x y guessed)
  (declare (number x y guessed))
  (let ((expected (expected x y)))
    (- expected guessed)))

(defun delta-weight (input weight error &optional (lc 0.1))
  (+ weight (* input error lc)))

;; Perform a test, get a result (probably wrong).
;; Get a delta-weight evaluation given the input and that result.
;; Shift the weights by delta-weight.
(defun test (&key (mutate nil) (verbose nil))
  (let* ((px (1- (random 2.0)))
         (py (1- (random 2.0)))
         (exp (expected px py))
         (result (activate (sum px py)))
         (err (error? px py result))
         (delta-x (delta-weight px w1 err))
         (delta-y (delta-weight py w2 err)))
    (when verbose
      (format t "Using point (~a, ~a)~%" px py)
      (format t "Expected val: ~a~%" exp)
      (format t "Given val: ~a~%" result)
      (format t "Error val: ~a~%" err)
      (format t "Current X: ~a. Change X to: ~a~%" w1 delta-x)
      (format t "Current Y: ~a. Change Y to: ~a~%" w2 delta-y))
    (when mutate
      (setf w1 delta-x)
      (setf w2 delta-y))
    (eq exp result)))

(defun evaluate-correctness (&optional (n 1000))
  (let* ((results
          (loop for i from 0 to n
             collect (test :mutate nil :verbose nil)))
         (correct (count t results))
         (incorrect (count nil results)))
    ;(format t "Correct ~a times out of ~a attempts~%Ratio: ~a" correct n (+ 0.0 (/ correct n)))
    (+ 0.0 (/ correct n))))

(defun train (&optional (n 1000))
  (let ((initial (+ 0.0 (evaluate-correctness 1000))))
    (loop for i from 0 to n
       do (test :mutate t))
    (let ((end (+ 0.0 (evaluate-correctness 1000))))
      (format t "Initial: ~a~%Result: ~a~%Improvement: ~a" initial end (+ 0.0 (- end initial))))))
 


;;;-----------------------------------------------
;;; Extending to multiple perceptrons.
;;; Frankenstein'd a XOR from an AND and OR perceptron.
;;; 

;; Weight for first neuron, AND operator
(defvar wx-a (- (random 2.0) 1))
(defvar wy-a (- (random 2.0) 1))

;; Weight for second neuron, OR operator
;; Becomes obvious why we use a matrix for this.
(defvar wy-o (- (random 2.0) 1))
(defvar wx-o (- (random 2.0) 1))

;;; Why I'm writing this:
;;; Troves of Machine Learning libraries already exist that create easy-to-use
;;; abstractions for very powerful ideas. However these do not further our
;;; understanding of the underlying models in these abstractions. They
;;; leverage someone else's understanding. Nothing wrong with that, just my
;;; interests are different.
;;; I wish to learn how to design and implement my own machine learning models.
;;; This project can be considered a tool to do that. Or in other words, a
;;; "library to construct other machine learning libraries".
;;; 
;;; Things Every Perceptron Needs:
;;; 1) Activation function. What conditions to fire under?
;;;    Would make sense for this to be a lambda input.
;;; 2) Function to the return the "Expected" value.
;;;    A more involved lambda that may chack a file or such.
;;;    Or in simpler cases, just a binary or mathematical function.
;;; 3.1) Inputs
;;; 3.2) Weights
;;; 3.3) Biases
;;; 3.4) Outputs
;;; Conceptualize the inputs and outputs similarly to streams. We tell the node
;;; where it will find its inputs. We also tell it where it will send its
;;; outputs.
;;;
;;; Experiment: GENERATE the NAND representation of complex neural nets. XOR?
;;;             Could a genetic algorithm do this?

(defun sigmoid (x)
  (/ 1 (+ 1 (exp (- x)))))



(defclass perceptron ()
  (()))

(defclass input-stream ()
  ((inputs :initarg :inputs
           :initform nil
           :reader inputs)
  (len :initarg :len
       :initform 0
       :accessor len)))
          
(make-instance 'perceptron
               :lc 0.1 :weights #'(lambda () (n-randoms 
               :activation #'activate :expected #'expected
               :inputs #'input-stream :outputs #'output-stream)
;; Input stream an object type that also contains a count of how many elements
;; it will give you?
                                          ))

(defun n-randoms (n &optional (limit 2) (inc 0))
  (loop for i from 1 to n
       collect (+ (random limit) inc)))



(defun and-fn (x y)
  (logand x y))

(defun or-fn (x y)
  (or x y))

(defun nand (x y)
  (if (zerop (and-fn x y))
      1
      0))

(defun xor (x y)
  (nand (or-fn x y)
        (and-fn x y)))

(defun sum (x y w1 w2)
  (+ (* x w1)
     (* y w2)))

(defun activate (sum)
  (if (> sum 0.5)
      1
      0 ))

(defun expected (x y fn)
  (cond
    ((eq fn 'AND) (and-fn x y))
    ((eq fn 'NAND) (nand x y))
    ((eq fn 'OR) (or-fn x y))
    ((eq fn 'XOR) (xor x y))))


(defun error? (x y guessed fn)
  (declare (number x y guessed))
  (let ((expected (expected x y fn)))
    (- expected guessed)))

(defun delta-weight (input weight error &optional (lc .1))
  (+ weight (* input error lc)))

(defun input (x y w1 w2)
  (activate (sum x y w1 w2)))

(defun test (fn &key (mutate nil) (verbose nil))
  (let* ((px (random 2))
         (py (random 2))
         (exp (expected px py fn))
         (result (activate (sum px py wx-a wy-a)))
         (err (error? px py result fn))
         (delta-x (delta-weight px wx-a err))
         (delta-y (delta-weight py wy-a err)))
    (when verbose
      (format t "Using point (~a, ~a)~%" px py)
      (format t "Expected val: ~a~%" exp)
      (format t "Given val: ~a~%" result)
      (format t "Error val: ~a~%" err)
      (format t "Current X: ~a. Change X to: ~a~%" wx-a delta-x)
      (format t "Current Y: ~a. Change Y to: ~a~%" wy-a delta-y))
    (when mutate
      (setf wx-a delta-x)
      (setf wy-a delta-y))
    (eq exp result))))

(defun test-or (&key (mutate nil) (verbose nil))
  (let* ((px (random 2))
         (py (random 2))
         (exp (expected px py 'OR))
         (result (activate (sum px py wx-o wy-o)))
         (err (error? px py result 'OR))
         (delta-x (delta-weight px wx-o err))
         (delta-y (delta-weight py wy-o err)))
    (when verbose
      (format t "Using point (~a, ~a)~%" px py)
      (format t "Expected val: ~a~%" exp)
      (format t "Given val: ~a~%" result)
      (format t "Error val: ~a~%" err)
      (format t "Current X: ~a. Change X to: ~a~%" wx-o delta-x)
      (format t "Current Y: ~a. Change Y to: ~a~%" wy-o delta-y))
    (when mutate
      (setf wx-o delta-x)
      (setf wy-o delta-y))
    (eq exp result))))

(defun evaluate-correctness (&optional (n 1000))
  (let* ((results
          (loop for i from 0 to n
             collect (test fn :mutate nil :verbose nil)))
         (correct (count t results))
         (incorrect (count nil results)))
    (+ 0.0 (/ correct n))))



(defun train (&optional (n 1000))
  (let ((initial (+ 0.0 (evaluate-correctness 1000))))
    (loop for i from 0 to n
       do (test :mutate t))
    (let ((end (+ 0.0 (evaluate-correctness 1000))))
      (format t "Initial: ~a~%Result: ~a~%Improvement: ~a" initial end (+ 0.0 (- end initial))))))


