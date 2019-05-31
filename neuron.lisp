;;;; Neuron
;;;; Fires when weighted sum of inputs meets an activation threshold.
;;;; Lets program an AND neuron.
;;;;
;;;;
;;;; WOOOHOOOO, FIRST WORKING PERCEPTRON MODEL THAT I BUILT!!!!!
;;;;
;;;;
;;;;

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
  x)

(defun expected (x y)
  "Is the point (x,y) above or below the line y = x?"
  (let ((y-line (y-line x))) ; The resultand y-value. If y is above this, the point is above the line.
  (cond
    ((eq y-line y) 0)  ; (x y) is on the line
    ((> y y-line) -1)  ; (x y) is below the line
    ((< y y-line) 1)))) ; (x y) is above the line

(defun error? (x y guessed)
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
    (/ correct n)))

(defun train (&optional (n 1000))
  (let ((initial (+ 0.0 (evaluate-correctness 1000))))
    (loop for i from 0 to n
       do (test :mutate t))
    (let ((end (+ 0.0 (evaluate-correctness 1000))))
      (format t "Initial: ~a~%Result: ~a~%Improvement: ~a" initial end (+ 0.0 (- end initial))))))
 
