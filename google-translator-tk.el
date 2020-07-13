;;; .local/straight/repos/google-translator.el/google-translator-tk.el -*- lexical-binding: t; -*-

(defconst google-translator-bit-v-len 32)

(defun google-translator-gen-tk (text &optional b-d1)
  (setq b-d1 (or b-d1 (google-translator-get-b-d1)))
  (let* ((b (cl-first b-d1))
         (d1 (cl-second b-d1))
         (ub "+-3^+b+-f")
         (vb "+-a^+6")
         (a (cl-reduce (lambda (a e) (google-translator-gen-rl (+ a e) vb))
                       (encode-coding-string text 'utf-8) :initial-value b)))
    (setq a (google-translator-gen-rl a ub))
    (setq a (google-translator-logxor a d1))
    (when (< a 0) ;; (abs a) + 2^31
      (setq a (+ (google-translator-logand a 2147483647.0) 2147483648.0)))
    (setq a (ffloor (mod a 1e6)))
    (format "%s.%s"
            (car (split-string (number-to-string a) "\\."))
            (car (split-string (number-to-string (google-translator-logxor a b)) "\\.")))))

(defun google-translator-get-b-d1 ()
  ;; TKK='427110.1469889687'
  (list 427110 1469889687))

(defun google-translator-gen-rl (a b)
  (cl-loop for c from 0 below (- (length b) 2) by 3
           for d = (aref b (+ c 2)) do
           (setq d (if (>= d ?a) (- d 87) (- d ?0)))
           (setq d (if (= (aref b (1+ c)) ?+)
                       (google-translator-lsh a (- d))
                     (google-translator-lsh a d)))
           (setq a (if (= (aref b c) ?+)
                       (google-translator-logand (+ a d) 4294967295.0)
                     (google-translator-logxor a d))))
  a)

(defun google-translator-lsh (n d)
  "Return a floating-point number.
Shift the bits in N to the left or rihgt D places.
D is an integer."
  (let ((v (google-translator-number-to-bit-v n))
        (v-result (make-vector google-translator-bit-v-len 0)))
    (if (< d 0) ;; Shift Right Logical
        ;; [x0 x1 ... xn-d ... xn] => [0 ... 0 x0 x1 ... xn-d]
        (cl-loop for i from (abs d) below google-translator-bit-v-len
                 for j from 0 do
                 (aset v-result i (aref v j)))
      ;; Shift Left Logical
      ;; [x0 x1 ... xd ... xn] => [xd ... xn 0 ... 0]
      (cl-loop for i from d below google-translator-bit-v-len
               for j from 0 do
               (aset v-result j (aref v i))))
    (google-translator-bit-v-to-number v-result)))

(defun google-translator-number-to-bit-v (n)
  "Return a bit vector from N."
  (if (< n 0) (google-translator-bit-v-2comp
               (google-translator-number-to-bit-v (abs n)))
    (let ((v (make-vector google-translator-bit-v-len 0)))
      (cl-loop for i downfrom (1- google-translator-bit-v-len) to 0
               with q
               when (< n 1) return nil do
               (setq q (ffloor (* n 0.5)))
               (aset v i (floor (- n (* 2.0 q))))
               (setq n q))
      v)))

(defun google-translator-bit-v-to-number (v)
  "Return a floating-point number from V."
  (if (and (> (aref v 0) 0)
           ;; Exclude [1 0 ... 0]
           (cl-loop for i from 1 below google-translator-bit-v-len
                    thereis (> (aref v i) 0)))
      (- (google-translator-bit-v-to-number (google-translator-bit-v-2comp v)))
    (funcall (if (> (aref v 0) 0)  #'- #'+)
             (cl-reduce (lambda (acc e) (+ (* acc 2.0) e))
                        v :initial-value 0.0))))

(defun google-translator-logand (n1 n2)
  "Return a floating-point number from N1 and N2."
  (google-translator-logfn #'logand n1 n2))

(defun google-translator-logfn (fn n1 n2)
  "Helper function for logical FN."
  (let ((v1 (google-translator-number-to-bit-v n1))
        (v2 (google-translator-number-to-bit-v n2))
        (v (make-vector google-translator-bit-v-len 0)))
    (cl-loop for i from 0 below google-translator-bit-v-len do
             (aset v i (funcall fn (aref v1 i) (aref v2 i))))
    (google-translator-bit-v-to-number v)))

(defun google-translator-logxor (n1 n2)
  "Return a floating-point number from N1 and N2."
  (google-translator-logfn #'logxor n1 n2))

(defun google-translator-bit-v-2comp (v)
  "Return the two's complement of V."
  (let* ((vc (vconcat v))
         (len (length vc)))
    ;; Complement of v
    (cl-loop for i from 0 below len do
             (aset vc i (logxor (aref vc i) 1)))
    ;; vc = complement of v + 1
    (cl-loop for i downfrom (1- len) to 0
             do (aset vc i (logxor (aref vc i) 1))
             when (> (aref vc i) 0) return nil)
    vc))

(provide 'google-translator-tk)
