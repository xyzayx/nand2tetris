
(defpackage :nand2tetris
  (:use :cl)
  (:shadow :not :and :or))

(in-package :nand2tetris)

;--------------------------------------------------------------------

(defmacro test (expr expected)
  `(block test
     (let ((want (multiple-value-list ,expected)))
       (handler-bind ((condition
                        (lambda (c)
                          (when (cl:and (symbolp (first want))
                                        (typep c (first want)))
                            (return-from test (type-of c))))))
         (let ((got (multiple-value-list ,expr)))
           (assert (equal got want))
           (values-list got))))))

;====================================================================

; Takes nand as primitive

(defun nand (a b)
  (check-type a bit)
  (check-type b bit)
  (if (cl:and (= a 1) (= b 1)) 0 1))

(test (nand 0 0) 1)
(test (nand 0 1) 1) 
(test (nand 1 0) 1)
(test (nand 1 1) 0)
(test (nand 0.2 1) 'error)
(test (nand 0 'x) 'error)
(test (nand 2 10) 'error)

;--------------------------------------------------------------------

(defun not (a) (nand a a))

(test (not 1) 0)
(test (not 0) 1)
(test (not 0.2) 'error)
(test (not 'x) 'error)

(defun and (a b) (not (nand a b)))

(test (and 0 1) 0)
(test (and 1 0) 0)
(test (and 0 0) 0)
(test (and 1 1) 1)

(defun or (a b) (nand (not a) (not b)))

(test (or 0 0) 0)
(test (or 1 0) 1)
(test (or 0 1) 1)
(test (or 1 1) 1)

(defun xor (a b) (and (nand a b) (or a b)))

(test (xor 0 0) 0)
(test (xor 1 0) 1)
(test (xor 0 1) 1)
(test (xor 1 1) 0)
(test (xor 0.2 1) 'error)

(defun mux (a b sel)
	(or (and a (not sel)) (and b sel) ))

(test (mux 0 0 0) 0)
(test (mux 0 1 0) 0)
(test (mux 1 0 0) 1)
(test (mux 1 1 0) 1)

(test (mux 0 0 1) 0)
(test (mux 0 1 1) 1)
(test (mux 1 0 1) 0)
(test (mux 1 1 1) 1)


(defun dmux (in sel)
  (values (and in (not sel)) (and in sel)))

(test (dmux 0 0) (values 0 0))
(test (dmux 1 0) (values 1 0))
(test (dmux 0 1) (values 0 0))
(test (dmux 1 1) (values 0 1))

;--------------------------------------------------------------------

(deftype bus16 () '(bit-vector 16))
(deftype bus8 () '(bit-vector 8))
(deftype bus3 () '(bit-vector 3))
(deftype bus2 () '(bit-vector 2))

;--------------------------------------------------------------------

(defun not16 (in16) 
  (check-type in16 bus16)
  (map 'bus16 #'not in16))

(test (not16 #*1000100010001000) #*0111011101110111)
(test (not16 #*0000000000000000) #*1111111111111111)
(test (not16 1) 'error)
(test (not16 #*001) 'error)
(test (not16 #*11110000111100001) 'error)

(defun and16 (a16 b16) 
  (check-type a16 bus16)
  (check-type b16 bus16)
  (map 'bus16 #'and a16 b16))

(test (and16 #*1000100010001000 #*0001000100010001) #*0000000000000000)
(test (and16 #*1000100010001000 #*1111111111111111) #*1000100010001000)
(test (and16 #*1111111111111111 #*1111111111111111) #*1111111111111111)
(test (and16 #*0000 #*1111) 'error)
(test (and16 #*1000100010001000 #*000100010001000101) 'error)

(defun or16 (a16 b16) 
  (check-type a16 bus16)
  (check-type b16 bus16)
  (map 'bus16 #'or a16 b16))

(test (or16 #*1000100010001000 #*0001000100010001) #*1001100110011001)
(test (or16 #*1000100010001000 #*1111111111111111) #*1111111111111111)
(test (or16 #*1111111111111111 #*1111111111111111) #*1111111111111111)
(test (or16 #*0000 #*1111) 'error)

(defun mux16 (a16 b16 sel)
  (check-type a16 bus16)
  (check-type b16 bus16)
  (check-type sel bit)
  (map 'bus16 (lambda (a b) (mux a b sel)) a16 b16))

(test (mux16 #*1000100010001000 #*0001000100010001 0) #*1000100010001000)
(test (mux16 #*1000100010001000 #*1111111111111111 1) #*1111111111111111)
(test (mux16 #*1000100010001000 #*1111111111111111 2) 'error)
(test (mux16 #*1000100010001000 #*111111111111111 0) 'error)

(defun dmux16 (in16 sel)
  (check-type in16 bus16)
  (check-type sel bit)
  (values (mux16 in16 #*0000000000000000 sel)
          (mux16 #*0000000000000000 in16 sel)))

(test (dmux16 #*1000100010001000 0) 
      (values #*1000100010001000 #*0000000000000000))
(test (dmux16 #*1000100010001000 1) 
      (values #*0000000000000000 #*1000100010001000))
(test (dmux16 #*1111111111111111 0) 
      (values #*1111111111111111 #*0000000000000000))
(test (dmux16 #*1111111111111111 1) 
      (values #*0000000000000000 #*1111111111111111))
(test (dmux16 #*0000000000000000 1) 
      (values #*0000000000000000 #*0000000000000000))
(test (dmux16 #*1000100010001000 2) 'error)
(test (dmux16 #*100010001000100 0) 'error)

;--------------------------------------------------------------------

#|
(defun or8way (in8)
  (check-type in8 bus8)
  (or (bit in8 7)
    (or (bit in8 6)
      (or (bit in8 5)
        (or (bit in8 4)
          (or (bit in8 3)
            (or (bit in8 2) 
              (or (bit in8 0) (bit in8 1)))))))))
|#

(defun or8way (in8)
  (check-type in8 bus8)
  ; cascading 7 or-gates in a binary tree is boilerplate
  (reduce #'or in8))

(test (or8way #*00001000) 1)
(test (or8way #*11111000) 1)
(test (or8way #*00000000) 0)
(test (or8way 1) 'error)

(defun or16way (in16)
  (check-type in16 bus16)
  (reduce #'or in16))

(test (or16way #*0000100000001000) 1)
(test (or16way #*1111100000001000) 1)
(test (or16way #*0000000000000000) 0)
(test (or16way 1) 'error)


(defun mux4way16 (a b c d sel)
  (check-type sel bus2)
  (mux16 
    (mux16 a b (bit sel 1)) 
    (mux16 c d (bit sel 1))
    (bit sel 0)))

(test (mux4way16 #*0000000000000000 #*0001000100010001 
                 #*1000100010001000 #*1111111111111111
                 #*00) #*0000000000000000)

(test (mux4way16 #*0000000000000000 #*0001000100010001 
                 #*1000100010001000 #*1111111111111111
                 #*01) #*0001000100010001)

(test (mux4way16 #*0000000000000000 #*0001000100010001 
                 #*1000100010001000 #*1111111111111111
                 #*10) #*1000100010001000)

(test (mux4way16 #*0000000000000000 #*0001000100010001 
                 #*1000100010001000 #*1111111111111111
                 #*11) #*1111111111111111)


(defun mux8way16 (a b c d e f g h sel)
  (check-type sel bus3)
  (mux16 
    (mux4way16 a b c d (subseq sel 1 (1+ 2))) 
    (mux4way16 e f g h (subseq sel 1 (1+ 2)))
    (bit sel 0)))

(test (mux8way16 #*0000000000000000 #*0001000100010001 
                 #*1000100010001000 #*1111111111111111
                 #*1100110011001100 #*0101010101010101 
                 #*0000000011111111 #*1111111111111110
                 #*000) #*0000000000000000)

(test (mux8way16 #*0000000000000000 #*0001000100010001 
                 #*1000100010001000 #*1111111111111111
                 #*1100110011001100 #*0101010101010101 
                 #*0000000011111111 #*1111111111111110
                 #*001) #*0001000100010001)

(test (mux8way16 #*0000000000000000 #*0001000100010001 
                 #*1000100010001000 #*1111111111111111
                 #*1100110011001100 #*0101010101010101 
                 #*0000000011111111 #*1111111111111110
                 #*010) #*1000100010001000)

(test (mux8way16 #*0000000000000000 #*0001000100010001 
                 #*1000100010001000 #*1111111111111111
                 #*1100110011001100 #*0101010101010101 
                 #*0000000011111111 #*1111111111111110
                 #*011) #*1111111111111111)

(test (mux8way16 #*0000000000000000 #*0001000100010001 
                 #*1000100010001000 #*1111111111111111
                 #*1100110011001100 #*0101010101010101 
                 #*0000000011111111 #*1111111111111110
                 #*100) #*1100110011001100)

(test (mux8way16 #*0000000000000000 #*0001000100010001 
                 #*1000100010001000 #*1111111111111111
                 #*1100110011001100 #*0101010101010101 
                 #*0000000011111111 #*1111111111111110
                 #*101) #*0101010101010101)

(test (mux8way16 #*0000000000000000 #*0001000100010001 
                 #*1000100010001000 #*1111111111111111
                 #*1100110011001100 #*0101010101010101 
                 #*0000000011111111 #*1111111111111110
                 #*110) #*0000000011111111)

(test (mux8way16 #*0000000000000000 #*0001000100010001 
                 #*1000100010001000 #*1111111111111111
                 #*1100110011001100 #*0101010101010101 
                 #*0000000011111111 #*1111111111111110
                 #*111) #*1111111111111110)

(defun dmux4way (in sel)
  (check-type sel bus2)
  (multiple-value-bind (a0 b0) (dmux in (bit sel 0))
    (multiple-value-call #'values
      (dmux a0 (bit sel 1))
      (dmux b0 (bit sel 1)))))

(test (dmux4way 1 #*00) (values 1 0 0 0))
(test (dmux4way 1 #*01) (values 0 1 0 0))
(test (dmux4way 1 #*10) (values 0 0 1 0))
(test (dmux4way 1 #*11) (values 0 0 0 1))

(test (dmux4way 0 #*00) (values 0 0 0 0))
(test (dmux4way 0 #*01) (values 0 0 0 0))
(test (dmux4way 0 #*10) (values 0 0 0 0))
(test (dmux4way 0 #*11) (values 0 0 0 0))


(defun dmux8way (in sel)
  (check-type sel bus3)
  (multiple-value-bind (a0 b0) (dmux in (bit sel 0))
    (multiple-value-call #'values
      (dmux4way a0 (subseq sel 1 (1+ 2)))
      (dmux4way b0 (subseq sel 1 (1+ 2))))))

(test (dmux8way 1 #*000) (values 1 0 0 0 0 0 0 0))
(test (dmux8way 1 #*001) (values 0 1 0 0 0 0 0 0))
(test (dmux8way 1 #*010) (values 0 0 1 0 0 0 0 0))
(test (dmux8way 1 #*011) (values 0 0 0 1 0 0 0 0))
(test (dmux8way 1 #*100) (values 0 0 0 0 1 0 0 0))
(test (dmux8way 1 #*101) (values 0 0 0 0 0 1 0 0))
(test (dmux8way 1 #*110) (values 0 0 0 0 0 0 1 0))
(test (dmux8way 1 #*111) (values 0 0 0 0 0 0 0 1))

(test (dmux8way 0 #*11) 'error)

(test (dmux8way 0 #*000) (values 0 0 0 0 0 0 0 0))
(test (dmux8way 0 #*001) (values 0 0 0 0 0 0 0 0))
(test (dmux8way 0 #*010) (values 0 0 0 0 0 0 0 0))
(test (dmux8way 0 #*011) (values 0 0 0 0 0 0 0 0))
(test (dmux8way 0 #*100) (values 0 0 0 0 0 0 0 0))
(test (dmux8way 0 #*101) (values 0 0 0 0 0 0 0 0))
(test (dmux8way 0 #*110) (values 0 0 0 0 0 0 0 0))
(test (dmux8way 0 #*111) (values 0 0 0 0 0 0 0 0))

;====================================================================


; macro level recursion effectively rewriting boilerplate nested multiple value binds representing ripple wiring
(defmacro wire (bindings &body body)
  (if (null bindings)
      `(progn ,@body)
      (destructuring-bind (vars form) (first bindings)
        `(multiple-value-bind ,(if (listp vars) vars (list vars)) ,form
           (wire ,(rest bindings) ,@body)))))

(test 
  (macroexpand-1
    '(wire (((carry0 sum0) (halfadder b c))
            ((carry1 sum) (halfadder a sum0))
            (carry (xor carry0 carry1)))
      (values carry sum)))
  (values 
    '(MULTIPLE-VALUE-BIND (CARRY0 SUM0) (HALFADDER B C) (WIRE (((CARRY1 SUM) (HALFADDER A SUM0)) (CARRY (XOR CARRY0 CARRY1))) (VALUES CARRY SUM)))
    T))

;====================================================================

(defun halfadder (a b)
  (values (and a b) (xor a b)))

(test (halfadder 0 0) (values 0 0))
(test (halfadder 0 1) (values 0 1))
(test (halfadder 1 0) (values 0 1))
(test (halfadder 1 1) (values 1 0))

(defun fulladder (a b c)
  (wire (((carry0 sum0) (halfadder b c))
         ((carry1 sum) (halfadder a sum0))
         (carry (xor carry0 carry1)))
    (values carry sum)))

(test (fulladder 0 0 0) (values 0 0))
(test (fulladder 0 0 1) (values 0 1))
(test (fulladder 0 1 0) (values 0 1))
(test (fulladder 0 1 1) (values 1 0))
(test (fulladder 1 0 0) (values 0 1))
(test (fulladder 1 0 1) (values 1 0))
(test (fulladder 1 1 0) (values 1 0))
(test (fulladder 1 1 1) (values 1 1))


(defun bus (&rest bits)
  (make-array (length bits) :element-type 'bit :initial-contents bits))

(defun add16 (a b)
  (wire (((c14  s15) (halfadder (bit a 15) (bit b 15)))
         ((c13  s14) (fulladder (bit a 14) (bit b 14) c14))
         ((c12  s13) (fulladder (bit a 13) (bit b 13) c13))
         ((c11  s12) (fulladder (bit a 12) (bit b 12) c12))
         ((c10  s11) (fulladder (bit a 11) (bit b 11) c11))
         ((c9   s10) (fulladder (bit a 10) (bit b 10) c10))
         ((c8   s9)  (fulladder (bit a 9)  (bit b 9)  c9))
         ((c7   s8)  (fulladder (bit a 8)  (bit b 8)  c8))
         ((c6   s7)  (fulladder (bit a 7)  (bit b 7)  c7))
         ((c5   s6)  (fulladder (bit a 6)  (bit b 6)  c6))
         ((c4   s5)  (fulladder (bit a 5)  (bit b 5)  c5))
         ((c3   s4)  (fulladder (bit a 4)  (bit b 4)  c4))
         ((c2   s3)  (fulladder (bit a 3)  (bit b 3)  c3))
         ((c1   s2)  (fulladder (bit a 2)  (bit b 2)  c2))
         ((c0   s1)  (fulladder (bit a 1)  (bit b 1)  c1))
         ((cout s0)  (fulladder (bit a 0)  (bit b 0)  c0)))
    cout ; effectively (declare (ignore cout))
    (bus s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15)))

(test (add16 #*0000000000000000 #*0001000100010001) #*0001000100010001)
(test (add16 #*1111111111111111 #*0001000100010001) #*0001000100010000)
(test (add16 #*1111111111111111 #*1111111111111111) #*1111111111111110)
(test (add16 #*1111111111111111 #*0000000000000001) #*0000000000000000)
(test (add16 #*0000000000100000 #*1000000000100000) #*1000000001000000)
(test (add16 #*0000000000100000 #*100000000010000) 'error)

(defun inc16 (in16) (add16 in16 #*0000000000000001))

(test (inc16 #*0000000000000000) #*0000000000000001)
(test (inc16 #*0001000100010001) #*0001000100010010)
(test (inc16 #*1111111111111111) #*0000000000000000)
(test (inc16 #*0000000000100000) #*0000000000100001)
(test (inc16 #*000000000010000) 'error)

;--------------------------------------------------------------------

(defun alu (x y zx nx zy ny f no)
  (check-type x bus16)
  (check-type y bus16)
  (check-type zx bit)
  (check-type nx bit)
  (check-type zy bit)
  (check-type ny bit)
  (check-type f bit)
  (check-type no bit)
  
  (wire ((x0 (dmux16 x zx))
         (y0 (dmux16 y zy))
         (x1 (mux16 x0 (not16 x0) nx))
         (y1 (mux16 y0 (not16 y0) ny))
         (out0 (mux16 (and16 x1 y1) (add16 x1 y1) f))
         (out (mux16 out0 (not16 out0) no)))
    (values out (not (or16way out)) (bit out 0))))


;; 1. Constant outputs: 0, 1, -1
(test (alu #*0000000000010001 #*0000000000000011 1 0 1 0 1 0)
      (values #*0000000000000000 1 0)) ; 0
(test (alu #*0000000000010001 #*0000000000000011 1 1 1 1 1 1)
      (values #*0000000000000001 0 0)) ; 1
(test (alu #*0000000000010001 #*0000000000000011 1 1 1 0 1 0)
      (values #*1111111111111111 0 1)) ; -1

;; 2. Single-operand identity & bitwise NOT: x, y, !x, !y
(test (alu #*0000000000010001 #*0000000000000011 0 0 1 1 0 0)
      (values #*0000000000010001 0 0)) ; x
(test (alu #*0000000000010001 #*0000000000000011 1 1 0 0 0 0)
      (values #*0000000000000011 0 0)) ; y
(test (alu #*0000000000010001 #*0000000000000011 0 0 1 1 0 1)
      (values #*1111111111101110 0 1)) ; !x
(test (alu #*0000000000010001 #*0000000000000011 1 1 0 0 0 1)
      (values #*1111111111111100 0 1)) ; !y

;; 3. Two's complement arithmetic negation: -x, -y
(test (alu #*0000000000010001 #*0000000000000011 0 0 1 1 1 1)
      (values #*1111111111101111 0 1)) ; -x (-17)
(test (alu #*0000000000010001 #*0000000000000011 1 1 0 0 1 1)
      (values #*1111111111111101 0 1)) ; -y (-3)

;; 4. Increment & decrement: x+1, y+1, x-1, y-1
(test (alu #*0000000000010001 #*0000000000000011 0 1 1 1 1 1)
      (values #*0000000000010010 0 0)) ; x+1 (18)
(test (alu #*0000000000010001 #*0000000000000011 1 1 0 1 1 1)
      (values #*0000000000000100 0 0)) ; y+1 (4)
(test (alu #*0000000000010001 #*0000000000000011 0 0 1 1 1 0)
      (values #*0000000000010000 0 0)) ; x-1 (16)
(test (alu #*0000000000010001 #*0000000000000011 1 1 0 0 1 0)
      (values #*0000000000000010 0 0)) ; y-1 (2)

;; 5. Two-operand arithmetic: x+y, x-y, y-x
(test (alu #*0000000000010001 #*0000000000000011 0 0 0 0 1 0)
      (values #*0000000000010100 0 0)) ; x+y (20)
(test (alu #*0000000000010001 #*0000000000000011 0 1 0 0 1 1)
      (values #*0000000000001110 0 0)) ; x-y (14)
(test (alu #*0000000000010001 #*0000000000000011 0 0 0 1 1 1)
      (values #*1111111111110010 0 1)) ; y-x (-14)

;; 6. Two-operand bitwise logic: x&y, x|y
(test (alu #*0000000000010001 #*0000000000000011 0 0 0 0 0 0)
      (values #*0000000000000001 0 0)) ; x&y (1)
(test (alu #*0000000000010001 #*0000000000000011 0 1 0 1 0 1)
      (values #*0000000000010011 0 0)) ; x|y (19)

;; 7. Zero-flag edge cases with non-zero inputs
(test (alu #*0000000000010001 #*0000000000010001 0 1 0 0 1 1)
      (values #*0000000000000000 1 0)) ; x-x = 0
(test (alu #*1111111111111111 #*0000000000000011 0 1 1 1 1 1)
      (values #*0000000000000000 1 0)) ; -1+1 = 0

;; 8. Type & width errors
(test (alu #*0000 #*0000000000000011 1 0 1 0 1 0) 'error)
(test (alu #*0000000000010001 #*0000 1 0 1 0 1 0) 'error)
(test (alu #*0000000000010001 #*0000000000000011 2 0 1 0 1 0)
      'error)

;====================================================================

#|
;; Parallel fixed-point propagation model (kept for reference):
;; Simulates simultaneous gate updates until outputs stabilize.
;; While faithful to ideal zero-delay logic, transitioning from
;; the forbidden (0 0) state to (1 1) causes infinite oscillation
;; due to symmetrical timing.
;;
;; Using the sequential version below instead: its fixed
;; evaluation order acts as a deterministic tie-breaker,
;; mirroring real-world propagation skew that collapses
;; metastability into a valid state without hanging.

(wire ((q 0)
       (q-not 1))
  (defun sr-latch (s r)
    (labels ((settle (curr-q curr-q-not)
               (let ((next-q (nand s curr-q-not))
                     (next-q-not (nand r curr-q)))
                 (if (cl:and (= next-q curr-q)
                             (= next-q-not curr-q-not))
                     (values (setf q next-q)
                             (setf q-not next-q-not))
                     (settle next-q next-q-not)))))
      (settle q q-not))))
|#

(defun make-sr-latch ()
 (let ((q 0)
       (q-not 1))
   (lambda (s r)
     (setf q-not (nand q r))
     (values (setf q (nand s q-not))
             (setf q-not (nand q r))))))

(defparameter test-latch (make-sr-latch))

; Ensure sequential testing
(test (funcall test-latch 1 1) (values 0 1))
(test (funcall test-latch 1 0) (values 0 1))
(test (funcall test-latch 1 0) (values 0 1))
(test (funcall test-latch 1 0) (values 0 1))
(test (funcall test-latch 1 1) (values 0 1))
(test (funcall test-latch 1 1) (values 0 1))
(test (funcall test-latch 1 1) (values 0 1))
(test (funcall test-latch 0 1) (values 1 0))
(test (funcall test-latch 0 1) (values 1 0))
(test (funcall test-latch 0 1) (values 1 0))
(test (funcall test-latch 0 1) (values 1 0))
(test (funcall test-latch 1 0) (values 0 1))
(test (funcall test-latch 0 1) (values 1 0))
(test (funcall test-latch 0 1) (values 1 0))
(test (funcall test-latch 0 1) (values 1 0))
(test (funcall test-latch 0 1) (values 1 0))
(test (funcall test-latch 0 0) (values 1 1))
; predictable metastable collapse
(test (funcall test-latch 1 1) (values 1 0)) 

#|
; works due to predictable behaviour when (0 0)
(defun d-latch (d e &aux (en (not e)))
  (values (not (sr-latch (mux d 1 en) en))))
|#

; standard construction avoids (0 0)
(defun make-d-latch ()
  (let ((latch (make-sr-latch)))
    (lambda (d en)
      (wire ((a (nand d en))
             (b (nand (not d) en)))
            (values (funcall latch a b))))))

(defparameter *test-d-latch-01* (make-d-latch))

(test (funcall *test-d-latch-01* 0 1) 0)
(test (funcall *test-d-latch-01* 0 0) 0)
(test (funcall *test-d-latch-01* 0 0) 0)
(test (funcall *test-d-latch-01* 1 1) 1)
(test (funcall *test-d-latch-01* 1 0) 1)
(test (funcall *test-d-latch-01* 1 0) 1)
(test (funcall *test-d-latch-01* 1 0) 1)
(test (funcall *test-d-latch-01* 0 0) 1)
(test (funcall *test-d-latch-01* 1 0) 1)
(test (funcall *test-d-latch-01* 0 1) 0)

(defun make-dff ()
  (let ((dl0 (make-d-latch))
        (dl1 (make-d-latch)))
    (lambda (d clock)
      (funcall dl1 (funcall dl0 d (not clock)) clock))))

(defparameter *test-dff-01* (make-dff))

(test (funcall *test-dff-01* 0 0) 0)
(test (funcall *test-dff-01* 0 1) 0)
(test (funcall *test-dff-01* 1 0) 0)
(test (funcall *test-dff-01* 1 1) 1)
(test (funcall *test-dff-01* 0 0) 1)
(test (funcall *test-dff-01* 0 1) 0)
(test (funcall *test-dff-01* 1 1) 0)
(test (funcall *test-dff-01* 1 0) 0)
(test (funcall *test-dff-01* 1 1) 1)
(test (funcall *test-dff-01* 0 1) 1)
(test (funcall *test-dff-01* 0 0) 1)
(test (funcall *test-dff-01* 0 1) 0)

#|
; Significantly faster version, not made from base gates
(defun make-dff ()
  (let ((q 0)
        (previous-clock 0))
    (lambda (d clock)
      (when (cl:and (= previous-clock 0) (= clock 1))
        (setf q d))
      (setf previous-clock clock)
      q)))
|#


(defun make-bit ()
  (let ((dff (make-dff))
        (dff-out 0))
    (lambda (in load clock)
      (setf dff-out (funcall dff (mux dff-out in load) clock)))))

(defparameter test-bit (make-bit))

(test (funcall test-bit 0 0 0) 0)
(test (funcall test-bit 0 0 1) 0)
(test (funcall test-bit 1 0 0) 0)
(test (funcall test-bit 1 0 1) 0)
(test (funcall test-bit 1 1 0) 0)
(test (funcall test-bit 1 1 1) 1)
(test (funcall test-bit 0 0 0) 1)
(test (funcall test-bit 0 0 1) 1)
(test (funcall test-bit 0 1 0) 1)
(test (funcall test-bit 0 1 1) 0)
(test (funcall test-bit 1 0 0) 0)
(test (funcall test-bit 1 0 1) 0)
(test (funcall test-bit 1 1 0) 0)
(test (funcall test-bit 1 1 1) 1)

#|
(defun make-register ()
  (let ((bits (make-array 16
                        :initial-contents
                        (loop repeat 16 collect (make-bit)))))
    (lambda (in16 load clock)
      (map 'bit-vector 
           (lambda (b d) (funcall b d load clock)) 
           bits in16))))
|#

(defun make-register ()
  (let ((b0  (make-bit))
        (b1  (make-bit))
        (b2  (make-bit))
        (b3  (make-bit))
        (b4  (make-bit))
        (b5  (make-bit))
        (b6  (make-bit))
        (b7  (make-bit))
        (b8  (make-bit))
        (b9  (make-bit))
        (b10 (make-bit))
        (b11 (make-bit))
        (b12 (make-bit))
        (b13 (make-bit))
        (b14 (make-bit))
        (b15 (make-bit)))
    (lambda (in16 load clock)
      (check-type in16 bus16)
      (make-array
        16
        :element-type 'bit
        :initial-contents
        (list
          (funcall b0  (bit in16 0)  load clock)
          (funcall b1  (bit in16 1)  load clock)
          (funcall b2  (bit in16 2)  load clock)
          (funcall b3  (bit in16 3)  load clock)
          (funcall b4  (bit in16 4)  load clock)
          (funcall b5  (bit in16 5)  load clock)
          (funcall b6  (bit in16 6)  load clock)
          (funcall b7  (bit in16 7)  load clock)
          (funcall b8  (bit in16 8)  load clock)
          (funcall b9  (bit in16 9)  load clock)
          (funcall b10 (bit in16 10) load clock)
          (funcall b11 (bit in16 11) load clock)
          (funcall b12 (bit in16 12) load clock)
          (funcall b13 (bit in16 13) load clock)
          (funcall b14 (bit in16 14) load clock)
          (funcall b15 (bit in16 15) load clock))))))

(defparameter *test-register* (make-register))

; Ensure sequential testing

(test (funcall *test-register* #*0000000000000000 1 0) #*0000000000000000)
(test (funcall *test-register* #*0000000000000000 1 1) #*0000000000000000)
(test (funcall *test-register* #*0000000000000000 1 0) #*0000000000000000)

(test (funcall *test-register* #*1111111111111111 1 0) #*0000000000000000)
(test (funcall *test-register* #*1111111111111111 1 1) #*1111111111111111)
(test (funcall *test-register* #*1111111111111111 1 0) #*1111111111111111)

(test (funcall *test-register* #*0000000000000000 0 0) #*1111111111111111)
(test (funcall *test-register* #*0000000000000000 0 1) #*1111111111111111)
(test (funcall *test-register* #*0000000000000000 0 0) #*1111111111111111)

(test (funcall *test-register* #*1010101010101010 1 0) #*1111111111111111)
(test (funcall *test-register* #*1010101010101010 1 1) #*1010101010101010)
(test (funcall *test-register* #*1010101010101010 1 0) #*1010101010101010)

(test (funcall *test-register* #*0101010101010101 0 0) #*1010101010101010)
(test (funcall *test-register* #*0101010101010101 0 1) #*1010101010101010)
(test (funcall *test-register* #*0101010101010101 0 0) #*1010101010101010)

(test (funcall *test-register* #*0101010101010101 1 0) #*1010101010101010)
(test (funcall *test-register* #*0101010101010101 1 1) #*0101010101010101)
(test (funcall *test-register* #*0101010101010101 1 0) #*0101010101010101)

#|
(defun make-ram8 ()
  (let ((registers (make-array 8
                      :initial-contents
                      (loop repeat 8 collect (make-register)))))
    (lambda (in16 load address clock)
      (let* ((loads (dmux8way load address))
             (mem-contents
               (map 'bus16
                    (lambda (register enable)
                      (funcall register in16 enable clock))
                    registers loads)))
        (mux8way16 mem-contents address)))))
|#

(defun make-ram8 ()
  (let ((r0 (make-register))
        (r1 (make-register))
        (r2 (make-register))
        (r3 (make-register))
        (r4 (make-register))
        (r5 (make-register))
        (r6 (make-register))
        (r7 (make-register)))
    (lambda (in16 address load clock)
      (multiple-value-bind
          (load0 load1 load2 load3 load4 load5 load6 load7)
          (dmux8way load address)
        (mux8way16
          (funcall r0 in16 load0 clock)
          (funcall r1 in16 load1 clock)
          (funcall r2 in16 load2 clock)
          (funcall r3 in16 load3 clock)
          (funcall r4 in16 load4 clock)
          (funcall r5 in16 load5 clock)
          (funcall r6 in16 load6 clock)
          (funcall r7 in16 load7 clock)
          address)))))


(defparameter *test-ram8* (make-ram8))
(test (funcall *test-ram8* #*0000000000000000 #*000 0 0) #*0000000000000000)
(test (funcall *test-ram8* #*0000000000000001 #*000 1 0) #*0000000000000000)
(test (funcall *test-ram8* #*0000000000000001 #*000 1 1) #*0000000000000001)
(test (funcall *test-ram8* #*0000000000000001 #*000 1 0) #*0000000000000001)
(test (funcall *test-ram8* #*0000000000000010 #*001 1 0) #*0000000000000000)
(test (funcall *test-ram8* #*0000000000000010 #*001 1 1) #*0000000000000010)
(test (funcall *test-ram8* #*0000000000000010 #*001 1 0) #*0000000000000010)
(test (funcall *test-ram8* #*0000000000000100 #*010 1 0) #*0000000000000000)
(test (funcall *test-ram8* #*0000000000000100 #*010 1 1) #*0000000000000100)
(test (funcall *test-ram8* #*0000000000000100 #*010 1 0) #*0000000000000100)
(test (funcall *test-ram8* #*0000000000001000 #*011 1 0) #*0000000000000000)
(test (funcall *test-ram8* #*0000000000001000 #*011 1 1) #*0000000000001000)
(test (funcall *test-ram8* #*0000000000001000 #*011 1 0) #*0000000000001000)
(test (funcall *test-ram8* #*0000000000010000 #*100 1 0) #*0000000000000000)
(test (funcall *test-ram8* #*0000000000010000 #*100 1 1) #*0000000000010000)
(test (funcall *test-ram8* #*0000000000010000 #*100 1 0) #*0000000000010000)
(test (funcall *test-ram8* #*0000000000100000 #*101 1 0) #*0000000000000000)
(test (funcall *test-ram8* #*0000000000100000 #*101 1 1) #*0000000000100000)
(test (funcall *test-ram8* #*0000000000100000 #*101 1 0) #*0000000000100000)
(test (funcall *test-ram8* #*0000000001000000 #*110 1 0) #*0000000000000000)
(test (funcall *test-ram8* #*0000000001000000 #*110 1 1) #*0000000001000000)
(test (funcall *test-ram8* #*0000000001000000 #*110 1 0) #*0000000001000000)
(test (funcall *test-ram8* #*0000000010000000 #*111 1 0) #*0000000000000000)
(test (funcall *test-ram8* #*0000000010000000 #*111 1 1) #*0000000010000000)
(test (funcall *test-ram8* #*0000000010000000 #*111 1 0) #*0000000010000000)
(test (funcall *test-ram8* #*1111111111111111 #*000 0 0) #*0000000000000001)
(test (funcall *test-ram8* #*1111111111111111 #*001 0 0) #*0000000000000010)
(test (funcall *test-ram8* #*1111111111111111 #*010 0 0) #*0000000000000100)
(test (funcall *test-ram8* #*1111111111111111 #*011 0 0) #*0000000000001000)
(test (funcall *test-ram8* #*1111111111111111 #*100 0 0) #*0000000000010000)
(test (funcall *test-ram8* #*1111111111111111 #*101 0 0) #*0000000000100000)
(test (funcall *test-ram8* #*1111111111111111 #*110 0 0) #*0000000001000000)
(test (funcall *test-ram8* #*1111111111111111 #*111 0 0) #*0000000010000000)
(test (funcall *test-ram8* #*1010101010101010 #*000 1 0) #*0000000000000001)
(test (funcall *test-ram8* #*1010101010101010 #*000 1 1) #*1010101010101010)
(test (funcall *test-ram8* #*0101010101010101 #*010 1 0) #*0000000000000100)
(test (funcall *test-ram8* #*0101010101010101 #*010 1 1) #*0101010101010101)
(test (funcall *test-ram8* #*0000000000000000 #*00 0 0 ) 'error)
(test (funcall *test-ram8* #*0000 #*000 0 0) 'error)

(test

  (mapcar
    (lambda (address)
      (funcall *test-ram8* #*0000000000000000 address 0 1))
    (list #*000 #*001 #*010 #*011 #*100 #*101 #*110 #*111))

  (list #*1010101010101010
        #*0000000000000010
        #*0101010101010101
        #*0000000000001000
        #*0000000000010000
        #*0000000000100000
        #*0000000001000000
        #*0000000010000000))


(defun make-ram64 ()
  (let ((r0 (make-ram8))
        (r1 (make-ram8))
        (r2 (make-ram8))
        (r3 (make-ram8))
        (r4 (make-ram8))
        (r5 (make-ram8))
        (r6 (make-ram8))
        (r7 (make-ram8)))
    (lambda (in16 address load clock)
      (wire ((bank-address (subseq address 0 3))
             (register-address (subseq address 3 6))
             ((load0 load1 load2 load3 load4 load5 load6 load7)
          (dmux8way load bank-address)))
        (mux8way16 
          (funcall r0 in16 register-address load0 clock)
          (funcall r1 in16 register-address load1 clock)
          (funcall r2 in16 register-address load2 clock)
          (funcall r3 in16 register-address load3 clock)
          (funcall r4 in16 register-address load4 clock)
          (funcall r5 in16 register-address load5 clock)
          (funcall r6 in16 register-address load6 clock)
          (funcall r7 in16 register-address load7 clock)
          bank-address)))))

(defparameter *test-ram64* (make-ram64))
(test (funcall *test-ram64* #*0000000000000001 #*000000 1 0) #*0000000000000000)
(test (funcall *test-ram64* #*0000000000000001 #*000000 1 1) #*0000000000000001)
(test (funcall *test-ram64* #*0000000000000010 #*000111 1 0) #*0000000000000000)
(test (funcall *test-ram64* #*0000000000000010 #*000111 1 1) #*0000000000000010)
(test (funcall *test-ram64* #*0000000000000100 #*111000 1 0) #*0000000000000000)
(test (funcall *test-ram64* #*0000000000000100 #*111000 1 1) #*0000000000000100)
(test (funcall *test-ram64* #*0000000000001000 #*101101 1 0) #*0000000000000000)
(test (funcall *test-ram64* #*0000000000001000 #*101101 1 1) #*0000000000001000)
(test (funcall *test-ram64* #*0000000000010000 #*010010 1 0) #*0000000000000000)
(test (funcall *test-ram64* #*0000000000010000 #*010010 1 1) #*0000000000010000)
(test (funcall *test-ram64* #*1111111111111111 #*000000 0 0) #*0000000000000001)
(test (funcall *test-ram64* #*1111111111111111 #*000111 0 0) #*0000000000000010)
(test (funcall *test-ram64* #*1111111111111111 #*111000 0 0) #*0000000000000100)
(test (funcall *test-ram64* #*1111111111111111 #*101101 0 0) #*0000000000001000)
(test (funcall *test-ram64* #*1111111111111111 #*010010 0 0) #*0000000000010000)


(defun test-ram (ram k)
  (let* ((size (ash 1 k)) (value 0)
         (expected (make-array size :initial-element #*0000000000000000)))
    (labels ((bus (n width)
               (make-array width :element-type 'bit :initial-contents
                           (loop for i downfrom (1- width) to 0 collect (ldb (byte 1 i) n))))
             (write-address (address)
               (let ((in16 (bus (incf value) 16)) (address-bus (bus address k)))
                 (test (funcall ram in16 address-bus 1 0) (aref expected address))
                 (test (funcall ram in16 address-bus 1 1) in16)
                 (setf (aref expected address) in16))))
      (loop for address below size do (write-address address))
      (loop for address downfrom (1- size) to 0 by 2 do (write-address address))
      (loop for address below size do
        (test (funcall ram #*0000000000000000 (bus address k) 0 0)
              (aref expected address)))))
  t)

(test-ram (make-ram8) 3)

(time (test-ram (make-ram64) 6))

(defun make-ram512 ()
  (let ((r0 (make-ram64))
        (r1 (make-ram64))
        (r2 (make-ram64))
        (r3 (make-ram64))
        (r4 (make-ram64))
        (r5 (make-ram64))
        (r6 (make-ram64))
        (r7 (make-ram64)))
    (lambda (in16 address load clock)
      (wire ((bank-address (subseq address 0 3))
             (ram64-address (subseq address 3 9))
             ((load0 load1 load2 load3 load4 load5 load6 load7)
          (dmux8way load bank-address)))
        (mux8way16 
          (funcall r0 in16 ram64-address load0 clock)
          (funcall r1 in16 ram64-address load1 clock)
          (funcall r2 in16 ram64-address load2 clock)
          (funcall r3 in16 ram64-address load3 clock)
          (funcall r4 in16 ram64-address load4 clock)
          (funcall r5 in16 ram64-address load5 clock)
          (funcall r6 in16 ram64-address load6 clock)
          (funcall r7 in16 ram64-address load7 clock)
          bank-address)))))

(time (test-ram (make-ram512) 9))

(defun make-ram4k ()
  (let ((r0 (make-ram512))
        (r1 (make-ram512))
        (r2 (make-ram512))
        (r3 (make-ram512))
        (r4 (make-ram512))
        (r5 (make-ram512))
        (r6 (make-ram512))
        (r7 (make-ram512)))
    (lambda (in16 address load clock)
      (wire ((bank-address (subseq address 0 3))
             (ram512-address (subseq address 3 12))
             ((load0 load1 load2 load3 load4 load5 load6 load7)
          (dmux8way load bank-address)))
        (mux8way16 
          (funcall r0 in16 ram512-address load0 clock)
          (funcall r1 in16 ram512-address load1 clock)
          (funcall r2 in16 ram512-address load2 clock)
          (funcall r3 in16 ram512-address load3 clock)
          (funcall r4 in16 ram512-address load4 clock)
          (funcall r5 in16 ram512-address load5 clock)
          (funcall r6 in16 ram512-address load6 clock)
          (funcall r7 in16 ram512-address load7 clock)
          bank-address)))))

(time (test-ram (make-ram4k) 12))

(defun make-ram16k ()
  (let ((r0 (make-ram4k))
        (r1 (make-ram4k))
        (r2 (make-ram4k))
        (r3 (make-ram4k)))
    (lambda (in16 address load clock)
      (wire ((bank-address (subseq address 0 2))
             (ram4k-address (subseq address 2 14))
             ((load0 load1 load2 load3)
          (dmux4way load bank-address)))
        (mux4way16 
          (funcall r0 in16 ram4k-address load0 clock)
          (funcall r1 in16 ram4k-address load1 clock)
          (funcall r2 in16 ram4k-address load2 clock)
          (funcall r3 in16 ram4k-address load3 clock)
          bank-address)))))

; (time (test-ram (make-ram16k) 14))

(defun quick-test-ram (ram k)
  (let* ((last (1- (ash 1 k))) (addresses (list 0 (logand last #xaaaa) last)))
    (labels ((bus (n width)
               (make-array width :element-type 'bit :initial-contents
                           (loop for i downfrom (1- width) to 0 collect (ldb (byte 1 i) n)))))
      (let ((written
              (loop for address in addresses
                    for in16 = (bus (1+ (random #xffff)) 16)
                    do (test (funcall ram in16 (bus address k) 1 0) #*0000000000000000)
                       (test (funcall ram in16 (bus address k) 1 1) in16)
                    collect in16)))
        (loop for address in addresses for in16 in written do
          (test (funcall ram #*0000000000000000 (bus address k) 0 0) in16)))))
  t)

(time (quick-test-ram (make-ram16k) 14))

;====================================================================
