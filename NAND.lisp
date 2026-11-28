
#+ccl
(progn
  (ccl:set-lisp-heap-gc-threshold (* 512 1024 1024))
  (ccl:use-lisp-heap-gc-threshold))

#+sbcl
(setf (sb-ext:bytes-consed-between-gcs) (* 512 1024 1024))

;-------------------------------------------------------------------------------

(defpackage :nand2tetris
  (:use :cl)
  (:shadow :not :and :or :____))

(in-package :nand2tetris)

;-------------------------------------------------------------------------------

(defun test-predicate (a b)
  (cl:or (if (numberp b) (eql a b) (equalp a b))
         (eq :____ b)))

(defmacro test (expr expected)
  (let ((want (gensym)))
    `(let* ((,want (multiple-value-list ,expected))
            (got (multiple-value-list ,expr)))
       ; EVERY stops at the shorter list.
       (assert (= (length got) (length ,want)))
       (assert (every #'test-predicate got ,want))
       (values-list got))))

(defmacro test-error (expr &optional (type 'error))
  `(handler-case (progn ,expr (values))
     (,type (c) (type-of c))
     (:no-error () (assert nil))))

(macroexpand '(test (list 0 1) :____))
(test ((lambda () (values 0 1))) (values 0 1))
(macroexpand '(test ((lambda () (values 0 1))) (values 0 1)))

;===============================================================================

; Takes nand as primitive

(defun nand (a b)
  (check-type a bit)
  (check-type b bit)
  (if (cl:and (= a 1) (= b 1)) 0 1))

(test (nand 0 0) 1)
(test (nand 0 1) 1) 
(test (nand 1 0) 1)
(test (nand 1 1) 0)
(test-error (nand 0.2 1))
(test-error (nand 0 'x))
(test-error (nand 2 10))

;-------------------------------------------------------------------------------

(defun not (a) (nand a a))

(test (not 1) 0)
(test (not 0) 1)
(test-error (not 0.2))
(test-error (not 'x))

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
(test-error (xor 0.2 1))

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

;-------------------------------------------------------------------------------

(deftype bus16 () '(bit-vector 16))
(deftype bus8 () '(bit-vector 8))
(deftype bus3 () '(bit-vector 3))
(deftype bus2 () '(bit-vector 2))

;-------------------------------------------------------------------------------

(defun not16 (in16) 
  (check-type in16 bus16)
  (map 'bus16 #'not in16))

(test (not16 #*1000100010001000) #*0111011101110111)
(test (not16 #*0000000000000000) #*1111111111111111)
(test-error (not16 1))
(test-error (not16 #*001))
(test-error (not16 #*11110000111100001))

(defun and16 (a16 b16) 
  (check-type a16 bus16)
  (check-type b16 bus16)
  (map 'bus16 #'and a16 b16))

(test (and16 #*1000100010001000 #*0001000100010001) #*0000000000000000)
(test (and16 #*1000100010001000 #*1111111111111111) #*1000100010001000)
(test (and16 #*1111111111111111 #*1111111111111111) #*1111111111111111)
(test-error (and16 #*0000 #*1111))
(test-error (and16 #*1000100010001000 #*000100010001000101))

(defun or16 (a16 b16) 
  (check-type a16 bus16)
  (check-type b16 bus16)
  (map 'bus16 #'or a16 b16))

(test (or16 #*1000100010001000 #*0001000100010001) #*1001100110011001)
(test (or16 #*1000100010001000 #*1111111111111111) #*1111111111111111)
(test (or16 #*1111111111111111 #*1111111111111111) #*1111111111111111)
(test-error (or16 #*0000 #*1111))

(defun mux16 (a16 b16 sel)
  (check-type a16 bus16)
  (check-type b16 bus16)
  (check-type sel bit)
  (map 'bus16 (lambda (a b) (mux a b sel)) a16 b16))

(test (mux16 #*1000100010001000 #*0001000100010001 0) #*1000100010001000)
(test (mux16 #*1000100010001000 #*1111111111111111 1) #*1111111111111111)
(test-error (mux16 #*1000100010001000 #*1111111111111111 2))
(test-error (mux16 #*1000100010001000 #*111111111111111 0))

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; redef optimized version (faster ram)

(defmacro mux16 (a b sel)
  `(if (zerop ,sel) ,a ,b))

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
(test-error (dmux16 #*1000100010001000 2))
(test-error (dmux16 #*100010001000100 0))

;-------------------------------------------------------------------------------

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
  (reduce #'or in8))

(test (or8way #*00001000) 1)
(test (or8way #*11111000) 1)
(test (or8way #*00000000) 0)
(test-error (or8way 1))

(defun or16way (in16)
  (check-type in16 bus16)
  (reduce #'or in16))

(test (or16way #*0000100000001000) 1)
(test (or16way #*1111100000001000) 1)
(test (or16way #*0000000000000000) 0)
(test-error (or16way 1))


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

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defun bit-vector-to-int (bv)
  (reduce (lambda (n b) (+ (* n 2) b))
          bv :initial-value 0))

; redef optimized versions (faster ram)

(defmacro mux4way16 (a b c d sel)
  `(case (bit-vector-to-int ,sel)
     (0 ,a)
     (1 ,b)
     (2 ,c)
     (3 ,d)))

(defmacro mux8way16 (a b c d e f g h sel)
  `(case (bit-vector-to-int ,sel)
     (0 ,a)
     (1 ,b)
     (2 ,c)
     (3 ,d)
     (4 ,e)
     (5 ,f)
     (6 ,g)
     (7 ,h)))

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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

(test-error (dmux8way 0 #*11))

(test (dmux8way 0 #*000) (values 0 0 0 0 0 0 0 0))
(test (dmux8way 0 #*001) (values 0 0 0 0 0 0 0 0))
(test (dmux8way 0 #*010) (values 0 0 0 0 0 0 0 0))
(test (dmux8way 0 #*011) (values 0 0 0 0 0 0 0 0))
(test (dmux8way 0 #*100) (values 0 0 0 0 0 0 0 0))
(test (dmux8way 0 #*101) (values 0 0 0 0 0 0 0 0))
(test (dmux8way 0 #*110) (values 0 0 0 0 0 0 0 0))
(test (dmux8way 0 #*111) (values 0 0 0 0 0 0 0 0))

;===============================================================================

; macro level recursion effectively rewriting boilerplate 
; nested multiple value binds representing ripple wiring
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
    '(MULTIPLE-VALUE-BIND 
      (CARRY0 SUM0) (HALFADDER B C) 
      (WIRE (((CARRY1 SUM) (HALFADDER A SUM0)) 
             (CARRY (XOR CARRY0 CARRY1))) (VALUES CARRY SUM)))
    T))

;===============================================================================

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
(test-error (add16 #*0000000000100000 #*100000000010000))

(defun inc16 (in16) (add16 in16 #*0000000000000001))

(test (inc16 #*0000000000000000) #*0000000000000001)
(test (inc16 #*0001000100010001) #*0001000100010010)
(test (inc16 #*1111111111111111) #*0000000000000000)
(test (inc16 #*0000000000100000) #*0000000000100001)
(test-error (inc16 #*000000000010000))

;-------------------------------------------------------------------------------

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


(test (alu    #*0000000000010001 
              #*0000000000000011 
                                       1 0 1 0 1 0) ; 0
      (values #*0000000000000000 1 0)) ; 0

(test (alu    #*0000000000010001 
              #*0000000000000011 
                                       1 1 1 1 1 1) ; 1
      (values #*0000000000000001 0 0)) ; 1

(test (alu    #*0000000000010001 
              #*0000000000000011 
                                       1 1 1 0 1 0) ; -1
      (values #*1111111111111111 0 1)) ; -1

(test (alu    #*0000000000010001 ; x = 17
              #*0000000000000011 
                                       0 0 1 1 0 0) ; x
      (values #*0000000000010001 0 0)) ; 17

(test (alu    #*0000000000010001 
              #*0000000000000011 ; y = 3
                                       1 1 0 0 0 0) ; y
      (values #*0000000000000011 0 0)) ; 3

(test (alu    #*0000000000010001 ; x = 17
              #*0000000000000011 
                                       0 0 1 1 0 1) ; !x
      (values #*1111111111101110 0 1)) ; -18

(test (alu    #*0000000000010001 
              #*0000000000000011 ; y = 3
                                       1 1 0 0 0 1) ; !y
      (values #*1111111111111100 0 1)) ; -4

(test (alu    #*0000000000010001 ; x = 17
              #*0000000000000011 
                                       0 0 1 1 1 1) ; -x
      (values #*1111111111101111 0 1)) ; -17

(test (alu    #*0000000000010001 
              #*0000000000000011 ; y = 3
                                       1 1 0 0 1 1) ; -y
      (values #*1111111111111101 0 1)) ; -3

(test (alu    #*0000000000010001 ; x = 17
              #*0000000000000011 
                                       0 1 1 1 1 1) ; x + 1
      (values #*0000000000010010 0 0)) ; 18

(test (alu    #*0000000000010001 
              #*0000000000000011 ; y = 3
                                       1 1 0 1 1 1) ; y + 1
      (values #*0000000000000100 0 0)) ; 4

(test (alu    #*0000000000010001 ; x = 17
              #*0000000000000011 
                                       0 0 1 1 1 0) ; x - 1
      (values #*0000000000010000 0 0)) ; 16

(test (alu    #*0000000000010001 
              #*0000000000000011 ; y = 3
                                       1 1 0 0 1 0) ; y - 1
      (values #*0000000000000010 0 0)) ; 2

(test (alu    #*0000000000010001 ; x = 17
              #*0000000000000011 ; y = 3
                                       0 0 0 0 1 0) ; x + y
      (values #*0000000000010100 0 0)) ; 20

(test (alu    #*0000000000010001 ; x = 17
              #*0000000000000011 ; y = 3
                                       0 1 0 0 1 1) ; x - y
      (values #*0000000000001110 0 0)) ; 14

(test (alu    #*0000000000010001 ; x = 17
              #*0000000000000011 ; y = 3
                                       0 0 0 1 1 1) ; y - x
      (values #*1111111111110010 0 1)) ; -14

(test (alu    #*0000000000010001 ; x = 17
              #*0000000000000011 ; y = 3
                                       0 0 0 0 0 0) ; x & y
      (values #*0000000000000001 0 0)) ; 1

(test (alu    #*0000000000010001 ; x = 17
              #*0000000000000011 ; y = 3
                                       0 1 0 1 0 1) ; x | y
      (values #*0000000000010011 0 0)) ; 19

(test (alu    #*0000000000010001 ; x = 17
              #*0000000000010001 ; y = 17
                                       0 1 0 0 1 1) ; x - y
      (values #*0000000000000000 1 0)) ; 0

(test (alu    #*1111111111111111 ; x = -1
              #*0000000000000011 
                                       0 1 1 1 1 1) ; x + 1
      (values #*0000000000000000 1 0)) ; 0

(test-error (alu #*0000 #*0000000000000011 1 0 1 0 1 0))
(test-error (alu #*0000000000010001 #*0000 1 0 1 0 1 0))
(test-error (alu #*0000000000010001 #*0000000000000011 2 0 1 0 1 0))

;===============================================================================

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
(test (funcall test-latch 1 1) (values 1 0)) 

#|
(defun d-latch (d e &aux (en (not e)))
  (values (not (sr-latch (mux d 1 en) en))))
|#

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

;===============================================================================

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

;-------------------------------------------------------------------------------

(defun to-bits (integer n-bits)
  (make-array n-bits :element-type 'bit
              :initial-contents
              (loop for i from (1- n-bits) downto 0
                    collect (ldb (byte 1 i) integer))))

(defun bus-size-int (bus-lines-n)
  (ash 1 bus-lines-n))

(defun sequential-ram-filler (bus-lines-n)
  (map 'vector
       (lambda (i) (to-bits i 16))
       (loop for i below (bus-size-int bus-lines-n)
             collect i)))

(defun fill-ram (ram filler bus-lines-n)
  (loop for in16 across filler
        for i from 0 below (bus-size-int bus-lines-n)
        for address = (to-bits i bus-lines-n)
        do (funcall ram in16 address 1 0)
           (funcall ram in16 address 1 1)))

(defun ram-to-array (ram bus-lines-n)
  (map 'vector
       (lambda (address)
         (funcall ram #*0000000000000000 address 0 0))
       (loop for i below (bus-size-int bus-lines-n)
             collect (to-bits i bus-lines-n))))

(defun test-ram (ram bus-lines-n)
  (let ((expected (sequential-ram-filler bus-lines-n)))
    (fill-ram ram expected bus-lines-n)
    (test (ram-to-array ram bus-lines-n) expected)
    t))

(defparameter *test-ram8* (make-ram8))

(fill-ram *test-ram8* (sequential-ram-filler 3) 3)
(test (ram-to-array *test-ram8* 3)
      #(#*0000000000000000 #*0000000000000001 
        #*0000000000000010 #*0000000000000011 
        #*0000000000000100 #*0000000000000101 
        #*0000000000000110 #*0000000000000111))

;-------------------------------------------------------------------------------

(test-ram (make-ram8) 3)

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

(test-ram (make-ram64) 6)

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

(test-ram (make-ram512) 9)

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

(test-ram (make-ram4k) 12)

(defun make-ram8k ()
  (let ((r0 (make-ram4k))
        (r1 (make-ram4k)))
    (lambda (in16 address load clock)
      (wire ((bank-address (bit address 0))
             (ram4k-address (subseq address 1 13))
             ((load0 load1) (dmux load bank-address)))
        (mux16
          (funcall r0 in16 ram4k-address load0 clock)
          (funcall r1 in16 ram4k-address load1 clock)
          bank-address)))))

(test-ram (make-ram8k) 13)

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

(test-ram (make-ram16k) 14)

(defun make-ram32k ()
  (let ((r0 (make-ram16k))
        (r1 (make-ram16k)))
    (lambda (in16 address load clock)
      (wire ((bank-address (bit address 0))
             (ram16k-address (subseq address 1 15))
             ((load0 load1) (dmux load bank-address)))
        (mux16
          (funcall r0 in16 ram16k-address load0 clock)
          (funcall r1 in16 ram16k-address load1 clock)
          bank-address)))))

(test-ram (make-ram32k) 15)

;===============================================================================
; Real output wires update instantly with hardware propagation.
; Here wire vars must be updated before calls.

(defun make-pc ()
  (let ((register (make-register))
        (out16 #*0000000000000000))
    (lambda (in16 inc load reset clock)
      ; update output wire vars
      (setf out16 (funcall register #*0000000000000000 0 clock))
      (funcall register 
               (mux16 
                 (mux16 
                   (mux16 out16 (inc16 out16) inc)
                   in16 load) 
                 #*0000000000000000 reset) 
               (or reset (or load inc)) 
               clock))))

(defparameter *test-pc* (make-pc))

(test (funcall *test-pc* #*0000000000000000 0 0 0 0) #*0000000000000000)
(test (funcall *test-pc* #*0000000000000000 0 0 0 1) #*0000000000000000)
(test (funcall *test-pc* #*0000000000000000 1 0 0 0) #*0000000000000000)
(test (funcall *test-pc* #*0000000000000000 1 0 0 1) #*0000000000000001)
(test (funcall *test-pc* #*0000000000000000 1 0 0 0) #*0000000000000001)
(test (funcall *test-pc* #*0000000000000000 1 0 0 1) #*0000000000000010)
(test (funcall *test-pc* #*1010101010101010 0 1 0 0) #*0000000000000010)
(test (funcall *test-pc* #*1010101010101010 0 1 0 1) #*1010101010101010)
(test (funcall *test-pc* #*1111111111111111 0 0 0 0) #*1010101010101010)
(test (funcall *test-pc* #*1111111111111111 0 0 0 1) #*1010101010101010)
(test (funcall *test-pc* #*0101010101010101 1 1 0 0) #*1010101010101010)
(test (funcall *test-pc* #*0101010101010101 1 1 0 1) #*0101010101010101)
(test (funcall *test-pc* #*1111000011110000 1 1 1 0) #*0101010101010101)
(test (funcall *test-pc* #*1111000011110000 1 1 1 1) #*0000000000000000)
(test (funcall *test-pc* #*1111111111111111 0 1 0 0) #*0000000000000000)
(test (funcall *test-pc* #*1111111111111111 0 1 0 1) #*1111111111111111)
(test (funcall *test-pc* #*1111111111111111 0 0 1 0) #*1111111111111111)
(test (funcall *test-pc* #*1111111111111111 0 0 1 1) #*0000000000000000)
(test (funcall *test-pc* #*0000000000000000 1 0 0 0) #*0000000000000000)
(test (funcall *test-pc* #*0000000000000000 1 0 0 1) #*0000000000000001)

;===============================================================================

; A ::   0 v v v  v  v  v  v   v  v  v  v   v  v  v  v   
; C ::   1 1 1 a  c1 c2 c3 c4  c5 c6 d1 d2  d3 j1 j2 j3

(defun make-cpu ()
  (wire ((A (make-register))
         (D (make-register))
         (pc (make-pc))
         (A-out #*0000000000000000)
         (D-out #*0000000000000000))

    (lambda (inM16 instruction16 reset clock)
      
      ; update output wire vars
      (setf A-out (funcall A #*0000000000000000 0 clock))
      (setf D-out (funcall D #*0000000000000000 0 clock))
      
      (wire ((C? (bit instruction16 0))
             (A? (not C?))
             (A-or-M (mux16 A-out inM16 (bit instruction16 3)))
             
             ((out eq lt) ; zr ng
              (alu D-out A-or-M
                   (bit instruction16 4)  ; zx
                   (bit instruction16 5)  ; nx
                   (bit instruction16 6)  ; zy
                   (bit instruction16 7)  ; ny
                   (bit instruction16 8)  ; f
                   (bit instruction16 9))) ; no
             (gt (and (not eq) (not lt)))
             
             (write-to-A (and C? (bit instruction16 10)))
             (write-to-D (and C? (bit instruction16 11)))
             (write-to-M (and C? (bit instruction16 12)))
             
             (j1 (bit instruction16 13))
             (j2 (bit instruction16 14))
             (j3 (bit instruction16 15))

             (load-pc? ; jump
              (and C? (or (and j1 lt) (or (and j2 eq) (and j3 gt)))))
             
             (inc-pc? (not load-pc?)))

        (funcall A (mux16 out instruction16 A?) (or A? write-to-A) clock)
        (funcall D out write-to-D clock)
        
        (values out
                write-to-M
                (subseq A-out 1) ; 15 bits (addressM)
                (subseq (funcall pc A-out inc-pc? load-pc? reset clock)
                        1)))))) ; 15 bits (pc)
    
(defparameter *test-cpu* (make-cpu))
(test (funcall *test-cpu* #*0000000000000000
               #*0011000000111001 0 0) ; A, @12345
      (values :____ 0 #*000000000000000 #*000000000000000)) ; :____, 0, 0, 0
(test (funcall *test-cpu* #*0000000000000000
               #*0011000000111001 0 1) ; A, @12345
      (values :____ 0 #*011000000111001 #*000000000000001)) ; :____, 0, 12345, 1

(test (funcall *test-cpu* #*0000000000000000
               #*1110110000010000 0 0) ; C, comp: A, dest: D, jump: none
      (values :____ 0 #*011000000111001 #*000000000000001)) ; :____, 0, 12345, 1
(test (funcall *test-cpu* #*0000000000000000
               #*1110110000010000 0 1) ; C, comp: A, dest: D, jump: none
      (values :____ 0 #*011000000111001 #*000000000000010)) ; :____, 0, 12345, 2

(test (funcall *test-cpu* #*0000000000000000
               #*0101101110100000 0 0) ; A, @23456
      (values :____ 0 #*011000000111001 #*000000000000010)) ; :____, 0, 12345, 2
(test (funcall *test-cpu* #*0000000000000000
               #*0101101110100000 0 1) ; A, @23456
      (values :____ 0 #*101101110100000 #*000000000000011)) ; :____, 0, 23456, 3

(test (funcall *test-cpu* #*0000000000000000
               #*1110000111110000 0 0) ; C, comp: A-D, dest: AD, jump: none
      (values :____ 0 #*101101110100000 #*000000000000011)) ; :____, 0, 23456, 3
(test (funcall *test-cpu* #*0000000000000000
               #*1110000111110000 0 1) ; C, comp: A-D, dest: AD, jump: none
      (values :____ 0 #*010101101100111 #*000000000000100)) ; :____, 0, 11111, 4

(test (funcall *test-cpu* #*0000000000000000
               #*0000001111101011 0 0) ; A, @1003
      (values :____ 0 #*010101101100111 #*000000000000100)) ; :____, 0, 11111, 4
(test (funcall *test-cpu* #*0000000000000000
               #*0000001111101011 0 1) ; A, @1003
      (values :____ 0 #*000001111101011 #*000000000000101)) ; :____, 0, 1003, 5

(test (funcall *test-cpu* #*0000000000000000
               #*1110001100001000 0 0) ; C, comp: D, dest: M, jump: none
      (values #*0010101101100111 1
              #*000001111101011 #*000000000000101)) ; 11111, 1, 1003, 5
(test (funcall *test-cpu* #*0000000000000000
               #*1110001100001000 0 1) ; C, comp: D, dest: M, jump: none
      (values #*0010101101100111 1
              #*000001111101011 #*000000000000110)) ; 11111, 1, 1003, 6

(test (funcall *test-cpu* #*0000000000000000
               #*0000001111101100 0 0) ; A, @1004
      (values :____ 0 #*000001111101011 #*000000000000110)) ; :____, 0, 1003, 6
(test (funcall *test-cpu* #*0000000000000000
               #*0000001111101100 0 1) ; A, @1004
      (values :____ 0 #*000001111101100 #*000000000000111)) ; :____, 0, 1004, 7

(test (funcall *test-cpu* #*0000000000000000
               #*1110001110011000 0 0) ; C, comp: D-1, dest: MD, jump: none
      (values #*0010101101100110 1
              #*000001111101100 #*000000000000111)) ; 11110, 1, 1004, 7
(test (funcall *test-cpu* #*0000000000000000
               #*1110001110011000 0 1) ; C, comp: D-1, dest: MD, jump: none
      (values #*0010101101100101 1
              #*000001111101100 #*000000000001000)) ; 11109, 1, 1004, 8

;===============================================================================

(defun make-rom32k (prog-array)
  (let ((mem (make-ram32k)))
    (fill-ram mem prog-array 15)
    (lambda (address clock)
      (funcall mem #*0000000000000000 address 0 clock))))

(let ((rom (make-rom32k #(#*0000000000000010
                         #*1110110000010000))))
  (test (funcall rom #*000000000000000 0) #*0000000000000010)
  (test (funcall rom #*000000000000001 0) #*1110110000010000))

;-------------------------------------------------------------------------------

(defun open-device-file (name &rest options)
  (apply #'open (merge-pathnames name *load-truename*)
         :element-type '(unsigned-byte 16) options))

(defun open-io-device-file (name)
  (open-device-file name :direction :io :if-exists :overwrite
                   :if-does-not-exist :create))

(defun open-input-device-file (name)
  (open-device-file name :direction :input
                   :if-does-not-exist :create))

(defparameter screen (open-io-device-file "screen.bin"))
(defparameter keyboard (open-input-device-file "keyboard.bin"))

(defun close-devices (&rest device-fds)
  (dolist (fd device-fds) (close fd)))

;-------------------------------------------------------------------------------

(defun fps-to-ms (fps) (/ 1000 fps))

(defun to-internal-time-units (ms)
  (ceiling (* ms internal-time-units-per-second) 1000))

(defun clear-interval (name)
  (remprop name 'interval-deadline))

(defmacro do-at-interval (name ms &body body)
  (let ((now (gensym)))
    `(let ((,now (get-internal-real-time)))
       (when (>= ,now (get ',name 'interval-deadline 0))
         ,@body
         (setf (get ',name 'interval-deadline)
               (+ ,now (to-internal-time-units ,ms)))))))

(macroexpand '(do-at-interval example 100 (print "pretty")))

;-------------------------------------------------------------------------------

(defun write-word-to-ram (ram address word)
  (funcall ram word address 1 0)
  (funcall ram word address 1 1))

(defun read-keyboard-from-file (stream)
  (file-position stream 0)
  (to-bits (read-byte stream nil 0) 16))

(defun update-keyboard-register (ram)
  (write-word-to-ram ram #*110000000000000 ; 24576
                    (read-keyboard-from-file keyboard)))

;-------------------------------------------------------------------------------

(defun read-word-from-ram (ram address)
  (funcall ram #*0000000000000000 address 0 1))

(defun write-screen-to-file (stream ram)
  (let ((frame (make-array 8192 :element-type '(unsigned-byte 16)))
        (start (bit-vector-to-int #*100000000000000))) ; 16384
    (loop for i below (length frame)
          for address = (to-bits (+ start i) 15)
          do (setf (aref frame i)
                   (bit-vector-to-int (read-word-from-ram ram address))))
    (file-position stream 0)
    (write-sequence frame stream)
    (finish-output stream)))

(defun refresh-screen (ram)
  (write-screen-to-file screen ram))

;###############################################################################

(defun make-computer (program)
  (let ((cpu (make-cpu))
        (ram (make-ram32k))
        (rom (make-rom32k program))
        (inM #*0000000000000000)
        (instruction #*0000000000000000))
    (values
     (lambda (reset clock)
       (wire (((outM writeM addressM pc)
               (funcall cpu inM instruction reset clock)))
         (setf inM (funcall ram outM addressM writeM clock)
               instruction (funcall rom pc clock))
         (values ram pc)))
     ram)))

;###############################################################################

(defun single-step (computer &optional (reset 0))
  (funcall computer reset 0)
  (funcall computer reset 1))

(defun run-computer (computer n-cycles end-pc
                     &key (fps 1) (keyboard-poll-ms 100))
  (clear-interval 'keyboard)
  (clear-interval 'screen)
  (wire ((frame-ms (fps-to-ms fps))
         ((ram pc) (single-step computer 1)))
    (dotimes (cycle n-cycles)
      (do-at-interval keyboard keyboard-poll-ms
        (update-keyboard-register ram))
      (setf pc (second (multiple-value-list (single-step computer))))
      (when (equal pc end-pc) (return))
      (do-at-interval screen frame-ms (refresh-screen ram)))
    (refresh-screen ram) ; commit screen before exit
    (values ram pc)))

;===============================================================================

(defun test-ram-word (ram address expected)
  (test (read-word-from-ram ram address) expected))

;-------------------------------------------------------------------------------

(defparameter *sum-1-to-100-program*
  #(#*0000000000010000 ; @i (RAM[16])
    #*1110111111001000 ; M=1
    #*0000000000010001 ; @sum (RAM[17])
    #*1110101010001000 ; M=0
    #*0000000000010000 ; @i (LOOP, ROM[4])
    #*1111110000010000 ; D=M
    #*0000000001100100 ; @100
    #*1110010011010000 ; D=D-A
    #*0000000000010010 ; @END
    #*1110001100000001 ; D;JGT
    #*0000000000010000 ; @i
    #*1111110000010000 ; D=M
    #*0000000000010001 ; @sum
    #*1111000010001000 ; M=D+M
    #*0000000000010000 ; @i
    #*1111110111001000 ; M=M+1
    #*0000000000000100 ; @LOOP
    #*1110101010000111 ; 0;JMP
    #*0000000000010010 ; @END (END, ROM[18])
    #*1110101010000111)) ; 0;JMP

(defun test-sum-1-to-100 ()
  (wire ((computer (make-computer *sum-1-to-100-program*))
         (end-pc #*000000000010010)
         ((ram pc) (run-computer computer 2000 end-pc)))
    (test pc end-pc)
    (test-ram-word ram #*000000000010001 #*0001001110111010))) ; 5050

(test-sum-1-to-100)

;-------------------------------------------------------------------------------

(defparameter *add-program*
  #(#*0000000000000010 ; @2
    #*1110110000010000 ; D=A
    #*0000000000000011 ; @3
    #*1110000010010000 ; D=D+A
    #*0000000000000000 ; @R0
    #*1110001100001000)) ; M=D

(defun preset-ram (ram &rest words)
  (fill-ram ram (coerce words 'vector) 15))

(defun test-computer-add ()
  (wire (((computer ram) (make-computer *add-program*))
         (end-pc #*000000000000110))
    (preset-ram ram #*0000000000000000)
    (test (nth-value 1 (run-computer computer 6 end-pc)) end-pc)
    (test-ram-word ram #*000000000000000 #*0000000000000101)))

(test-computer-add)

;-------------------------------------------------------------------------------

(defparameter *max-program*
  #(#*0000000000000000 ; @R0
    #*1111110000010000 ; D=M
    #*0000000000000001 ; @R1
    #*1111010011010000 ; D=D-M
    #*0000000000001010 ; @FIRST
    #*1110001100000001 ; D;JGT
    #*0000000000000001 ; @R1
    #*1111110000010000 ; D=M
    #*0000000000001100 ; @STORE
    #*1110101010000111 ; 0;JMP
    #*0000000000000000 ; @R0 (FIRST, ROM[10])
    #*1111110000010000 ; D=M
    #*0000000000000010 ; @R2 (STORE, ROM[12])
    #*1110001100001000 ; M=D
    #*0000000000001110 ; @END (END, ROM[14])
    #*1110101010000111)) ; 0;JMP

(defun test-computer-max ()
  (wire (((computer ram) (make-computer *max-program*))
         (end-pc #*000000000001110)) ; ROM[14]
    (preset-ram ram #*0000000000000011 #*0000000000000101
                #*0000000000000000) ; 3, 5, 0
    (test (nth-value 1 (run-computer computer 14 end-pc)) end-pc)
    (test-ram-word ram #*000000000000000 #*0000000000000011) ; R0 = 3
    (test-ram-word ram #*000000000000001 #*0000000000000101) ; R1 = 5
    (test-ram-word ram #*000000000000010 #*0000000000000101) ; R2 = 5

    (preset-ram ram #*0101101110100000 #*0011000000111001
                #*0000000000000000) ; 23456, 12345, 0
    (test (nth-value 1 (run-computer computer 10 end-pc)) end-pc)
    (test-ram-word ram #*000000000000000 #*0101101110100000) ; R0 = 23456
    (test-ram-word ram #*000000000000001 #*0011000000111001) ; R1 = 12345
    (test-ram-word ram #*000000000000010 #*0101101110100000))) ; R2 = 23456

(test-computer-max)

;-------------------------------------------------------------------------------

(defparameter *rect-program*
  #(#*0000000000000000 ; @R0 ; height
    #*1111110000010000 ; D=M
    #*0000000000010111 ; @END
    #*1110001100000110 ; D;JLE
    #*0000000000010000 ; @rows_counter
    #*1110001100001000 ; M=D
    #*0100000000000000 ; @SCREEN
    #*1110110000010000 ; D=A
    #*0000000000010001 ; @address
    #*1110001100001000 ; M=D
    #*0000000000010001 ; @address (LOOP, ROM[10])
    #*1111110000100000 ; A=M
    #*1110111010001000 ; M=-1
    #*0000000000010001 ; @address
    #*1111110000010000 ; D=M
    #*0000000000100000 ; @32
    #*1110000010010000 ; D=D+A
    #*0000000000010001 ; @address
    #*1110001100001000 ; M=D
    #*0000000000010000 ; @rows_counter
    #*1111110010011000 ; MD=M-1
    #*0000000000001010 ; @LOOP
    #*1110001100000001 ; D;JGT
    #*0000000000010111 ; @END (END, ROM[23])
    #*1110101010000111)) ; 0;JMP

(defun test-rect-in-screen-ram (ram height)
  (let ((screen-start (bit-vector-to-int #*100000000000000))) ; 16384
    (dotimes (row 256) ; screen height: 256 px
      (dotimes (column 32) ; 32 word cols x 16 bits = 512 px (1 bit/px)
        (test-ram-word
         ram (to-bits (+ screen-start (* row 32) column) 15)
         (if (cl:and (< row height) (zerop column))
             #*1111111111111111
             #*0000000000000000))))))

(defun test-computer-rect ()
  (wire (((computer ram) (make-computer *rect-program*))
         (height #*0000000000000100) ; 4
         (end-pc #*000000000010111)) ; @END, ROM[23]
    (preset-ram ram height)
    (test (nth-value 1 (run-computer computer 63 end-pc)) end-pc)
    (test-rect-in-screen-ram ram (bit-vector-to-int height))))

(test-computer-rect)

;-------------------------------------------------------------------------------

(close-devices screen keyboard)

:end
