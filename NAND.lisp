
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
