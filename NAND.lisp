
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

(defun not16 (in16) (map 'bus16 #'not in16))

(test (not16 #*1000100010001000) #*0111011101110111)
(test (not16 #*0000000000000000) #*1111111111111111)
(test (not16 1) 'error)

(defun and16 (a16 b16) (map 'bus16 #'and a16 b16))

(test (and16 #*1000100010001000 #*0001000100010001) #*0000000000000000)
(test (and16 #*1000100010001000 #*1111111111111111) #*1000100010001000)
(test (and16 #*1111111111111111 #*1111111111111111) #*1111111111111111)
(test (and16 #*0000 #*1111) 'error)

(defun or16 (a16 b16) (map 'bus16 #'or a16 b16))

(test (or16 #*1000100010001000 #*0001000100010001) #*1001100110011001)
(test (or16 #*1000100010001000 #*1111111111111111) #*1111111111111111)
(test (or16 #*1111111111111111 #*1111111111111111) #*1111111111111111)
(test (or16 #*0000 #*1111) 'error)

(defun mux16 (a16 b16 sel)
  (check-type sel bit)
  (map 'bus16 (lambda (a b) (mux a b sel)) a16 b16))

(test (mux16 #*1000100010001000 #*0001000100010001 0) #*1000100010001000)
(test (mux16 #*1000100010001000 #*1111111111111111 1) #*1111111111111111)
(test (mux16 #*1000100010001000 #*1111111111111111 2) 'error)
(test (mux16 #*1000100010001000 #*111111111111111 0) 'error)

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

(defun mux4way16 (a b c d sel)
  (check-type a bus16)
  (check-type b bus16)
  (check-type c bus16)
  (check-type d bus16)
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
  (check-type a bus16)
  (check-type b bus16)
  (check-type c bus16)
  (check-type d bus16)
  (check-type e bus16)
  (check-type f bus16)
  (check-type g bus16)
  (check-type h bus16)
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
  (check-type in bit)
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
  (check-type in bit)
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
