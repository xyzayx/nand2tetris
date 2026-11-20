
(defpackage :nand2tetris
  (:use :cl)
  (:shadow :not :and :or))

(in-package :nand2tetris)

;--------------------------------------------------------------------

; Takes nand as primitive

(defun nand (a b)
  (check-type a bit)
  (check-type b bit)
  (if (cl:and (= a 1) (= b 1)) 0 1))

(nand 0 0) ; 1
(nand 0 1) ; 1
(nand 1 0) ; 1
(nand 1 1) ; 0
(nand 0.2 1)
(nand 0 'x)
(nand 2 10)

;--------------------------------------------------------------------

(defun not (a) (nand a a))

(not 1) ; 0
(not 0) ; 1
(not 0.2)
(not 'x)

(defun and (a b) (not (nand a b)))

(and 0 1) ; 0
(and 1 0) ; 0
(and 0 0) ; 0
(and 1 1) ; 1

(defun or (a b) (nand (not a) (not b)))

(or 0 0) ; 0
(or 1 0) ; 1
(or 0 1) ; 1
(or 1 1) ; 1

(defun xor (a b) (and (nand a b) (or a b)))

(xor 0 0) ; 0
(xor 1 0) ; 1
(xor 0 1) ; 1
(xor 1 1) ; 0
(xor 0.2 1)

(defun mux (a b sel)
	(or (and a (not sel)) (and b sel) ))

(mux 0 0 0) ; 0
(mux 0 1 0) ; 0
(mux 1 0 0) ; 1
(mux 1 1 0) ; 1

(mux 0 0 1) ; 0
(mux 0 1 1) ; 1
(mux 1 0 1) ; 0
(mux 1 1 1) ; 1


(defun demux (in sel)
  (values (and in (not sel)) (and in sel)))

(demux 0 0) ; 0 0
(demux 1 0) ; 1 0
(demux 0 1) ; 0 0
(demux 1 1) ; 0 1

;--------------------------------------------------------------------

(defun not16 (in16) (map '(bit-vector 16) #'not in16))

(not16 #*1000100010001000) ; #*0111011101110111
(not16 #*0000000000000000) ; #*1111111111111111
(not16 1)

(defun and16 (a16 b16) (map '(bit-vector 16) #'and a16 b16))

(and16 #*1000100010001000 #*0001000100010001) ; #*0000000000000000
(and16 #*1000100010001000 #*1111111111111111) ; #*1000100010001000
(and16 #*1111111111111111 #*1111111111111111) ; #*1111111111111111
(and16 #*0000 #*1111)

(defun or16 (a16 b16) (map '(bit-vector 16) #'or a16 b16))

(or16 #*1000100010001000 #*0001000100010001) ; #*1001100110011001
(or16 #*1000100010001000 #*1111111111111111) ; #*1111111111111111
(or16 #*1111111111111111 #*1111111111111111) ; #*1111111111111111
(or16 #*0000 #*1111)

(defun mux16 (a16 b16 sel)
  (check-type sel bit)
  (map '(bit-vector 16) (lambda (a b) (mux a b sel)) a16 b16))

(mux16 #*1000100010001000 #*0001000100010001 0) ; #*1000100010001000
(mux16 #*1000100010001000 #*1111111111111111 1) ; #*1111111111111111
(mux16 #*1000100010001000 #*1111111111111111 2) ; 
(mux16 #*1000100010001000 #*111111111111111 0) ; 

;--------------------------------------------------------------------

(defun or8way (in8)
  (check-type in8 (bit-vector 8))
  ; cascading 7 or-gates in a binary tree is boilerplate
  (reduce #'or in8))

(or8way #*00001000) ;
(or8way #*11111000) ;
(or8way #*00000000) ;
(or8way 1)

