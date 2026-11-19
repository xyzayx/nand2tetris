
(defpackage :nand2tetris
  (:use :cl)
  (:shadow :not :and :or))

(in-package :nand2tetris)

;--------------------------------------------------------------------

; Takes nand as primitive

(defun nand (a b)
  (cond ((cl:and (eq a 0) (eq b 0)) 1)
        ((cl:and (eq a 0) (eq b 1)) 1)
        ((cl:and (eq a 1) (eq b 0)) 1)
        ((cl:and (eq a 1) (eq b 1)) 0)
        (t (error "Primitive 'nand' error: cond not met"))))

(nand 0 0) ; 1
(nand 0 1) ; 1
(nand 1 0) ; 1
(nand 1 1) ; 0
(nand 0.2 1)
(nand 0 'x)

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