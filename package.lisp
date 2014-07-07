;;;; package.lisp

(defpackage #:equals
  (:use #:cl)
  (:export equals
           compare
           lt
           gt
           lte
           gte
           lessp
           not-lessp
           greaterp
           not-greaterp
           hash-code))
