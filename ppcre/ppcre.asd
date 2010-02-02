;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; Programmer: Kevin Rosenberg


(in-package #:cl-user)

(asdf:defsystem :com.b9.puri.ppcre
  :name "com.b9.puri.ppcre"
  :maintainer "james.anderson@setf.de"
  :licence "GNU Lesser General Public License"
  :description "additions to Portable Universal Resource Indentifier Library"
  :depends-on (:com.b9.puri)
  :components
  ((:file "puri-ppcre")))
