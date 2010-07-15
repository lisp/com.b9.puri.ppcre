;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10; Package: cl-user; -*-

(in-package #:cl-user)

(asdf:defsystem :puri-ppcre
  :name "com.b9.puri.ppcre"
  :maintainer "james.anderson@setf.de"
  :licence "GNU Lesser General Public License"
  :description "additions to the Portable Universal Resource Indentifier Library"
  :depends-on (:cl-ppcre :puri)
  :components ((:file "puri-ppcre"))
  :long-description
  "This extension to PURI replaces the URI parser with an implementation in terms of regular expressions
 and changes the instantiation logic to recognize specialized URI classes.")
