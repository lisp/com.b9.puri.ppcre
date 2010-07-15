;; -*- mode: common-lisp; package: puri -*-
;; Patches to portable support for URIs to permit specialization and to use cl-ppcre
;; For general URI information see RFC2396.
;; As of 2010, see also http://tools.ietf.org/html/rfc3986
;;
;; copyright (c) 1999-2005 Franz Inc: the orginal API and much of the
;;   logic remaining in parse-uri
;; copyright (c) 2003-2010 Kevin Rosenbrg: the puri port
;; copyright (c) 2010 james anderson : cl-ppcre integration
;; 
;; This code is free software; you can redistribute it and/or
;; modify it under the terms of the version 2.1 of
;; the GNU Lesser General Public License as published by
;; the Free Software Foundation, as clarified by the
;; preamble found here:
;;     http://opensource.franz.com/preamble.html
;;

;; This code is distributed in the hope that it will be useful,
;; but without any warranty; without even the implied warranty of
;; merchantability or fitness for a particular purpose.  See the GNU
;; Lesser General Public License for more details.
;;

(in-package #:puri)

(eval-when (:compile-toplevel) (declaim (optimize (speed 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Content:
;;
;; This file replaces two operators in the original to use a RE-based parser
;;
;;  parse-uri (designator &key class package)
;;    Coerces the given object to an URI. If given a string, parses it and
;;    makes an instance. If the scheme designates an URI specialization in the
;;    the given package, that class is used for instantiation.
;;  parse-uri-string (string)
;;    This version uses cl-ppcre based on the regular expression from rfc3986.
;;    (see *uri-pattern-string*) It separates the compnent validation from the
;;    parsing. Returns values scheme, host, port, path, query, fragment, and
;;    userinfo.
;;  uri-userinfo, uri-user, uri-password (uri)
;;    The operators are defined to manipulate the instance's property list.
;;    NB. The userinfo value does not appear in the printed representation.
;;  uri, uri-p (uri)
;;    replaced with a generic operator which treats the argument as a
;;    designator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing

;; patch parse-uri to allow class to be designated by the scheme

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(uri-user uri-password uri-userinfo *class.uri* make-uri) :puri))

(fmakunbound 'parse-uri)
(fmakunbound 'parse-uri-string)

(defparameter *standard-ports*
  '((:amqp . 5672)
    (:ftp . 21)
    (:http . 80)
    (:https . 443)
    (:telnet . 23)))

;;;
;;; replace the constructor and predicate with generic operators

(fmakunbound 'uri)
(fmakunbound 'uri-p)

(defvar *class.uri* 'uri
  "Binds the concrete class designator to use when instantiating
 these, but specification is initargs only.
 this must be a symbol as it may be used as the constructor
 function designator")

(defgeneric uri (designator &rest args)
  (declare (dynamic-extent args))
  (:documentation "Construct a uri given initialization arguments. the
 first argument may be a keyword, in which case the value of *class.uri* designate the
 class, or it may be a class designator. In other cases, the first argument
 is interpreted as a context to dereference the respective uri.")
  (:method ((thing string) &key &allow-other-keys) (parse-uri thing))
  (:method ((class-designator (eql t)) &rest initargs)
    (apply 'uri *class.uri* initargs))
  (:method ((instance uri) &key &allow-other-keys)
    instance)
  (:method ((initargs common-lisp:list) &rest args)
    "given a list, spread it and recurse."
    (apply 'uri (first initargs) (nconc args (rest initargs))))
  (:method ((keyword symbol) &rest rest-initargs)
    (if (keywordp keyword)
      (apply #'make-instance *class.uri* keyword rest-initargs)
      (uri (apply #'make-instance keyword rest-initargs)))))

(defun make-uri (&rest initargs)
  (declare (dynamic-extent initargs))
  (apply #'uri *class.uri* initargs))

(defgeneric uri-p (object)
  (:method ((object t)) nil)
  (:method ((object uri)) t))

;;;
;;; augment initialization

(defmethod shared-initialize :after ((instance uri) (slots t)
                                     &key (userinfo nil userinfo-s))
  "Iff userinfo is specified, cache the result."
  (when userinfo-s
    (setf (uri-userinfo instance) userinfo)))

;;;
;;; additional pseudo-accessors


(defmethod uri-userinfo ((uri uri))
  (getf (uri-plist uri) 'userinfo))

(defmethod (setf uri-userinfo) (info (uri uri))
  (setf (uri-string uri) nil
        (uri-hashcode uri) nil)
  (setf (getf (uri-plist uri) 'userinfo) info))

(defmethod uri-user ((uri uri))
  (let ((user (getf (uri-plist uri) 'user uri)))
    (when (eq user uri)
      (let* ((info (uri-userinfo uri))
             (colon (position #\: info)))
        (setf (getf (uri-plist uri) 'user)
              (setf user (if colon (subseq info 0 colon) info)))))
    user))

(defmethod uri-password ((uri uri))
  (let ((password (getf (uri-plist uri) 'password uri)))
    (when (eq password uri)
      (let* ((info (uri-userinfo uri))
             (colon (position #\: info)))
        (setf (getf (uri-plist uri) 'password)
              (setf password (if colon (subseq info (1+ colon)) nil)))))
    password))
    


(defgeneric parse-uri (designator &key class package)
  (:documentation "Coerce the designator to an URI.

 designator : (or string uri) : an URI is returned, a string is parsed
 value : uri")

  (:method ((uri uri) &key class package)
    (declare (ignore class package))
    uri)

  (:method (thing &key (class 'uri) (package *package*) &aux escape)
    (when (uri-p thing) (return-from parse-uri thing))
    
    (setq escape (escape-p thing))
    (multiple-value-bind (scheme host port path query fragment userinfo)
                         (parse-uri-string thing)
      (when scheme
        (setq scheme
              (intern (funcall
                       (case *current-case-mode*
                         ((:case-insensitive-upper :case-sensitive-upper)
                          #'string-upcase)
                         ((:case-insensitive-lower :case-sensitive-lower)
                          #'string-downcase))
                       (decode-escaped-encoding scheme escape))
                      (find-package :keyword))))
      
      (when (and scheme (eq :urn scheme))
        ;; perform the nid/nss split here based on the path content
        (let ((position (position #\: path)))
          (unless position
            (.parse-error "Invalid URN syntax: ~s." thing))
          (setf host (subseq path 0 position)
                path (subseq path (1+ position))))
        (unless (and (plusp (length host))
                     (alphanumericp (char host 0))
                     (dotimes (i (length host) t)
                       (when (= 0 (sbit *valid-nid-characters* (char-code (char host i))))
                         (return nil))))
          (.parse-error "Invalid URN NID syntax: ~s." host))
        (unless (and (plusp (length path))
                     (dotimes (i (length path) t)
                       (when (/= 0 (sbit *reserved-nss-characters* (char-code (char path i))))
                         (return nil))))
          (.parse-error "Invalid URN NSS syntax: ~s." path))
        (return-from parse-uri
          (make-instance 'urn :scheme scheme :nid host :nss path)))
      
      (when host (setq host (decode-escaped-encoding host escape)))
      (when port
        (setq port (read-from-string port))
        (when (not (numberp port)) (error "port is not a number: ~s." port))
        (when (not (plusp port))
          (error "port is not a positive integer: ~d." port))
        (when (eql port (rest (assoc scheme *standard-ports*)))
          (setq port nil)))
      (when (or (string= "" path)
                (and ;; we canonicalize away a reference to just /:
                 scheme
                 (member scheme '(:http :https :ftp) :test #'eq)
                 (string= "/" path)))
        (setq path nil))
      (when path
        (setq path
              (decode-escaped-encoding path escape *reserved-path-characters*)))
      (when query (setq query (decode-escaped-encoding query escape)))
      (when fragment
        (setq fragment
              (decode-escaped-encoding fragment escape
                                       *reserved-fragment-characters*)))
      
      (let* ((scheme-class-name (find-symbol (string scheme) package))
             (scheme-class (and scheme-class-name (find-class scheme-class-name nil))))
        (when (and scheme-class (subtypep scheme-class-name class))
          (setf class scheme-class)))
      
      (let ((uri
             (if* (eq 'uri class)
                  then ;; allow the compiler to optimize the make-instance call:
                  (make-instance 'uri
                    :scheme scheme
                    :host host
                    :port port
                    :path path
                    :query query
                    :fragment fragment
                    :escaped escape
                    :userinfo userinfo)
                  else ;; do it the slow way:
                  (make-instance class
                    :scheme scheme
                    :host host
                    :port port
                    :path path
                    :query query
                    :fragment fragment
                    :escaped escape
                    :userinfo userinfo))))
                
        uri))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing

;; replace parse-uri-string with a ppcre-based implementation which uses
;; the rfc3986 regular expression.
;; the immediate reason was to suppor user information.

(defparameter *uri-pattern-string*
   "^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?"
  "The regular-expression from rfc3986. NB. the escaped '?'.")

(defparameter *uri-scanner*
  (ppcre:create-scanner *uri-pattern-string*))

(defun parse-uri-string (string)
  (multiple-value-bind (matched elements)
                       (cl-ppcre:scan-to-strings  *uri-scanner* string)
    (unless (= (length matched) (length string))
      (.parse-error "URI ~s contained invalid character at position ~d."
                    string (length matched)))
    (macrolet ((validate ((element-name string index character &key (start 0)) &body body)
                 (let ((length (gensym)))
                   `(do* ((,index ,start (1+ ,index))
                          (,length (length ,string))
                          (,character nil))
                         ((>= ,index ,length) ,string)
                      (setf ,character (char ,string ,index))
                      (unless ,@body
                        (.parse-error ,(format nil "~%Invalid ~a syntax @ ~~d: ~~s" element-name)
                                      ,string ,index))))))
      (labels ((split (string char)
                 (let ((position (position char string)))
                   (if position
                     (values (subseq string 0 position) (subseq string (1+ position)))
                     string)))
               (scheme (s)
                 (when s
                   (if (and (plusp (length s))
                            (alpha-char-p (char s 0)))
                     (validate (scheme s i c :start 1)
                               (or (alpha-char-p c)
                                   (digit-char-p c)
                                   (find c "+-.")))
                     (.parse-error "Invalid scheme: ~s." s))))
               (userinfo (s)
                 (when s (validate (userinfo s i c)
                                   (or (unreserved-p c)
                                       (subdelim-p c)
                                       (eql c #\:)
                                       (pct-encoded-p s i c)))))
               (host (s)
                 ;; coerce "" to nil.
                 ;; the original, this signals an error, but the regular expression permits it,
                 ;; as does appendix a
                 (when (plusp (length s)) s))
               (port (s)
                 (when (plusp (length s))
                   (if (every #'digit-char-p s)
                     s
                     (.parse-error "Invalid port: ~s." s))))
               (path (s)
                 (when s (validate (path s i c)
                                   (or (eql c #\/)
                                       (pchar-p s i c)))))
               (pchar-p (s i c)
                 (or (unreserved-p c) (pct-encoded-p s i c) (subdelim-p c) (find c ":@")))
               (query (s)
                 (when (plusp (length s))
                   (validate (query s i c)
                             (or (pchar-p s i c)
                                 (find c (if *strict-parse* "/?" "/?^|#"))))))
               (fragment (s)
                 (when (plusp (length s))
                   (validate (fragment s i c)
                             (or (pchar-p s i c) (find c #(#\/ #\?))))))
               (unreserved-p (c) (or (alpha-char-p c) (digit-char-p c) (find c "-._~")))
               (subdelim-p (c) (find c "!$&'()*+,;="))
               (pct-encoded-p (s i c)
                 (and (>= (length s) (+ i 3))
                      (eql c #\%)
                      (digit-char-p (aref s (incf i)) 16)
                      (digit-char-p (aref s (incf i)) 16))))
        (let ((scheme (aref elements 1))
              (authority (aref elements 3))
              (host nil)
              (port nil)
              (userinfo nil)
              (path (aref elements 4))
              (query (aref elements 6))
              (fragment (aref elements 8)))
          (multiple-value-bind (part1 part2) (split authority #\@)
            (cond (part2
                   (setf userinfo part1)
                   (multiple-value-setq (host port) (split part2 #\:)))
                  (t
                   (multiple-value-setq (host port) (split part1 #\:)))))
          ;; validate and/or normalize the raw parse results
          (values (scheme scheme)
                  (host host)
                  (port port )
                  (path path)
                  (query query)
                  (fragment fragment)
                  (userinfo userinfo)))))))

;; (uri "amqp://guest:test@test.com:23/asdf/qwer.txt")


