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
(fmakunbound 'uri)
(fmakunbound 'uri-p)
(fmakunbound 'merge-uris)
(fmakunbound 'render-uri)

(defparameter *standard-ports*
  '((:amqp . 5672)
    (:ftp . 21)
    (:http . 80)
    (:https . 443)
    (:telnet . 23)))

(defparameter *normalize-standard-ports* t
  "When true suppress a scheme's standard port.")

;;;
;;; replace the constructor and predicate with generic operators

(defvar *class.uri* 'uri
  "Binds the concrete class designator to use when instantiating
 these, but specification is initargs only.
 this must be a symbol as it may be used as the constructor
 function designator")

(defgeneric uri (designator &rest args)
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
    

(defparameter *parse-uri-string* 'parse-uri-string)
(defparameter *escape-uri-string* t)

(defgeneric parse-uri (designator &key class package parse-uri-string)
  (:documentation "Coerce the designator to an URI.

 designator : (or string uri) : an URI is returned, a string is parsed
 value : uri")

  (:method ((uri uri) &key class package parse-uri-string escape)
    (declare (ignore class package parse-uri-string escape))
    uri)

  (:method (thing &key (class 'uri) (package *package*) (parse-uri-string *parse-uri-string*)
                  (escape (and *escape-uri-string* (escape-p thing))))
    (multiple-value-bind (scheme host port path query fragment userinfo)
                         (funcall parse-uri-string thing)
      (when scheme
        (setq scheme
              (intern (funcall
                       (case *current-case-mode*
                         (:preserve
                          #'identity)
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
        (when (and (eql port (rest (assoc scheme *standard-ports*)))
                   *normalize-standard-ports*)
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
        (setf (uri-string uri) thing)
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

(defun alpha-p (c)                      ; independent of runtime variations on alpha range
  (let ((code (char-code c)))
    (or (and (>= code #x41) (<= code #x5A))
        (and (>= code #x61) (<= code #x7A)))))
  

(defun unreserved-p (c)
  (declare (type character c))
  (or (alpha-p c) (digit-char-p c) (find c "-._~")))
(defun iunreserved-p (c)
  (declare (type character c))
  (or (alpha-p c) (digit-char-p c) (find c "-._~") (ucschar-p c)))
      
(defun ucschar-p (c)
  (let ((code (char-code c)))
    (cond ((and (<= #xA0 code)    (<= code #xD7FF)  t))
          ((and (<= #xF900 code)  (<= code #xFDCF)  t))
          ((and (<= #xFDF0 code)  (<= code #xFFEF)  t))
          ((and (<= #x10000 code) (<= code #x1FFFD) t))
          ((and (<= #x20000 code) (<= code #x2FFFD) t))
          ((and (<= #x30000 code) (<= code #x3FFFD) t))
          ((and (<= #x40000 code) (<= code #x4FFFD) t))
          ((and (<= #x50000 code) (<= code #x5FFFD) t))
          ((and (<= #x60000 code) (<= code #x6FFFD) t))
          ((and (<= #x70000 code) (<= code #x7FFFD) t))
          ((and (<= #x80000 code) (<= code #x8FFFD) t))
          ((and (<= #x90000 code) (<= code #x9FFFD) t))
          ((and (<= #xA0000 code) (<= code #xAFFFD) t))
          ((and (<= #xB0000 code) (<= code #xBFFFD) t))
          ((and (<= #xC0000 code) (<= code #xCFFFD) t))
          ((and (<= #xD0000 code) (<= code #xDFFFD) t))
          ((and (<= #xE1000 code) (<= code #xEFFFD) t))
          (t nil))))

(defun iprivate-p (c)
  (declare (type character c))
  (let ((code (char-code c)))
    (cond ((<= #xE000 code #xF8FF) t)
          ((<= #xF0000 code #xFFFFD) t)
          ((<= #x100000 code #x10FFFD) t)
          (t nil))))

(defun subdelim-p (c)
  (declare (type character c))
  (find c "!$&'()*+,;="))
               
(defun pchar-p (c)
  (declare (type character c))
  (or (unreserved-p c) (subdelim-p c) (eql c #\:) (eql c #\@)))

(defun ipchar-p (c)
  (declare (type character c))
  (or (iunreserved-p c) (subdelim-p c) (eql c #\:) (eql c #\@)))

(defun qchar-p (c)
  (declare (type character c))
  (or (pchar-p c) (eql c #\/) (eql c #\?)
      (and (not *strict-parse*) (find c "^|#"))))

(defun iqchar-p (c)
  (declare (type character c))
  (or (pchar-p c) (eql c #\/) (eql c #\?)
      (and (not *strict-parse*) (find c "^|#"))
      (iprivate-p c)))

(defun fchar-p (c)
  (declare (type character c))
  (or (pchar-p c) (eql c #\/) (eql c #\?)))

(defun ifchar-p (c)
  (declare (type character c))
  (or (ipchar-p c) (eql c #\/) (eql c #\?)))
  

(defun parse-iri-string (string &key (pchar-p #'ipchar-p) (fchar-p #'ifchar-p) (qchar-p #'iqchar-p))
  (declare (dynamic-extent pchar-p fchar-p qchar-p))
  (puri::parse-uri-string string
                          :pchar-p pchar-p :fchar-p fchar-p :qchar-p qchar-p))

(defun parse-uri-string (string &key (pchar-p #'pchar-p) (fchar-p #'fchar-p) (qchar-p #'qchar-p))
  (declare (dynamic-extent pchar-p fchar-p qchar-p))
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
                                      ,index ,string))))))
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
                 (or (pct-encoded-p s i c) (funcall pchar-p c)))
               (fchar-p (s i c)
                 (or (pct-encoded-p s i c) (funcall fchar-p c)))
               (qchar-p (s i c)
                 (or (pct-encoded-p s i c) (funcall qchar-p c)))
               (query (s)
                 (when (plusp (length s))
                   (validate (query s i c) (qchar-p s i c))))
               (fragment (s)
                 (when s
                   (if (plusp (length s))
                     (validate (fragment s i c) (fchar-p s i c))
                     s)))
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

#+(or)
( (uri "amqp://guest:test@test.com:23/asdf/qwer.txt")
  (let ((string (format nil "http://www.w3.org/2001/sw/DataAccess/tests/data/i18n/normalization.ttl#resum~c"
                        (code-char 769))))
    (list (nth-value 1 (ignore-errors (puri:parse-uri string)))
          (puri::parse-uri string :parse-uri-string 'puri::parse-iri-string)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; merging and unmerging

(defmethod merge-uris ((uri string) (base string) &optional place (strict-p t))
  (merge-uris (parse-uri uri) (parse-uri base) place strict-p))

(defmethod merge-uris ((uri uri) (base string) &optional place (strict-p t))
  (merge-uris uri (parse-uri base) place strict-p))

(defmethod merge-uris ((uri string) (base uri) &optional place (strict-p t))
  (merge-uris (parse-uri uri) base place strict-p))


(defmethod merge-uris ((uri uri) (base uri) &optional place (strict-p t))
  ;; See ../doc/rfc2396.txt for info on the algorithm we use to merge
  ;; URIs.
  ;; RFC3986 introduced the option, that a "non-strict" process would
  ;; treat identical schemes as if the reference uri had none
  ;;
  (tagbody
;;;; step 2
    (when (and (null (uri-parsed-path uri))
               (null (uri-scheme uri))
               (null (uri-host uri))
               (null (uri-port uri))
               (null (uri-query uri)))
      (return-from merge-uris
        (let ((new (copy-uri base :place place)))
          (when (uri-query uri)
            (setf (uri-query new) (uri-query uri)))
          (when (uri-fragment uri)
            (setf (uri-fragment new) (uri-fragment uri)))
          new)))

    (setq uri (copy-uri uri :place place))

;;;; step 3
    (if (and (uri-scheme uri)
             strict-p)
      (unless (string-equal (uri-scheme uri) (uri-scheme base))
        (return-from merge-uris uri))
      (setf (uri-scheme uri) (uri-scheme base)))

;;;; step 4
    (when (uri-host uri) (go :done))
    (setf (uri-host uri) (uri-host base))
    (setf (uri-port uri) (uri-port base))

;;;; step 5
    (let ((p (uri-parsed-path uri)))

      ;; bug13133:
      ;; The following form causes our implementation to be at odds with
      ;; RFC 2396, however this is apparently what was intended by the
      ;; authors of the RFC.  Specifically, (merge-uris "?y" "/foo")
      ;; should return #<uri /foo?y> instead of #<uri ?y>, according to
      ;; this:
;;; http://www.apache.org/~fielding/uri/rev-2002/issues.html#003-relative-query
      (when (null p)
        (setf (uri-path uri) (uri-path base))
        (go :done))

      (when (and p (eq :absolute (car p)))
        (when (equal '(:absolute "") p)
          ;; Canonicalize the way parsing does:
          (setf (uri-path uri) nil))
        (go :done)))

;;;; step 6
    (let* ((base-path
            (or (uri-parsed-path base)
                ;; needed because we canonicalize away a path of just `/':
                '(:absolute "")))
           (path (uri-parsed-path uri))
           new-path-list)
      (when (not (eq :absolute (car base-path)))
        (error "Cannot merge ~a and ~a, since latter is not absolute."
               uri base))

      ;; steps 6a and 6b:
      (setq new-path-list
        (append (butlast base-path)
                (if* path then (cdr path) else '(""))))

      ;; steps 6c and 6d:
      (let ((last (last new-path-list)))
        (if* (atom (car last))
           then (when (string= "." (car last))
                  (setf (car last) ""))
           else (when (string= "." (caar last))
                  (setf (caar last) ""))))
      (setq new-path-list
        (delete "." new-path-list :test #'(lambda (a b)
                                            (if* (atom b)
                                               then (string= a b)
                                               else nil))))

      ;; steps 6e and 6f:
      (let ((npl (cdr new-path-list))
            index tmp fix-tail)
        (setq fix-tail
          (string= ".." (let ((l (car (last npl))))
                          (if* (atom l)
                             then l
                             else (car l)))))
        (loop
          (setq index
            (position ".." npl
                      :test #'(lambda (a b)
                                (string= a
                                         (if* (atom b)
                                            then b
                                            else (car b))))))
          (when (null index) (return))
          (when (= 0 index)
            ;; The RFC says, in 6g, "that the implementation may handle
            ;; this error by retaining these components in the resolved
            ;; path, by removing them from the resolved path, or by
            ;; avoiding traversal of the reference."  The examples in C.2
            ;; imply that we should do the first thing (retain them), so
            ;; that's what we'll do.
            (return))
          (if* (= 1 index)
             then (setq npl (cddr npl))
             else (setq tmp npl)
                  (dotimes (x (- index 2)) (setq tmp (cdr tmp)))
                  (setf (cdr tmp) (cdddr tmp))))
        (setf (cdr new-path-list) npl)
        (when fix-tail (setq new-path-list (nconc new-path-list '("")))))

      ;; step 6g:
      ;; don't complain if new-path-list starts with `..'.  See comment
      ;; above about this step.

      ;; step 6h:
      (when (or (equal '(:absolute "") new-path-list)
                (equal '(:absolute) new-path-list))
        (setq new-path-list nil))
      (setf (uri-path uri)
        (render-parsed-path new-path-list
                            ;; don't know, so have to assume:
                            t)))

;;;; step 7
   :done
    (return-from merge-uris uri)))


;;; printing

(defun render-uri (uri stream
                   &aux (escape (uri-escaped uri))
                        (*print-pretty* nil))
  (unless (eq escape (uri-escaped uri))
    (warn "incompatible uri escape: ~s ~s" escape (uri-escaped uri)))
  (if* stream
     then (format stream "~a" (uri-string uri))
     else (uri-string uri)))

(defmethod uri-string :before ((uri uri))
  (with-slots (string (escape escaped)) uri
    ;; if the cached string has been erased, generate a new one.
    ;; nb. any canonicalization will render this different than the original
    (when (null string)
      (setf string
            (let ((scheme (uri-scheme uri))
                  (host (uri-host uri))
                  (port (uri-port uri))
                  (path (uri-path uri))
                  (query (uri-query uri))
                  (fragment (uri-fragment uri)))
              (concatenate 'string
                           (when scheme
                             (encode-escaped-encoding
                              ;; for upper case lisps - so long as parsing maps to a symbol
                              (string-downcase (symbol-name scheme))
                              *reserved-characters* escape))
                           (when scheme ":")
                           (when (or host (eq :file scheme)) "//")
                           (when host
                             (encode-escaped-encoding
                              host *reserved-authority-characters* escape))
                           (when port ":")
                           (when port
                             #-allegro (format nil "~D" port)
                             #+allegro (with-output-to-string (s)
                                         (excl::maybe-print-fast s port))
                             )
                           (encode-escaped-encoding (or path "/")
                                                    nil
                                                    ;;*reserved-path-characters*
                                                    escape)
                           (when query "?")
                           (when query (encode-escaped-encoding query nil escape))
                           (when fragment "#")
                           (when fragment (encode-escaped-encoding fragment nil escape))))))))