(defpackage #:glider/tools/pps
  (:use #:cl)
  (:export #:parse-ps
           #:interpret-ps
           #:read-ps))
(in-package #:glider/tools/pps)

;;;; Psuedo PostScript
;; the imitated PostScript interpreter for extracting points on paths

(defpackage #:glider/tools/pps/symbols
  (:use) (:export))

(deftype token-type ()
  '(member :lit :name :num :str :ex))
(defstruct token
  (type nil :type token-type)
  value)
(defmethod print-object ((o token) s)
  (case (token-type o)
    (:lit (format s "/~a" (token-value o)))
    (:name (format s "~a" (token-value o)))
    (:num (format s "~a" (token-value o)))
    (:ex (format s "{~{~s~^ ~}}" (token-value o)))))

(defun parse-ps (stream &optional (rec-p nil))
  (let ((program))
    (loop
      :for c := (peek-char nil stream nil :eof)
      :until (eq c :eof)
      :do (cond
            ;; returns parsed program
            ((eq c :eof)
             (if rec-p
                 (error "there is unclosed '{'")
                 (return-from parse-ps (nreverse program))))

            ;; skips white spaces
            ((or (char= c #\space) (char= c #\newline))
             (read-char stream))

            ;; skips before newline because it's comment line
            ((char= c #\%)
             (loop
               :for c := (peek-char nil stream nil :eof)
               :until (or (eq c :eof) (char= c #\newline))
               :do (read-char stream)))

            ;; literal name
            ((char= c #\/)
             (read-char stream)
             (let ((token))
               (loop
                 :for c := (peek-char nil stream nil :eof)
                 :until (or (eq c :eof) (char= c #\space) (char= c #\newline))
                 :do (push (read-char stream) token))
               (push (make-token :type :lit
                                 :value (intern (coerce (nreverse token) 'string)
                                                :glider/tools/pps/symbols))
                     program)))

            ;; numbers
            ((or (digit-char-p c) (char= c #\-))
             (let ((token))
               (when (char= c #\-)
                 (read-char stream))
               (loop
                 :for c := (peek-char nil stream nil :eof)
                 :while (or (digit-char-p c) (char= c #\.))
                 :do (push (read-char stream) token))
               (push (make-token :type :num
                                 :value (read-from-string (coerce (nreverse token) 'string)))
                     program)))

            ;; executable arrays
            ((char= c #\{)
             (read-char stream)
             (push (make-token :type :ex :value (parse-ps stream t))
                   program))

            ((char= c #\})
             (read-char stream)
             (if rec-p
                 (return-from parse-ps (nreverse program))
                 (error "unexpected '}'")))

            ;; any token (now includes execution tokens)
            (t
             (let ((token))
               (loop
                 :for c := (peek-char nil stream nil :eof)
                 :until (or (eq c :eof) (char= c #\space) (char= c #\newline))
                 :do (push (read-char stream) token))
               (push (make-token :type :name
                                 :value (intern (coerce (nreverse token) 'string)
                                                :glider/tools/pps/symbols))
                     program)))))
    (when rec-p
      (error "unclosed executable array '}'."))
    (nreverse program)))

(defstruct pitp  ;; psuedo interpreter state
  stack dict tmppath paths)

(defun interpret (itp program)
  (loop
    :for p :in program
    :do (case (token-type p)
          (:lit (push p (pitp-stack itp)))
          (:num (push p (pitp-stack itp)))
          (:ex (push p (pitp-stack itp)))
          (:name (let ((ex (getf (pitp-dict itp) (token-value p))))
                   (when (null ex)
                     (error (format nil "unknown executable name '~s'" (token-value p))))
                   (if (functionp ex)
                       (funcall ex itp)
                       (interpret itp (token-value ex))))))))

(defun make-ex (name fn)
  (list (intern name :glider/tools/pps/symbols) fn))

(defun fake-ex (name)
  (make-ex name #'identity))

(defun init-dict ()
  (append
   ;; prepend postscript procedures
   (fake-ex "where") (fake-ex "ifelse") (fake-ex "lt") (fake-ex "if")
   (fake-ex "bind") (fake-ex "def")
   (fake-ex "cairo_set_page_size")
   (fake-ex "rectclip") (fake-ex "showpage")
   (fake-ex "q") (fake-ex "Q") (fake-ex "cm") (fake-ex "g")
   (fake-ex "f") (fake-ex "re")

   ;; get points
   (make-ex "m" (lambda (itp)
                  (setf (pitp-tmppath itp)
                        (let ((y (token-value (pop (pitp-stack itp))))
                              (x (token-value (pop (pitp-stack itp)))))
                          (list (cons x y))))))
   (make-ex "c" (lambda (itp)
                  (dotimes (i 4)  ; control points are discarded
                    (pop (pitp-stack itp)))
                  (push (let ((y (token-value (pop (pitp-stack itp))))
                              (x (token-value (pop (pitp-stack itp)))))
                          (cons x y))
                        (pitp-tmppath itp))))
   (make-ex "l" (lambda (itp)
                  (push (let ((y (token-value (pop (pitp-stack itp))))
                              (x (token-value (pop (pitp-stack itp)))))
                          (cons x y))
                        (pitp-tmppath itp))))
   (make-ex "h" (lambda (itp)
                  (push (nreverse (pitp-tmppath itp)) (pitp-paths itp))
                  (setf (pitp-tmppath itp) nil)))
   ))

(defun interpret-ps (program)
  (let ((itp (make-pitp :stack nil
                        :dict (init-dict))))
    (interpret itp program)
    (pitp-paths itp)))

(defun read-ps (pathname)
  (with-open-file (in pathname :direction :input)
    (interpret-ps (parse-ps in))))
