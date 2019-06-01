(defpackage #:glider/tools/pps
  (:use #:cl)
  (:export #:parse-ps
           #:read-ps))
(in-package #:glider/tools/pps)

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
                                                :glider/tools/pps))
                     program)))

            ;; numbers
            ((digit-char-p c)
             (let ((token))
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
                                                :glider/tools/pps))
                     program)))))
    (when rec-p
      (error "unclosed executable array '}'."))
    (nreverse program)))

;; (defun psuedo-interpret (program)
;;   ())

(defun read-ps (pathname)
  (with-open-file (in pathname :direction :input)
    (parse-ps in)))
