(cl:in-package :cl-user)

(require 'asdf)
(require 'uiop)

(defvar *script-path* (directory-namestring *load-truename*))
(defvar *script-args* (uiop:command-line-arguments))


(defun keywordify (name)
  (uiop:intern* (uiop:standard-case-symbol-name name) :keyword))


(defun trim (name)
  (string-trim '(#\Tab #\Space #\Newline) (string name)))


(defun shout (control &rest params)
  (handler-case
      (format t "~&~A~&" (apply #'format nil control params))
    (serious-condition (c)
      (warn "Failed to shout `~A` with arguments ~A: ~A" control params c)))
  (finish-output t))


(defun dir (base &rest pathnames)
  (flet ((ensure-relative-dir (dir)
           (uiop:ensure-directory-pathname (uiop:enough-pathname dir "/"))))
    (reduce #'merge-pathnames (mapcar #'ensure-relative-dir pathnames)
            :initial-value (uiop:ensure-directory-pathname base)
            :from-end t)))


(defun file (&rest pathnames)
  (flet ((ensure-file (pathname)
           (let ((pathname (pathname pathname)))
             (if (uiop:directory-pathname-p pathname)
                 (let* ((dir (pathname-directory pathname))
                        (namepath (pathname (first (last dir)))))
                   (make-pathname :directory (butlast dir)
                                  :name (pathname-name namepath)
                                  :type (pathname-type namepath)
                                  :defaults pathname))
                 pathname))))
    (multiple-value-bind (neck last)
        (loop for (path . rest) on pathnames
              if rest
                collect path into neck
              else
                return (values neck (ensure-file path)))
      (if neck
          (merge-pathnames (uiop:enough-pathname last "/") (apply #'dir neck))
          last))))


(defmacro bind-arguments (&body params)
  (shout "Binding ~A to ~A" params *script-args*)
  (multiple-value-bind (optional keys)
      (loop for (param . rest) on params
            until (string= param '&key)
            collect param into optional-params
            finally (return (values optional-params rest)))
    (multiple-value-bind (args value-map)
        (loop with value-map = (make-hash-table)
              with values = nil
              for args = *script-args* then (rest args)
              for arg = (first args)
              while args
              do (cond
                   ((uiop:string-prefix-p "--" (trim arg))
                    (setf (gethash (keywordify (subseq (trim arg) 2)) value-map) (second args)
                          args (rest args)))
                   ((find-if (lambda (key)
                               (destructuring-bind (full-name &rest things)
                                   (uiop:ensure-list key)
                                 (declare (ignore things))
                                 (destructuring-bind (key &rest things)
                                     (uiop:ensure-list full-name)
                                   (declare (ignore things))
                                   (eql key arg))))
                             keys)
                    (setf (gethash arg value-map) (second args)
                          args (rest args)))
                   (t (uiop:appendf values (list arg))))
              finally (return (values values value-map)))
      `(progn ,@(loop for opt in optional
                      for rest-args = args then (rest rest-args)
                      collect `(defparameter ,opt
                                 ,(first rest-args)))
              ,@(loop for key in keys
                      append (destructuring-bind (&optional full-name
                                                    default-value
                                                    provided-p)
                                 (uiop:ensure-list key)
                               (destructuring-bind (designator &optional name)
                                   (uiop:ensure-list full-name)
                                 (multiple-value-bind (value found-p)
                                     (gethash (keywordify designator) value-map)
                                   `((defparameter ,(or name designator) ,(if found-p
                                                                              value
                                                                              default-value))
                                     ,@(when provided-p
                                         `((defparameter ,provided-p ,found-p))))))))))))


(defun script (name &rest args)
  (let ((*script-args* args))
    (let ((package *package*))
      (load (merge-pathnames (format nil "~A.lisp" (uiop:native-namestring name)) *script-path*))
      (setf *package* package))))


(defun quit-lisp (code)
  #+sbcl
  (quit :recklessly-p t :unix-status code)
  #+ccl
  (#__exit code))


(defun command-line-script (name)
  (unwind-protect
       (handler-case
           (apply #'script name (uiop:command-line-arguments))
         (serious-condition (e)
           (shout "Unexpected error: ~A" e)
           (quit-lisp -1)))
    (quit-lisp 0)))


(defun read-safely (string)
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (with-input-from-string (in string)
        (read in)))))


(defun enable-features (&rest features)
  (setf *features* (nunion *features* features :test #'string=)))


(defun disable-features (&rest features)
  (setf *features* (nset-difference *features* features :test #'string=)))


(defmacro with-features ((&rest features) &body body)
  (let ((%features (gensym "features")))
    `(let ((,%features (list ,@features)))
       (unwind-protect
            (progn
              (apply #'enable-features ,%features)
              ,@body)
         (apply #'disable-features ,%features)))))


(defvar *supress-errors* nil)
(defvar *shell-output* nil)


(defun shell (&rest args)
  (flet ((quote-arg (arg)
           (cond
             ((stringp arg) (concatenate 'string "\"" arg "\""))
             ((keywordp arg) (format nil "~(~A~)" arg))
             ((pathnamep arg) (uiop:native-namestring arg))
             (t (string arg)))))
    (let ((command (format nil "~{~A~^ ~}" (mapcar #'quote-arg args))))
      (multiple-value-bind (std err code)
          (uiop:run-program command :output (or *shell-output* *standard-output*)
                                    :error-output (unless *supress-errors*
                                                    *error-output*)
                                    :force-shell t
                                    :ignore-error-status t)
        (declare (ignore err))
        (if (= code 0)
            (values nil std)
            (values code))))))


(defun string* (control &rest args)
  (apply #'format nil control args))


(defun string+ (&rest args)
  (format nil "~{~A~}" args))


(defun script-directory ()
  (directory-namestring (or *load-truename* *compile-file-truename*)))


(defun wget (url path)
  (let ((target (namestring
                 (if (uiop:directory-pathname-p path)
                     (merge-pathnames (file-namestring url) path)
                     path))))
    (ensure-directories-exist (directory-namestring target))
    (shell "wget" "-O" target "-q" url)))


(defun unzip (archive target-dir)
  (let ((target-dir (namestring (uiop:ensure-directory-pathname target-dir))))
    (ensure-directories-exist target-dir)
    (shell "cd" target-dir :&& "unzip" "-o" (namestring archive))))


(defun real-time-seconds ()
  (float (/ (get-internal-real-time) internal-time-units-per-second) 0d0))


(defun wait-for-pathname (pathname &key remove timeout (sleep 1))
  (flet ((pathname-exists-p ()
           (if (uiop:directory-pathname-p pathname)
               (uiop:directory-exists-p pathname)
               (uiop:file-exists-p pathname)))
         (remove-pathname ()
           (if (uiop:directory-pathname-p pathname)
               (uiop:delete-directory-tree pathname :validate (constantly t) :if-does-not-exist :ignore)
               (uiop:delete-file-if-exists pathname))))
    (loop with start-time = (real-time-seconds)
          for exists = (pathname-exists-p)
          until (or exists
                    (and timeout (> (- (real-time-seconds) timeout) start-time)))
          do (sleep sleep)
          finally (when exists
                    (when remove
                      (remove-pathname))
                    (return t)))))


(defun load-relative (file)
  (load (file (script-directory) file)))
