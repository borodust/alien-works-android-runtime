(cl:in-package :cl-user)

(load (merge-pathnames "build-common.lisp" *load-pathname*))

(define-script-parameter output)
(define-script-parameter system)
(define-script-parameter work-dir)

(declaim (special *sources*))


(defun merge-work-path (&optional path)
  (uiop:native-namestring
   (merge-pathnames (or path "")
                    (uiop:pathname-directory-pathname (or *work-dir*
                                                          *load-pathname*)))))

(progn
  (defclass gather-op (asdf:non-propagating-operation) ())

  (defmethod asdf:operation-done-p ((o gather-op) (c asdf:system))
    nil)

  (defmethod asdf:perform ((o gather-op) (c asdf:system))
    (setf *sources* (asdf:input-files o c)))

  (defmethod asdf:input-files ((o gather-op) (c asdf:system))
    (when (not (asdf::builtin-system-p c))
      (loop for sub in (asdf:required-components c
                                                 :goal-operation 'asdf:load-op
                                                 :keep-operation 'asdf:compile-op
                                                 :other-systems t)
            when (typep sub 'asdf:cl-source-file)
              append (asdf:input-files (asdf:make-operation 'asdf:compile-op) sub)))))


(defun dump-sources ()
  (let (*sources*)
    (asdf:operate 'gather-op (asdf:find-system *system*))
    (with-open-file (out *output* :direction :output :if-exists :supersede)
      (format out "~{~A~%~}" *sources*))))


(unless (member :devmode *features*)
  (prepare-android-ecl-features :ffi :dlopen)
  (dump-sources))
