(require 'uiop)
(require 'asdf)
(cl:in-package :cl-user)


(defvar *argument-table* (make-hash-table :test 'equal))


(loop for (param value) on (uiop:command-line-arguments) by #'cddr
      for name = (subseq param 2)
      do (setf (gethash (string-downcase name) *argument-table*) value))


(defmacro define-script-parameter (name)
  `(defparameter ,(intern (format nil "*~A*" name))
     (gethash ,(string-downcase name) *argument-table*)))


(defun update-features (add &optional remove)
  (setf *features* (append
                    add
                    (nset-difference *features* remove))))


(defun prepare-android-ecl-features (&key (ffi :c/c++))
  (update-features
   `(:unix :android :aarch64
           ;; ECL
           ,ffi)
   '(:x86 :x86-64 :x86_64
     :linux :unix :darwin
     :win :windows :win32 :win64
     ;; cleanup ECL ffi methods
     :dlopen :c/c++ :dffi)))
