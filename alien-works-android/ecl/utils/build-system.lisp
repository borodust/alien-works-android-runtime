(cl:require :cmp)

(ext:install-bytecodes-compiler)

(cl:in-package :cl-user)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))


(defparameter *ndk-path* nil)
(defparameter *work-path* nil)
(defparameter *ecl-path* nil)
(defparameter *system* nil)
(defparameter *sources* nil)


(defun merge-ecl-path (&optional path)
  (uiop:native-namestring (merge-pathnames (or path "") *ecl-path*)))


(defun merge-ecl-versioned-path (&optional path)
  (let* ((lib-path (merge-ecl-path "lib/"))
         (versioned-path (first (uiop:subdirectories lib-path))))
    (unless versioned-path
      (error "Failed to find versioned precompiled binaries"))
    (uiop:native-namestring (merge-pathnames (or path "") versioned-path))))


(defun merge-ndk-path (&optional path)
  (uiop:native-namestring (merge-pathnames (or path "") *ndk-path*)))


(defun merge-ndk-toolchain-path (&optional path)
  (merge-ndk-path (merge-pathnames path "toolchains/llvm/prebuilt/linux-x86_64/")))


(defun merge-work-path (&optional path)
  (uiop:native-namestring
   (merge-pathnames (or path "")
                    (uiop:pathname-directory-pathname (or *work-path*
                                                          *load-pathname*)))))


(defun ecl-config (&rest flags)
  (uiop:run-program (list* (merge-ecl-path "bin/ecl-config") flags)
                    :output :string))


(defun string+ (&rest args)
  (format nil "~{~A~}" args))


(defun init-script ()
  (setf *features* (list* :unix :android :aarch64 :c/c++
                          :alien-works-android-builder
                          (nset-difference *features* '(:x86 :x86-64 :x86_64
                                                        :linux :unix :darwin
                                                        :win :windows :win32 :win64
                                                        ;; only c/c++
                                                        :dlopen :dffi))))
  (loop for (name value) on (uiop:command-line-arguments) by #'cddr
        do (cond
             ((string= name "--ndk")
              (setf *ndk-path* (merge-pathnames
                                (uiop:ensure-directory-pathname value)
                                (uiop:getcwd))))
             ((string= name "--ecl")
              (setf *ecl-path* (merge-pathnames
                                (uiop:ensure-directory-pathname value)
                                (uiop:getcwd))))
             ((string= name "--dir")
              (setf *work-path* (merge-pathnames
                                 (uiop:ensure-directory-pathname value)
                                 (uiop:getcwd))))
             ((string= name "--system")
              (setf *system* value))))
  (unless (and *ndk-path* *ecl-path* *system*)
    (error "Missing arguments. Need: --ndk, --ecl, --system")))


(defmethod asdf:perform :after ((o asdf:load-op) (c asdf:cl-source-file))
  (push (asdf:component-pathname c) *sources*))


(defun collect-sources ()
  (asdf:load-system *system*)
  (setf *sources* (nreverse *sources*)))


(defun setup-compiler ()
  (ext:install-c-compiler)
  (setf c::*ecl-include-directory* (merge-ecl-path "include/")
        c::*ecl-library-directory* (merge-ecl-path "lib/"))


  (setf c::*cc* (merge-ndk-toolchain-path "bin/aarch64-linux-android21-clang")
        c::*ld* (merge-ndk-toolchain-path "bin/aarch64-linux-android-ld")
        c::*ar* (merge-ndk-toolchain-path "bin/aarch64-linux-android-ar")
        c::*ranlib* (merge-ndk-toolchain-path "bin/aarch64-linux-android-ranlib")
        c::*cc-flags* (string+
                       (ecl-config "--cflags")
                       " -DANDROID -DPLATFORM_ANDROID -O2"
                       " -fPIC -fno-common -D_THREAD_SAFE"
                       " -I" (merge-ecl-path))
        c::*ld-flags* (string+ "-L" (merge-ecl-path "lib")
                               " -lecl -ldl -lm"
                               " -L" (merge-ndk-toolchain-path "sysroot/usr/lib/aarch64-linux-android/"))
        c::*ld-rpath* nil
        c::*ld-shared-flags* (string+ "-shared " c::*ld-flags*)
        c::*ld-bundle-flags* c::*ld-shared-flags*))


(defmacro with-temporary-directory ((dir) &body body)
  (let ((tmp-file (gensym)))
    `(uiop:with-temporary-file (:pathname ,tmp-file)
       (let ((,dir (uiop:ensure-directory-pathname (merge-pathnames
                                                    (string+ (file-namestring ,tmp-file) ".dir/")
                                                    (uiop:pathname-directory-pathname ,tmp-file)))))
         (ensure-directories-exist ,dir)
         (unwind-protect
              (progn ,@body)
           (uiop:delete-directory-tree ,dir :validate t :if-does-not-exist :ignore))))))


(defun build-system ()
  (with-temporary-directory (build-dir)
    (labels ((%compile (source)
               (let* ((destination-source (merge-pathnames
                                           (uiop:relativize-pathname-directory source)
                                           build-dir))
                      (destination-object (merge-pathnames
                                           (string+ (file-namestring destination-source) ".o")
                                           destination-source)))
                 (ensure-directories-exist destination-object)
                 (format t "~&Source: ~A~&Object: ~A" source destination-object)
                 (compile-file source :output-file destination-object :system-p t)
                 destination-object))
             (%compile-sources ()
               (loop for source in *sources*
                     collect (%compile source))))
      (let ((objects `(,(%compile (merge-work-path "aux/prologue.lisp"))
                       ,(merge-ecl-versioned-path "libasdf.a")
                       ,@(%compile-sources)
                       ,(%compile (merge-work-path "aux/epilogue.lisp")))))
        (c:build-shared-library (merge-pathnames "libalienworks.so"
                                                 (uiop:ensure-directory-pathname (uiop:getcwd)))
                                :lisp-files objects
                                :init-name "__alien_works_android_init")))))


(unless (member :devmode *features*)
  (init-script)
  (collect-sources)
  (setup-compiler)
  (build-system))
