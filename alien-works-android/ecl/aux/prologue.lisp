(cl:in-package :cl-user)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf *features* (list* :c/c++
                          (nset-difference *features* '(:dlopen)))
        si::*keep-documentation* nil
        si::*documentation-pool* nil))

(eval-when (:load-toplevel :execute)
  (ext:install-bytecodes-compiler))
