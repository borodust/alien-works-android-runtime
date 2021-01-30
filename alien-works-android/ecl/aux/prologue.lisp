(cl:in-package :cl-user)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf *features* (list* :iffi-ephemeral-metadata :iffi-skip-documentation
                          :c/c++
                          (nset-difference *features* '(:c/c++ :dlopen :dffi)))
        si::*keep-documentation* nil
        si::*documentation-pool* nil))

(eval-when (:load-toplevel :execute)
  (ext:install-bytecodes-compiler))
