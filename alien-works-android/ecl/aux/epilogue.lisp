(cl:in-package :cl-user)


(eval-when (:load-toplevel :execute)
  (when (uiop:find-symbol* :run :alien-works nil)
    (uiop:symbol-call :alien-works :run)))
