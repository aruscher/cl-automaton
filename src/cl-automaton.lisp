;;;; cl-automaton.lisp

(in-package #:cl-automaton)


(let ((automaton (make-instance 'automaton :init-state 'bak)))
  (add-state automaton 'foo)
  (add-state automaton 'bar)
  (add-transition automaton 'foo nil 'bar)
  (describe automaton)
  (write-line "###################")
  (with-open-file (stream "~/test.gv" :direction :output :if-exists :overwrite)
    (to-dot automaton stream))
  (write-line "###################"))
