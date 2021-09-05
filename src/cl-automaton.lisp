;;;; cl-automaton.lisp

(in-package #:cl-automaton)


(let ((automaton (make-instance 'finite-state-machine :init-state 1
				:states '(1 2 3))))
  (add-state automaton 'foo)
  (add-state automaton 'bar)
  (add-transition automaton 'foo nil 'bar)
  (describe automaton)
  (print (list (states automaton)))
  (write-line "")
  (write-line "###################")
  (with-open-file (stream "~/test.gv" :direction :output :if-exists :overwrite)
    (to-dot automaton stream))
  (write-line "###################"))
