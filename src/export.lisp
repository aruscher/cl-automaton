(in-package #:cl-automaton)

(defgeneric to-dot (automaton &optional stream))

(defmethod to-dot ((automaton automaton) &optional (stream *standard-output*))
  (let ((dummy-start-id (gensym "START")))
    (write-line "digraph automaton {" stream)
    (write-line "rankdir=\"LR\"" stream)
    ;; States
    (write-line "" stream)
    (write-line "node [shape=\"circle\"]" stream)
    (format stream "~&~{~a~^ ~}~%" (states automaton))
    ;; Final states
    (write-line "" stream)
    (write-line "node [shape=\"doublecircle\"]" stream)
    (format stream "~&~{~a~^ ~}~%" (final-states automaton))
    ;; Arrow to starting state
    (write-line "" stream)
    (format stream "~&~a [label=\"\", shape=none, height=0,width=0]~%" dummy-start-id)
    (format stream "~&~a -> ~a [label=\"Start\"]~%" dummy-start-id (init-state automaton))
    ;; Transitions
    (write-line "" stream)
    (loop :for transition :in (transitions automaton)
	  :for from-node = (first transition)
	  :for label = (or (second transition) "<&#949;>")
	  :for to-node = (third transition)
	  :do (format stream "~&~a->~a [label=~a]~%" from-node to-node label)
	  :do (print (list from-node label to-node)))
    (write-line "}" stream)
    (force-output stream)))
