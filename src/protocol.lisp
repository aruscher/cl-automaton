(in-package #:cl-automaton)

(defclass automaton ()())

(defgeneric states (automaton)
  (:documentation "Returns a list of all states of the AUTOMATON."))


(defgeneric add-state (automaton state &optional is-final-p)
  (:documentation "Adds a new (final) STATE to the AUTOMATON. 

A STATE can only be added once - subsequent calls are ignored.
The STATE is marked as final when IS-FINAL-P is t.
Signals error when STATE is nil.
Returns STATE."))

(defmethod add-state :around ((automaton automaton) state &optional is-final-p)
  (declare (ignore is-final-p))
  (assert state)
  (unless (has-state-p automaton state)
    (call-next-method))
  state)

(defmethod add-state :after ((automaton automaton) state &optional is-final-p)
  (when is-final-p
    (mark-state-final automaton state)))

(defgeneric has-state-p (automaton state)
  (:documentation "Returns t when the AUTOMATON has STATE, nil otherwise. "))

(defgeneric advance-state (automaton event))

(defgeneric final-states (automaton)
  (:documentation "Returns a list of all final states of the AUTOMATON."))

(defgeneric mark-state-final (automaton state)
  (:documentation "Marks STATE as a final state in the AUTOMATON.

A STATE can only be marked once - subsequent calls are ignored.
Signals error when STATE is nil or when STATE is not in the AUTOMATON.
Returns STATE."))

(defmethod mark-state-final :around ((automaton automaton) state)
  (assert state)
  (assert (has-state-p automaton state))
  (unless (final-state-p automaton state)
    (call-next-method))
  state)

(defgeneric final-state-p (automaton state)
  (:documentation "Return t when AUTOMATON has STATE and STATE is markded as final."))

(defmethod final-state-p :around ((automaton automaton) state)
  (and (has-state-p automaton state) (call-next-method)))

(defgeneric transitions (automaton))
(defgeneric add-transition (automaton from-state input to-state))

; --------------------??????

(defgeneric next-states (automaton state &optional input))





