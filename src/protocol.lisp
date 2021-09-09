(in-package #:cl-automaton)

(defclass automaton ()
  ((current-state :initform nil
		  :accessor current-state)
   (init-state :initarg :init-state
	       :initform nil
	       :reader init-state)))

(defmethod initialize-instance :after ((automaton automaton) &key (states nil) (transitions nil) (final-states nil))
  (with-slots (init-state current-state) automaton
    (when init-state 
      (add-state automaton init-state)
      (setf current-state init-state))
    (dolist (state states)
      (add-state automaton state))
    (dolist (final-state final-states)
      (add-state automaton final-state t))
    (dolist (transition transitions)
      (apply #'add-transition automaton transition)))
  automaton)

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
  (:documentation "Returns t when AUTOMATON has STATE and STATE is markded as final."))

(defmethod final-state-p :around ((automaton automaton) state)
  (and (has-state-p automaton state) (call-next-method)))

(defgeneric transitions (automaton)
  (:documentation "Returns a list of transitions of the AUTOMATON.

Each transition is a list of form: (from-state input to-state)"))


(defgeneric add-transition (automaton from-state input to-state)
  (:documentation "Adds a transition to the automaton.

Signals error when FROM-STATE or TO-STATE is not in the AUTOMATON.
Returns a list of form: (from-state input to-state)."))

(defmethod add-transition :around ((automaton automaton) from-state input to-state)
  (declare (ignore input))
  (assert (has-state-p automaton from-state))
  (assert (has-state-p automaton to-state))
  (call-next-method)
  (list from-state input to-state))

(defgeneric next-states (automaton state &optional input))





