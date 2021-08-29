(in-package #:cl-automaton)


(defclass transition-table ()
  ())

(defun make-transition-table (&rest args)
  (apply #'make-instance 'transition-table args))

(defun make-transition (&rest args))


(defclass automaton ()
  ((state-test :initarg :state-test
	       :initform #'eql
	       :reader state-test
	       :type function)
   (states :initarg :states
	   :initform nil
	   :accessor states
	   :type list)
   (transitions :initarg :transitions
		:initform nil
		:accessor transitions
		:type list)
   (transition-table :initform nil
		     :reader transition-table)))

(defmethod initialize-instance :after ((automaton automaton) &key)
  (setf (slot-value automaton 'transition-table) )
  automaton)

(defgeneric add-state (automaton state))

(defmethod add-state ((automaton automaton) state)
  (push state (states automaton)))

(defgeneric next-state (automaton &optional state))
(defgeneric advance-state (automaton event))
(defgeneric add-transition (automaton transition))

;;;
(defclass finite-state-machine (automaton)
  ())

(defclass mealy-automaton (finite-state-machine)
  ())

(defclass moore-automaton (finite-state-machine)
  ())

;;;


