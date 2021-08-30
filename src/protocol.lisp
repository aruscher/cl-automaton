(in-package #:cl-automaton)

(defgeneric add-state (automaton/table state &optional is-final-p))
(defgeneric next-states (automaton/table state &optional input))
(defgeneric has-state-p (automaton/table state))
(defgeneric add-transition (automaton/table from-state input to-state))

(defclass transition-table ()
  ((table :initform nil
	  :type hash-table
	  :reader table)
   (state-test-f :initarg :state-test-f
		 :initform (error "STATE-TEST-F required.")
		 :reader state-test-f
		 :type function)
   (input-test-f :initarg :input-test-f
		 :initform (error "INPUT-TEST-F required.")
		 :reader input-test-f
		 :type function)))

(defmethod initialize-instance :after ((transition-table transition-table) &key)
  (with-slots (state-test-f input-test-f) transition-table
    (assert (and state-test-f (typep state-test-f 'function)))
    (assert (and input-test-f (typep input-test-f 'function)))
    (setf (slot-value transition-table 'table)
	  (make-hash-table :test state-test-f))))

(defmethod add-state ((transition-table transition-table) state &optional is-final-p)
  (declare (ignore is-final-p))
  (with-slots (table input-test-f) transition-table
    (unless (has-state-p transition-table state)
      (setf (gethash state table)
	    (make-hash-table :test input-test-f)))))

(defmethod has-state-p ((transition-table transition-table) state)
  (gethash state  (table transition-table)))

(defmethod add-transition ((transition-table transition-table) from-state input to-state)
  (assert (has-state-p transition-table from-state))
  (assert (has-state-p transition-table to-state))
  (with-slots (table) transition-table
    (let ((from-state-table (gethash from-state table)))
      (push to-state (gethash input from-state-table)))))

;; --------------------

(defclass automaton ()
  ((states :initarg :states
	   :initform nil
	   :accessor states
	   :type list)
   (current-state :initform nil
		  :accessor current-state)
   (init-state :initarg :init-state
	       :initform (error "INIT-STATE required.")
	       :reader init-state)
   (final-states :initarg :final-states
		 :initform nil
		 :accessor final-states
		 :type list)
   (transitions :initarg :transitions
		:initform nil
		:accessor transitions
		:type list)
   (transition-table :initform nil
		     :reader transition-table)))

(defgeneric is-final-state-p (automaton state))
(defgeneric mark-state-final (automaton state))
(defgeneric advance-state (automaton event))

(defmethod initialize-instance :after ((automaton automaton) &key (state-test-f #'eql) (input-test-f #'eql))
  (with-slots (current-state init-state states transitions transition-table) automaton
    (setf transition-table
	  (make-instance 'transition-table :state-test-f state-test-f
					   :input-test-f input-test-f))
    (add-state automaton init-state)
    (setf current-state init-state)
    (dolist (state states)
      (add-state automaton state))
    (dolist (transition transitions)
      (apply #'add-transition automaton transition)))
  automaton)

(defmethod add-state ((automaton automaton) state &optional is-final-p)
  (with-slots (states transition-table) automaton
    (unless (member state states)
      (push state states)
      (add-state transition-table state)
      (when is-final-p
	(mark-state-final automaton state)))))

(defmethod has-state-p ((automaton automaton) state)
  (member state (states automaton)))

(defmethod is-final-state-p ((automaton automaton) state)
  (and (has-state-p automaton state)
       (member state (final-states automaton))))

(defmethod mark-state-final ((automaton automaton) state)
  (with-slots (states final-states) automaton
    (assert (has-state-p automaton state))
    (unless (is-final-state-p automaton state)
      (push state final-states))))

(defmethod add-transition ((automaton automaton) from-state input to-state)
  (add-transition (transition-table automaton) from-state input to-state)
  (push (list from-state input to-state) (transitions automaton)))

;;;
(defclass finite-state-machine (automaton)
  ())

(defclass mealy-automaton (finite-state-machine)
  ())

(defclass moore-automaton (finite-state-machine)
  ())

;;;


