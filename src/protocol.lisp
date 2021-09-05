(in-package #:cl-automaton)

(defgeneric states (automaton/table))
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

(defmethod states ((transition-table transition-table))
  (loop :for key :being the hash-key of (table transition-table)
	:collect key))

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

(defclass automaton ()())

(defgeneric is-final-state-p (automaton state))
(defgeneric mark-state-final (automaton state))
(defgeneric advance-state (automaton event))

;;;
(defclass finite-state-machine (automaton)
  ((current-state :initform nil
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


(defmethod initialize-instance :after ((fsm finite-state-machine) &key (state-test-f #'eql) (input-test-f #'eql) (states nil))
  (with-slots (current-state init-state transitions transition-table) fsm
    (setf transition-table
	  (make-instance 'transition-table :state-test-f state-test-f
					   :input-test-f input-test-f))
    (add-state fsm init-state)
    (setf current-state init-state)
    (dolist (state states)
      (add-state fsm state))
    (dolist (transition transitions)
      (apply #'add-transition fsm transition)))
  fsm)

(defmethod states ((fsm finite-state-machine))
  (states (transition-table fsm)))

(defmethod add-state ((fsm finite-state-machine) state &optional is-final-p)
  (with-slots (transition-table) fsm
    (unless (has-state-p fsm state)
      (add-state transition-table state)
      (when is-final-p
	(mark-state-final fsm state)))))

(defmethod has-state-p ((fsm finite-state-machine) state)
  (has-state-p (transition-table fsm) state))

(defmethod is-final-state-p ((fsm finite-state-machine) state)
  (and (has-state-p fsm state)
       (member state (final-states fsm))))

(defmethod mark-state-final ((fsm finite-state-machine) state)
  (with-slots (final-states) fsm
    (assert (has-state-p fsm state))
    (unless (is-final-state-p fsm state)
      (push state final-states))))

(defmethod add-transition ((fsm finite-state-machine) from-state input to-state)
  (add-transition (transition-table fsm) from-state input to-state)
  (push (list from-state input to-state) (transitions fsm)))

(defclass mealy-automaton (finite-state-machine)
  ())

(defclass moore-automaton (finite-state-machine)
  ())

;;;


