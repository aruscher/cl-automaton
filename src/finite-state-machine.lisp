(in-package :cl-automaton)

(defclass transition-table ()
  ((table :type hash-table
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
  (alexandria:hash-table-keys (table transition-table)))

(defmethod add-state ((transition-table transition-table) state &optional is-final-p)
  (declare (ignore is-final-p))
  (with-slots (table input-test-f) transition-table
    (unless (has-state-p transition-table state)
      (setf (gethash state table)
	    (make-hash-table :test input-test-f)))))

(defmethod has-state-p ((transition-table transition-table) state)
  (gethash state (table transition-table)))

(defmethod transitions ((transition-table transition-table))
  (let ((transitions nil))
    (maphash
     (lambda (from-state state-table)
       (maphash
	(lambda (input to-states)
	  (dolist (to-state to-states)
	    (push (list from-state input to-state) transitions)))
	state-table))
     (table transition-table))
    transitions))

(defmethod add-transition ((transition-table transition-table) from-state input to-state)
  (assert (has-state-p transition-table from-state))
  (assert (has-state-p transition-table to-state))
  (with-slots (table) transition-table
    (let ((from-state-table (gethash from-state table)))
      (push to-state (gethash input from-state-table)))))

(defgeneric state-table (transition-table state))

(defmethod state-table ((transition-table transition-table) state)
  (gethash state (table transition-table)))

;;; --------------------

(defclass finite-state-machine (automaton)
  ((transition-table :reader transition-table
		     :type transition-table)
   (final-state-table :accessor final-state-table
		      :type hash-table)
   (current-state :initform nil
		  :accessor current-state)
   (init-state :initarg :init-state
	       :initform nil
	       :reader init-state)))


(defmethod initialize-instance :after ((fsm finite-state-machine) &key (final-states nil) (state-test-f #'eql) (input-test-f #'eql) (states nil) (transitions nil))
  (with-slots (current-state init-state transition-table final-state-table) fsm
    (setf transition-table
	  (make-instance 'transition-table :state-test-f state-test-f
					   :input-test-f input-test-f))
    (setf final-state-table
	  (make-hash-table :test state-test-f))
    (when init-state 
      (add-state fsm init-state)
      (setf current-state init-state))
    (dolist (state states)
      (add-state fsm state))
    (dolist (final-state final-states)
      (add-state fsm final-state t))
    (dolist (transition transitions)
      (apply #'add-transition fsm transition)))
  fsm)

(defmethod states ((fsm finite-state-machine))
  (states (transition-table fsm)))

(defmethod final-states ((fsm finite-state-machine))
  (alexandria:hash-table-keys (final-state-table fsm)))

(defmethod add-state ((fsm finite-state-machine) state &optional is-final-p)
  (declare (ignore is-final-p))
  (with-slots (transition-table) fsm
    (add-state transition-table state)))

(defmethod has-state-p ((fsm finite-state-machine) state)
  (has-state-p (transition-table fsm) state))

(defmethod final-state-p ((fsm finite-state-machine) state)
  (gethash state (final-state-table fsm)))

(defmethod mark-state-final ((fsm finite-state-machine) state)
  (with-slots (final-state-table) fsm
    (setf (gethash state final-state-table) t)))

(defmethod transitions ((fsm finite-state-machine))
  (transitions (transition-table fsm)))

(defmethod add-transition ((fsm finite-state-machine) from-state input to-state)
  (add-transition (transition-table fsm) from-state input to-state))


