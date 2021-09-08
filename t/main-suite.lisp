(in-package #:cl-automaton/tests)

(def-suite cl-automaton-suite)

(def-suite protocol-suite
  :in cl-automaton-suite)

(def-suite finite-state-machine-suite
  :in protocol-suite)

(defun run-tests ()
  (run! 'cl-automaton-suite))
