;;; sysver-test-common.el --- general definitions for various UTCs (Unit-Test-Case)

;; NOTE: could not find a way to step-through a macro, there is a way to let edebug how to instrument your macro
;; via a debug-declaration, but it's too complex.
;; The easy work around is to expand the macro by placing the cursor right before the macro-call and execute the
;; command: emacs-lisp-macroexpand
;; After the debug is over, the buffer should be reverted to collapse the macro again.
(defmacro sysver-utc-environment (test-string setup-to-test test-body)
  "Freshly reload the `sysver' major mode and setup the environment for an unit-test-case (UTC).

The TEST-STRING is a string to be inserted into the current buffer.
The SETUP-TO-TEST should be an unquoted list of statements to setup the feature under test, while
the TEST-BODY is an unquoted list of statements to verify the tested feature."
  (declare (debug (form sexp sexp)))

  `(progn
     ;; reload sysver
     (unload-feature 'sysver t)
     (load-file "../sysver.el")

     ;; add the setup features to be tested
     ,@setup-to-test

     ;; enable sysver
     (sysver-mode)
     (insert ,test-string)
     (goto-char (point-min))
     (font-lock-fontify-buffer)

     ,@test-body))

(defun str-compare-test (test-string)
  "Compare the given TEST-STRING string against the current buffer content."
  (should (equal (buffer-substring-no-properties (point-min) (point-max))
                 test-string)))

(defun info-msg (format-string &rest args)
  (message (concat "INFO: " format-string) args))

(provide 'sysver-common)
