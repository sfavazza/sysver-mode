;;; sysver-test-common.el --- general definitions for various UTCs (Unit-Test-Case)

(defmacro sysver-utc-environment (test-string setup-to-test test-body)
  "Freshly reload the `sysver' major mode and setup the environment for an unit-test-case (UTC).

The TEST-STRING is a string to be inserted into the current buffer.
The SETUP-TO-TEST should be an unquoted list of statements to setup the feature under test, while
the TEST-BODY is an unquoted list of statements to verify the tested feature."

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

(provide 'sysver-common)
