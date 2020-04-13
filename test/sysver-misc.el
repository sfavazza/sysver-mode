;;; sysver-misc.el --- test the miscellaneous options for sysver-mode

(require 'ert)
(require 'sysver-mode)

(ert-deftest sysver-test-underscore-part-of-word ()
  "Test the behavior of the underscore based on a major-mode option.

The test is performed by setting both possible options for `sysver-underscore-is-word-constituent'
and forwarding one word on the test string. The reached point positions are then compared against
the expected values."

  (let* ((test-string "this_is_a_word")
         (test-options-and-exp-results (list (cons t (1+ (length test-string)))
                                             (cons nil 5))))

    (while test-options-and-exp-results
      (with-temp-buffer

        (setq option-expresult (pop test-options-and-exp-results))
        (setq option (car option-expresult))
        (setq expresult (cdr option-expresult))

        ;; setup
        (setq-local sysver-underscore-is-word-constituent option)
        (sysver-mode)
        (insert test-string)
        (goto-char (point-min))

        ;; test
        (forward-word)

        ;; verify
        (should (equal (point) expresult))))))

