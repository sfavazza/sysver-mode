;;; sysver-misc.el --- test the miscellaneous options for sysver-mode

(require 'ert)
(require 'sysver-mode)

(ert-deftest sysver-test-underscore-part-of-word-on ()
  "Test the behavior of the underscore based on a major-mode option.

The test is performed by setting both possible options for `sysver-underscore-is-word-constituent'
and forwarding one word on the test string. The reached point positions are then compared against
the expected values."

  (let* ((test-string "is_this_a_word")
         (options (list nil t))
         (exp-results (list 3 (1+ (length test-string)))))

    (while options
      (with-temp-buffer

        ;; setup
        (unload-feature 'sysver-mode t)
        (load-file "../sysver-mode.el")
        (setq sysver-underscore-is-word-constituent (pop options))
        (sysver-mode)
        (insert test-string)
        (goto-char (point-min))

        ;; test
        (forward-word)

        ;; verify
        (should (equal (point) (pop exp-results))))))


  ;; ensure the other tests are not affected
  (require 'sysver-mode))
