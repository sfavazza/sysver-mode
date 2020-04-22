;;; sysver-misc.el --- test the miscellaneous options for sysver-mode

(require 'ert)
(require 'sysver-mode)

(defmacro sysver-utc-environment (test-string setup-to-test test-body)
  "Freshly reload the `sysver' major mode and setup the environment for an unit-test-case (UTC).

The TEST-STRING is a string to be inserted into the current buffer.
The SETUP-TO-TEST should be an unquoted list of statements to setup the feature under test, while
the TEST-BODY is an unquoted list of statements to verify the tested feature."

  `(progn
     ;; reload sysver
     (unload-feature 'sysver-mode t)
     (load-file "../sysver-mode.el")

     ;; add the setup features to be tested
     ,@setup-to-test

     ;; enable sysver
     (sysver-mode)
     (insert test-string)
     (goto-char (point-min))
     (font-lock-fontify-buffer)

     ,@test-body))

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

        (sysver-utc-environment

         test-string

         ;; feature to test
         ((setq sysver-underscore-is-word-constituent (pop options)))

         (;; test
          (forward-word)

          ;; verify
          (should (equal (point) (pop exp-results)))))))))

(ert-deftest sysver-test-emphasize-operators ()
  "Test that the emphasize highlighting for the operators is effective.

A string with the target elements to be highlighted is created and the `sysver-emphasize-operators'
assigned. The result is checked by evaluating the face-property change of the target string."

  ;; all operators are tested by cycling through their list
  (let ((operator-list (list
                        "=" "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "<<=" ">>=" "<<<=" ">>>="
                        "?" "+" "-" "!" "~" "&" "~&" "|" "~|" "^" "~^" "^~"
                        "*" "/" "%" "==" "!=" "===" "!==" "==?" "!=?" "&&" "||" "**"
                        "<" "<=" ">" ">=" ">>>" "<<<" "->" "<->" "##" ">>" "<<" "++" "--")))

    (while operator-list
      (let* ((test-string (concat "### " (pop operator-list) " "))
             (options '(nil t))
             (exp-face '(nil font-lock-constant-face))
             (exp-points `(,(1+ (length test-string)) 4)))

        (while options
          (with-temp-buffer

            (sysver-utc-environment

             test-string

             ;; feature to test
             ((setq sysver-emphasize-operators (pop options)))

             (;; test
              (goto-char (next-single-property-change (point) 'face nil (point-max)))

              ;; verify
              (should (equal (point) (pop exp-points))) ; next reached point
              (should (equal
                       (get-text-property (point) 'face)
                       (pop exp-face)))))))))))

(ert-deftest sysver-test-emphasize-block-statements ()
  "test that the emphasize highlight for the block statements delimiters is effective.

The face property of the `begin'/ `end' keywords are evaluated when emphasis is enabled or not."

  (let ((test-string "  end begin ")
        (options '(nil t))
        (exp-faces '(font-lock-type-face font-lock-constant-face)))

    (while options
      (with-temp-buffer
        (sysver-utc-environment

         test-string

         ;; feature to test
         ((setq sysver-emphasize-block-statements (pop options)))

         ;; test & verify
         ((goto-char (point-max))
          (backward-word)
          (should (equal (get-text-property (point) 'face)
                         (pop exp-faces)))))))))
