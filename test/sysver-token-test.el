;;; sysver-token-test.el --- test the indentation engine configuration

(require 'ert)
(require 'sysver)
(require 'sysver-common)

(ert-deftest sysver-test-default-token-search ()
  "Test the default token search behavior"

  (let* (
         ;; tokens list without the synthetic ones
         (real-tokens '("module"
                        "some_operators"
                        "\"the search function shall be able to interpret strings as a single token\""
                        ":" "<=" "++" "<" "==" "&&" ">>>"
                        "endmodule"
                        ;; the last is a comment which shall be included in a single SYNTOK
                        "// a commment to test the synthetic-token generation"))

         ;; list of all expected tokens: the REAL-TOKENS list interleaved with SYNTOK-SPACING tokens
         (exp-tokens (let (full-tok-list '())
                       (dolist (token real-tokens full-tok-list)
                         (setq full-tok-list (append full-tok-list `(,token ,SYNTOK-SPACING))))
                       ;; Refine the output list as the last comment shall be matched together with the previous
                       ;; space character (hence the obtained list less the last tokens).
                       (setq full-tok-list (butlast full-tok-list 2))))

         ;; turn the REAL-TOKENS list into a space-interleaved-list
         (current-string (mapconcat 'identity real-tokens " "))

         ;; create an empty list for the reverse list of the expected token
         (exp-tokens-reverse))

    ;; create also the reversed list to test the backward search token function
    (dolist (tok exp-tokens exp-tokens-reverse)
      (setq exp-tokens-reverse (cons tok exp-tokens-reverse)))

    (with-temp-buffer
      (sysver-utc-environment
       ;; test-string
       current-string

       ;; no parameters to be tested
       ()

       ;; verify step
       ((goto-char (point-min))

        ;; -------------------------------------------------------------------------------------------
        (info-msg "testing the FORWARD basic token search function")
        ;; walk trough the expected token list...
        (while exp-tokens
          (should (equal (sysver-basic-forward-token) (pop exp-tokens))))
        ;; ...and verify that the other extreme of the string is reached, this ensures that no tokens
        ;; were skipped
        (should (eobp))

        ;; -------------------------------------------------------------------------------------------
        (info-msg "testing the BACKWARD basic token search function")
        ;; not test the default reverse
        (while exp-tokens-reverse
          (message "point max %d - current point %d" (point-max) (point))
          (dolist (exp-tok exp-tokens-reverse)
            (message "token: %s" exp-tok))
          (should (equal (sysver-basic-backward-token) (pop exp-tokens-reverse)))))))))
