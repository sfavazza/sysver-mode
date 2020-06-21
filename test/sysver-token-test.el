;;; sysver-token-test.el --- test the indentation engine configuration

(require 'ert)
(require 'sysver)
(require 'sysver-common)
(require 'cl-lib)

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
         (exp-tokens (let ((full-tok-list '()))
                       ;; interleave the expected tokens list with SYNTOK-SPACING
                       (dolist (token real-tokens full-tok-list)
                         (setq full-tok-list (append full-tok-list `(,token ,SYNTOK-SPACING))))
                       ;; Refine the output list:
                       ;; - the string shall be returned as a SYNTOK-STRING token
                       ;; - the last comment shall be part of the previous space, hence remove the last 2 tokens
                       (setq full-tok-list (append (cl-subseq full-tok-list 0 4)
                                                   `(,SYNTOK-STRING)
                                                   (cl-subseq full-tok-list 5 (- (length full-tok-list) 2))))))

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

(ert-deftest sysver-test-gen-syntok-space ()
  "Test the behavior of the basic token search functions, when:

- right before a comment delimiter
- in the middle of a comment"

  (let ((tok-search-functions '(sysver-basic-forward-token
                                sysver-basic-backward-token))
        (buffer-extremes-list '(eobp bobp))
        search-func
        buf-limit-p)

    (while tok-search-functions

      ;; pop test symbols
      (fset 'search-func (pop tok-search-functions))
      (fset 'buf-limit-p (pop buffer-extremes-list))

      (with-temp-buffer
        (sysver-utc-environment
         ;; test string
         "   // a long comment with fancy ch@ract3rs  "

         ;; no parameters to be tested
         ()

         ;; verify steps
         (
          ;; before a comment
          (info-msg "testing the `%s' function before a comment delimiter" (symbol-function 'search-func))
          (skip-syntax-forward " ")
          (should (equal (search-func) SYNTOK-SPACING))
          (should (buf-limit-p))

          ;; in the middle of a comment
          (info-msg "testing the `%s' function in the middle of a comment" (symbol-function 'search-func))
          (goto-char (/ (buffer-size) 2))
          (should (equal (search-func) SYNTOK-SPACING))
          (should (buf-limit-p))))))))

(ert-deftest sysver-test-gen-syntok-string ()
  "Test the behavior of the basic token search functions, when:

- right before a string delimiter
- in the middle of a string
- right after a string delimiter"

  (let ((tok-search-functions '(sysver-basic-forward-token
                                sysver-basic-backward-token))
        search-func)

    (while tok-search-functions

      ;; pop test symbols
      (fset 'search-func (pop tok-search-functions))

      (with-temp-buffer
        (sysver-utc-environment
         ;; test string
         " \"a long string object to generate a single token\" "

         ;; no parameters to be tested
         ()

         ;; verify steps
         (
          ;; before a string
          (when (eq #'sysver-basic-forward-token (symbol-function 'search-func))
            (info-msg "testing the `%s' function before a string delimiter" (symbol-function 'search-func))
            (skip-syntax-forward " ")
            (should (equal (search-func) SYNTOK-STRING)))

          ;; in the middle of a string
          (info-msg "testing the `%s' function in the middle of a string" (symbol-function 'search-func))
          (goto-char (/ (buffer-size) 2))
          (should (equal (search-func) SYNTOK-STRING))

          ;; after a string
          (when (eq #'sysver-basic-backward-token (symbol-function 'search-func))
            (goto-char (point-max))
            (skip-syntax-backward " ")
            (should (equal (search-func) SYNTOK-STRING)))
          ))))))
