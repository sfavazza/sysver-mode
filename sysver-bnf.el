;;; sysver-bnf.el

;; Copyright (C) Samuele Favazza

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains the description of the System-Verilog (Verilog) syntax for the sole purpose to
;; handle the indentation engine. As indentation is the only goal, the BNF description contains only
;; the rules required to beautify the code.

;;; Code:

(require 'smie)
(require 'sysver-options)

;; TODO: consider the following handy function:
;; Command: smie-close-block
;;     This command closes the most recently opened (and not yet closed) block. 
;;
;; Command: smie-down-list &optional arg
;;     This command is like down-list but it also pays attention to nesting of tokens other than
;;     parentheses, such as begin...end.

;; -------------------------------------------------------------------------------------------------
;; synthetic tokens
(defvar SYNTOK-PARAMS-START-LIST "SYNTOK-PARAMS-START-LIST")
(defvar SYNTOK-ASSIGN-SEMICOLON-CLOSER "SYNTOK-ASSIGN-SEMICOLON-CLOSER")
(defvar SYNTOK-SPACING "SYNTOK-SPACING")
(defvar SYNTOK-STRING "SYNTOK-STRING")

;; token search functions

;; NOTE: normally when the default token search function returns `nil' or an `empty-string', SMIE
;; tries to obtain the token using the local syntax-table. Because the System-Verilog / Verilog
;; grammar is not BNF-friendly (it contains consecutive non-terminal), the token search functions
;; should also consider the comments and/or spaces between any two tokens (either terminal or
;; non-terminal) as synthetic-token candidates. The following are customized versions of SMIE
;; default token search functions optimized for sysver-mode.
(defun sysver-basic-forward-token ()
  "Default forward search token function based on the syntax classes."

  ;; modify the default forward token function such that it returns a token even for
  ;; parenthesis characters "(" ")"
  (progn
    (let* ((start-pnt (point))
           (syntax-state (syntax-ppss start-pnt))
           ;; do not try to get the state of the incremented point when at the end of the buffer
           (syntax-state-next (if (not (eobp))
                                  (save-excursion
                                    (syntax-ppss (1+ start-pnt)))
                                syntax-state))) ; it is expected a list so recycle the previous one

      (cond
       ;; the token search must stop when reaching the end of the buffer
       ((eobp) nil)

       ;; when inside a comment the `forward-comment' returns always `nil', hence use the syntax
       ;; state to skip it
       ((nth 4 syntax-state)
        (goto-char (nth 8 syntax-state)) ; to the comment start to easily skip it
        ;; move forward skipping comments and space characters
        (forward-comment (point-max))
        SYNTOK-SPACING)

       ;; FIXME: the string case fails to leave a string context when the current string contains
       ;; the escaped apexes.

       ;; When inside of a string or just before a string opener character, a string-token shall be
       ;; returned.
       ((or (nth 3 syntax-state) (nth 3 syntax-state-next))
        ;; skip all the characters inside the string (^ negate the class to be skipped)
        (skip-syntax-forward "\"")      ; move inside the string when placed just before its opener
        (skip-syntax-forward "^\"")     ; reach the string closer
        (skip-syntax-forward "\"")      ; move after the string closer
        SYNTOK-STRING)

       ;; as not inside a comment or string search the next token based on the syntax class (after
       ;; skipping comments and white spaces)
       (t
        (forward-comment (point-max))

        (if (> (point) start-pnt)
            ;; we just skipped a space-comments group, hence the spacing-token is returned, the caller of this
            ;; function will refine the search
            SYNTOK-SPACING
          (buffer-substring-no-properties
           (point)
           (cond
            ((and (zerop (skip-syntax-forward "."))
                  (zerop (skip-syntax-forward "("))
                  (zerop (skip-syntax-forward ")")))
             (skip-syntax-forward "w_'")
             (point))
            (t (point))))))))))
(defun sysver-basic-backward-token ()
  "Default backward search token function based on the syntax classes."

  ;; NOTE: all explanations and comments are the same as for the `sysver-basic-forward-token'
  (progn
    (let* ((start-pnt (point))
           (syntax-state (syntax-ppss start-pnt))
           (syntax-state-prev (if (not (bobp))
                                  (save-excursion
                                    (syntax-ppss (1- start-pnt)))
                                syntax-state)))

      (cond
       ;; the token search must stop when reaching the start of the buffer
       ((bobp) nil)

       ;; inside a comment
       ((nth 4 syntax-state)
        ;; move to the comment beginning as "forward-comment" does not work from inside it
        (goto-char (nth 8 syntax-state))
        ;; move backward skipping comments and space characters
        (forward-comment (- (point)))
        SYNTOK-SPACING)

       ;; FIXME: the string case fails to leave a string context when the current string contains
       ;; the escaped apexes.

       ;; When inside of a string or just before a string opener character, a string-token shall be
       ;; returned.
       ((or (nth 3 syntax-state) (nth 3 syntax-state-prev))
        ;; skip all the characters inside the string (^ negate the class to be skipped)
        (skip-syntax-backward "\"")      ; move inside the string when placed just before its opener
        (skip-syntax-backward "^\"")     ; reach the string closer
        (skip-syntax-backward "\"")      ; move after the string closer
        SYNTOK-STRING)

       ;; elsewhere
       (t
        (forward-comment (- (point)))

        (if (< (point) start-pnt)
            SYNTOK-SPACING
          (buffer-substring-no-properties
           (point)
           (cond
            ((and (zerop (skip-syntax-backward "."))
                  (zerop (skip-syntax-backward "("))
                  (zerop (skip-syntax-backward ")")))
             (skip-syntax-backward "w_'")
             (point))
            (t (point))))))))))

;; Through the token search, the indentation can overcome the BNF grammar limits and
;; return "synthetic" tokens to ease the grammar definition.
(defun sysver-forward-token ()
  "Return the next found token and move point to its end."
  (let ((smie-token (sysver-basic-forward-token)))

    ;; As the default token search functions are syntax-table based they cannot find the "#(" token,
    ;; hence they are made smarter to generate a synthetic token to indicate the presence of "#(".
    ;; The same hold true for the `sysver-backward-token' function.
    (if (and (not (eobp))
             (string= (buffer-substring-no-properties (1- (point)) (1+ (point)))
                      "#("))
        (progn
          (goto-char (1+ (point)))
          smie-syntoken-params-start-list) ; synthetic token
      smie-token)))                        ; default token
(defun sysver-backward-token ()
  "Return the previous token and move point to its beginning."
  (let ((smie-token (sysver-basic-backward-token)))

    ;; check the comment description of `sysver-forward-token' for more details.
    (if (and (not (bobp))
             (string= (buffer-substring-no-properties (- (point) 2) (point))
                      "#("))
        (progn
          (goto-char (- (point) 2))
          smie-syntoken-params-start-list) ; synthetic token
      smie-token)))                        ; default token

;; -------------------------------------------------------------------------------------------------
;; grammar definition

;; resolvers: they aim at solving operator conflicts. Every time the engine finds a sequence of
;; tokens with similar behavior (like separators) a resolver shall make the precedence unambiguous.
;; The conflicts warnings are reported by the `smie-set-prec2tab' function which 
(setq sysver-resolver-assignments
      (mapcar (lambda (x) `(assoc ,x))
              '("=" "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "<<=" ">>=" "<<<=" ">>>=")))
(setq sysver-resolver-operators
      (mapcar (lambda (x) `(assoc ,x))
              '("?" "+" "-" "!" "~" "&" "~&" "|" "~|" "^" "~^" "^~"
                "*" "/" "%" "==" "!=" "===" "!==" "==?" "!=?" "&&" "||" "**"
                "<" "<=" ">" ">=" ">>>" "<<<"
                "->" "<->" "##"
                ">>" "<<"
                "++" "--")))

;; grammar
(defconst sysver-smie-grammar  
  (smie-prec2->grammar
   (smie-bnf->prec2
    ;; BNF: list of nonterminal definitions with the form (NONTERM RHS1 RHS2 ...)
    `((id)                                 ; any identifier
      ;; module description
      (mod_descr ("module" statements "endmodule"))
      ;; (mod_body (id ";" id))
      ;; expressions
      (assignment ("assign" statements)
                  (statements))
      (statements (statement ";" statement))
      (statement (id "=" exprs)
                 (id "<=" exprs)
                 (id "+=" exprs)
                 (id "-=" exprs)
                 (id "*=" exprs)
                 (id "/=" exprs)
                 (id "%=" exprs)
                 (id "&=" exprs)
                 (id "|=" exprs)
                 (id "^=" exprs)
                 (id "<<=" exprs)
                 (id ">>=" exprs)
                 (id "<<<=" exprs)
                 (id ">>>=" exprs))
      (exprs (exprs "?" exprs)
             (exprs "+" exprs)
             (exprs "-" exprs)
             (exprs "!" exprs)
             (exprs "~" exprs)
             (exprs "&" exprs)
             (exprs "~&" exprs)
             (exprs "|" exprs)
             (exprs "~|" exprs)
             (exprs "^" exprs)
             (exprs "~^" exprs)
             (exprs "^~" exprs)
             (exprs "*" exprs)
             (exprs "/" exprs)
             (exprs "%" exprs)
             (exprs "==" exprs)
             (exprs "!=" exprs)
             (exprs "===" exprs)
             (exprs "!==" exprs)
             (exprs "==?" exprs)
             (exprs "!=?" exprs)
             (exprs "&&" exprs)
             (exprs "||" exprs)
             (exprs "**" exprs)
             (exprs "<" exprs)
             (exprs "<=" exprs)
             (exprs ">" exprs)
             (exprs ">=" exprs)
             (exprs ">>>" exprs)
             (exprs "<<<" exprs)
             (exprs "->" exprs)
             (exprs "<->" exprs)
             (exprs "##" exprs)
             (exprs ">>" exprs)
             (exprs "<<" exprs)
             (exprs "++" exprs)
             (exprs "--" exprs)
             (params_list)              ; list of parameters
             (id))
      ;; parameter lists
      (params_list ("(" params ")")
                   (,smie-syntoken-params-start-list params ")"))
      (params (params "," params)
              (id))
      )
    ;; RESOLVERS: list of precs tables (left, right, assoc, nonassoc), more tokens in the same precs
    ;; share the same precedence. The precs are sorted by precedence, the first below has the
    ;; highest precedence.
    '((assoc ","))
    sysver-resolver-assignments
    sysver-resolver-operators
    '((assoc ";"))
    ))
  "Define a grammar for the language to be given to SMIE")

;; indentation rules
(defun sysver-smie-rules (kind token)

  ;; -----------------------------------------------------------------------------------------------
  ;; rule behavior explanation:
  ;; -----------------------------------------------------------------------------------------------
  ;; "before": refers to the token on the current line and its own indentation
  ;; "after": refers to the previous line token and the indentation of the current line
  ;; "list-intro": rule used to indent a list of expression NOT interleaved by any token, usually
  ;;               used for an arg-list of a function for example:
  ;;               (          <-- the first token would be "("
  ;;                 a        <-- apply the rule :list-intro "("
  ;;                 b        <-- here will just apply the default indentation "smie-indent-basic"
  ;;                 c
  ;;                 d
  ;;                 )
  
  ;; the following sequence of rules are always run in sequence until the first one returns non-nil
  (cond
   ;; default rules
   ;; ('(:elem . args)  sysver-default-indent)
   ;; ('(:elem . basic) sysver-default-indent)
   ((and (eq kind :elem) (equal token 'empty-line-token))
    0)

   ;; ----------------------------------------------------------------------------------------------
   ;; module structure
   ((and (eq kind :before) (member token `("(" ,smie-syntoken-params-start-list)))
    sysver-default-indent)
   ((and (eq kind :before) (equal token ","))
    ;; align to the end of "#(" and "(" start-list-delimiters
    (save-excursion
      ;; nil if not inside a list, t otherwise
      (if (condition-case nil
              ;; return true if point is inside a parameters-list
              (progn (backward-up-list) t)
            (scan-error nil))
          (cond
           ((member (sysver-forward-token) `(,smie-syntoken-params-start-list "("))
            `(column . ,(current-column)))
           ;; ((equal (sysver-forward-token) "(")
           ;;  1)
           ;; keep the default indentation otherwise
           (t nil)))))
   ((and (eq kind :after) (member token `(,smie-syntoken-params-start-list "(")))
    (save-excursion
      (sysver-forward-token)
      `(column . ,(current-column))))
   ((and (eq kind :before) (equal token "endmodule"))
    0)

   ;; begin-end blocks

   ;; loops (for, while)

   ;; case

   ;; as all these rules are applied in order and the function exits as the first returns non-nil,
   ;; here the default option is placed
   ;; (t sysver-default-indent)
   )

  ;; (pcase (cons method arg)
  ;;   ('(:before . "endmodule") 0)
  ;;   ('(:before . ";") sysver-default-indent)
  ;;   )
  )

(provide 'sysver-bnf)
