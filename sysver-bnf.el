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
(defvar smie-syntoken-params-start-list "PARAMS-START-LIST")

;; token search functions

;; NOTE: normally when the default token search function returns `nil' or an `empty-string', SMIE
;; tries to obtain the token using the local syntax-table. The following are customized versions of
;; SMIE default token search functions optimized for sysver-mode.
(defun sysver-basic-forward-token ()
  "Forward search token function based on the syntax class."

  ;; modify the default forward token function such that it returns a token even for
  ;; parenthesis characters "(" ")"
  (progn
    (forward-comment (point-max))
    (buffer-substring-no-properties
     (point)
     (progn (if (and (zerop (skip-syntax-forward "."))
                     (zerop (skip-syntax-forward "("))
                     (zerop (skip-syntax-forward ")")))
                (skip-syntax-forward "w_'"))
            (point)))))
(defun sysver-basic-backward-token ()
  "Backward search token function based on the syntax class."
  (progn
    (forward-comment (- (point)))
    (buffer-substring-no-properties
     (point)
     (progn (if (and (zerop (skip-syntax-backward "."))
                     (zerop (skip-syntax-backward "("))
                     (zerop (skip-syntax-backward ")")))
                (skip-syntax-backward "w_'"))
            (point)))))

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

;; grammar definition
(defconst sysver-smie-grammar  
  (smie-prec2->grammar
   (smie-bnf->prec2
    ;; BNF: list of nonterminal definitions with the form (NONTERM RHS1 RHS2 ...)
    `((id)                                 ; any identifier
      ;; module description
      (mod_descr ("module" mod_body "endmodule"))
      (mod_body (id ";" id))
      (params_list ("(" params ")")
                   (,smie-syntoken-params-start-list params ")"))
      (params (params "," params)
              (id))
      )
    ;; RESOLVERS: list of precs tables (left, right, assoc, nonassoc), more tokens in the same precs
    ;; share the same precedence. The precs are sorted by precedence, the first below has the
    ;; highest precedence.
    '((assoc ","))
    ))
  "Define a grammar for the language to be given to SMIE")

;; indentation rules
(setq smie-indent-basic sysver-default-indent)            ; big to be sure it has an effect
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
   ;; ('(:elem . args)  smie-indent-basic)
   ;; ('(:elem . basic) smie-indent-basic)
   ((and (eq kind :elem) (equal token 'empty-line-token))
    0)

   ;; ----------------------------------------------------------------------------------------------
   ;; module structure
   ((and (eq kind :before) (member token `("(" ,smie-syntoken-params-start-list)))
    smie-indent-basic)
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
   )

  ;; (pcase (cons method arg)
  ;;   ('(:before . "endmodule") 0)
  ;;   ('(:before . ";") smie-indent-basic)
  ;;   )
  )

(provide 'sysver-bnf)
