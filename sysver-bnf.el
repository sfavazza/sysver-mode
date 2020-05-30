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



;; TODO: consider the following handy function:
;; Command: smie-close-block
;;     This command closes the most recently opened (and not yet closed) block. 
;;
;; Command: smie-down-list &optional arg
;;     This command is like down-list but it also pays attention to nesting of tokens other than
;;     parentheses, such as begin...end.

;; synthetic tokens
(defvar smie-syntoken-params-start-list "PARAMS-START-LIST")

;; token search functions
;; NOTE: the following are customized versions of SMIE token search functions optimized for
;; sysver-mode. Through the token search, the indentation can overcome the BNF grammar limits and
;; return "synthetic" tokens to ease the grammar definition.
(defun sysver-forward-token ()
  "Return the next found token and move point to its end."
  (let ((smie-token (smie-default-forward-token)))
    (if (and (not (eobp))
             (string= (buffer-substring-no-properties (1- (point)) (1+ (point)))
                      "#("))
        (progn
          (goto-char (1+ (point)))
          smie-syntoken-params-start-list) ; synthetic token
      smie-token)))                        ; default token

(defun sysver-backward-token ()
  "Return the previous token and move point to its beginning."
  (let ((smie-token (smie-default-backward-token)))

    ;; The default token search function returns punctuation (syntax table class ".") and symbols
    ;; (syntax table class "w_"). As such the "#(" token cannot be found, hence this function is
    ;; made smarter to generate a synthetic token to indicate the presence of "#(".
    (if (and (not (bobp))
             (string= (buffer-substring-no-properties (- (point) 2) (point))
                      "#("))
        (progn
          (goto-char (- (point) 2))
          smie-syntoken-params-start-list) ; synthetic token
      smie-token)                       ; default token
    ))

;; grammar definition
(defconst sysver-smie-grammar  
  (smie-prec2->grammar
   (smie-bnf->prec2
    ;; BNF: list of nonterminal definitions with the form (NONTERM RHS1 RHS2 ...)
    `((id)                                 ; any identifier
      ;; module description
      (mod_descr ("module" mod_body "endmodule"))
      (mod_body (id ";" id))
      ;; (mod_body (id "#(" params ")")
      ;;           (id "(" params ")"))
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
(setq smie-indent-basic 2)            ; big to be sure it has an effect
(defun sysver-smie-rules (method arg)

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
  
  ;; the following sequence of rules are always run in sequence until the first one returning non-nil
  (pcase (cons method arg)

    ;; default
    ('(:elem . args)  smie-indent-basic)
    ;; ('(:elem . basic) smie-indent-basic)
    ('(:elem . empty-line-token) 0)

    ;; ('(:list-intro . "module") t)
    ('(:after . "module") smie-indent-basic)
    ('(:before . "endmodule") 0)
    ('(:before . ";") smie-indent-basic)
    ('(:before . "(") smie-indent-basic)
    (`(:before . ,smie-syntoken-params-start-list) smie-indent-basic)
    
    ))

(provide 'sysver-bnf)
