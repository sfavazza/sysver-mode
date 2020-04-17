;;; sysver-mode.el --- Emacs mode to handle Varilog/System-Verilog files -*- lexical-binding: t -*-

;; Copyright (C) Samuele Favazza

;; Author: Samuele Favazza <sfavazza.github@gmail.com>
;; Version: 0.1
;; Package-Requires: 
;; Keywords: verilog, rtl, hdl, system-verilog, sv
;; URL: https://github.com/sfavazza/sysver-mode

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

;; A major-mode to develop Verilog and System-Verilog code.

;; It offers a robust and fast code-indentation based on code parsing (no reg-exp only).
;; To enable it simply type `M-x sysver-mode' on the buffer to work on.

;; For more information, read the sysver-mode documentation on the official repository page:

;; https://sysver-mode.readthedocs.io/en/latest/

;;; Code:

;; =================================================================================================
;; user customization

;; -------------------------------------------------------------------------------------------------
;; groups
(defgroup sysver nil
  "System-Verilog mode customization."
  :group 'languages)

(defgroup sysver-misc nil
  "System-Verilog mode miscellaneous options."
  :group 'sysver)
(defgroup sysver-indentation nil
  "System-Verilog mode indentation options."
  :group 'sysver)

;; -------------------------------------------------------------------------------------------------
;; options
(defcustom sysver-underscore-is-word-constituent t
  "If `non-nil' (default) the `forward-word' and `backward-word' commands will consider the `_'
character as part of a word. In `nil' the `_' character is considered as punctuation"
  :type 'boolean
  :group 'sysver-misc)

;; =================================================================================================
;; navigation functions
;; TODO

;; =================================================================================================
;; mode implementation

;; -------------------------------------------------------------------------------------------------
;; customize the fundamental syntax-table for this major mode
(defun sysver-define-syntax-table ()
  "Return this major-mode syntax table.
This is useful to let the user customize it via the customization options"
  (let ((table (make-syntax-table)))
    ;; operators symbols
    ;; (modify-syntax-entry ? "" table) TODO
    ;; word constituents
    (if sysver-underscore-is-word-constituent
        (modify-syntax-entry ?_ "w" table)
      (modify-syntax-entry ?_ "_" table))
    (modify-syntax-entry ?` "w" table)
    ;; comments setup
    ;; (modify-syntax-entry ? "" table) TODO

    ;; return the table object
    table))

;; -------------------------------------------------------------------------------------------------
;; key-words groups
(setq sysver-keywords-design-elements
      (list
       ;; start code-block
       "module" "program" "interface" "checker" "package" "primitive" "task" "function" "config"
       ;; end code-block
       "endmodule" "endprogram" "endinterface" "endchecker" "endpackage" "endprimitive" "endtask"
       "endfunction" "endconfig"))
(setq sysver-keywords-aggregate-data-types
      (list
       "packed" "void" "struct" "union" "tagged" "typedef" "new" "with" "inside"))
(setq sysver-keywords-classes
      (list
       "virtual" "extends" "implements" "pure" "extern" "static" "protected" "local"
       "rand" "randc"))
(setq sysver-keywords-struct-procedures-waits
      (list
       "always" "always_comb" "always_latch" "always_ff"
       "initial" "final"
       "wait" "wait_order" "disable"))
(setq sysver-keywords-block-statements
      (list
       "begin" "end" "for" "fork" "join"))
(setq sysver-keywords-assignments-statements
      (list
       "vectored" "scalared" "interconnect" "assign" ; continuous
       "deassign" "force" "release"
       "supply1" "strong1" "pull1" "weak1" "highz1"  ; strength
       "supply0" "strong0" "pull0" "weak0" "highz0"
       "default"                        ; patterns
       "alias"                          ; aliasing
       "null"
       ))

(setq sysver-keywords-operators
      (list
       "=" "\\+=" "\\-=" "\\*=" "/=" "%=" "&=" "|=" "\\^=" "<<=" ">>=" "<<<=" ">>>=" ; assignment
       ;; cond & unary & binary
       "?" "\\+" "\\-" "\\!" "~" "&" "~&" "|" "~|" "\\^" "~\\^" "\\^~"
       "*" "/" "%" "==" "\\!=" "===" "\\!==" "==?" "\\!=?" "&&" "||" "**"
       "<" "<=" ">" ">=" ">>>" "<<<"
       "->" "<->"
       ">>" "<<"                        ; stream
       "++" "--"                        ; inc & dec
       ))
(setq sysver-keywords-procedural-programming-statements
      (list
       "unique" "unique0" "priority"    ; unique priority
       "if" "else" "matches" "&&&"      ; if else
       "case" "casez" "casex" "endcase" ; case
       "forever" "repeat" "while" "for" "do" "foreach" ; loop
       "return" "break" "continue"
       ))
(setq sysver-keywords-compiler-directives
      (mapcar (lambda (karg) (concat "`" karg))
              (list
               "__FILE__" "__LINE__" "begin_keywords" "celldefine" "default_nettype"
               "define" "else" "elsif" "end_keywords" "endcelldefine" "endif" "ifdef" "ifndef"
               "include" "line" "nounconnected_drive" "pragma" "resetall" "timescale"
               "unconnected_drive" "undef" "undefineall")))


;; -------------------------------------------------------------------------------------------------
;; key-words organized by fontification levels

;; *** default

;; operators

;; processes

;;;###autoload
(define-derived-mode sysver-mode prog-mode "SysVer"
  "A major-mode to deal with Verilog and System-Verilog files."
  :group 'sysver

  (setq-local sysver-mode-syntax-table (sysver-define-syntax-table))
  (set-syntax-table sysver-mode-syntax-table)

  ;; define the code-highlighting
  ;; TODO
  ;; (setq font-lock-defaults
  ;;       ())
  )

(provide 'sysver-mode)
;; sysver.el ends here
