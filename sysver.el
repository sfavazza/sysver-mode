;;; sysver.el --- Emacs mode to handle Varilog/System-Verilog files -*- lexical-binding: t -*-

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

(require 'sysver-options)
(require 'sysver-bnf)

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
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?\' "." table)
    ;; word constituents
    (if sysver-underscore-is-word-constituent
        (modify-syntax-entry ?_ "w" table)
      (modify-syntax-entry ?_ "_" table))
    (modify-syntax-entry ?` "w" table)  ; part of a symbol definition
    ;; comments setup
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?\n ">" table)

    ;; return the table object
    table))

;; -------------------------------------------------------------------------------------------------
;; key-words groups
(setq sysver-keywords-design-elements
      (list
       ;; start code-block
       "module" "program" "interface" "checker" "package" "primitive" "task" "function" "config"
       "fork"
       ;; end code-block
       "endmodule" "endprogram" "endinterface" "endchecker" "endpackage" "endprimitive" "endtask"
       "endfunction" "endconfig" "join" "join_none" "join_any"))
(setq sysver-keywords-aggregate-data-types
      (list
       "packed" "void" "struct" "union" "tagged" "typedef" "new" "with" "inside"))
(setq sysver-keywords-classes
      (list
       "virtual" "extends" "implements" "pure" "extern" "static" "protected" "local"))
(setq sysver-keywords-struct-procedures-waits
      (list
       "always" "always_comb" "always_latch" "always_ff"
       "initial" "final"
       "wait" "wait_order" "disable"))
(setq sysver-keywords-block-statements
      (list
       "begin" "end"))
(setq sysver-keywords-assignments-statements
      (list
       "vectored" "scalared" "interconnect" "assign" ; continuous
       "deassign" "force" "release"
       "supply1" "strong1" "pull1" "weak1" "highz1"  ; strength
       "supply0" "strong0" "pull0" "weak0" "highz0"
       "default"                        ; patterns
       "alias"                          ; aliasing
       "null"))
(setq sysver-keywords-operators
      (list
       "=" "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "<<=" ">>=" "<<<=" ">>>=" ; assignment
       ;; cond & unary & binary
       "?" "+" "-" "!" "~" "&" "~&" "|" "~|" "^" "~^" "^~"
       "*" "/" "%" "==" "!=" "===" "!==" "==?" "!=?" "&&" "||" "**"
       "<" "<=" ">" ">=" ">>>" "<<<"
       "->" "<->" "##"
       ">>" "<<"                        ; stream
       "++" "--"))                      ; inc & dec
(setq sysver-keywords-procedural-programming-statements
      (list
       "unique" "unique0" "priority"                   ; unique priority
       "if" "else" "iff" "matches" "&&&"               ; if else
       "case" "casez" "casex" "endcase"                ; case
       "forever" "repeat" "while" "for" "do" "foreach" ; loop
       "return" "break" "continue"))
(setq sysver-keywords-clocking
      (list
       "clocking" "global clocking" "endclocking" ; clocking block
       "edge" "posedge" "negedge"))
(setq sysver-keywords-assertions
      (list
       "assert" "assume" "cover" "restrict" "except"))
(setq sysver-keywords-constrained-random-gen
      (list
       "rand" "randc" "constraint" "solve" "soft" "before" "randsequence" "endsequence" "repeat"))
(setq sysver-keywords-functional-coverage
      (list
       "covergroup" "endgroup" "option" "type_option" "sample" "coverpoint" "cross"))
(setq sysver-keywords-system-utilities
      (mapcar (lambda (karg) (concat "$" karg))
              (list
               "finish" "stop" "fatal" "error" "exit" "warning" "info" "realtime" "stime"
               "asserton" "assertoff" "time" "assertkill" "assertcontrol" "assertpasson"
               "assertpassoff" "assertfailon" "assertfailoff" "printtimescale" "timeformat"
               "assertnonvacuouson" "assertvacuousoff" "bitstoreal" "realtobits" "sampled" "rose"
               "bitstoshortreal" "shortrealtobits" "fell" "stable" "itor" "rtoi" "changed" "past"
               "signed" "unsigned" "past_gclk" "rose_gclk" "cast" "fell_gclk" "stable_gclk"
               "changed_gclk" "future_gclk" "rising_gclk" "falling_gclk" "bits" "isunbounded"
               "steady_gclk" "changing_gclk" "typename" "coverage_control" "coverage_get_max"
               "unpacked_dimensions" "dimensions" "coverage_get" "coverage_merge" "left" "right"
               "coverage_save" "get_coverage" "low" "high" "set_coverage_db_name" "load_coverage_db"
               "increment" "size" "random" "dist_chi_square" "clog2" "asin" "dist_erlang"
               "dist_exponential" "ln" "acos" "dist_normal" "dist_poisson" "log10" "atan" "dist_t"
               "dist_uniform" "exp" "atan2" "sqrt" "hypot" "pow" "sinh" "q_initialize" "q_add"
               "floor" "cosh" "q_remove" "q_full" "ceil" "tanh" "q_exam" "sin" "asinh" "cos"
               "acosh" "tan" "atanh" "async" "and" "array" "async" "and" "plane" "async" "nand"
               "array" "async" "nand" "plane" "async" "or" "array" "async" "or" "plane"
               "countbits" "countones" "async" "nor" "array" "async" "nor" "plane" "onehot"
               "onehot0" "sync" "and" "array" "sync" "and" "plane" "isunknown" "sync" "nand"
               "array" "sync" "nand" "plane" "sync" "or" "array" "sync" "or" "plane" "sync"
               "nor" "array" "sync" "nor" "plane" "fatal" "error" "warning" "info" "system")))
(setq sysver-keywords-system-io
      (mapcar (lambda (karg) (concat "$" karg))
              (list
               "display" "displayb" "displayh" "displayo" "strobe" "strobeb" "strobeh" "strobeo"
               "fdisplay" "fdisplayb" "fdisplayh" "fdisplayo" "fstrobe" "fstrobeb" "fstrobeh"
               "fstrobeo" "swrite" "swriteb" "swriteh" "swriteo" "fscanf" "fread" "fseek"
               "fflush" "feof" "write" "writeb" "writeh" "writeo" "monitor" "monitorb"
               "monitorh" "monitoro" "monitoroff" "monitoron" "fopen" "fclose" "fwrite" "fwriteb"
               "fwriteh" "fwriteo" "fmonitor" "fmonitorb" "fmonitorh" "fmonitoro" "sformat"
               "sformatf" "fgetc" "ungetc" "fgets" "sscanf" "rewind" "ftell" "ferror"
               "readmemb" "readmemh" "writememb" "writememh" "test" "plusargs" "value"
               "plusargs" "dumpfile" "dumpoff" "dumpall" "dumpflush" "dumpportsoff"
               "dumpportsall" "dumpportsflush" "dumpvars" "dumpon" "dumplimit" "dumpports"
               "dumpportson" "dumpportslimit")))
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

  ;; define the comment format details (newcomment library)
  (setq-local comment-start "//")       ; it's complex to use the other comment style /* */
  (setq-local comment-use-syntax t)     ; use the syntax table to look for comments

  ;; define the code-highlighting
  (setq font-lock-defaults
        ;; keywords argument
        `((
           ;; special handling for inclusion strings
           ,(cons "`include\s+\\(<.+>\\)"
                  '(1 font-lock-string-face t))
           ,(cons (regexp-opt (append
                               sysver-keywords-design-elements
                               sysver-keywords-struct-procedures-waits
                               sysver-keywords-procedural-programming-statements
                               sysver-keywords-clocking
                               sysver-keywords-assertions
                               sysver-keywords-functional-coverage
                               sysver-keywords-system-io) 'symbols)
                  font-lock-keyword-face)

           ,(cons (regexp-opt (append
                               sysver-keywords-constrained-random-gen
                               sysver-keywords-system-utilities) 'symbols)
                  font-lock-function-name-face)

           ,(cons (regexp-opt (append
                               sysver-keywords-aggregate-data-types
                               sysver-keywords-classes
                               sysver-keywords-assignments-statements) 'symbols)
                  font-lock-type-face)
           ;; operators dedicated highlighting (user option)
           ,(cons (concat "\\s-" (regexp-opt (append
                                              sysver-keywords-operators)) "\\s-")
                  (if sysver-emphasize-operators
                      font-lock-constant-face
                    nil))               ; maintain default face
           ;; block delimiter highlighting (user option)
           ,(cons (regexp-opt (append
                               sysver-keywords-block-statements) 'symbols)
                  (if sysver-emphasize-block-statements
                      font-lock-constant-face
                    font-lock-type-face))

           ,(cons (regexp-opt sysver-keywords-compiler-directives 'symbols)
                  font-lock-preprocessor-face))
          nil                            ; fontify also string and comments
          nil                            ; make the search case-sensitive
          ))
  ;; re-fontify current buffer as the defaults are directly changed
  (font-lock-refresh-defaults)

  ;; set up the indentation engine SMIE
  (setq smie-indent-basic sysver-default-indent)
  (smie-setup sysver-smie-grammar #'sysver-smie-rules
              :forward-token #'sysver-forward-token
              :backward-token #'sysver-backward-token)
  )

(provide 'sysver)
;; sysver.el ends here
