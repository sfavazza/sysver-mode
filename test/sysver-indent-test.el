;;; sysver-indent.el --- test the indentation engine configuration

(require 'ert)
(require 'sysver)
(require 'sysver-common)

(ert-deftest sysver-test-module-header-indent ()
  "Test indentation of the module header constructs with general params."

  (let ((options '(3 4 2)))

    ;; loop through the options
    (while options
      (let* ((indent-test (pop options))
             (strings `(,(concat "module a_module_name_id0 #(param0, param1, param2)\n"
                                 (make-string indent-test ?\ ) "(param3, param4, param5)\n"
                                 "endmodule")
                        ,(concat "module a_module_name_id1 #(param0,\n"
                                 ;; The list of parameters after the first one in the previous line
                                 ;; is indented to the parameter-list starting column.
                                 (make-string (+ (length "module a_module_name_id0 ") 2) ?\ )
                                 "param1, param2)\n" 
                                 (make-string indent-test ?\ ) "(param3, param4, param5)\n"
                                 "endmodule")
                        ,(concat "module a_module_name_id2\n"
                                 (make-string indent-test ?\ ) "#(param0, param1, param2)\n"
                                 (make-string indent-test ?\ ) "(param3, param4, param5)\n"
                                 "endmodule")
                        ,(concat "module a_module_name_id3\n"
                                 (make-string indent-test ?\ ) "#(param0,\n"
                                 (make-string (+ indent-test 2) ?\ ) "param1, param2)\n"
                                 (make-string indent-test ?\ ) "(\n"
                                 (make-string (+ indent-test 1) ?\ ) "param3, param4, param5)\n"
                                 "endmodule")
                        ,(concat "module a_module_name_id4\n"
                                 (make-string indent-test ?\ ) "#(\n"
                                 (make-string (+ indent-test 2) ?\ ) "param0,\n"
                                 (make-string (+ indent-test 2) ?\ ) "param1, param2)\n"
                                 (make-string indent-test ?\ ) "(param3,\n"
                                 (make-string (+ indent-test 1) ?\ ) "param4,\n"
                                 (make-string (+ indent-test 1) ?\ ) "param5)\n"
                                 "endmodule"))))

        ;; loop through the test strings
        (while strings
          (setq current-string (pop strings))
          (with-temp-buffer
            (sysver-utc-environment
             current-string

             ;; set-up indentation parameters
             ((setq sysver-default-indent indent-test))

             ;; apply indentation
             ((save-excursion
                (indent-region (point-min) (point-max)))
              ;; verify
              (should (equal (buffer-substring-no-properties (point-min) (point-max))
                             current-string))))))))))
