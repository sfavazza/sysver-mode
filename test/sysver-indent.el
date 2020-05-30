;;; sysver-indent.el --- test the indentation engine configuration

(require 'ert)
(require 'sysver)
(require 'sysver-test-common)

(ert-deftest sysver-test-module-header-indent ()
  ""

  (let ((test-string ""))

    (with-temp-buffer
      (sysver-utc-environment
       test-string

       ()

       ()))))
