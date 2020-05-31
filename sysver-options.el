;;; sysver-options.el

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

;; This file contains all the sysver options to customize this major-mode behavior.

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

(defcustom sysver-emphasize-operators nil
  "If `non-nil' highlight all operators for an improved visibility."
  :type 'boolean
  :group 'sysver-misc)

(defcustom sysver-emphasize-block-statements nil
  "if `non-nil' highlight the `begin'-`end' block delimiters for an improved visibility."
  :type 'boolean
  :group 'sysver-misc)

(defcustom sysver-default-indent 2
  "Default indentation"
  :type 'integer
  :group 'sysver-indentation)


(provide 'sysver-options)
