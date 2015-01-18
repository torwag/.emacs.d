;;; my-x.el --- X stuff                              -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Fredrik Bergroth

;; Author: Fredrik Bergroth <fredrik@fredrik-ThinkPad-T430s>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'dash)

(defun my-x-set-urgency-hint ()
  (-let* (((_ . ((frame))) (current-frame-configuration))
          ((flags . hints) (append (x-window-property "WM_HINTS" frame "WM_HINTS" nil nil t) nil))
          (wm-hints (cons (logior flags #x100) hints)))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(provide 'my-x)
;;; my-x.el ends here
