;;; flycheck-follow.el --- Follow errors in flycheck error list -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Fredrik Bergroth

;; Author: Fredrik Bergroth <fredrik@fredrik-ThinkPad-L440>
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

(require 'flycheck)
(require 'shackle)

(defun flycheck-follow-list-errors ()
  (let ((shackle-rules '((flycheck-error-list-mode :ratio 0.25 :align t))))
    (flycheck-list-errors)))

(defun flycheck-follow-post-command-hook ()
  (unless (-contains? '(flycheck-follow-next-error flycheck-follow-previous-error) this-command)
    (remove-hook 'post-command-hook #'flycheck-follow-post-command-hook)
    (-when-let (win (get-buffer-window flycheck-error-list-buffer))
      (delete-window win))))

(defun flycheck-follow-next-error ()
  (interactive)
  (add-hook 'post-command-hook #'flycheck-follow-post-command-hook)
  (when (flycheck-next-error)
    (flycheck-follow-list-errors)))

(defun flycheck-follow-previous-error ()
  (interactive)
  (add-hook 'post-command-hook #'flycheck-follow-post-command-hook)
  (when (flycheck-previous-error)
    (flycheck-follow-list-errors)))

(provide 'flycheck-follow)
;;; flycheck-follow.el ends here
