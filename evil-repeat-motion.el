;;; evil-repeat-motion.el --- evil repeat motion     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'evil)
(require 'seq)

(defvar-local evil-repeat-motion--last-command nil)

(defun evil-repeat-motion--advice (orig &rest args)
  "Evil repeat motion advice.
ORIG - the original function.
ARGS - the args of the original command."
  (when (equal this-command orig)
    (setq-local evil-repeat-motion--last-command (list* orig args))))

(defun evil-repeat-motion-do (&optional count)
  "Repeat last motion COUNT times."
  (interactive "P")
  (when evil-repeat-motion--last-command
    (dotimes (_ (or count 1))
      (apply #'funcall-interactively evil-repeat-motion--last-command))))

(defun evil-repeat-motion--setup ()
  "Configure `evil-repeat-motion'."
  (seq-do
   (lambda (command-data)
     (when (eq (plist-get (cdr command-data) :repeat) 'motion)
       (let ((command (car command-data)))
         (advice-add command
                     :after (apply-partially #'evil-repeat-motion--advice command)
                     '((name . evil-repeat-motion-adv))))))
   evil-command-properties))

(defun evil-repeat-motion-cleanup ()
  "Cleanup the `evil-repeat-motion' advices."
  (seq-do
   (lambda (command-data)
     (when (eq (plist-get (cdr command-data) :repeat) 'motion)
       (let ((command (car command-data)))
         (advice-remove command 'evil-repeat-motion-adv))))
   evil-command-properties))

(defvar evil-repeat-motion-mode-map (make-sparse-keymap)
  "Keymap for `evil-repeat-motion-mode'.")

(evil-define-key 'normal evil-repeat-motion-mode-map  ";" 'evil-repeat-motion-do)
(evil-define-key 'visual evil-repeat-motion-mode-map  ";" 'evil-repeat-motion-do)

(define-minor-mode evil-repeat-motion-mode ""
  nil nil nil
  :global t
  :keymap evil-repeat-motion-mode-map
  :group 'evil-repeat-motion
  (cond
   (evil-repeat-motion-mode (evil-repeat-motion--setup))
   (t (evil-repeat-motion-cleanup))) )

(provide 'evil-repeat-motion)
;;; evil-repeat-motion.el ends here
