;;; evil-repeat-motion.el --- Evil repeat motion  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Homepage: https://github.com/yyoncho/evil-repeat-motion
;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: convenience
;; Version: 0.0.1
;; Package-Requires: ((evil "1.0") (emacs "25.1"))

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

;; Implements evil repeatable motions.

;;; Code:

(require 'evil)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar evil-repeat-motion--last-command nil)

(defvar evil-repeat-motion-pairs
  '(("forward" . "backward")
    ("next" . "previous")
    ("beginning-of" . "end-of")
    ("up" . "down")
    ("first" . "last")))

(defun evil-repeat-motion--advice (orig &rest args)
  "Evil repeat motion advice.
ORIG - the original function.
ARGS - the args of the original command."
  (when (equal this-command orig)
    (setq evil-repeat-motion--last-command (cl-list* orig args))))

(defun evil-repeat-motion-apply (&optional count)
  "Repeat last motion COUNT times."
  (interactive "P")
  (when evil-repeat-motion--last-command
    (dotimes (_ (or count 1))
      (apply #'funcall-interactively evil-repeat-motion--last-command))))

(defun evil-repeat-motion-reverse (&optional count)
  "Repeat last motion COUNT times."
  (interactive "P")
  (when evil-repeat-motion--last-command
    (when-let (reverse (evil-repeat-motion-find-reverse-command (cl-first evil-repeat-motion--last-command)))
      (dotimes (_ (or count 1))
        (apply #'funcall-interactively reverse
               (cl-rest evil-repeat-motion--last-command))))))

(defun evil-repeat-motion-reverse-command (&optional count)
  "Repeat last motion COUNT times."
  (interactive "P")
  (when evil-repeat-motion--last-command
    (when-let (reverse (evil-repeat-motion-find-reverse-command (cl-first evil-repeat-motion--last-command)))
      (dotimes (_ (or count 1))
        (apply #'funcall-interactively reverse
               (cl-rest evil-repeat-motion--last-command)))
      (setq evil-repeat-motion--last-command (cl-list* reverse (cl-rest evil-repeat-motion--last-command))))))

(defun evil-repeat-motion-reverse-command (command)
  "Repeat last motion COMMAND times."
  (let ((command (format "%s" command)))
    (cl-labels ((matches (pair)
                         (cond
                          ((string-match-p (regexp-quote (car pair)) command) (cons (cdr pair)
                                                                                    (car pair)))
                          ((string-match-p (regexp-quote (cdr pair)) command) pair))))
      (pcase-let ((`(,new . ,old) (matches (seq-find #'matches evil-repeat-motion-pairs))))
        (intern (replace-regexp-in-string (regexp-quote old) new command t t))))))

(defun evil-repeat-motion--setup ()
  "Configure `evil-repeat-motion-apply'."
  (seq-do
   (lambda (command-data)
     (when (eq (plist-get (cdr command-data) :repeat) 'motion)
       (let ((command (car command-data)))
         (advice-add command
                     :after (lambda (&rest args)
                              (interactive)
                              (evil-repeat-motion--advice command args))
                     '((name . evil-repeat-motion-adv))))))
   evil-command-properties))

(defun evil-repeat-motion-cleanup ()
  "Cleanup the `evil-repeat-motion-apply' advices."
  (seq-do
   (lambda (command-data)
     (when (eq (plist-get (cdr command-data) :repeat) 'motion)
       (let ((command (car command-data)))
         (advice-remove command 'xxx))))
   evil-command-properties))

(defvar evil-repeat-motion-mode-map (make-sparse-keymap)
  "Keymap for `evil-repeat-motion-mode'.")

(evil-define-key 'normal evil-repeat-motion-mode-map  ";" 'evil-repeat-motion-apply)
(evil-define-key 'visual evil-repeat-motion-mode-map  ";" 'evil-repeat-motion-apply)

;;;###autoload
(define-minor-mode evil-repeat-motion-mode ""
  nil nil nil
  :global t
  :keymap evil-repeat-motion-mode-map
  :group 'evil-repeat-motion-apply
  (cond
   (evil-repeat-motion-mode (evil-repeat-motion--setup))
   (t (evil-repeat-motion-cleanup))) )

(provide 'evil-repeat-motion)
;;; evil-repeat-motion.el ends here
