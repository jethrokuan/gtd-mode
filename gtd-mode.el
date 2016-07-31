;;; gtd-mode.el --- GTD minor mode for org-mode -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2016 Jethro Kuan <jethrokuan95@gmail.com>
;;
;; Package-Requires: ((emacs "24.1") (ivy "0.8.0") (org-mode "8.3.5"))
;;
;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://gihub.com/jethrokuan/gtd-mode
;; Keywords: org, gtd

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

(require 'org)
(require 'org-element)
(require 'ivy)

(define-minor-mode gtd-mode
  "GTD mode"
  :lighter " gtd"
  :global t)

(defgroup gtd nil
  "Provides an Emacs implementation of the GTD workflow."
  :group 'extensions)

(defcustom gtd-folder "~/.org/gtd"
  "Folder that contains all GTD-related files. Defaults to \"~/.org/gtd\".")

(defvar gtd-actionable-options
  '((?p "(p) Project" gtd-refile-to-project) 
    (?d "(d) Delegate" gtd-delegate-item)
    (?c "(c) Create Next-action" gtd-new-next-action)
    (?\ "(SPC) DO NOW" gtd-do-now))
  "Options for actionables")

(defvar gtd-nonactionable-options
  '((?t "(t) Trash" gtd-trash-item)
    (?i "(i) Incubate" gtd-incubate-item)
    (?r "(r) Reference" gtd-reference-item))
  "Options for nonactionables")

;;; Setup
(setq gtd-inbox-file (concat (file-name-as-directory gtd-folder) "inbox.org"))
(setq gtd-projects-file (concat (file-name-as-directory gtd-folder) "projects.org"))
(setq gtd-na-file (concat (file-name-as-directory gtd-folder) "next_actions.org"))
(setq gtd-waiting-file (concat (file-name-as-directory gtd-folder) "waiting_for.org"))
(setq gtd-reference-file (concat (file-name-as-directory gtd-folder) "reference.org"))
(setq gtd-someday-file (concat (file-name-as-directory gtd-folder) "someday.org"))

(add-to-list 'org-capture-templates '("i" "Inbox" entry (file gtd-inbox-file)
                                      "* %?%i\n"))

;;; Helpers
(defun gtd-refile-to (file prompt)
  "Moves current org-element to new file"
  (let ((headline (with-temp-buffer
                    (insert-file-contents file)
                    (ivy-read prompt
                              (org-element-map (org-element-parse-buffer 'headline) 'headline
                                (lambda (h) (when (equal (org-element-property :level h) 1)
                                              (org-element-property :raw-value h))))))))
    (let ((pos (save-window-excursion
                 (find-file file)
                 (end-of-buffer)
                 (let ((position (org-find-exact-headline-in-buffer headline)))
                   (if position
                       position                     
                     (with-temp-buffer
                       (insert (concat "* " headline))
                       (append-to-buffer (find-file-noselect file) (point-min) (point-max))) 
                     (org-find-exact-headline-in-buffer headline))))))
      (org-refile nil nil (list headline file nil pos)))))

(defun gtd-into-inbox ()
  (interactive)
  (org-capture nil "i"))

;;; Actions
;;; Actionables
(defun gtd-refile-to-project () 
  (gtd-alter-headline "Enter new actionable: ")
  (gtd-refile-to gtd-projects-file "Project: "))

(defun gtd-delegate-item ()
  (gtd-alter-headline "Enter new actionable: ")
  (gtd-refile-to gtd-waiting-file "Person: "))

(defun gtd-new-next-action ()
  (gtd-alter-headline "Enter next action: ")
  (let ((item (concat "* " (org-element-property :raw-value (org-element-at-point)))))
    (with-temp-buffer 
      (insert item)
      (append-to-buffer (find-file-noselect gtd-na-file) (point-min) (point-max))))
  (kill-whole-line))

(defun gtd-do-now ()
  (let ((val (org-element-property :raw-value (org-element-at-point))))
    (if (y-or-n-p "Do it now! Type \"y\" when done:")
        (org-archive-subtree)
      (throw 'premature-end nil))))

;;; Non-actionables
(defun gtd-trash-item ()
  (kill-whole-line))

(defun gtd-incubate-item ()
  (gtd-refile-to gtd-someday-file "Select category: "))

(defun gtd-reference-item ()
  (gtd-refile-to gtd-reference-file "Select category: "))

;;; Main Functions
(defun gtd-alter-headline (prompt)
  (let ((headline (read-string prompt (org-element-property :raw-value (org-element-at-point)))))
    (org-element-put-property (org-element-at-point) :raw-value headline)))

(defun gtd-select-options (options-var)
  (let ((options (mapconcat (lambda (x) (car (cdr x))) options-var " ")))
    (let ((opt (read-char options)))
      (or (assoc opt options-var)
          (progn
                                        ;TODO: Message that option is incorrect
            (gtd-select-options options-var)))
      (dolist (action options-var)
        (when (equal opt (nth 0 action))
          (funcall (nth 2 action)))))))

(defun gtd-clear-inbox ()
  (interactive)
  (save-window-excursion
    (find-file gtd-inbox-file)
    (beginning-of-buffer)
    (catch 'premature-end
      (while (equal ?* (char-after 1))
        (let ((title (org-element-property :raw-value (org-element-at-point))))
          (if (y-or-n-p (concat "\"" title "\". Is this actionable?"))
              (gtd-select-options gtd-actionable-options) 
            (gtd-select-options gtd-nonactionable-options))))
      (message "Inbox clear!"))
    (save-current-buffer)))

(provide 'gtd-mode)
