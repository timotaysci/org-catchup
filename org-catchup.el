;;; org-catchup.el --- Prepare structured 1:1 catch-up agendas  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Timothy Johnson

;; Author: Timothy Johnson <timotaysci@gmail.com>
;; Maintainer: Timothy Johnson <timotaysci@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: outlines, calendar, convenience
;; URL: https://github.com/timotaysci/org-catchup

;; This file is not part of GNU Emacs.

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

;; org-catchup generates dated catch-up agendas for recurring 1:1
;; meetings.  Between meetings you capture items with a tag indicating
;; which section they belong in.  When it is time to prep, a single
;; command assembles the agenda from a template, pulling in open
;; actions and captured items automatically.
;;
;; Quick start:
;;
;;   (setq org-catchup-directory "~/my-catchups/")
;;
;; Then use the following commands:
;;
;;   M-x org-catchup-new           Generate or open a dated catch-up file
;;   M-x org-catchup-capture       Quick-add a tagged item to capture.org
;;   M-x org-catchup-sweep-inbox   Mark all Inbox TODOs as DONE after a call
;;   M-x org-catchup-open-capture  Open capture.org
;;   M-x org-catchup-open-actions  Open actions.org
;;   M-x org-catchup-open-team     Open team.org
;;
;; The directory should contain:
;;
;;   templates/catchup.org   Template with %DATE% placeholder
;;   capture.org             Inbox with a top-level "* Inbox" heading
;;   actions.org             Open action items as TODO headlines
;;   team.org                Team reference (optional)
;;   catchups/               Generated catch-up files go here

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)

;; ---------------------------------------------------------------------------
;; Customization
;; ---------------------------------------------------------------------------

(defgroup org-catchup nil
  "Prepare structured 1:1 catch-up agendas."
  :group 'org
  :prefix "org-catchup-")

(defcustom org-catchup-directory "~/org-catchup/"
  "Root directory for org-catchup files."
  :type 'directory
  :group 'org-catchup)

(defcustom org-catchup-capture-tags
  '("update" "decision" "risk" "highlight" "ask" "action")
  "Valid tags for captured items."
  :type '(repeat string)
  :group 'org-catchup)

(defcustom org-catchup-tag-section-alist
  '(("update"    . "Key Updates")
    ("decision"  . "Decisions Needed")
    ("risk"      . "Risks & Escalations")
    ("highlight" . "Team Highlights")
    ("ask"       . "Resource & Support Asks")
    ("action"    . "Notes & New Actions"))
  "Alist mapping capture tags to section headings in the template."
  :type '(alist :key-type string :value-type string)
  :group 'org-catchup)

;; ---------------------------------------------------------------------------
;; Internal helpers
;; ---------------------------------------------------------------------------

(defun org-catchup--file (relative)
  "Return absolute path for RELATIVE within `org-catchup-directory'."
  (expand-file-name relative org-catchup-directory))

(defun org-catchup--open-file (relative)
  "Open the file at RELATIVE path inside `org-catchup-directory'."
  (find-file (org-catchup--file relative)))

(defun org-catchup--read-file (path)
  "Return contents of PATH as a string, or nil if it does not exist."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun org-catchup--collect-todo-headlines (file)
  "Parse FILE and return a list of TODO headline elements.
Each element is an Org element headline node whose todo-keyword
is \"TODO\".  Return nil if FILE does not exist."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (let ((tree (org-element-parse-buffer 'headline)))
        (org-element-map tree 'headline
          (lambda (hl)
            (when (string= (org-element-property :todo-keyword hl) "TODO")
              hl)))))))

(defun org-catchup--headline-text (hl)
  "Return the raw text of headline HL with any priority cookie stripped."
  (let ((raw (org-element-property :raw-value hl)))
    (if (string-match "\\`\\[#.\\] " raw)
        (substring raw (match-end 0))
      raw)))

(defun org-catchup--headline-tags (hl)
  "Return the list of tags on headline HL."
  (org-element-property :tags hl))

(defun org-catchup--format-action-item (hl)
  "Format action headline HL as a plain-text list item.
Extract any :from_YYYY_MM_DD: tag and append it as (from YYYY-MM-DD)."
  (let ((text (org-catchup--headline-text hl))
        (tags (org-catchup--headline-tags hl))
        (from-date nil))
    (dolist (tag tags)
      (when (string-match "\\`from_\\([0-9]+\\)_\\([0-9]+\\)_\\([0-9]+\\)\\'" tag)
        (setq from-date (format "%s-%s-%s"
                                (match-string 1 tag)
                                (match-string 2 tag)
                                (match-string 3 tag)))))
    (if from-date
        (format "  - %s (from %s)" text from-date)
      (format "  - %s" text))))

(defun org-catchup--format-capture-item (hl)
  "Format capture headline HL as a plain-text list item."
  (format "  - %s" (org-catchup--headline-text hl)))

(defun org-catchup--capture-tag-to-section (hl)
  "Return the section heading that HL maps to, or nil.
The mapping is defined by `org-catchup-tag-section-alist'."
  (let ((tags (org-catchup--headline-tags hl)))
    (cl-some (lambda (tag)
               (cdr (assoc tag org-catchup-tag-section-alist)))
             tags)))

(defun org-catchup--inbox-headlines (file)
  "Collect TODO headlines under the Inbox subtree in FILE.
Return nil if FILE does not exist or has no Inbox heading."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (let ((tree (org-element-parse-buffer 'headline))
            (results nil))
        (org-element-map tree 'headline
          (lambda (hl)
            (when (and (string= (org-element-property :raw-value hl) "Inbox")
                       (= (org-element-property :level hl) 1))
              (org-element-map hl 'headline
                (lambda (child)
                  (when (and (string= (org-element-property :todo-keyword child) "TODO")
                             (> (org-element-property :level child) 1))
                    (push child results)))))))
        (nreverse results)))))

(defun org-catchup--inject-items (buffer heading items)
  "Insert ITEMS after section HEADING in BUFFER.
ITEMS is a list of strings.  They are inserted before the next
top-level heading."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^\\*\\s-+" (regexp-quote heading)) nil t)
      (forward-line 1)
      (while (and (not (eobp))
                  (not (looking-at "^\\* ")))
        (forward-line 1))
      (insert (mapconcat #'identity items "\n") "\n"))))

;; ---------------------------------------------------------------------------
;; Interactive commands
;; ---------------------------------------------------------------------------

;;;###autoload
(defun org-catchup-new ()
  "Generate a new catch-up file for a chosen date, or open it if it exists.
Pull open actions from actions.org and captured items from
capture.org into the appropriate template sections."
  (interactive)
  (let* ((date (org-read-date nil nil nil "Catch-up date: "))
         (outfile (org-catchup--file (format "catchups/%s.org" date)))
         (template-path (org-catchup--file "templates/catchup.org")))
    (if (file-exists-p outfile)
        (progn
          (find-file outfile)
          (message "Opened existing catch-up: %s" date))
      (let ((template (org-catchup--read-file template-path)))
        (unless template
          (user-error "Template not found: %s" template-path))
        (let ((buf (generate-new-buffer (format "*catchup-%s*" date))))
          (with-current-buffer buf
            (insert (replace-regexp-in-string "%DATE%" date template))
            (org-mode)
            ;; Inject open actions
            (let* ((action-hls (org-catchup--collect-todo-headlines
                                (org-catchup--file "actions.org")))
                   (action-items (mapcar #'org-catchup--format-action-item
                                         action-hls)))
              (when action-items
                (org-catchup--inject-items
                 buf "Open Actions from Previous" action-items)))
            ;; Inject captured items
            (let ((capture-hls (org-catchup--inbox-headlines
                                (org-catchup--file "capture.org"))))
              (dolist (hl capture-hls)
                (let ((section (org-catchup--capture-tag-to-section hl)))
                  (when section
                    (org-catchup--inject-items
                     buf section
                     (list (org-catchup--format-capture-item hl)))))))
            ;; Write file
            (let ((dir (file-name-directory outfile)))
              (unless (file-directory-p dir)
                (make-directory dir t)))
            (write-region (point-min) (point-max) outfile)
            (kill-buffer buf))
          (find-file outfile)
          (message "Created catch-up: %s" date))))))

;;;###autoload
(defun org-catchup-capture ()
  "Quick-add a tagged TODO item to the Inbox in capture.org.
Prompt for item text and a tag, then append the entry under the
Inbox heading."
  (interactive)
  (let* ((text (read-string "Capture item: "))
         (tag (completing-read "Tag: " org-catchup-capture-tags nil t))
         (capture-file (org-catchup--file "capture.org")))
    (when (string= text "")
      (user-error "Empty capture text"))
    (with-current-buffer (find-file-noselect capture-file)
      (org-mode)
      (goto-char (point-min))
      (unless (re-search-forward "^\\* Inbox" nil t)
        (user-error "No * Inbox heading found in capture.org"))
      (org-end-of-subtree t)
      (unless (bolp) (insert "\n"))
      (insert (format "** TODO %s\t\t\t\t\t\t\t:%s:\n" text tag))
      (save-buffer)
      (message "Captured: %s [:%s:]" text tag))))

;;;###autoload
(defun org-catchup-sweep-inbox ()
  "Mark all TODO items under Inbox in capture.org as DONE.
Run this after a catch-up to clear processed items so they are
not pulled into the next catch-up file."
  (interactive)
  (let ((capture-file (org-catchup--file "capture.org"))
        (count 0))
    (with-current-buffer (find-file-noselect capture-file)
      (org-mode)
      (goto-char (point-min))
      (unless (re-search-forward "^\\* Inbox" nil t)
        (user-error "No * Inbox heading found in capture.org"))
      (let ((end (save-excursion (org-end-of-subtree t) (point))))
        (while (re-search-forward "^\\*+ TODO " end t)
          (org-todo "DONE")
          (setq count (1+ count))
          (setq end (save-excursion
                      (goto-char (point-min))
                      (re-search-forward "^\\* Inbox" nil t)
                      (org-end-of-subtree t)
                      (point)))))
      (save-buffer)
      (message "Swept %d item%s to DONE" count (if (= count 1) "" "s")))))

;;;###autoload
(defun org-catchup-open-capture ()
  "Open capture.org in `org-catchup-directory'."
  (interactive)
  (org-catchup--open-file "capture.org"))

;;;###autoload
(defun org-catchup-open-actions ()
  "Open actions.org in `org-catchup-directory'."
  (interactive)
  (org-catchup--open-file "actions.org"))

;;;###autoload
(defun org-catchup-open-team ()
  "Open team.org in `org-catchup-directory'."
  (interactive)
  (org-catchup--open-file "team.org"))

(provide 'org-catchup)
;;; org-catchup.el ends here
