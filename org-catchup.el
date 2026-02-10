;;; org-catchup.el --- Catch-up meeting prep workflow  -*- lexical-binding: t; -*-

;;; Commentary:
;; Manages weekly catch-up files for 1:1 meetings.
;; Generates dated catch-up files from a template, pulling in open actions
;; and captured inbox items.  Provides quick capture for tagging items
;; between meetings.
;;
;; Commands:
;;   M-x org-catchup-new          Generate (or open) a dated catch-up file
;;   M-x org-catchup-capture      Quick-add a tagged item to capture.org
;;   M-x org-catchup-open-capture Open capture.org
;;   M-x org-catchup-open-actions Open actions.org
;;   M-x org-catchup-open-team    Open team.org

;;; Code:

(require 'org)
(require 'org-element)

(defvar org-catchup-directory "~/Dropbox/work/catchips/"
  "Root directory for org-catchup files.")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun org-catchup--file (relative)
  "Return absolute path for RELATIVE within `org-catchup-directory'."
  (expand-file-name relative org-catchup-directory))

(defun org-catchup--open-file (relative)
  "Open RELATIVE file inside `org-catchup-directory'."
  (find-file (org-catchup--file relative)))

(defun org-catchup--read-file (path)
  "Return contents of PATH as a string, or nil if it doesn't exist."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun org-catchup--collect-todo-headlines (file)
  "Parse FILE and return a list of TODO headline elements.
Each element is an org-element headline node with todo-keyword = \"TODO\"."
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
  "Return the raw text of headline HL, stripped of priority cookie."
  (let ((raw (org-element-property :raw-value hl)))
    ;; Remove leading priority like "[#A] "
    (if (string-match "\\`\\[#.\\] " raw)
        (substring raw (match-end 0))
      raw)))

(defun org-catchup--headline-tags (hl)
  "Return the list of tags on headline HL."
  (org-element-property :tags hl))

;; ---------------------------------------------------------------------------
;; org-catchup-new
;; ---------------------------------------------------------------------------

(defvar org-catchup--tag-section-alist
  '(("update"   . "Key Updates")
    ("decision" . "Decisions Needed")
    ("risk"     . "Risks & Escalations")
    ("highlight" . "Team Highlights")
    ("ask"      . "Resource & Support Asks")
    ("action"   . "Notes & New Actions"))
  "Mapping from capture tags to section headings in the template.")

(defun org-catchup--format-action-item (hl)
  "Format an action headline HL as a plain-text list item.
Extracts :from_YYYY_MM_DD: tag and appends (from YYYY-MM-DD)."
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
  "Format a capture headline HL as a plain-text list item.
Strips tags from the display text."
  (format "  - %s" (org-catchup--headline-text hl)))

(defun org-catchup--capture-tag-to-section (hl)
  "Return the section heading that HL's tag maps to, or nil."
  (let ((tags (org-catchup--headline-tags hl)))
    (cl-some (lambda (tag)
               (cdr (assoc tag org-catchup--tag-section-alist)))
             tags)))

(defun org-catchup--inbox-headlines (file)
  "Collect TODO headlines that are under the * Inbox subtree in FILE."
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
              ;; Collect TODO children of this Inbox heading
              (org-element-map hl 'headline
                (lambda (child)
                  (when (and (string= (org-element-property :todo-keyword child) "TODO")
                             (> (org-element-property :level child) 1))
                    (push child results)))))))
        (nreverse results)))))

(defun org-catchup--inject-items (buffer heading items)
  "In BUFFER, insert ITEMS (list of strings) after the section HEADING.
Inserts after any existing description lines, before the next heading."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^\\*\\s-+" (regexp-quote heading)) nil t)
      ;; Move past description lines until we hit a blank line followed by
      ;; a heading, or end of buffer
      (forward-line 1)
      (let ((insert-point nil))
        ;; Scan forward to find the next top-level heading
        (while (and (not (eobp))
                    (not (looking-at "^\\* ")))
          (forward-line 1))
        (setq insert-point (point))
        (goto-char insert-point)
        (insert (mapconcat #'identity items "\n") "\n")))))

;;;###autoload
(defun org-catchup-new ()
  "Generate a new catch-up file for a chosen date, or open it if it exists.
Pulls open actions from actions.org and captured items from capture.org."
  (interactive)
  (let* ((date (org-read-date nil nil nil "Catch-up date: "))
         (outfile (org-catchup--file (format "catchups/%s.org" date)))
         (template-path (org-catchup--file "templates/catchup.org")))
    ;; If file exists, just open it
    (when (file-exists-p outfile)
      (find-file outfile)
      (message "Opened existing catch-up: %s" date)
      (cl-return-from org-catchup-new))
    ;; Read template
    (let ((template (org-catchup--read-file template-path)))
      (unless template
        (user-error "Template not found: %s" template-path))
      ;; Create buffer with template content
      (let ((buf (generate-new-buffer (format "*catchup-%s*" date))))
        (with-current-buffer buf
          (insert (replace-regexp-in-string "%DATE%" date template))
          (org-mode)
          ;; --- Inject open actions ---
          (let* ((action-hls (org-catchup--collect-todo-headlines
                              (org-catchup--file "actions.org")))
                 (action-items (mapcar #'org-catchup--format-action-item action-hls)))
            (when action-items
              (org-catchup--inject-items buf "Open Actions from Previous" action-items)))
          ;; --- Inject captured items ---
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
        ;; Open the new file
        (find-file outfile)
        (message "Created catch-up: %s" date)))))

;; ---------------------------------------------------------------------------
;; org-catchup-capture
;; ---------------------------------------------------------------------------

(defvar org-catchup--capture-tags
  '("update" "decision" "risk" "highlight" "ask" "action")
  "Valid tags for captured items.")

;;;###autoload
(defun org-catchup-capture ()
  "Quick-add a tagged TODO item to the Inbox in capture.org."
  (interactive)
  (let* ((text (read-string "Capture item: "))
         (tag (completing-read "Tag: " org-catchup--capture-tags nil t))
         (capture-file (org-catchup--file "capture.org")))
    (when (string-empty-p text)
      (user-error "Empty capture text"))
    (with-current-buffer (find-file-noselect capture-file)
      (org-mode)
      (goto-char (point-min))
      ;; Find * Inbox heading
      (unless (re-search-forward "^\\* Inbox" nil t)
        (user-error "No * Inbox heading found in capture.org"))
      ;; Go to end of the Inbox subtree
      (org-end-of-subtree t)
      ;; Make sure we're on a fresh line
      (unless (bolp) (insert "\n"))
      (insert (format "** TODO %s\t\t\t\t\t\t\t:%s:\n" text tag))
      (save-buffer)
      (message "Captured: %s [:%s:]" text tag))))

;; ---------------------------------------------------------------------------
;; Open-file commands
;; ---------------------------------------------------------------------------

;;;###autoload
(defun org-catchup-open-capture ()
  "Open capture.org."
  (interactive)
  (org-catchup--open-file "capture.org"))

;;;###autoload
(defun org-catchup-open-actions ()
  "Open actions.org."
  (interactive)
  (org-catchup--open-file "actions.org"))

;;;###autoload
(defun org-catchup-open-team ()
  "Open team.org."
  (interactive)
  (org-catchup--open-file "team.org"))

(provide 'org-catchup)
;;; org-catchup.el ends here
