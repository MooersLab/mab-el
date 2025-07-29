;;; mab.el --- Utilities for managing modular and annotated bibliography -*- lexical-binding: t -*-

;; Copyright (C) 2025 Blaine Mooers

;; Author: Blaine Mooers
;; Keywords: bib, ebib, bibliography, org-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (ebib "2.0"))
;; URL: https://github.com/MooersLab/mab

;;; Commentary:

;; This package provides utilities for managing bibliographic entries
;; with ebib, particularly for adding selected entries to reading lists
;; in org-mode files. The main functionality allows appending bibliographic
;; entries to a specified org file with proper formatting for LaTeX export.
;;
;; The main command is `mab-add-bib-item', which is bound to "B"
;; in ebib-index-mode.

;;; Code:

(require 'ebib)

(defgroup mab nil
  "Utilities for managing bibliography with ebib."
  :group 'applications
  :prefix "mab-")

(defcustom mab-path "~/1097QuantumCrystallography/mab1097/mab1097.org"
  "File path of the org file to which references will be appended from inside ebib.
This should be the full path to the mabXXXX.org file you want to use for your reading list.
Default is set to the quantum crystallography project file."
  :type 'file
  :group 'mab)

;;;###autoload
(defun mab-add-bib-item ()
  "Append the currently selected ebib entry to the mab file.
The mab file is specified by the variable `mab-path',
but the user will be prompted for the file path with the current value as default.
The entry is added under the section 'Illustrated and annotated bibliography'
and before 'Backmatter' with the format using org-mode LATEX directives,
INCLUDE statement, and a Notes drawer with file links to both papers and books."
  (interactive)
  (unless (derived-mode-p 'ebib-index-mode)
    (error "This command can only be used in Ebib's index buffer"))
  
  (let ((key (ebib--get-key-at-point)))
    (unless key
      (error "No bibliography entry selected"))
    
    (let* ((default-path (expand-file-name mab-path))
           (file-path (expand-file-name 
                       (read-file-name 
                        (format "Modular and annotated bibliography (mab) file (default %s): " mab-path)
                        (file-name-directory default-path)
                        default-path)))
           ;; Update the customizable variable with the new path for future use
           (setq mab-path file-path))
      (if (file-exists-p file-path)
          (with-temp-buffer
            (insert-file-contents file-path)
            (goto-char (point-min))
            ;; Find the bibliography section
            (if (re-search-forward "\\\\section\\*{Illustrated and annotated bibliography}" nil t)
                (let ((start-pos (point)))
                  ;; Check if we have a Backmatter section
                  (if (re-search-forward "\\\\section\\*{Backmatter}" nil t)
                      (progn
                        (beginning-of-line)
                        ;; Insert the new entry before Backmatter using the new format
                        (insert (format "#+LATEX: \\subsection*{\\bibentry{%s}}\n" key))
                        (insert (format "#+LATEX: \\addcontentsline{toc}{subsection}{%s}\n" key))
                        (insert (format "#+INCLUDE: /Users/blaine/abibNotes/%s.org\n" key))
                        (insert ":Notes:\n")
                        (insert (format "file:~/abibNotes/%s.org\n" key))
                        (insert (format "file:~/0papersLabeled/%s.pdf\n" key))
                        (insert (format "file:~/0booksLabeled/%s.pdf\n" key))
                        (insert "Add more prose. Add tables. Add figures.\n")
                        (insert ":END:\n\n"))
                    ;; No Backmatter section found, go to end of the document
                    (goto-char (point-max))
                    ;; Insert the new entry at the end using the new format
                    (insert (format "#+LATEX: \\subsection*{\\bibentry{%s}}\n" key))
                    (insert (format "#+LATEX: \\addcontentsline{toc}{subsection}{%s}\n" key))
                    (insert (format "#+INCLUDE: /Users/blaine/abibNotes/%s.org\n" key))
                    (insert ":Notes:\n")
                    (insert (format "file:~/abibNotes/%s.org\n" key))
                    (insert (format "file:~/0papersLabeled/%s.pdf\n" key))
                    (insert (format "file:~/0booksLabeled/%s.pdf\n" key))
                    (insert "Add more prose. Add tables. Add figures.\n")
                    (insert ":END:\n"))
                  ;; Write the file
                  (write-region (point-min) (point-max) file-path)
                  (message "Added %s to %s" key file-path))
              (message "Could not find 'Illustrated and annotated bibliography' section in %s" file-path)))
        (message "File %s does not exist" file-path)))))

;;;###autoload
(eval-after-load 'ebib
  '(define-key ebib-index-mode-map (kbd "B") #'mab-add-bib-item))

(provide 'mab)
;;; mab.el ends here
