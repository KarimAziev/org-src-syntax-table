;;; org-src-syntax-table.el --- Configure org src -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/org-src-syntax-table
;; Keywords: outlines
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:

;; Add syntax table to org src blocks according to their major mode

;; Usage:
;; (require 'org-src-syntax-table)
;; (add-hook 'org-mode-hook 'org-src-syntax-table-mode)

;;; Code:

(require 'org)

(defvar-local org-src-syntax-table-block-last-table nil)
(defvar-local org-src-syntax-table-lookup-properties-orig nil)

(defun org-src-syntax-table-propertize (beg end)
  "Propertize region from BEG to END with `syntax-table'."
  (let ((inhibit-read-only t))
    (add-text-properties beg end
                         `(syntax-table
                           ,org-src-syntax-table-block-last-table))))

(defun org-src-syntax-table-add-syntax-table (limit)
  "Propertize src blocks from point to LIMIT with syntax table according to mode.
This function should be used added globally to a variable `org-font-lock-hook'."
  (let ((case-fold-search t))
    (if-let ((beg (when org-src-syntax-table-block-last-table
                    (point))))
        (progn
          (if (null (re-search-forward "#\\+end_src" limit t 1))
              (org-src-syntax-table-propertize beg limit)
            (forward-line -1)
            (end-of-line)
            (org-src-syntax-table-propertize beg (point))
            (setq org-src-syntax-table-block-last-table nil)))
      (while (re-search-forward "#\\+begin_src[\s\t]+\\([a-z][^\s\t\n]+\\)"
                                limit t 1)
        (let ((lang (match-string-no-properties 1)))
          (forward-line 1)
          (beginning-of-line 1)
          (when-let ((mode (and
                            (not (looking-at "#\\+end_src"))
                            (org-src-get-lang-mode lang)))
                     (beg (point)))
            (when (fboundp mode)
              (setq org-src-syntax-table-block-last-table
                    (with-temp-buffer
                      (delay-mode-hooks (funcall mode)
                                        (syntax-table))))
              (if (null (re-search-forward "#\\+end_src" limit t 1))
                  (org-src-syntax-table-propertize beg limit)
                (forward-line -1)
                (end-of-line)
                (org-src-syntax-table-propertize beg (point))
                (setq org-src-syntax-table-block-last-table nil)))))))))

;;;###autoload
(define-minor-mode org-src-syntax-table-mode
  "Propertize org src blocks with syntax tables from their major-mode."
  :lighter " km-org"
  :global nil
  (if org-src-syntax-table-mode
      (progn
        (setq org-src-syntax-table-lookup-properties-orig
              parse-sexp-lookup-properties)
        (setq-local parse-sexp-lookup-properties t)
        (save-excursion
          (goto-char (point-min))
          (org-src-syntax-table-add-syntax-table nil))
        (add-hook 'org-font-lock-hook
                  'org-src-syntax-table-add-syntax-table nil t))
    (remove-hook 'org-font-lock-hook 'org-src-syntax-table-add-syntax-table t)
    (setq-local parse-sexp-lookup-properties
                org-src-syntax-table-lookup-properties-orig)))

(provide 'org-src-syntax-table)
;;; org-src-syntax-table.el ends here