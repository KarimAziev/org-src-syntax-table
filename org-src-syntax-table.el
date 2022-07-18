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
                    (when (re-search-forward "." nil t 1)
                      (forward-char -1)
                      (point)))))
        (progn
          (if (null (re-search-forward "#\\+end_src" limit t 1))
              (org-src-syntax-table-propertize beg limit)
            (forward-line -1)
            (end-of-line)
            (when (re-search-backward "." beg t 1)
              (org-src-syntax-table-propertize beg (point)))
            (setq org-src-syntax-table-block-last-table nil)))
      (while (re-search-forward "#\\+begin_src[\s\t]+\\([a-z][^\s\t\n]+\\)"
                                limit t 1)
        (let ((lang (match-string-no-properties 1)))
          (forward-line 1)
          (beginning-of-line 1)
          (when (re-search-forward "." limit t 1)
            (forward-char -1)
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
                  (setq org-src-syntax-table-block-last-table nil))))))))))

(defun org-src-syntax-table-get-src-params ()
  "If point is inside body of src block return list - (LANGUAGE BEGINNING END)."
  (save-excursion
    (let ((case-fold-search t))
      (when (re-search-forward "#\\+\\(begin\\|end\\)_src\\($\\|[\s\f\t\n\r\v]\\)"
                               nil t 1)
        (when-let ((word (match-string-no-properties 1))
                   (end (match-beginning 0)))
          (when (or (string= word "end")
                    (string= word "END"))
            (when (re-search-backward
                   "^\\([ \t]*\\)#\\+begin_src[ \t]+\\([^ \f\t\n\r\v]+\\)[ \t]*"
                   nil t 1)
              (let ((lang (match-string-no-properties 2)))
                (forward-line 1)
                (list lang (point) end)))))))))

(defun org-src-syntax-table-pre-command-hook ()
  "If point is inside body of src block return list - (LANGUAGE BEGINNING END)."
  (if-let* ((params (and
                     (memq this-command '(forward-sexp
                                          backward-sexp
                                          forward-list
                                          backward-list))
                     (org-src-syntax-table-get-src-params)))
            (mode (and
                   (car params)
                   (org-src-get-lang-mode (car params))))
            (table (with-temp-buffer
                     (delay-mode-hooks (funcall mode)
                                       (syntax-table)))))
      (let ((inhibit-read-only t))
        (add-text-properties (nth 1 params) (nth 2 params)
                             `(syntax-table ,table))
        (setq-local parse-sexp-lookup-properties t))
    (setq-local parse-sexp-lookup-properties
                org-src-syntax-table-lookup-properties-orig)))

(defun org-src-syntax-table-region-property-boundaries (prop &optional pos)
  "Return property boundaries for PROP at POS."
  (unless pos (setq pos (point)))
  (let (beg end val)
    (setq val (get-text-property pos prop))
    (if (null val)
        val
      (if (or (bobp)
              (not (eq (get-text-property (1- pos) prop) val)))
          (setq beg pos)
        (setq beg (previous-single-property-change pos prop))
        (when (null beg) (setq beg (point-min))))
      (if (or (eobp)
              (not (eq (get-text-property (1+ pos) prop) val)))
          (setq end pos)
        (setq end (next-single-property-change pos prop))
        (when (null end) (setq end (point-min))))
      (cons beg end))))

;;;###autoload
(define-minor-mode org-src-syntax-table-mode
  "Propertize org src blocks with syntax tables from their major-mode."
  :lighter " stx-tbl"
  :global nil
  (if org-src-syntax-table-mode
      (progn (setq org-src-syntax-table-lookup-properties-orig
                   parse-sexp-lookup-properties)
             (add-hook 'pre-command-hook
                       'org-src-syntax-table-pre-command-hook nil t))
    (remove-hook 'pre-command-hook 'org-src-syntax-table-pre-command-hook t)
    (setq-local parse-sexp-lookup-properties
                org-src-syntax-table-lookup-properties-orig)))

(provide 'org-src-syntax-table)
;;; org-src-syntax-table.el ends here