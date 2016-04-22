;;; make-context.el --- Generate context handling code

;; Copyright (C) 2016 Bar Magal

;; Author: Bar Magal (2016)
;; Version: 0.1
;; Homepage: https://github.com/bmag/local-context

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;; See `make-context' docstring

;;; Code:

(defconst template-file
  (expand-file-name
   "context.template"
   (file-name-as-directory (or (and load-file-name
                                    (file-name-directory load-file-name))
                               default-directory))))

(defun make-context (prefix context-buffer-name)
  "Generate context handling code in a disposable buffer.
PREFIX is a prefix for differentiating your context-powered
feature from context-powered features.
CONTEXT-BUFFER-NAME is the base name to give your local context buffer."
  (interactive "sPrefix: \nsContext Buffer Name: ")
  (switch-to-buffer
   (with-current-buffer (get-buffer-create "*temporary context*")
     (erase-buffer)
     (insert-file-contents-literally template-file)
     (goto-char (point-min))
     (while (search-forward "#PREFIX#" nil t)
       (replace-match prefix t t))
     (goto-char (point-min))
     (while (search-forward "#CONTEXT-BUFFER-NAME#" nil t)
       (replace-match context-buffer-name t t))
     (current-buffer))))

(provide 'make-context)

;;; make-context.el ends here
