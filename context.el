;;; context.el --- example code for making a context-aware feature

;; Copyright (C) 2016 Bar Magal

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

;;; access local context variables

(defun context-set (parameter value &optional frame)
  (set-frame-parameter frame (intern (format "context-%s" parameter)) value)
  value)

(defun context-get (parameter &optional frame)
  (frame-parameter frame (intern (format "context-%s" parameter))))

;;; state of local context

(defun context-state-var (param-name local-variable-name &optional init-value init-form)
  (list param-name local-variable-name init-value init-form))

(defun context-state-var-param-name (state-var)
  (car state-var))

(defun context-state-var-local-name (state-var)
  (nth 1 state-var))

(defun context-state-var-init-val (state-var)
  (if (nth 3 state-var)
      (eval (nth 3 state-var))
    (nth 2 state-var)))

(defvar context-state-variables
  (mapcar (lambda (item)
            (apply #'context-state-var item))
          '(;; example variable
            (var-name name-as-frame-parameter optional-init-value optional-init-form)))
  "List of variables that consist the state of the local context.
It is an list with elements of the form (param-name
local-variable-name initial-value). For each variable in the
alist, there is a frame parameter named
\"context-<param-name>\", and buffer-local variable in the
local context buffer named \"<local-variable-name>.\". The
initial value is optional, and defaults to nil.

When changing perspectives or workspaces, the old frame
parameters are stored buffer-locally in the old context buffer,
and the new frame parameters are loaded from the buffer-locally
stored values in the new context buffer.")

(defun context-store-state (&optional frame)
  (when (buffer-live-p (context-get-buffer))
    (with-current-buffer (context-get-buffer)
      (dolist (var context-state-variables)
        (set (context-state-var-local-name var)
             (context-get (context-state-var-param-name var) frame))))))

(defun context-attach-to-frame (ctx-buff &optional frame)
  "Attach context buffer CTX-BUFF to FRAME.
FRAME defaults to the selected frame.
Return CTX-BUFF."
  (context-set 'buffer ctx-buff frame)
  (with-current-buffer (context-get-buffer)
    (dolist (var context-state-variables)
      (context-set (context-state-var-param-name var)
                  (symbol-value (context-state-var-local-name var))
                  frame)))
  ctx-buff)

(defun context-detach-from-frame (&optional frame)
  (context-set 'buffer nil frame))

;;; get/create context buffer

(defconst context-base-name "*context*")

(defun context-get-buffer (&optional frame deadp)
  "Get FRAME's context buffer.
Return nil if FRAME has no context buffer.
FRAME defaults to the selected frame.
If the context buffer is killed, return nil. If DEADP is non-nil, return
the context buffer even if it was killed."
  (and (or deadp
           (buffer-live-p (context-get 'buffer frame)))
       (context-get 'buffer frame)))

(defun context-create-buffer (&optional frame)
  (context-attach-to-frame
   (with-current-buffer
       (get-buffer-create (generate-new-buffer context-base-name))
     ;; put any buffer initialization code here, before creating the
     ;; buffer-local variables.
     ;; if you want to put code after the local variables are created, make sure
     ;; that code doesn't erase existing buffer-local variables.
     (dolist (var context-state-variables)
       (make-local-variable (context-state-var-local-name var))
       (set (context-state-var-local-name var) (context-state-var-init-val var)))
     (current-buffer))
   frame))

(defun context-get-buffer-create (&optional frame)
  "Get FRAME's context buffer, create one if necessary.
FRAME defaults to the selected frame."
  (or (context-get-buffer frame)
      (context-create-buffer frame)))

;;; workspace-local context buffer

(defvar context-want-eyebrwose-local nil
  "If non-nil, enable Eyebrowse integration.
This means that each Eyebrowse workspace has its own context.
This variable must be set before loading context library.")

(when context-want-eyebrwose-local
  (with-eval-after-load 'eyebrowse
    (defun context-save-to-workspace ()
      "Save context-buffer's state to current workspace."
      (unless (frame-parameter nil 'eyebrowse-context-buffers)
        (set-frame-parameter nil 'eyebrowse-context-buffers (make-hash-table)))
      (let ((current-workspace (eyebrowse--get 'current-slot))
            (ctx-buff (context-get-buffer)))
        (context-store-state)
        (puthash current-workspace ctx-buff (frame-parameter nil 'eyebrowse-context-buffers))))

    (defun context-load-from-workspace ()
      "Load context-buffer's state from current workspace."
      (let* ((eyebrowse-context-buffers (frame-parameter nil 'eyebrowse-context-buffers))
             (current-workspace (eyebrowse--get 'current-slot))
             (ctx-buff (and eyebrowse-context-buffers
                            (gethash current-workspace eyebrowse-context-buffers))))
        (if ctx-buff
            (context-attach-to-frame ctx-buff)
          (context-detach-from-frame))))

    (add-hook 'eyebrowse-pre-window-switch-hook #'context-save-to-workspace)
    (add-hook 'eyebrowse-post-window-switch-hook #'context-load-from-workspace)))

;;; perspective-local context buffer

(defvar context-want-persp-mode-local nil
  "If non-nil, enable persp-mode integration.
This means that each persp-mode perspective has its own context.
This variable must be set before loading context library.")

(when context-want-persp-mode-local
  (with-eval-after-load 'persp-mode
    (defun context-save-to-perspective (&rest _args)
      "Save context-buffer's state to current perspective."
      (context-store-state)
      (set-persp-parameter 'context-buffer (context-get-buffer)))

    (defun context-load-from-perspective ()
      "Load context-buffer's state from current perspective."
      (let ((ctx-buff (persp-parameter 'context-buffer)))
        (if ctx-buff
            (context-attach-to-frame ctx-buff)
          (context-detach-from-frame))))

    (add-hook 'persp-before-switch-functions #'context-save-to-perspective)
    (add-hook 'persp-activated-hook #'context-load-from-perspective)))
