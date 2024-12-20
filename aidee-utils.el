;;; aidee-utils.el  --- AI powered Development Environtment for Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 zbelial zjyzhaojiyang@gmail.com

;; Author: zbelial zjyzhaojiyang@gmail.com

;; Maintainer: zbelial zjyzhaojiyang@gmail.com

;; Homepage: https://github.com/zbelial/aidee.el.git
;; Version: 0.1.0
;; Keywords: AI LLM Emacs Coding


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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


;;; Code:


(require 'aidee-core)

(defcustom aidee-generate-commit-message-template "<INSTRUCTIONS>
You are professional software developer.

Write concise commit message based on diff in the following format:
<FORMAT>
First line should contain short title described major change in functionality.
Then one empty line. Then detailed description of all changes.
</FORMAT>
<EXAMPLE>
Improve abc

Improved abc feature by adding new xyz module.
</EXAMPLE>

**Reply with commit message only without any quotes.**
</INSTRUCTIONS>

<DIFF>
%s
</DIFF>"
  "Prompt template for `aidee-generate-commit-message'."
  :group 'aidee
  :type 'string)

;;;; Chain
(defcustom aidee-always-show-chain-steps nil
  "Always show aidee chain buffers."
  :type 'boolean
  :group 'aidee)

(defun aidee-chain (initial-prompt forms &optional acc)
  "Call chain of FORMS on INITIAL-PROMPT.
ACC will collect responses in reverse order (previous answer will be on top).
Each form is a plist that can contain different options:

:provider PROVIDER - use PROVIDER instead of `aidee-provider'.

:transform FUNCTION - use FUNCTION to transform result of previous step to new
prompt.  FUCTION will be called with two arguments INITIAL-PROMPT and ACC.

:session SESSION - use SESSION in current step.

:session-id ID -- ID is a aidee session unique identifier.

:chat BOOL - if BOOL use chat buffer, otherwise use temp buffer.  Make sense for
last step only.

:show BOOL - if BOOL show buffer for this step."
  (let* ((hd (car forms))
	 (tl (cdr forms))
	 (provider (or (plist-get hd :provider) aidee-provider))
	 (transform (plist-get hd :transform))
	 (prompt (if transform
		     (apply transform (list initial-prompt acc))
		   initial-prompt))
	 (session-id (plist-get hd :session-id))
	 (session (or (plist-get hd :session)
		      (when session-id
			(with-current-buffer (aidee-get-session-buffer session-id)
			  aidee--current-session))))
	 (chat (plist-get hd :chat))
	 (show (or (plist-get hd :show) aidee-always-show-chain-steps))
	 (buf (if (or (and (not chat)) (not session))
		  (get-buffer-create (make-temp-name
				      (aidee-generate-name provider real-this-command prompt)))
		(aidee-get-session-buffer aidee--current-session-id))))
    (when show
      (display-buffer buf (if chat (when aidee-chat-display-action-function
				     `((ignore . (,aidee-chat-display-action-function))))
			    (when aidee-instant-display-action-function
			      `((ignore . (,aidee-instant-display-action-function)))))))
    (with-current-buffer buf
      (funcall aidee-major-mode))
    (if chat
	(aidee-chat
	 prompt
	 nil
	 :provider provider
	 :on-done (lambda (res)
		    (when tl
		      (aidee-chain res tl (cons res acc)))))
      (aidee-stream
       prompt
       :provider provider
       :buffer buf
       :session session
       :filter (when (derived-mode-p 'org-mode)
		 #'aidee--translate-markdown-to-org-filter)
       :on-done (lambda (res)
		  (when tl
		    (aidee-chain res tl (cons res acc))))))))

;;;###autoload
(defun aidee-solve-reasoning-problem (problem)
  "Solve reasoning PROBLEM with absctraction of thought.
Problem will be solved with the chain of questions to LLM."
  (interactive "sProblem: ")
  (aidee-chain
   problem
   '((:chat t
	    :transform (lambda (problem _)
			 (format "Problem:
%s

Let's think logically and provide abstract higher order plan how to solve this kind
of problems. Don't dive into small details only provide high-level plan." problem)))
     (:chat t
	    :transform (lambda (_ _)
			 "Provide more detailed plan. On what details should we pay attention?"))
     (:chat t
	    :transform (lambda (_ _)
			 "Now revise the plan and provide the final solution."))
     (:chat t
	    :transform (lambda (_ _)
			 "Provide short final answer based on final solution.")))))

;;;###autoload
(defun aidee-solve-domain-specific-problem (problem)
  "Solve domain-specific PROBLEM with `aidee-chain'."
  (interactive "sProblem: ")
  (aidee-chain
   problem
   `((:transform (lambda (problem _)
		   (format "Problem:
%s

Which specialist suits better for solving this kind of problems?"
			   problem)))
     (:transform (lambda (res _)
		   (format "Message:
%s

Extract profession from this message. Be short and concise."
			   res)))
     (:chat t
	    :transform (lambda (profession _)
			 (format
			  "You are professional %s. Do your best and create detailed plan how to solve this problem:
%s"
			  (string-trim profession) ,problem)))
     (:chat t
	    :transform (lambda (_ _)
			 "Now revise the plan and provide the final solution."))
     (:chat t
	    :transform (lambda (_ _)
			 "Provide short final answer based on final solution.")))))

;;;###autoload
(defun aidee-chat-send-last-message ()
  "Send last user message extracted from current aidee chat buffer."
  (interactive)
  (when-let* ((session aidee--current-session)
	      (message (aidee-get-last-user-message))
	      ((length> message 0))
	      (text (if (derived-mode-p 'org-mode)
			(aidee-convert-org-to-md message)
		      message)))
    (goto-char (point-max))
    (insert "\n\n")
    (when (aidee-session-context session)
      (insert (aidee--format-context session)))
    (insert (aidee-get-nick-prefix-for-mode) " " aidee-assistant-nick ":\n")
    (aidee-stream text
		  :session session
		  :on-done #'aidee-chat-done
		  :filter (when (derived-mode-p 'org-mode)
			    #'aidee--translate-markdown-to-org-filter))))

;;;###autoload
(defun aidee-ask-about ()
  "Ask aidee about selected region or current buffer."
  (interactive)
  (let ((input (read-string "Ask aidee about this text: ")))
    (if (region-active-p)
	(aidee-context-add-selection)
      (aidee-context-add-buffer (buffer-name (current-buffer))))
    (aidee-chat input)))

;;;###autoload
(defun aidee-ask-selection ()
  "Send selected region or current buffer to aidee chat."
  (interactive)
  (let ((text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max)))))
    (aidee-chat text)))

;;;###autoload
(defun aidee-complete ()
  "Complete text in current buffer."
  (interactive)
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point)))
	 (text (buffer-substring-no-properties beg end)))
    (aidee-stream text)))

(defvar vc-git-diff-switches)
(declare-function vc-diff-internal "vc")
(declare-function vc-deduce-fileset "vc")

(defun aidee--diff-cached ()
  "Diff staged."
  (require 'vc)
  (let* ((default-directory
	  (if (string= ".git"
		       (car (reverse
			     (cl-remove
			      ""
			      (file-name-split default-directory)
			      :test #'string=))))
	      (file-name-parent-directory default-directory)
	    default-directory))
	 (vc-git-diff-switches "--cached")
	 (diff (with-temp-buffer
		 (vc-diff-internal
		  nil (vc-deduce-fileset t) nil nil nil (current-buffer))
		 (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string-empty-p diff)
	nil
      diff)))

(defun aidee--diff ()
  "Diff unstaged."
  (require 'vc)
  (let* ((default-directory
	  (if (string= ".git"
		       (car (reverse
			     (cl-remove
			      ""
			      (file-name-split default-directory)
			      :test #'string=))))
	      (file-name-parent-directory default-directory)
	    default-directory))
	 (vc-git-diff-switches t)
	 (diff (with-temp-buffer
		 (vc-diff-internal
		  nil (vc-deduce-fileset t) nil nil nil (current-buffer))
		 (buffer-substring-no-properties (point-min) (point-max)))))
    (if (string-empty-p diff)
	nil
      diff)))

;;;###autoload
(defun aidee-generate-commit-message ()
  "Generate commit message based on diff."
  (interactive)
  (save-window-excursion
    (when-let* ((default-directory
		 (if (string= ".git"
			      (car (reverse
				    (cl-remove
				     ""
				     (file-name-split default-directory)
				     :test #'string=))))
		     (file-name-parent-directory default-directory)
		   default-directory))
		(diff (or (aidee--diff-cached)
			  (aidee--diff))))
      (aidee-stream
       (format aidee-generate-commit-message-template diff)
       :provider aidee-coding-provider))))

;;;###autoload
(defun aidee-ask-line ()
  "Send current line to aidee chat."
  (interactive)
  (let ((text (thing-at-point 'line)))
    (aidee-chat text)))


;;;###autoload
(defun aidee-provider-select ()
  "Select aidee provider."
  (interactive)
  (let* ((ollama-binary (executable-find aidee-ollama-binary))
	 (providers (append
                     `(("default model" . aidee-provider)
		       ,(if (and ollama-binary
				 (file-exists-p ollama-binary))
			    '("ollama model" . (aidee-get-ollama-local-model))))
                     aidee-providers))
	 (variants (mapcar #'car providers)))
    (setq aidee-provider
	  (eval (alist-get
		 (completing-read "Select model: " variants)
		 providers nil nil #'string=)))
    (setq aidee--current-session-id nil)))

;;;; Other routines
(defconst aidee--uri-path-allowed-chars
  (let ((vec (copy-sequence url-path-allowed-chars)))
    (aset vec ?: nil)
    vec)
  "Like `url-path-allows-chars' but more restrictive.")

(defun aidee--path-to-uri (path)
  "URIfy PATH."
  (let* ((truepath (file-truename path))
         (full-name (directory-file-name (file-local-name truepath))))
    (if (eq system-type 'windows-nt)
        (let ((label (url-type (url-generic-parse-url path)))
              prefix)
          (setq prefix (concat label ":"))
          (concat "file:///"
                  prefix
                  (url-hexify-string
                   (substring full-name (length prefix))
                   aidee--uri-path-allowed-chars)))
      (concat "file://"
              (url-hexify-string
               ;; Again watch out for trampy paths.
               (directory-file-name (file-local-name truepath))
               aidee--uri-path-allowed-chars)))))

(defun aidee--uri-to-path (uri)
  "Convert URI to a file path."
  (when (keywordp uri)
    (setq uri (substring (symbol-name uri) 1)))
  (let ((retval (url-unhex-string (url-filename (url-generic-parse-url uri)))))
    (if (eq system-type 'windows-nt)
        (substring retval 1)
      retval)))

(defun aidee--root-uri ()
  (when-let* ((proj (project-current))
              (root-uri (project-root proj)))
    (aidee--path-to-uri root-uri)))

(defmacro aidee-with-file-open-temporarily (file close-after-body &rest body)
  "Load FILE and then evaluate BODY with FILE's buffer the current buffer.
If FILE has not been loaded, and CLOSE-AFTER-BODY is not nil,
then kill the buffer after evaluating BODY.
The value returned is the value of the last form in BODY."
  (declare (indent 1) (debug t))
  (let ((tempvar-buffer (make-symbol "buffer"))
        (tempvar-close-flag (make-symbol "close-flag"))
        (tempvar-result (make-symbol "result")))
    `(let ((,tempvar-buffer (get-file-buffer ,file))
           (,tempvar-close-flag nil)
           (,tempvar-result nil))
       (if ,tempvar-buffer
           (setq ,tempvar-close-flag nil)
         (when ,close-after-body
           (setq ,tempvar-close-flag t))
         (setq ,tempvar-buffer (find-file-noselect ,file)))
       (with-current-buffer ,tempvar-buffer
         (setq ,tempvar-result (progn ,@body))
         (when ,tempvar-close-flag
           (save-buffer ,tempvar-buffer)
           (kill-buffer ,tempvar-buffer))
         ,tempvar-result))))

(defun aidee--current-timestamp ()
  (time-convert (current-time) 'integer))

(provide 'aidee-utils)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; aidee-utils.el ends here
