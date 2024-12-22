;;; aidee-core.el  --- AI powered Development Environtment for Emacs. -*- lexical-binding: t; -*-

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


(eval-when-compile
  (require 'cl-macs))
(require 'cl-lib)

;; Copied from ellama and modified
(require 'eieio)
(require 'llm)
(require 'llm-provider-utils)
(require 'spinner)
(require 'compat)
(eval-when-compile (require 'rx))

(defgroup aidee nil
  "Tool for interacting with LLMs."
  :group 'tools)

(defcustom aidee-user-nick "User"
  "User nick in logs."
  :group 'aidee
  :type 'string)

(defcustom aidee-assistant-nick "Aidee"
  "Assistant nick in logs."
  :group 'aidee
  :type 'string)

(defcustom aidee-nick-prefix-depth 2
  "Prefix depth."
  :group 'aidee
  :type 'integer)

(defcustom aidee-provider nil
  "Backend LLM provider."
  :group 'aidee
  :type '(sexp :validate 'llm-standard-provider-p))

(defcustom aidee-spinner-type 'progress-bar
  "Spinner type for aidee."
  :group 'aidee
  :type `(choice ,@(mapcar
		    (lambda (type)
		      `(const ,(car type)))
		    spinner-types)))

(defcustom aidee-command-map
  (let ((map (make-sparse-keymap)))
    ;; code
    (define-key map (kbd "c c") 'aidee-code-complete)
    (define-key map (kbd "c a") 'aidee-code-add)
    (define-key map (kbd "c e") 'aidee-code-edit)
    (define-key map (kbd "c i") 'aidee-code-improve)
    (define-key map (kbd "c r") 'aidee-code-review)
    map)
  "Keymap for aidee commands."
  :group 'aidee
  :type 'keymap)

(defun aidee-setup-keymap ()
  "Set up the Aidee keymap and bindings."
  (interactive)
  (when (boundp 'aidee-keymap-prefix)
    (defvar aidee-keymap (make-sparse-keymap)
      "Keymap for Aidee Commands")

    (when aidee-keymap-prefix
      (define-key global-map (kbd aidee-keymap-prefix) aidee-command-map))))

(defcustom aidee-keymap-prefix nil
  "Key sequence for Aidee Commands."
  :type 'string
  :set (lambda (symbol value)
	 (custom-set-default symbol value)
	 (when value
	   (aidee-setup-keymap)))
  :group 'aidee)

(defcustom aidee-fill-paragraphs '(text-mode)
  "When to wrap paragraphs."
  :group 'aidee
  :type `(choice
          (const :tag "Never fill paragraphs" nil)
          (const :tag "Always fill paragraphs" t)
          (function :tag "By predicate")
          (repeat :tag "In specific modes" (symbol))))

(defcustom aidee-name-prompt-words-count 5
  "Count of words in prompt to generate name."
  :group 'aidee
  :type 'integer)

(defcustom aidee-naming-scheme 'aidee-generate-name-by-words
  "How to name sessions.
If you choose custom function, that function should accept PROVIDER, ACTION
and PROMPT arguments.

PROVIDER is an llm provider.

ACTION is a symbol, current command.

PROMPT is a prompt string."
  :group 'aidee
  :type `(choice
          (const :tag "By first N words of prompt" aidee-generate-name-by-words)
          (const :tag "By current time" aidee-generate-name-by-time)
          (function :tag "By custom function")))

(defcustom aidee-long-lines-length 100
  "Long lines length for fill paragraph call.
Too low value can break generated code by splitting long comment lines."
  :group 'aidee
  :type 'integer)

(defcustom aidee-session-auto-save t
  "Automatically save aidee sessions if set."
  :group 'aidee
  :type 'boolean)

(defcustom aidee-instant-display-action-function nil
  "Display action function for `aidee-instant'."
  :group 'aidee
  :type 'function)

(define-minor-mode aidee-session-mode
  "Minor mode for aidee session buffers."
  :interactive nil
  (if aidee-session-mode
      (progn
        (add-hook 'after-save-hook 'aidee--save-session nil t)
        (add-hook 'kill-buffer-hook 'aidee--session-deactivate nil t))
    (remove-hook 'kill-buffer-hook 'aidee--session-deactivate)
    (remove-hook 'after-save-hook 'aidee--save-session)
    (aidee--session-deactivate)))

(define-minor-mode aidee-request-mode
  "Minor mode for aidee buffers with active request to llm."
  :interactive nil
  :keymap '(([remap keyboard-quit] . aidee--cancel-current-request-and-quit))
  (if aidee-request-mode
      (add-hook 'kill-buffer-hook 'aidee--cancel-current-request nil t)
    (remove-hook 'kill-buffer-hook 'aidee--cancel-current-request)
    (aidee--cancel-current-request)))

(defvar-local aidee--change-group nil)

(defvar-local aidee--current-request nil)

(defcustom aidee-enable-keymap t
  "Enable or disable Aidee keymap."
  :type 'boolean
  :group 'aidee
  :set (lambda (symbol value)
	 (custom-set-default symbol value)
	 (if value
	     (aidee-setup-keymap)
	   ;; If aidee-enable-keymap is nil, remove the key bindings
	   (define-key global-map (kbd aidee-keymap-prefix) nil))))

;;;; Session

(defcustom aidee-sessions-directory (file-truename
				     (file-name-concat
				      user-emacs-directory
				      "aidee-sessions"))
  "Directory for saved aidee sessions."
  :type 'string
  :group 'aidee)

(defvar-local aidee--current-session nil)

(defvar aidee--current-session-id nil)

(defvar aidee--active-sessions (make-hash-table :test #'equal))

(cl-defstruct aidee-session
  "A structure represent aidee session.

ID is an unique identifier of session, string.

PROVIDER is an llm provider of session.

FILE is a path to file contains string representation of this session, string.

PROMPT is a variable contains last prompt in this session.

RESPONSE is a variable contains last response in this session.

CONTEXT contains context for next request."
  id provider file buffer prompt response context)

(defun aidee-get-session-buffer (id)
  "Return aidee session buffer by provided ID."
  (gethash id aidee--active-sessions))

(defun aidee-generate-name-by-words (provider action prompt)
  "Generate name for ACTION by PROVIDER by getting first N words from PROMPT."
  (let* ((cleaned-prompt (replace-regexp-in-string "/" "_" prompt))
         (prompt-words (split-string cleaned-prompt)))
    (string-join
     (flatten-tree
      (list (split-string (format "%s" action) "-")
	    (seq-take prompt-words aidee-name-prompt-words-count)
	    (if (> (length prompt-words) aidee-name-prompt-words-count)
		"..."
	      nil)
	    (format "(%s)" (llm-name provider))))
     " ")))

(defun aidee-get-current-time ()
  "Return string representation of current time."
  (replace-regexp-in-string
   "\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)\\'" "\\1:\\2"
   (format-time-string "%FT%T%z" (current-time))))

(defun aidee-generate-name-by-time (_provider _action _prompt)
  "Generate name for aidee session by current time."
  (aidee-get-current-time))

(defun aidee-generate-name (provider action prompt)
  "Generate name for aidee ACTION by PROVIDER according to PROMPT."
  (replace-regexp-in-string "/" "_" (funcall aidee-naming-scheme provider action prompt)))

(defun aidee-get-session-file-extension ()
  "Return file extension based om the current mode.
Defaults to md, but supports org.  Depends on \"aidee-major-mode.\""
  "md")

(defun aidee-new-session (provider label &optional ephemeral)
  "Create new aidee session with unique id.
Provided PROVIDER and NAME will be used in new session.
If EPHEMERAL non nil new session will not be associated with any file."
  (let* ((name (aidee-generate-name provider 'aidee label))
	 (count 1)
	 (name-with-suffix (format "%s %d" name count))
	 (id (if (not (aidee-get-session-buffer name))
		 name
	       (while (aidee-get-session-buffer name-with-suffix)
		 (setq count (+ count 1))
		 (setq name-with-suffix (format "%s %d" name count)))
	       name-with-suffix))
	 (file-name (when (and (not ephemeral)
			       aidee-session-auto-save)
		      (file-name-concat
		       aidee-sessions-directory
		       (concat id "." (aidee-get-session-file-extension)))))
         (buffer (if file-name
		     (progn
		       (make-directory aidee-sessions-directory t)
		       (find-file-noselect file-name))
		   (get-buffer-create id)))
	 (session (make-aidee-session
		   :id id :provider provider :file file-name
                   :buffer buffer
		   :context nil)))
    (setq aidee--current-session-id id)
    (puthash id buffer aidee--active-sessions)
    (with-current-buffer buffer
      (setq aidee--current-session session)
      (aidee-session-mode +1))
    session))

(defun aidee--cancel-current-request ()
  "Cancel current running request."
  (when aidee--current-request
    (llm-cancel-request aidee--current-request)
    (setq aidee--current-request nil)))

(defun aidee--cancel-current-request-and-quit ()
  "Cancel the current request and quit."
  (interactive)
  (aidee--cancel-current-request)
  (keyboard-quit))

(defun aidee--session-deactivate ()
  "Deactivate current session."
  (aidee--cancel-current-request)
  (when-let* ((session aidee--current-session)
              (id (aidee-session-id session)))
    (when (string= (buffer-name)
                   (buffer-name (aidee-get-session-buffer id)))
      (remhash id aidee--active-sessions)
      (when (equal aidee--current-session-id id)
	(setq aidee--current-session-id nil)))))

(defun aidee--get-session-file-name (file-name)
  "Get aidee session file name for FILE-NAME."
  (let* ((base-name (file-name-nondirectory file-name))
	 (dir (file-name-directory file-name))
	 (session-file-name
	  (file-name-concat
	   dir
	   (concat "." base-name ".session.el"))))
    session-file-name))

(defun aidee--save-session ()
  "Save current aidee session."
  (when aidee--current-session
    (let* ((session aidee--current-session)
	   (file-name (aidee-session-file session))
	   (session-file-name (aidee--get-session-file-name file-name)))
      (with-temp-file session-file-name
	(insert (prin1-to-string session))))))

(defun aidee-stream (prompt &rest args)
  "Query aidee for PROMPT.
ARGS contains keys for fine control.

:provider PROVIDER -- PROVIDER is an llm provider for generation.

:buffer BUFFER -- BUFFER is the buffer (or `buffer-name') to insert aidee reply
in.  Default value is (current-buffer).

:point POINT -- POINT is the point in buffer to insert aidee reply at.

:filter FILTER -- FILTER is a function that's applied to (partial) response
strings before they're inserted into the BUFFER.

:session SESSION -- SESSION is a aidee conversation session.

:session-id ID -- ID is a aidee session unique identifier.

:ephemeral-session BOOL -- if BOOL is set session will not be saved to named
file by default.

:on-error ON-ERROR -- ON-ERROR a function that's called with an error message on
failure (with BUFFER current).

:on-done ON-DONE -- ON-DONE a function or list of functions that's called with
 the full response text when the request completes (with BUFFER current)."
  (let* ((session-id (plist-get args :session-id))
	 (session (or (plist-get args :session)
		      (when session-id
			(with-current-buffer (aidee-get-session-buffer session-id)
			  aidee--current-session))))
         (provider (or (plist-get args :provider)
                       (if session
                           (aidee-session-provider session)
                         aidee-provider)))
	 (buffer (or (plist-get args :buffer)
		     (when (aidee-session-p session)
		       (aidee-get-session-buffer (aidee-session-id session)))
		     (current-buffer)))
	 (point (or (plist-get args :point)
		    (with-current-buffer buffer (point))))
	 (filter (or (plist-get args :filter) #'identity))
	 (errcb (or (plist-get args :on-error)
		    (lambda (msg)
		      (error "Error calling the LLM: %s" msg))))
	 (donecb (or (plist-get args :on-done) #'ignore))
         (llm-prompt (llm-make-simple-chat-prompt prompt))
         (invoke-buffer (current-buffer)))
    (with-current-buffer invoke-buffer
      (spinner-start aidee-spinner-type))
    (with-current-buffer buffer
      (aidee-request-mode +1)
      (let* ((start (make-marker))
	     (end (make-marker))
	     (insert-text
	      (lambda (text)
		;; Erase and insert the new text between the marker cons.
		(with-current-buffer buffer
		  ;; Manually save/restore point as save-excursion doesn't
		  ;; restore the point into the middle of replaced text.
		  (let ((pt (point)))
		    (goto-char start)
		    (delete-region start end)
		    (insert (funcall filter text))
                    (when (pcase aidee-fill-paragraphs
                            ((cl-type function) (funcall aidee-fill-paragraphs))
                            ((cl-type boolean) aidee-fill-paragraphs)
                            ((cl-type list) (and (apply #'derived-mode-p
							aidee-fill-paragraphs)
						 (not (equal major-mode 'org-mode)))))
                      (fill-region start (point)))
		    (goto-char pt))
		  (undo-amalgamate-change-group aidee--change-group)))))
	(setq aidee--change-group (prepare-change-group))
	(activate-change-group aidee--change-group)
	(set-marker start point)
	(set-marker end point)
	(set-marker-insertion-type start nil)
	(set-marker-insertion-type end t)
	(setq aidee--current-request
	      (llm-chat-streaming provider
				  llm-prompt
				  nil
				  (lambda (text)
				    (funcall insert-text (string-trim text))
                                    (with-current-buffer invoke-buffer
                                      (spinner-stop))
				    (with-current-buffer buffer
				      (accept-change-group aidee--change-group)
				      (if (and (listp donecb)
					       (functionp (car donecb)))
					  (mapc (lambda (fn) (funcall fn text))
						donecb)
					(funcall donecb text))
                                      (when session
                                        (setf (aidee-session-response session) text))
				      (setq aidee--current-request nil)
				      (aidee-request-mode -1)))
	                          (lambda (_ msg)
		                    (with-current-buffer buffer
		                      (cancel-change-group aidee--change-group)
		                      (spinner-stop)
		                      (funcall errcb msg)
		                      (setq aidee--current-request nil)
		                      (aidee-request-mode -1)))))))))

(defun aidee-instant (prompt &rest args)
  "Prompt aidee for PROMPT to reply instantly.

ARGS contains keys for fine control.

:provider PROVIDER -- PROVIDER is an llm provider for generation."
  (let* ((provider (or (plist-get args :provider)
		       aidee-provider))
	 (buffer-name (aidee-generate-name provider real-this-command prompt))
	 (buffer (get-buffer-create (if (get-buffer buffer-name)
					(make-temp-name (concat buffer-name " "))
				      buffer-name))))
    (display-buffer buffer (when aidee-instant-display-action-function
			     `((ignore . (,aidee-instant-display-action-function)))))
    (aidee-stream prompt
		  :buffer buffer
		  :provider provider)))

(provide 'aidee-core)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; aidee-core.el ends here

