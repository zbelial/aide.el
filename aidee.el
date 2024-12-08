;;; aidee.el  --- AI powered Development Environtment for Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 zbelial zjyzhaojiyang@gmail.com

;; Author: zbelial zjyzhaojiyang@gmail.com

;; Maintainer: zbelial zjyzhaojiyang@gmail.com

;; Homepage: https://github.com/zbelial/aidee.el.git
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (ellama "0.12.7"))
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


(eval-and-compile
  (require 'cl-macs))
(require 'cl-lib)

;;;; copied from ellama and modified
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

(defcustom aidee-provider
  (progn
    (declare-function make-llm-ollama "llm-ollama")
    (require 'llm-ollama)
    (make-llm-ollama
     :chat-model "zephyr" :embedding-model "zephyr"))
  "Backend LLM provider."
  :group 'aidee
  :type '(sexp :validate 'llm-standard-provider-p))

(defcustom aidee-coding-provider nil
  "LLM provider for coding tasks."
  :group 'aidee
  :type '(sexp :validate 'llm-standard-provider-p))

(defcustom aidee-providers nil
  "LLM provider list for fast switching."
  :group 'aidee
  :type '(alist :key-type string
		:value-type (sexp :validate 'llm-standard-provider-p)))

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
    (define-key map (kbd "c m") 'aidee-generate-commit-message)
    ;; session
    (define-key map (kbd "s l") 'aidee-load-session)
    (define-key map (kbd "s r") 'aidee-session-rename)
    (define-key map (kbd "s d") 'aidee-session-remove)
    (define-key map (kbd "s a") 'aidee-session-switch)
    ;; ask
    (define-key map (kbd "a a") 'aidee-ask-about)
    (define-key map (kbd "a i") 'aidee-chat)
    (define-key map (kbd "a l") 'aidee-ask-line)
    (define-key map (kbd "a s") 'aidee-ask-selection)
    ;; text
    (define-key map (kbd "t c") 'aidee-complete)
    ;; context
    (define-key map (kbd "x b") 'aidee-context-add-buffer)
    (define-key map (kbd "x f") 'aidee-context-add-file)
    (define-key map (kbd "x s") 'aidee-context-add-selection)
    (define-key map (kbd "x i") 'aidee-context-add-info-node)
    ;; provider
    (define-key map (kbd "p s") 'aidee-provider-select)
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

(defcustom aidee-ollama-binary "ollama"
  "Path to ollama binary."
  :type 'string
  :group 'aidee)

(defcustom aidee-auto-scroll nil
  "If enabled aidee buffer will scroll automatically during generation."
  :type 'boolean
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

(defcustom aidee-code-review-prompt-template "You are professional software engineer. Review provided code and make concise suggestions."
  "Prompt template for `aidee-code-review'."
  :group 'aidee
  :type 'string)

(defcustom aidee-change-prompt-template "Change the following text, %s, just output the final text without additional quotes around it:\n%s"
  "Prompt template for `aidee-change'."
  :group 'aidee
  :type 'string)

(defcustom aidee-code-edit-prompt-template "Regarding the following code, %s, only output the result code in format ```language\n...\n```:\n```\n%s\n```\nWrite all the code in single code block."
  "Prompt template for `aidee-code-edit'."
  :group 'aidee
  :type 'string)

(defcustom aidee-code-improve-prompt-template "Enhance the following code, only output the result code in format ```language\n...\n```:\n```\n%s\n```\nWrite all the code in single code block."
  "Prompt template for `aidee-code-improve'."
  :group 'aidee
  :type 'string)

(defcustom aidee-code-complete-prompt-template "Continue the following code, only write new code in format ```language\n...\n```:\n```\n%s\n```\nWrite all the code in single code block."
  "Prompt template for `aidee-code-complete'."
  :group 'aidee
  :type 'string)

(defcustom aidee-code-add-prompt-template "Context: \n```\n%s\n```\nBased on this context, %s, only output the result in format ```\n...\n```\nWrite all the code in single code block."
  "Prompt template for `aidee-code-add'."
  :group 'aidee
  :type 'string)

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

(defcustom aidee-chat-done-callback nil
  "Callback that will be called on aidee chat response generation done.
It should be a function with single argument generated text string."
  :group 'aidee
  :type 'function)

(defcustom aidee-major-mode 'org-mode
  "Major mode for aidee commands."
  :group 'aidee
  :type 'symbol)

(defcustom aidee-long-lines-length 100
  "Long lines length for fill paragraph call.
Too low value can break generated code by splitting long comment lines."
  :group 'aidee
  :type 'integer)

(defcustom aidee-session-auto-save t
  "Automatically save aidee sessions if set."
  :group 'aidee
  :type 'boolean)

(defcustom aidee-show-quotes nil
  "Show quotes in chat context."
  :group 'aidee
  :type 'boolean)

(defcustom aidee-chat-display-action-function nil
  "Display action function for `aidee-chat'."
  :group 'aidee
  :type 'function)

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

(defconst aidee--code-prefix
  (rx (minimal-match
       (zero-or-more anything) (literal "```") (zero-or-more anything) (+ (or "\n" "\r")))))

(defconst aidee--code-suffix
  (rx (minimal-match
       (literal "```") (zero-or-more anything))))

(defun aidee--code-filter (text)
  "Filter code prefix/suffix from TEXT."
  ;; Trim left first as `string-trim' trims from the right and ends up deleting all the code.
  (string-trim-right (string-trim-left text aidee--code-prefix) aidee--code-suffix))

(defun aidee--fill-long-lines (text)
  "Fill long lines only in TEXT."
  (with-temp-buffer
    (insert (propertize text 'hard t))
    (let ((fill-column aidee-long-lines-length)
	  (use-hard-newlines t))
      (fill-region (point-min) (point-max) nil t t))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun aidee--replace-first-begin-src (text)
  "Replace first begin src in TEXT."
  (if (not (string-match-p (rx (literal "#+BEGIN_SRC")) text))
      (replace-regexp-in-string "^[[:space:]]*```\\(\\(.\\|\n\\)*\\)" "#+BEGIN_SRC\\1" text)
    text))

(defun aidee--replace-bad-code-blocks (text)
  "Replace code src blocks in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; skip good code blocks
    (while (re-search-forward "#\\+BEGIN_SRC\\(.\\|\n\\)*?#\\+END_SRC" nil t))
    (while (re-search-forward "#\\+END_SRC\\(\\(.\\|\n\\)*?\\)#\\+END_SRC" nil t)
      (replace-match "#+BEGIN_SRC\\1#+END_SRC"))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun aidee--replace (from to beg end)
  "Replace FROM to TO in region BEG END."
  (goto-char beg)
  (while (re-search-forward from end t)
    (replace-match to)))

(defun aidee--apply-transformations (beg end)
  "Apply md to org transformations for region BEG END."
  ;; headings
  (aidee--replace "^# " "* " beg end)
  (aidee--replace "^## " "** " beg end)
  (aidee--replace "^### " "*** " beg end)
  (aidee--replace "^#### " "**** " beg end)
  (aidee--replace "^##### " "***** " beg end)
  (aidee--replace "^###### " "****** " beg end)
  ;; bold
  (aidee--replace "__\\(.+?\\)__" "*\\1*" beg end)
  (aidee--replace "\\*\\*\\(.+?\\)\\*\\*" "*\\1*" beg end)
  (aidee--replace "<b>\\(.+?\\)</b>" "*\\1*" beg end)
  ;; italic
  (aidee--replace "_\\(.+?\\)_" "/\\1/" beg end)
  (aidee--replace "<i>\\(.+?\\)</i>" "/\\1/" beg end)
  ;; underlined
  (aidee--replace "<u>\\(.+?\\)</u>" "_\\1_" beg end)
  ;; inline code
  (aidee--replace "`\\(.+?\\)`" "~\\1~" beg end)
  ;; lists
  (aidee--replace "^\\* " "+ " beg end)
  ;; strikethrough
  (aidee--replace "~~\\(.+?\\)~~" "+\\1+" beg end)
  (aidee--replace "<s>\\(.+?\\)</s>" "+\\1+" beg end)
  ;; badges
  (aidee--replace "\\[\\!\\[.*?\\](\\(.*?\\))\\](\\(.*?\\))" "[[\\2][file:\\1]]" beg end)
  ;;links
  (aidee--replace "\\[\\(.*?\\)\\](\\(.*?\\))" "[[\\2][\\1]]" beg end)

  ;; filling long lines
  (goto-char beg)
  (let ((fill-column aidee-long-lines-length)
	(use-hard-newlines t))
    (fill-region beg end nil t t)))

(defun aidee--replace-outside-of-code-blocks (text)
  "Replace some markdown elements to org in TEXT outside of code blocks."
  (with-temp-buffer
    (insert (propertize text 'hard t))
    (goto-char (point-min))
    ;; apply transformations outside of code blocks
    (let ((beg (point-min))
	  (end (or (re-search-forward "#\\+BEGIN_SRC" nil t)
		   (point-max))))
      (aidee--apply-transformations beg end)
      (goto-char beg)
      (re-search-forward "#\\+BEGIN_SRC" nil t)
      (while (when-let ((beg (re-search-forward "#\\+END_SRC" nil t))
			(end (or (re-search-forward "#\\+BEGIN_SRC" nil t)
				 (point-max))))
	       (aidee--apply-transformations beg end)
	       (goto-char beg))))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun aidee--translate-markdown-to-org-filter (text)
  "Filter to translate code blocks from markdown syntax to org syntax in TEXT.
This filter contains only subset of markdown syntax to be good enough."
  (thread-last
    text
    ;; code blocks
    (replace-regexp-in-string "^[[:space:]]*```\\(.+\\)$" "#+BEGIN_SRC \\1")
    (aidee--replace-first-begin-src)
    (replace-regexp-in-string "^<!-- language: \\(.+\\) -->\n```" "#+BEGIN_SRC \\1")
    (replace-regexp-in-string "^[[:space:]]*```$" "#+END_SRC")
    (replace-regexp-in-string "^[[:space:]]*```" "#+END_SRC\n")
    (replace-regexp-in-string "```" "\n#+END_SRC\n")
    (aidee--replace-bad-code-blocks)
    (aidee--replace-outside-of-code-blocks)))

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

(defcustom aidee-sessions-directory (file-truename
				     (file-name-concat
				      user-emacs-directory
				      "aidee-sessions"))
  "Directory for saved aidee sessions."
  :type 'string
  :group 'aidee)

(defcustom aidee-always-show-chain-steps nil
  "Always show aidee chain buffers."
  :type 'boolean
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

CONTEXT contains context for next request."
  id provider file prompt context)

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

(defvar aidee--new-session-context nil)

(defun aidee-get-nick-prefix-for-mode ()
  "Return preferred header prefix char based om the current mode.
Defaults to #, but supports `org-mode'.  Depends on `aidee-major-mode'."
  (let* ((prefix-char
          (cond ((provided-mode-derived-p aidee-major-mode 'org-mode) ?*)
                (t ?#))))
    (make-string aidee-nick-prefix-depth prefix-char)))

(defun aidee-get-session-file-extension ()
  "Return file extension based om the current mode.
Defaults to md, but supports org.  Depends on \"aidee-major-mode.\""
  (cond ((provided-mode-derived-p aidee-major-mode 'org-mode) "org")
        (t "md")))

(defun aidee-new-session (provider prompt &optional ephemeral)
  "Create new aidee session with unique id.
Provided PROVIDER and PROMPT will be used in new session.
If EPHEMERAL non nil new session will not be associated with any file."
  (let* ((name (aidee-generate-name provider 'aidee prompt))
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
 	 (previous-session
	  (when aidee--current-session-id
	    (with-current-buffer
		(aidee-get-session-buffer aidee--current-session-id)
	      aidee--current-session)))
	 (session (make-aidee-session
		   :id id :provider provider :file file-name
		   :context (if previous-session
				(aidee-session-context previous-session)
			      aidee--new-session-context)))
	 (buffer (if file-name
		     (progn
		       (make-directory aidee-sessions-directory t)
		       (find-file-noselect file-name))
		   (get-buffer-create id))))
    (setq aidee--new-session-context nil)
    (setq aidee--current-session-id id)
    (puthash id buffer aidee--active-sessions)
    (with-current-buffer buffer
      (funcall aidee-major-mode)
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

(defun aidee--get-translation-file-name (file-name)
  "Get aidee translation file name for FILE-NAME."
  (let* ((base-name (file-name-base file-name))
	 (ext (file-name-extension file-name))
	 (dir (file-name-directory file-name))
	 (translation-file-name
	  (file-name-concat
	   dir
	   (concat base-name ".translation"
		   (when ext
		     (concat "." ext))))))
    translation-file-name))

(defun aidee--save-session ()
  "Save current aidee session."
  (when aidee--current-session
    (let* ((session aidee--current-session)
	   (file-name (aidee-session-file session))
	   (session-file-name (aidee--get-session-file-name file-name)))
      (with-temp-file session-file-name
	(insert (prin1-to-string session))))))

;;;###autoload
(defun aidee-load-session ()
  "Load aidee session from file."
  (interactive)
  (when-let* ((dir (if current-prefix-arg
		       (read-directory-name
			"Select directory containing sessions: "
			aidee-sessions-directory)
		     aidee-sessions-directory))
	      (file-name (file-name-concat
			  aidee-sessions-directory
			  (completing-read
			   "Select session to load: "
			   (directory-files
			    aidee-sessions-directory nil "^[^\.].*"))))
	      (session-file-name (aidee--get-session-file-name file-name))
	      (session-file-exists (file-exists-p session-file-name))
	      (buffer (find-file-noselect file-name))
	      (session-buffer (find-file-noselect session-file-name)))
    (with-current-buffer session-buffer
      (goto-char (point-min))
      ;; old sessions support
      (when (string= "(setq "
		     (buffer-substring-no-properties 1 7))
	(goto-char (point-min))
	;; skip "("
	(forward-char)
	;; skip setq
	(forward-sexp)
	;; skip aidee--current-session
	(forward-sexp)
	;; skip space
	(forward-char)
	;; remove all above
	(kill-region (point-min) (point))
	(goto-char (point-max))
	;; remove ")"
	(delete-char -1)
	;; save session in new format
	(save-buffer)
	(goto-char (point-min))))
    (with-current-buffer buffer
      ;; support sessions without user nick at the end of buffer
      (when (not (save-excursion
		   (save-match-data
		     (goto-char (point-max))
		     (and (search-backward (concat (aidee-get-nick-prefix-for-mode) " " aidee-user-nick ":\n") nil t)
			  (search-forward (concat (aidee-get-nick-prefix-for-mode) " " aidee-user-nick ":\n") nil t)
			  (equal (point) (point-max))))))
	(goto-char (point-max))
	(insert (aidee-get-nick-prefix-for-mode) " " aidee-user-nick ":\n")
	(save-buffer))
      (let ((session (read session-buffer)))
	(setq aidee--current-session
	      (make-aidee-session
	       :id (aidee-session-id session)
	       :provider (aidee-session-provider session)
	       :file (aidee-session-file session)
	       :prompt (aidee-session-prompt session)
	       :context aidee--new-session-context)))
      (setq aidee--new-session-context nil)
      (setq aidee--current-session-id (aidee-session-id aidee--current-session))
      (puthash (aidee-session-id aidee--current-session)
	       buffer aidee--active-sessions)
      (aidee-session-mode +1))
    (kill-buffer session-buffer)
    (display-buffer buffer (when aidee-chat-display-action-function
			     `((ignore . (,aidee-chat-display-action-function)))))))

;;;###autoload
(defun aidee-session-remove ()
  "Remove aidee session."
  (interactive)
  (let* ((id (completing-read
	      "Select session to remove: "
	      (hash-table-keys aidee--active-sessions)))
	 (buffer (aidee-get-session-buffer id))
	 (file (buffer-file-name buffer))
	 (session-file (aidee--get-session-file-name file))
	 (translation-file (aidee--get-translation-file-name file)))
    (kill-buffer buffer)
    (delete-file file t)
    (delete-file session-file t)
    (mapc
     (lambda (buf)
       (when (and (buffer-file-name buf)
		  (file-equal-p (buffer-file-name buf)
				translation-file))
	 (kill-buffer buf)))
     (buffer-list))
    (when (file-exists-p translation-file)
      (delete-file translation-file t))))

(defun aidee-activate-session (id)
  "Change current active session to session with ID."
  (setq aidee--current-session-id id))

;;;###autoload
(defun aidee-session-switch ()
  "Change current active session."
  (interactive)
  (let* ((id (completing-read
	      "Select session to activate: "
	      (hash-table-keys aidee--active-sessions)))
	 (buffer (aidee-get-session-buffer id)))
    (aidee-activate-session id)
    (display-buffer buffer (when aidee-chat-display-action-function
			     `((ignore . (,aidee-chat-display-action-function)))))))

;;;###autoload
(defun aidee-session-rename ()
  "Rename current aidee session."
  (interactive)
  (when-let* ((id (if aidee--current-session
		      (aidee-session-id aidee--current-session)
		    aidee--current-session-id))
	      (buffer (aidee-get-session-buffer id))
	      (session (with-current-buffer buffer
			 aidee--current-session))
	      (file-name (buffer-file-name buffer))
	      (file-ext (file-name-extension file-name))
	      (dir (file-name-directory file-name))
	      (session-file-name (aidee--get-session-file-name file-name))
	      (new-id (read-string
		       "New session name: "
		       id))
	      (new-file-name (file-name-concat
			      dir
			      (concat new-id "." file-ext)))
	      (new-session-file-name
	       (aidee--get-session-file-name new-file-name)))
    (with-current-buffer buffer
      (set-visited-file-name new-file-name))
    (when (file-exists-p file-name)
      (rename-file file-name new-file-name))
    (when (file-exists-p session-file-name)
      (rename-file session-file-name new-session-file-name))
    (setf (aidee-session-id session) new-id)
    (when (equal aidee--current-session-id id)
      (setq aidee--current-session-id new-id))
    (remhash id aidee--active-sessions)
    (puthash new-id buffer aidee--active-sessions)))

;; Context elements

(defclass aidee-context-element () ()
  "A structure for holding information about a context element.")

(cl-defgeneric aidee-context-element-add (element)
  "Add the ELEMENT to the Aidee context.")

(cl-defgeneric aidee-context-element-extract (element)
  "Extract the content of the context ELEMENT.")

(cl-defgeneric aidee-context-element-format (element mode)
  "Format the context ELEMENT for the major MODE.")

(cl-defmethod aidee-context-element-add ((element aidee-context-element))
  "Add the ELEMENT to the Aidee context."
  (if-let* ((id aidee--current-session-id)
	    (session (with-current-buffer (aidee-get-session-buffer id)
		       aidee--current-session)))
      (push element (aidee-session-context session))
    (push element aidee--new-session-context)))

;; Buffer context element

(defclass aidee-context-element-buffer (aidee-context-element)
  ((name :initarg :name :type string))
  "A structure for holding information about a context element.")

(cl-defmethod aidee-context-element-extract
  ((element aidee-context-element-buffer))
  "Extract the content of the context ELEMENT."
  (with-slots (name) element
    (with-current-buffer name
      (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defmethod aidee-context-element-format
  ((element aidee-context-element-buffer) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "```emacs-lisp\n(display-buffer \"%s\")\n```\n" name)))

(cl-defmethod aidee-context-element-format
  ((element aidee-context-element-buffer) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "[[elisp:(display-buffer \"%s\")][%s]]" name name)))

;; File context element

(defclass aidee-context-element-file (aidee-context-element)
  ((name :initarg :name :type string))
  "A structure for holding information about a context element.")

(cl-defmethod aidee-context-element-extract
  ((element aidee-context-element-file))
  "Extract the content of the context ELEMENT."
  (with-slots (name) element
    (with-temp-buffer
      (insert-file-contents name)
      (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defmethod aidee-context-element-format
  ((element aidee-context-element-file) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "[%s](<%s>)" name name)))

(cl-defmethod aidee-context-element-format
  ((element aidee-context-element-file) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "[[file:%s][%s]]" name name)))

;; Info node context element

(defclass aidee-context-element-info-node (aidee-context-element)
  ((name :initarg :name :type string))
  "A structure for holding information about a context element.")

(cl-defmethod aidee-context-element-extract
  ((element aidee-context-element-info-node))
  "Extract the content of the context ELEMENT."
  (with-slots (name) element
    (with-temp-buffer
      (info name (current-buffer))
      (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defmethod aidee-context-element-format
  ((element aidee-context-element-info-node) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "```emacs-lisp\n(info \"%s\")\n```\n" name)))

(cl-defmethod aidee-context-element-format
  ((element aidee-context-element-info-node) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name) element
    (format "[[%s][%s]]"
	    (replace-regexp-in-string
	     "(\\(.?*\\)) \\(.*\\)" "info:\\1#\\2" name)
            name)))

;; Text context element

(defclass aidee-context-element-text (aidee-context-element)
  ((content :initarg :content :type string))
  "A structure for holding information about a context element.")

(cl-defmethod aidee-context-element-extract
  ((element aidee-context-element-text))
  "Extract the content of the context ELEMENT."
  (oref element content))

(cl-defmethod aidee-context-element-format
  ((element aidee-context-element-text) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (oref element content))

(cl-defmethod aidee-context-element-format
  ((element aidee-context-element-text) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (oref element content))

;; Webpage quote context elements

(defclass aidee-context-element-webpage-quote (aidee-context-element)
  ((name :initarg :name :type string)
   (url :initarg :url :type string)
   (content :initarg :content :type string))
  "A structure for holding information about a context element.")

(cl-defmethod aidee-context-element-extract
  ((element aidee-context-element-webpage-quote))
  "Extract the content of the context ELEMENT."
  (oref element content))

(defun aidee--quote-buffer (quote)
  "Return buffer name for QUOTE."
  (let* ((buf-name (concat (make-temp-name "*aidee-quote-") "*"))
	 (buf (get-buffer-create buf-name t)))
    (with-current-buffer buf
      (with-silent-modifications
	(insert quote)))
    buf-name))

(cl-defmethod aidee-context-element-format
  ((element aidee-context-element-webpage-quote) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name url content) element
    (if aidee-show-quotes
	(format "[%s](%s):\n%s\n\n"
		name url
		(aidee--md-quote content))
      (format
       "[%s](%s):\n```emacs-lisp\n(display-buffer \"%s\")\n```\n"
       name url (aidee--quote-buffer content)))))

(defun aidee--md-quote (content)
  "Return quoted CONTENT for markdown."
  (with-temp-buffer
    (insert (propertize content 'hard t))
    (let ((fill-prefix "> ")
	  (fill-column aidee-long-lines-length)
	  (use-hard-newlines t)
	  (comment-start ">")
	  (comment-empty-lines t))
      (comment-region (point-min) (point-max) ">")
      (fill-region (point-min) (point-max) nil t t))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun aidee--org-quote (content)
  "Return transformed CONTENT for org quotes."
  (replace-regexp-in-string "^*" " *" content))

(cl-defmethod aidee-context-element-format
  ((element aidee-context-element-webpage-quote) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name url content) element
    (if aidee-show-quotes
	(format "[[%s][%s]]:\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n"
		url name (aidee--org-quote content))
      (format "[[%s][%s]] [[elisp:(display-buffer \"%s\")][show]]"
	      url name (aidee--quote-buffer content)))))

;; Info node quote context elements

(defclass aidee-context-element-info-node-quote (aidee-context-element)
  ((name :initarg :name :type string)
   (content :initarg :content :type string))
  "A structure for holding information about a context element.")

(cl-defmethod aidee-context-element-extract
  ((element aidee-context-element-info-node-quote))
  "Extract the content of the context ELEMENT."
  (oref element content))

(cl-defmethod aidee-context-element-format
  ((element aidee-context-element-info-node-quote) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name content) element
    (if aidee-show-quotes
	(format "```emacs-lisp\n(info \"%s\")\n```\n%s\n\n"
		name
		(aidee--md-quote content))
      (format "```emacs-lisp\n(info \"%s\")\n```\nshow:\n```emacs-lisp\n(display-buffer \"%s\")\n```\n" name (aidee--quote-buffer content)))))

(cl-defmethod aidee-context-element-format
  ((element aidee-context-element-info-node-quote) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (name content) element
    (if aidee-show-quotes
	(format "[[%s][%s]]:\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n"
		(replace-regexp-in-string
		 "(\\(.?*\\)) \\(.*\\)" "info:\\1#\\2" name)
                name
		(aidee--org-quote content))
      (format "[[%s][%s]] [[elisp:(display-buffer \"%s\")][show]]"
	      (replace-regexp-in-string
	       "(\\(.?*\\)) \\(.*\\)" "info:\\1#\\2" name)
	      name
	      (aidee--quote-buffer content)))))

;; File quote context elements

(defclass aidee-context-element-file-quote (aidee-context-element)
  ((path :initarg :path :type string)
   (content :initarg :content :type string))
  "A structure for holding information about a context element.")

(cl-defmethod aidee-context-element-extract
  ((element aidee-context-element-file-quote))
  "Extract the content of the context ELEMENT."
  (oref element content))

(cl-defmethod aidee-context-element-format
  ((element aidee-context-element-file-quote) (mode (eql 'markdown-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (path content) element
    (if aidee-show-quotes
	(format "[%s](%s):\n%s\n\n"
		path path
		(aidee--md-quote content))
      (format "[%s](%s):\n```emacs-lisp\n(display-buffer \"%s\")"
	      path path (aidee--quote-buffer content)))))

(cl-defmethod aidee-context-element-format
  ((element aidee-context-element-file-quote) (mode (eql 'org-mode)))
  "Format the context ELEMENT for the major MODE."
  (ignore mode)
  (with-slots (path content) element
    (if aidee-show-quotes
	(format "[[%s][%s]]:\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n"
		path path (aidee--org-quote content))
      (format "[[%s][%s]] [[elisp:(display-buffer \"%s\")][show]]"
	      path path (aidee--quote-buffer content)))))


;;;###autoload
(defun aidee-context-add-file ()
  "Add file to context."
  (interactive)
  (let* ((file-name (read-file-name "Select file: " nil nil t))
         (element (aidee-context-element-file :name file-name)))
    (aidee-context-element-add element)))

(defun aidee-context-add-file-quote-noninteractive (path content)
  "Add file with PATH quote CONTENT to context."
  (let ((element (aidee-context-element-file-quote
		  :path path :content content)))
    (aidee-context-element-add element)))

;;;###autoload
(defun aidee-context-add-file-quote ()
  "Add file quote to context interactively."
  (interactive)
  (let ((path (buffer-file-name (current-buffer)))
	(content (if (region-active-p)
		     (buffer-substring-no-properties
		      (region-beginning)
		      (region-end))
		   (buffer-substring-no-properties
		    (point-min)
		    (point-max)))))
    (if (not path)
	(warn "should be called from buffer associated with file")
      (aidee-context-add-file-quote-noninteractive path content))))

;;;###autoload
(defun aidee-context-add-buffer (buf)
  "Add BUF to context."
  (interactive "bSelect buffer: ")
  (let ((element (aidee-context-element-buffer :name buf)))
    (aidee-context-element-add element)))

;;;###autoload
(defun aidee-context-add-selection ()
  "Add file to context."
  (interactive)
  (if (region-active-p)
      (let* ((content (buffer-substring-no-properties (region-beginning) (region-end)))
             (element (aidee-context-element-text :content content)))
        (aidee-context-element-add element))
    (warn "No active region")))

(defun aidee-context-add-text (text)
  "Add TEXT to context."
  (let ((element (aidee-context-element-text :content text)))
    (aidee-context-element-add element)))

(declare-function Info-copy-current-node-name "info")

;;;###autoload
(defun aidee-context-add-info-node (node)
  "Add info NODE to context."
  (interactive (list (Info-copy-current-node-name)))
  (let ((element (aidee-context-element-info-node :name node)))
    (aidee-context-element-add element)))

(defun aidee-context-add-info-node-quote-noninteractive (name content)
  "Add info node with NAME quote CONTENT to context."
  (let ((element (aidee-context-element-info-node-quote
		  :name name :content content)))
    (aidee-context-element-add element)))

;;;###autoload
(defun aidee-context-add-info-node-quote ()
  "Add info node quote to context interactively."
  (interactive)
  (let ((name (Info-copy-current-node-name))
	(content (if (region-active-p)
		     (buffer-substring-no-properties
		      (region-beginning)
		      (region-end))
		   (buffer-substring-no-properties
		    (point-min)
		    (point-max)))))
    (if (not name)
	(warn "should be called from `info' buffer")
      (aidee-context-add-info-node-quote-noninteractive name content))))

(defun aidee-context-add-webpage-quote-noninteractive (name url content)
  "Add webpage with NAME and URL quote CONTENT to context."
  (let ((element (aidee-context-element-webpage-quote
		  :name name :url url :content content)))
    (aidee-context-element-add element)))

;;;###autoload
(defun aidee-context-add-webpage-quote-eww ()
  "Add webpage quote to context interactively from `eww'."
  (interactive)
  (defvar eww-data)
  (declare-function eww-current-url "eww")
  (if (eq major-mode 'eww-mode)
      (let* ((name (plist-get eww-data :title))
	     (url (eww-current-url))
	     (content (if (region-active-p)
			  (buffer-substring-no-properties
			   (region-beginning)
			   (region-end))
			(buffer-substring-no-properties
			 (point-min)
			 (point-max)))))
	(aidee-context-add-webpage-quote-noninteractive name url content))
    (warn "Should be called from `eww'.")))


(defun aidee--format-context (session)
  "Format SESSION context for chat buffer."
  (let ((mode (if (derived-mode-p 'org-mode) 'org-mode 'markdown-mode)))
    (if-let* ((context (aidee-session-context session)))
        (concat (string-join
	         (cons "Context:"
                       (mapcar (lambda (elt)
                                 (aidee-context-element-format elt mode))
                               context))
	         "\n")
	        "\n\n")
      "")))

(defun aidee--prompt-with-context (prompt)
  "Add context to PROMPT for sending to llm."
  (if-let* ((session aidee--current-session)
	    (context (aidee-session-context session)))
      (concat (string-join
	       (cons "Context:"
		     (mapcar #'aidee-context-element-extract context))
	       "\n")
	      "\n\n"
	      prompt)
    prompt))

(defun aidee-chat-buffer-p (buffer)
  "Return non-nil if BUFFER is an aidee chat buffer."
  (with-current-buffer buffer
    (not (not aidee--current-session))))

(defun aidee-get-current-session-id ()
  "Return current session id.
If buffer contains aidee session return its id.
Otherwire return id of current active session."
  (if aidee--current-session
      (aidee-session-id aidee--current-session)
    aidee--current-session-id))

(defun aidee-get-current-session ()
  "Return current session.
If buffer contains aidee session return it.
Otherwire return current active session."
  (if aidee--current-session
      aidee--current-session
    (when aidee--current-session-id
      (with-current-buffer (aidee-get-session-buffer aidee--current-session-id)
	aidee--current-session))))

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
	 (provider (if session
		       (aidee-session-provider session)
		     (or (plist-get args :provider) aidee-provider)))
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
	 (prompt-with-ctx (aidee--prompt-with-context prompt))
	 (llm-prompt (if session
			 (if (llm-chat-prompt-p (aidee-session-prompt session))
			     (progn
			       (llm-chat-prompt-append-response
				(aidee-session-prompt session)
				prompt-with-ctx)
			       (aidee-session-prompt session))
			   (setf (aidee-session-prompt session)
				 (llm-make-simple-chat-prompt prompt-with-ctx)))
		       (llm-make-simple-chat-prompt prompt-with-ctx))))
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
		  (when-let ((aidee-auto-scroll)
			     (window (get-buffer-window buffer)))
		    (when (aidee-chat-buffer-p buffer)
		      (with-selected-window window
			(goto-char (point-max))
			(recenter -1))))
		  (undo-amalgamate-change-group aidee--change-group)))))
	(setq aidee--change-group (prepare-change-group))
	(activate-change-group aidee--change-group)
	(set-marker start point)
	(set-marker end point)
	(set-marker-insertion-type start nil)
	(set-marker-insertion-type end t)
	(spinner-start aidee-spinner-type)
	(when session
	  (setf (aidee-session-context session) nil))
	(setq aidee--current-request
	      (llm-chat-streaming provider
				  llm-prompt
				  insert-text
				  (lambda (text)
				    (funcall insert-text (string-trim text))
				    (with-current-buffer buffer
				      (accept-change-group aidee--change-group)
				      (spinner-stop)
				      (if (and (listp donecb)
					       (functionp (car donecb)))
					  (mapc (lambda (fn) (funcall fn text))
						donecb)
					(funcall donecb text))
				      (setq aidee--current-request nil)
				      (aidee-request-mode -1)))
				  (lambda (_ msg)
				    (with-current-buffer buffer
				      (cancel-change-group aidee--change-group)
				      (spinner-stop)
				      (funcall errcb msg)
				      (setq aidee--current-request nil)
				      (aidee-request-mode -1)))))))))

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

(declare-function org-export-to-buffer "ox")
(defvar org-export-show-temporary-export-buffer)

(defun aidee-convert-org-to-md (text)
  "Translate TEXT from org syntax to markdown syntax."
  (require 'ox)
  (require 'ox-md)
  (let ((buf (make-temp-name "aidee-"))
	(org-export-show-temporary-export-buffer nil))
    (with-temp-buffer
      (insert "#+OPTIONS: toc:nil\n" text)
      (org-export-to-buffer 'md buf
	nil nil t t nil (lambda () (text-mode))))
    (with-current-buffer buf
      (prog1
	  (string-trim (buffer-substring-no-properties (point-min) (point-max)))
	(kill-buffer)))))

(defun aidee-get-last-user-message ()
  "Return last not sent user message in current session buffer."
  (when aidee--current-session
    (save-excursion
      (save-match-data
	(goto-char (point-max))
	(and (search-backward (concat (aidee-get-nick-prefix-for-mode) " " aidee-user-nick ":\n") nil t)
	     (search-forward (concat (aidee-get-nick-prefix-for-mode) " " aidee-user-nick ":\n") nil t)
	     (buffer-substring-no-properties (point) (point-max)))))))

(defun aidee-chat-done (text &optional on-done)
  "Chat done.
Will call `aidee-chat-done-callback' and ON-DONE on TEXT."
  (save-excursion
    (goto-char (point-max))
    (insert "\n\n" (aidee-get-nick-prefix-for-mode) " " aidee-user-nick ":\n")
    (when aidee-session-auto-save
      (save-buffer)))
  (when aidee-chat-done-callback
    (funcall aidee-chat-done-callback text))
  (when on-done
    (funcall on-done text)))


;;;###autoload
(defun aidee-chat (prompt &optional create-session &rest args)
  "Send PROMPT to aidee chat with conversation history.

If CREATE-SESSION set, creates new session even if there is an active session.
ARGS contains keys for fine control.

:provider PROVIDER -- PROVIDER is an llm provider for generation.

:session SESSION -- SESSION is a aidee conversation session.

:session-id ID -- ID is a aidee session unique identifier.

:on-done ON-DONE -- ON-DONE a function that's called with
the full response text when the request completes (with BUFFER current)."
  (interactive "sAsk aidee: ")
  (let* ((ollama-binary (executable-find aidee-ollama-binary))
	 (providers (append
                     `(("default model" . aidee-provider)
		       ,(if (and ollama-binary
				 (file-exists-p ollama-binary))
			    '("ollama model" . (aidee-get-ollama-local-model))))
                     aidee-providers))
	 (variants (mapcar #'car providers))
	 (donecb (plist-get args :on-done))
	 (provider (if current-prefix-arg
		       (eval (alist-get
			      (completing-read "Select model: " variants)
			      providers nil nil #'string=))
		     (or (plist-get args :provider)
			 aidee-provider)))
	 (session (or (plist-get args :session)
		      (if (or create-session
			      current-prefix-arg
			      (and provider
				   (or (plist-get args :provider)
				       (not (equal provider aidee-provider)))
				   aidee--current-session-id
				   (with-current-buffer (aidee-get-session-buffer
							 aidee--current-session-id)
				     (not (equal
					   provider
					   (aidee-session-provider aidee--current-session)))))
			      (and (not aidee--current-session)
				   (not aidee--current-session-id)))
			  (aidee-new-session provider prompt)
			(or aidee--current-session
			    (with-current-buffer (aidee-get-session-buffer
						  (or (plist-get args :session-id)
						      aidee--current-session-id))
			      aidee--current-session)))))
	 (buffer (aidee-get-session-buffer
		  (aidee-session-id session)))
	 (file-name (aidee-session-file session))
	 (translation-buffer nil))
    (display-buffer buffer (when aidee-chat-display-action-function
			     `((ignore . (,aidee-chat-display-action-function)))))
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-max))
	(if (equal (point-min) (point-max)) ;; empty buffer
	    (insert (aidee-get-nick-prefix-for-mode) " " aidee-user-nick ":\n"
		    (aidee--format-context session) (aidee--fill-long-lines prompt) "\n\n"
		    (aidee-get-nick-prefix-for-mode) " " aidee-assistant-nick ":\n")
	  (insert (aidee--format-context session) (aidee--fill-long-lines prompt) "\n\n"
		  (aidee-get-nick-prefix-for-mode) " " aidee-assistant-nick ":\n"))
	(aidee-stream prompt
		      :session session
		      :on-done (if donecb (list 'aidee-chat-done donecb)
				 'aidee-chat-done)
		      :filter (when (derived-mode-p 'org-mode)
				#'aidee--translate-markdown-to-org-filter))))))

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

(defun aidee-instant (prompt &rest args)
  "Prompt aidee for PROMPT to reply instantly.

ARGS contains keys for fine control.

:provider PROVIDER -- PROVIDER is an llm provider for generation."
  (let* ((provider (or (plist-get args :provider)
		       aidee-provider))
	 (buffer-name (aidee-generate-name provider real-this-command prompt))
	 (buffer (get-buffer-create (if (get-buffer buffer-name)
					(make-temp-name (concat buffer-name " "))
				      buffer-name)))
	 filter)
    (with-current-buffer buffer
      (funcall aidee-major-mode)
      (when (derived-mode-p 'org-mode)
	(setq filter 'aidee--translate-markdown-to-org-filter)))
    (display-buffer buffer (when aidee-instant-display-action-function
			     `((ignore . (,aidee-instant-display-action-function)))))
    (aidee-stream prompt
		  :buffer buffer
		  :filter filter
		  :provider provider)))

;;;###autoload
(defun aidee-code-review ()
  "Review code in selected region or current buffer."
  (interactive)
  (if (region-active-p)
      (aidee-context-add-selection)
    (aidee-context-add-buffer (current-buffer)))
  (aidee-chat aidee-code-review-prompt-template nil :provider aidee-coding-provider))

;;;###autoload
(defun aidee-change (change &optional edit-template)
  "Change selected text or text in current buffer according to provided CHANGE.
When the value of EDIT-TEMPLATE is 4, or with one `universal-argument' as
prefix (\\[universal-argument]), prompt the user to amend the template."
  (interactive "sWhat needs to be changed: \np")
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
         (template-orig (format aidee-change-prompt-template change "%s"))
         (template (if (= edit-template 4)
                       (read-from-minibuffer "Template: " template-orig)
                     template-orig))
	 (text (buffer-substring-no-properties beg end)))
    (kill-region beg end)
    (aidee-stream
     (format template text)
     :point beg)))

;;;###autoload
(defun aidee-code-edit (change)
  "Change selected code or code in current buffer according to provided CHANGE."
  (interactive "sWhat needs to be changed in this code: ")
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (text (buffer-substring-no-properties beg end)))
    (kill-region beg end)
    (aidee-stream
     (format
      aidee-code-edit-prompt-template
      change text)
     :provider aidee-coding-provider
     :filter #'aidee--code-filter
     :point beg)))

;;;###autoload
(defun aidee-code-improve ()
  "Change selected code or code in current buffer according to provided CHANGE."
  (interactive)
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (text (buffer-substring-no-properties beg end)))
    (kill-region beg end)
    (aidee-stream
     (format
      aidee-code-improve-prompt-template
      text)
     :provider aidee-coding-provider
     :filter #'aidee--code-filter
     :point beg)))

;;;###autoload
(defun aidee-code-complete ()
  "Complete selected code or code in current buffer."
  (interactive)
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point)))
	 (text (buffer-substring-no-properties beg end)))
    (aidee-stream
     (format
      aidee-code-complete-prompt-template
      text)
     :provider aidee-coding-provider
     :filter #'aidee--code-filter
     :point end)))

;;;###autoload
(defun aidee-code-add (description)
  "Add new code according to DESCRIPTION.
Code will be generated with provided context from selected region or current
buffer."
  (interactive "sDescribe the code to be generated: ")
  (let* ((beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (text (buffer-substring-no-properties beg end)))
    (aidee-stream
     (format
      aidee-code-add-prompt-template
      text description)
     :provider aidee-coding-provider
     :filter #'aidee--code-filter)))


(defun aidee-get-ollama-local-model ()
  "Return llm provider for interactively selected ollama model."
  (interactive)
  (declare-function llm-ollama-p "ext:llm-ollama")
  (declare-function llm-ollama-host "ext:llm-ollama")
  (declare-function llm-ollama-port "ext:llm-ollama")
  (let ((model-name
	 (completing-read "Select ollama model: "
			  (mapcar (lambda (s)
				    (car (split-string s)))
				  (seq-drop
				   (process-lines
				    (executable-find aidee-ollama-binary) "ls")
				   1))))
	(host (when (llm-ollama-p aidee-provider)
		(llm-ollama-host aidee-provider)))
	(port (when (llm-ollama-p aidee-provider)
		(llm-ollama-port aidee-provider))))
    (if host
	(make-llm-ollama
	 :chat-model model-name :embedding-model model-name :host host :port port)
      (make-llm-ollama
       :chat-model model-name :embedding-model model-name))))


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

;;;; file skeleton

(cl-defstruct aidee-buffer-skeleton
  (bufname)
  (skeleton)
  (timestamp))

(setq aidee--treesit-language-queries
      '((python . (("class_definition" (class_definition ":" @context.body @context.surrounding.start body: (_)) @context)
                   ("function_definition" (function_definition ":" @context.body @context.surrounding.start body: (_)) @context)))
        (java . (("class_declaration" (class_declaration body: (class_body :anchor "{" @context.surrounding.start _ "}" @context.surrounding.end :anchor) @context.body) @context)
                 ("method_declaration" (method_declaration body: (block :anchor "{" @context.surrounding.start _ "}" @context.surrounding.end :anchor) @context.body) @context)
                 ("field_declaration" (field_declaration declarator: (_) ";") @context)))
        ))

(defun aidee--treesit-language-node-types (language)
  (let ((queries (assoc-default language aidee--treesit-language-queries))
        node-types)
    (when queries
      (setq node-types (mapcar 'car queries)))
    node-types))

(defun aidee--treesit-language-node-query (language node-type)
  (let ((queries (assoc-default language aidee--treesit-language-queries))
        result)
    (when queries
      (cl-dolist (query queries)
        (when (string-equal (car query) node-type)
          (setq result (append result (cdr query))))))
    result))

(defvar-local aidee--buffer-skeleton nil
  "Skeleton of current buffer.")

(defvar aidee--buffer-skeletons (make-hash-table :test #'equal)
  "Buffers' skeleton in current project.
Key is file name, value is of type `aidee-buffer-skeleton'.")

(defvar aidee-treesit-suffix-language-map (make-hash-table :test #'equal)
  "Use file name suffix to determine language.")

(defvar aidee-treesit-major-mode-language-map (make-hash-table :test #'equal)
  "Use buffer's major mode (in string form) to determine language.")

(defun aidee--buffer-treesit-language (&optional buf)
  "Get the language of BUF, the default of which is current buffer."
  (let* ((buf (or buf (current-buffer)))
         (filename (buffer-file-name buf))
         (suffix "")
         mm
         language)
    (with-current-buffer buf
      (setq mm (symbol-name major-mode))
      (when buffer-file-name
        (setq suffix (file-name-extension buffer-file-name)))
      (setq language (or (gethash suffix aidee-treesit-suffix-language-map)
                         (gethash mm aidee-treesit-major-mode-language-map)))
      (unless language
        (setq language
              (cond
               ((member suffix '("js"))
                'javascript)
               ((member suffix '("ts"))
                'typescript)
               ((member suffix '("el"))
                'elisp)
               ((string-suffix-p "-ts-mode" mm)
                (intern (string-remove-suffix "-ts-mode" mm)))
               (t
                (intern (string-remove-suffix "-mode" mm)))))))
    language))

(defvar-local aidee--treesit-parser nil
  "treesit parser of the current buffer.")

(defun aidee--ensure-treesit-parser (&optional buf)
  (let ((buf (or buf (current-buffer)))
        language
        parser)
    (with-current-buffer buf
      (setq parser aidee--treesit-parser)
      (unless parser
        (setq language (aidee--buffer-treesit-language))
        (if (and language
                 (treesit-language-available-p language))
            (progn
              (setq parser (treesit-parser-create language))
              (setq-local aidee--treesit-parser parser))
          (message "language: %s without parser available" language))))
    parser))


(defvar aidee--indent-step nil)
(defvar aidee--travel-result nil)
(defvar aidee--surrounding-end-stack nil)

(defun aidee--treesit-capture (node query &optional beg end node-only)
  "Capture nodes and return them as a pair.
The car of the pair is context, and the cdr is context.body."
  (let (captures
        (result (make-hash-table :test #'equal))
        capture-name)
    (setq captures (treesit-query-capture node query (or beg (treesit-node-start node)) (or end (1+ (point)))))
    (when captures
      (cl-dolist (c captures)
        (setq capture-name (car c))
        (puthash capture-name (cdr c) result)))
    result))

(defun aidee--treesit-traval-sparse-tree-1 (language ele)
  (cond
   ((null ele)
    ;; nothing
    )
   ((atom ele)
    (let ((node-type (treesit-node-type ele))
          (node-start (treesit-node-start ele))
          (node-end (treesit-node-end ele))
          (indentation (make-string (* aidee--indent-step 4) ?\s))
          result
          query
          captures)
      (when-let* ((query (aidee--treesit-language-node-query language node-type))
                  (captures (aidee--treesit-capture ele query node-start node-end)))
        (let (context
              context.body
              context.surrounding.start
              context.surrounding.end
              (surrounding.start "")
              (surrounding.end "")
              start-pos
              end-pos)
          (save-excursion
            (save-restriction
              (widen)
              (setq context (gethash 'context captures)
                    context.body (gethash 'context.body captures)
                    context.surrounding.start (gethash 'context.surrounding.start captures)
                    context.surrounding.end (gethash 'context.surrounding.end captures))
              (setq start-pos (treesit-node-start context)
                    end-pos (treesit-node-end context))
              (when context.body
                (setq end-pos (treesit-node-start context.body)))
              (when context.surrounding.start
                (setq surrounding.start (buffer-substring-no-properties (treesit-node-start context.surrounding.start) (treesit-node-end context.surrounding.start))))
              (when context.surrounding.end
                (setq surrounding.end (buffer-substring-no-properties (treesit-node-start context.surrounding.end) (treesit-node-end context.surrounding.end))))
              ;; push to stack, pop one after dealing with a list. see [1]
              (push (concat indentation surrounding.end "\n") aidee--surrounding-end-stack)
              (setq aidee--travel-result (concat aidee--travel-result (concat indentation (buffer-substring-no-properties start-pos end-pos) surrounding.start "\n")))))))))
   ((listp ele)
    (setq aidee--indent-step (1+ aidee--indent-step))
    (cl-dolist (e ele)
      (aidee--treesit-traval-sparse-tree-1 language e))
    (setq aidee--indent-step (1- aidee--indent-step))
    ;; pop surrouding end [1]
    (setq aidee--travel-result (concat aidee--travel-result (pop aidee--surrounding-end-stack))))))

(defun aidee--treesit-traval-sparse-tree (language sparse-tree)
  "Perform a depth-first, pre-order traversal of SPARSE-TREE."
  (setq aidee--indent-step -1
        aidee--travel-result nil
        aidee--surrounding-end-stack nil)
  (when (null (car sparse-tree))
    (setq aidee--indent-step -2))
  (aidee--treesit-traval-sparse-tree-1 language sparse-tree)
  aidee--travel-result)

(defun aidee--retrieve-buffer-skeleton (&optional buf)
  (let* ((buf (or buf (current-buffer)))
         (language (aidee--buffer-treesit-language buf))
         (parser (aidee--ensure-treesit-parser buf))
         (node-types (aidee--treesit-language-node-types language))
         sparse-tree)
    (with-current-buffer buf
      (if (and node-types
               parser)
          (progn
            (when-let ((root (treesit-parser-root-node parser)))
              (setq sparse-tree (treesit-induce-sparse-tree
                                 root
                                 (lambda (node)
                                   (member (treesit-node-type node) node-types))))
              (when sparse-tree
                (aidee--treesit-traval-sparse-tree language sparse-tree)))
            aidee--travel-result)
        (message "parser or node types are nil")
        nil))))

;;;; repomap


(provide 'aidee)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; aidee.el ends here

