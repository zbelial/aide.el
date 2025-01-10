;;; eureka-coding.el  --- AI powered Development Environment for Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 zbelial zjyzhaojiyang@gmail.com

;; Author: zbelial zjyzhaojiyang@gmail.com

;; Maintainer: zbelial zjyzhaojiyang@gmail.com

;; Homepage: https://github.com/zbelial/eureka.el.git
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
(require 'cl-lib)
(require 'eureka-core)
(require 'eureka-utils)
;; from https://github.com/zbelial/cache,
;; which is forked from https://github.com/nflath/cache
(require 'cache)

(defcustom eureka-coding-language "English"
  "Language for eureka coding."
  :group 'eureka
  :type 'string)

;; 4 placeholders: context, context format, user instrument and suffix.
(defcustom eureka-project-code-prompt-template "Context:\n%s\n%s\nBased on the above context, act as an expert software developer, %s.\n%s"
  "Prompt template for all project code task."
  :group 'eureka
  :type 'string)

(defcustom eureka-project-code-review-instruction-template "review the following code and make concise suggestions: \n```%s\n```"
  "Prompt template for `eureka-project-code-review'."
  :group 'eureka
  :type 'string)

(defcustom eureka-project-code-improve-instruction-template "enhance the following code: \n```\n%s\n```"
  "Prompt template for `eureka-project-code-improve'."
  :group 'eureka
  :type 'string)

(defcustom eureka-project-code-explain-instruction-template "explain the following code in a detailed way: \n```\n%s\n```"
  "Prompt template for `eureka-project-code-explain'."
  :group 'eureka
  :type 'string)

(defcustom eureka-project-code-context-format "Context at the begining contains one or more *CONTEXT block*,
each block represents a file and its content.

# *CONTEXT block* Format:

Every *CONTEXT block* follows this format:
1. The opening fence: ```
2. The *FULL* file path alone on a line, verbatim. No bold asterisks, no quotes around it, no escaping of characters, etc.
3. The closing fence: ```
4. The opening fence and code language, eg: ```python, here python will be replaced with the code language following.
5. A contiguous chunk of codes in the existing file represented by the *FULL* file path. This part may be the exact content of the file, or just class name, method signatures or class properties, etc.
6. The closing fence: ```

"
  "Context format."
  :group 'eureka
  :type 'string)

;; 1 placeholder(s): language
(defcustom eureka-project-code-edit-suffix-template "Always use best practices when coding.
Respect and use existing conventions, libraries, etc that are already present in the code base.

You NEVER leave comments describing code without implementing it!
You always COMPLETELY IMPLEMENT the needed code!

Take requests for changes to the supplied code.
If the request is ambiguous, ask questions.

Always reply to the user in %s.

Once you understand the request you MUST:

1. Decide if you need to propose *SEARCH/REPLACE* edits to any files that haven't been added to the chat. You can create new files without asking!

2. Think step-by-step and explain the needed changes in a few short sentences.

3. Describe each change with a *SEARCH/REPLACE block* per rules below.

All changes to files must use this *SEARCH/REPLACE block* format.
ONLY EVER RETURN CODE IN A *SEARCH/REPLACE BLOCK*!

# *SEARCH/REPLACE block* Rules:

Every *SEARCH/REPLACE block* must use this format:
1. The opening fence and code language, eg: ```python, here python should be replaced with the code language that is generated
2. The *FULL* file path alone on a line, verbatim. No bold asterisks, no quotes around it, no escaping of characters, etc.
3. The start of search block: <<<<<<< SEARCH
4. A contiguous chunk of lines to search for in the existing source code
5. The dividing line: =======
6. The lines to replace into the source code
7. The end of the replace block: >>>>>>> REPLACE
8. The closing fence: ```

Use the *FULL* file path, as shown to you by the user.

Every *SEARCH* section must *EXACTLY MATCH* the existing file content, character for character, including all comments, docstrings, etc.
If the file contains code or other data wrapped/escaped in json/xml/quotes or other containers, you need to propose edits to the literal contents of the file, including the container markup.

*SEARCH/REPLACE* blocks will *only* replace the first match occurrence.
Including multiple unique *SEARCH/REPLACE* blocks if needed.
Include enough lines in each SEARCH section to uniquely match each set of lines that need to change.

Keep *SEARCH/REPLACE* blocks concise.
Break large *SEARCH/REPLACE* blocks into a series of smaller blocks that each change a small portion of the file.
Include just the changing lines, and a few surrounding lines if needed for uniqueness.
Do not include long runs of unchanging lines in *SEARCH/REPLACE* blocks.

To move code within a file, use 2 *SEARCH/REPLACE* blocks: 1 to delete it from its current location, 1 to insert it in the new location.

Pay attention to which filenames the user wants you to edit, especially if they are asking you to create a new file.

If you want to put code in a new file, use a *SEARCH/REPLACE block* with:
- A new file path, including dir name if needed
- An empty `SEARCH` section
- The new file's contents in the `REPLACE` section

You NEVER leave comments describing code without implementing it!
You always COMPLETELY IMPLEMENT the needed code!

ONLY EVER RETURN CODE IN A *SEARCH/REPLACE BLOCK*!

"
  "Prompt template for `eureka-project-code-edit'."
  :group 'eureka
  :type 'string)

(defcustom eureka-ediff-directory (file-truename
				   (file-name-concat
				    user-emacs-directory
                                    "eureka"
				    "ediff"))
  "Directory for storing temp project files to be used by ediff."
  :type 'string
  :group 'eureka)

(defvar eureka--code-search-label "<<<<<<< SEARCH")
(defvar eureka--code-replace-label ">>>>>>> REPLACE")

(defconst eureka--project-code-search-replace-pattern
  (rx
   (seq
    ;;code fence start
    (minimal-match
     (zero-or-more anything) (literal "```") (one-or-more alpha) (+ (or "\n" "\r")))
    ;;SEARCH
    (minimal-match
     (eval eureka--code-search-label) (+ (or "\n" "\r")))
    (group (minimal-match
            (zero-or-more anything)))
    ;; =======
    (literal "=======") (+ (or "\n" "\r"))
    ;;REPLACE
    (group (minimal-match
            (zero-or-more anything)))
    (minimal-match
     (eval eureka--code-replace-label) (+ (or "\n" "\r")))
    ;;code fence end
    (literal "```")))
  )

(defconst eureka--project-code-edit-pattern
  (rx
   ;;fence start
   (minimal-match
    (literal "```") (one-or-more alpha) (+ (or "\n" "\r")))
   ;;filename
   (group (minimal-match
           (one-or-more (not (any "\n" "\r")))))
   (+ (or "\n" "\r"))
   ;;SEARCH
   (minimal-match
    (eval eureka--code-search-label) (+ (or "\n" "\r")))
   (group (minimal-match
           (zero-or-more anything)))
   ;; =======
   (literal "=======") (+ (or "\n" "\r"))
   ;;REPLACE
   (group (minimal-match
           (zero-or-more anything)))
   (minimal-match
    (eval eureka--code-replace-label) (+ (or "\n" "\r")))
   ;;fence end
   (literal "```")))

;; copied from s.el's s-match and modified
(defun eureka--match (regexp s &optional start)
  "When the given expression matches the string, this function returns a list
of the whole matching string and a string for each matched subexpressions.
Subexpressions that didn't match are represented by nil elements
in the list, except that non-matching subexpressions at the end
of REGEXP might not appear at all in the list.  That is, the
returned list can be shorter than the number of subexpressions in
REGEXP plus one.  If REGEXP did not match the returned value is
an empty list (nil).

When START is non-nil the search will start at that index."
  (declare (side-effect-free t))
  (save-match-data
    (if (string-match regexp s start)
        (let ((match-data-list (match-data))
              result)
          (while match-data-list
            (let* ((beg (car match-data-list))
                   (end (cadr match-data-list))
                   subs)
              (when (and beg end)
                (setq subs (list (substring s beg end) beg end))
                (setq result (push subs result)))
              (setq match-data-list
                    (cddr match-data-list))))
          (nreverse result)))))

(cl-defstruct eureka--file-edit
  search
  replace)

(cl-defstruct eureka--file-action
  filename
  edits)

(defun eureka--count-substring (substring string)
  "Count the number of occurrences of SUBSTRING in STRING."
  (let ((count 0)
        (start 0))
    (while (string-match substring string start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(defun eureka--project-code-edit-parse-search-replace (text)
  (let ((edits nil)
        search replace
        edit
        start
        stop?
        match-result)
    (while (not stop?)
      (setq match-result (eureka--match eureka--project-code-search-replace-pattern text start))
      (if (and match-result
               (length= match-result 3))
          (progn
            (setq start (nth 2 (nth 0 match-result))
                  search (nth 0 (nth 1 match-result))
                  replace (nth 0 (nth 2 match-result)))
            (setq edit (make-eureka--file-edit :search search :replace replace))
            (push edit edits))
        (setq stop? t)))
    (nreverse edits)))

(defun eureka--project-code-edit-parse-response (text)
  (let ((actions (make-hash-table :test #'equal))
        filename
        action old-action
        match-result
        edits
        total-end
        start
        stop?)
    (if (= (eureka--count-substring eureka--code-search-label text)
           (eureka--count-substring eureka--code-replace-label text))
        (progn
          (while (not stop?)
            (progn
              (setq match-result (eureka--match eureka--project-code-edit-pattern text start))
              (if (and match-result
                       (length> match-result 2))
                  (progn
                    (setq total-end (nth 2 (nth 0 match-result))
                          filename (nth 0 (nth 1 match-result))
                          search (nth 0 (nth 2 match-result))
                          replace (nth 0 (nth 3 match-result)))
                    (setq edits (list (make-eureka--file-edit :search search :replace replace)))
                    (when (and filename
                               edits)
                      ;; merge all edits of a file
                      (setq old-action (gethash filename actions))
                      (if old-action
                          (setq action (make-eureka--file-action :filename filename
                                                                 :edits (append (eureka--file-action-edits old-action) edits)))
                        (setq action (make-eureka--file-action :filename filename
                                                               :edits edits)))
                      (puthash filename action actions))
                    (setq start total-end))
                (setq stop? t))))
          (hash-table-values actions))
      (message "Ill-formed response."))))

(defun eureka--get-project-from-session-id (session-id)
  (when-let* ((project-root (gethash session-id eureka--session-project-map))
              (project (gethash project-root eureka--projects)))
    project))

(defun eureka--create-temp-dir (prefix)
  "Create a temporary directory under BASEDIR.
Returns the path of the created directory or nil if failed."
  (let* ((temp-dir-prefix (concat "eureka-" (replace-regexp-in-string "/" "_" prefix) "-" (eureka-get-current-time) "-"))
         (temp-dir (make-temp-file temp-dir-prefix t)))
    (if (file-directory-p temp-dir)
        temp-dir
      (message "Failed to create temporary directory in %s" basedir)
      nil)))

(defun eureka--project-code-edit-done-callback (text session-id)
  "Use ediff to merge changes."
  (let ((project (eureka--get-project-from-session-id session-id))
        (actions (eureka--project-code-edit-parse-response text))
        project-root
        project-name
        relative-filename
        temp-dir temp-filename temp-filename-base bases
        ediff-regexp)
    (if (and project
             actions)
        (progn
          (require 'ediff)
          (setq project-root (eureka-project-root project)
                project-name (file-name-nondirectory (directory-file-name project-root))
                temp-dir (eureka--create-temp-dir project-name))
          (cl-dolist (action actions)
            (let ((filename (eureka--file-action-filename action))
                  (edits (eureka--file-action-edits action))
                  oldbuf content
                  tmpbuf tmpcont
                  search
                  replace)
              (setq relative-filename (string-replace project-root "" filename)
                    temp-filename (file-name-concat temp-dir relative-filename)
                    temp-filename-base (file-name-nondirectory temp-filename))
              (push temp-filename-base bases)
              (eureka-with-file-open-temporarily
                  filename t
                  (setq oldbuf (current-buffer)
                        content (buffer-substring-no-properties (point-min) (point-max)))
                  (setq tmpcont content)
                  (make-empty-file temp-filename t)
                  (setq tmpbuf (find-file-noselect temp-filename))
                  (cl-dolist (edit edits)
                    (setq search (eureka--file-edit-search edit)
                          replace (eureka--file-edit-replace edit))
                    (cond
                     ((and (length= search 0)
                           (length= tmpcont 0))
                      (setq tmpcont replace))
                     ((length= search 0)
                      (setq tmpcont (concat tmpcont replace)))
                     (t
                      (setq tmpcont (string-replace search replace tmpcont)))))
                  (with-current-buffer tmpbuf
                    (erase-buffer)
                    (insert tmpcont)
                    (save-buffer)))))
          ;; (setq ediff-regexp (regexp-opt bases))
          (ediff-directories project-root temp-dir nil))
      (message "No project or No actions."))))

;;;###autoload
(defun eureka-project-code-edit (task)
  "Do some coding edit in the project."
  (interactive "sWhat needs to be done: ")
  (let* ((project-root (eureka--project-root))
         (project (eureka--project project-root))
         (context (eureka--file-context t t t))
         session
         session-file
         buffer
         text)
    (when project
      (setq session (eureka-project-session project))
      (setq buffer (eureka-session-buffer session))
      ;; if buffer has been destroyed, recreate it
      (unless (buffer-live-p buffer)
        (setq session-file (eureka-session-file session))
        (setq buffer (find-file-noselect session-file))
        (setf (eureka-session-buffer session) buffer))
      (display-buffer buffer)
      (eureka-stream
       (format
        eureka-project-code-prompt-template
        context
        eureka-project-code-context-format
        task
        (format eureka-project-code-edit-suffix-template eureka-coding-language))
       :provider eureka-provider
       :session session
       :buffer buffer
       :point (with-current-buffer buffer (goto-char (point-max)) (point))
       :on-done #'eureka--project-code-edit-done-callback))))

;;;###autoload
(defun eureka-project-code-explain ()
  "Explain code in selected region or current buffer."
  (interactive)
  (let ((text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max))))
        (context (eureka--file-context nil nil nil)))
    (eureka-instant (format
                    eureka-project-code-prompt-template
                    context
                    eureka-project-code-context-format
                    (format eureka-project-code-explain-instruction-template text)
                    "")
                   :provider eureka-provider)))

;;;###autoload
(defun eureka-project-code-review ()
  "Review code in selected region or current buffer."
  (interactive)
  (let ((text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max))))
        (context (eureka--file-context nil nil nil)))
    (eureka-instant (format
                    eureka-project-code-prompt-template
                    context
                    eureka-project-code-context-format
                    (format eureka-project-code-review-instruction-template text)
                    "")
                   :provider eureka-provider)))

;;;###autoload
(defun eureka-project-code-improve ()
  "Improve selected code or the code in current buffer."
  (interactive)
  (let* ((project-root (eureka--project-root))
         (project (eureka--project project-root))
         (context (eureka--file-context t t t))
         session
         session-file
         buffer
         (beg (if (region-active-p)
		  (region-beginning)
		(point-min)))
	 (end (if (region-active-p)
		  (region-end)
		(point-max)))
	 (text (buffer-substring-no-properties beg end)))
    (when project
      (setq session (eureka-project-session project))
      (setq buffer (eureka-session-buffer session))
      (unless (buffer-live-p buffer)
        (setq session-file (eureka-session-file session))
        (setq buffer (find-file-noselect session-file))
        (setf (eureka-session-buffer session) buffer))
      (display-buffer buffer)
      (eureka-stream
       (format
        eureka-project-code-prompt-template
        context
        eureka-project-code-context-format
        (format eureka-project-code-improve-instruction-template text)
        (format eureka-project-code-edit-suffix-template eureka-coding-language))
       :provider eureka-provider
       :session session
       :buffer buffer
       :point (with-current-buffer buffer (goto-char (point-max)) (point))
       :on-done #'eureka--project-code-edit-done-callback))))

;;;###autoload
(defun eureka-project-ask (task)
  "Ask llm."
  (interactive "sWhat do you wanna ask: ")
  (let* ((project-root (eureka--project-root))
         (project (eureka--project project-root))
         (context (eureka--file-context))
         session
         session-file
         buffer
         text)
    (when project
      (setq session (eureka-project-session project))
      (setq buffer (eureka-session-buffer session))
      ;; if buffer has been destroyed, recreate it
      (unless (buffer-live-p buffer)
        (setq session-file (eureka-session-file session))
        (setq buffer (find-file-noselect session-file))
        (setf (eureka-session-buffer session) buffer))
      (display-buffer buffer)
      (eureka-instant (format
                       eureka-project-code-prompt-template
                       context
                       eureka-project-code-context-format
                       task
                       "")
                      :session session
                      :buffer buffer
                      :provider eureka-provider))))

;;;; File skeleton

(cl-defstruct eureka-file-content
  (filename)
  (content)
  (timestamp))

;; NOTE: We may change the way to capture the file skeleton.

;; For now, every query can capture 4 node at the most
;; @context                   *MUST*
;;         the whole code block, e.g. a class or a function
;; @context.body              *OPTIONAL*
;;         the body of the @context, e.g. java class body between "{" and "}"
;; @context.surrouding.start  *OPTIONAL*
;;         e.g. the "{" of a java class body
;; @context.surrouding.end    *OPTIONAL*
;;         e.g. the "}" of a java class body
;; For example, if we have a java file with a class like the following

;; class C {
;;         ^     // 1
;;     C1 c1;
;;     void f1 (int a) {
;;         int v1 = 10;
;;         int v2 = 20;
;;     }
;; }
;; ^   // 2

;; @context should capture the whole class C, and @context.body should capture
;; class C's body from 1 to 2, and the content between @context's start
;; and @context.body's start is what we're interested in and should be part
;; of the skeleton of the file.
;; @context.surrouding.start captures "{" at 1, and @context.surrouding.end
;; captures "}" at 2. We capture these two things because we want to make
;; the skeleton more natural.

;; These four captures have the same meaning for function f1.

;; For class property c1, we only need a @context to capture it. No @context.body,
;; @context.surroungding.start or @context.surroungding.end.

;; So the file skeleton for this file looks like the following:

;; class C {
;;     C1 c1;
;;     void f1 (int a) {
;;     }
;; }


;; For a python class, we can consider ":" after the class name as surrounding start,
;; but there is no surrounding end, so no need to use @context.surrounding.end.
(setq eureka--treesit-language-queries
      '((python . (("class_definition" (class_definition ":" @context.body @context.surrounding.start body: (_)) @context)
                   ("function_definition" (function_definition ":" @context.body @context.surrounding.start body: (_)) @context)))
        (java . (("class_declaration" (class_declaration body: (class_body :anchor "{" @context.surrounding.start _ "}" @context.surrounding.end :anchor) @context.body) @context)
                 ("method_declaration" (method_declaration body: (block :anchor "{" @context.surrounding.start _ "}" @context.surrounding.end :anchor) @context.body) @context)
                 ("field_declaration" (field_declaration declarator: (_) ";") @context)))
        ))

(defun eureka--treesit-language-node-types (language)
  (let ((queries (assoc-default language eureka--treesit-language-queries))
        node-types)
    (when queries
      (setq node-types (mapcar 'car queries)))
    node-types))

(defun eureka--treesit-language-node-query (language node-type)
  (let ((queries (assoc-default language eureka--treesit-language-queries))
        result)
    (when queries
      (cl-dolist (query queries)
        (when (string-equal (car query) node-type)
          (setq result (append result (cdr query))))))
    result))

(defvar-local eureka--file-skeleton nil
  "Skeleton of current buffer's file.")

(defun eureka--file-modified-timestamp (filename)
  (let ((timestamp 0))
    (when (and filename
               (file-exists-p filename))
      (time-convert (file-attribute-modification-time (file-attributes filename)) 'integer))))

(defun eureka--file-cache-init-fn (data)
  "DATA is a filename."
  (eureka--file-modified-timestamp data))

(defun eureka--file-cache-test-fn (info data)
  "INFO is what `eureka--file-cache-init-fn' returns.
DATA is a filename."
  (> (eureka--file-modified-timestamp data) info))

(defvar eureka--file-skeletons (cache-make-cache #'eureka--file-cache-init-fn
                                                 #'eureka--file-cache-test-fn
                                                 #'ignore
                                                 :test #'equal)
  "File's skeleton.
Key is file name, value is of type `eureka-file-content'.")

(defun eureka--file-skeleton (filename)
  "Get FILENAME's skeleton.
Return nil if no treesitter support for FILENAME."
  (let ((file-skeleton (cache-get filename eureka--file-skeletons filename))
        skeleton)
    (unless file-skeleton
      (progn
        (setq skeleton (eureka--retrieve-file-skeleton filename))
        (when skeleton
          (setq file-skeleton (make-eureka-file-content :filename filename
                                                        :content skeleton
                                                        :timestamp (eureka--current-timestamp)))
          (cache-put filename file-skeleton eureka--file-skeletons filename))))
    file-skeleton))

(defvar eureka--file-contents (cache-make-cache #'eureka--file-cache-init-fn
                                                #'eureka--file-cache-test-fn
                                                #'ignore
                                                :test #'equal)
  "File's content.
Key is file name, value is of type `eureka-file-content'.")

(defun eureka--file-full-content (filename)
  "Get FILENAME's content."
  (let ((file-content (cache-get filename eureka--file-contents filename))
        content)
    (unless file-content
      (progn
        (setq content (eureka--retrieve-file-content filename))
        (when content
          (setq file-content (make-eureka-file-content :filename filename
                                                       :content content
                                                       :timestamp (eureka--current-timestamp)))
          (cache-put filename file-content eureka--file-contents filename))))
    file-content))

(defvar eureka-treesit-suffix-language-map
  '(("py" . "python")
    ("el" . "elisp")
    ("js" . "javascript")
    ("ts" . "typescript"))
  "Map file extension to language that can be used to determine tree sitter parser.")

(defvar eureka-treesit-major-mode-language-map nil
  "Use buffer's major mode (in string form) to determine language that can
be used to determine tree sitter parser.")

(defun eureka--buffer-treesit-language (&optional buf)
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
      (setq language (or (assoc-default suffix eureka-treesit-suffix-language-map)
                         (assoc-default mm eureka-treesit-major-mode-language-map)))
      (unless language
        (setq language
              (cond
               ((string-suffix-p "-ts-mode" mm)
                (intern (string-remove-suffix "-ts-mode" mm)))
               (t
                (intern (string-remove-suffix "-mode" mm)))))))
    language))

(defvar-local eureka--treesit-parser nil
  "treesit parser of the current buffer.")

(defun eureka--ensure-treesit-parser (&optional buf)
  (let ((buf (or buf (current-buffer)))
        language
        parser)
    (with-current-buffer buf
      (setq parser eureka--treesit-parser)
      (unless parser
        (setq language (eureka--buffer-treesit-language))
        (if (and language
                 (treesit-language-available-p language))
            (progn
              (setq parser (treesit-parser-create language))
              (setq-local eureka--treesit-parser parser))
          (message "language: %s without parser available" language))))
    parser))


(defvar eureka--indent-step nil)
(defvar eureka--travel-result nil)
(defvar eureka--surrounding-end-stack nil)

(defun eureka--treesit-capture (node query &optional beg end node-only)
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

(defun eureka--treesit-traval-sparse-tree-1 (language ele)
  (cond
   ((null ele)
    ;; nothing
    )
   ((atom ele)
    (let ((node-type (treesit-node-type ele))
          (node-start (treesit-node-start ele))
          (node-end (treesit-node-end ele))
          (indentation (make-string (* eureka--indent-step 4) ?\s))
          result
          query
          captures)
      (when-let* ((query (eureka--treesit-language-node-query language node-type))
                  (captures (eureka--treesit-capture ele query node-start node-end)))
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
              (push (concat indentation surrounding.end "\n") eureka--surrounding-end-stack)
              (setq eureka--travel-result (concat eureka--travel-result (concat indentation (buffer-substring-no-properties start-pos end-pos) surrounding.start "\n")))))))))
   ((listp ele)
    (setq eureka--indent-step (1+ eureka--indent-step))
    (cl-dolist (e ele)
      (eureka--treesit-traval-sparse-tree-1 language e))
    (setq eureka--indent-step (1- eureka--indent-step))
    ;; pop surrouding end [1]
    (setq eureka--travel-result (concat eureka--travel-result (pop eureka--surrounding-end-stack))))))

(defun eureka--treesit-traval-sparse-tree (language sparse-tree)
  "Perform a depth-first, pre-order traversal of SPARSE-TREE."
  (setq eureka--indent-step -1
        eureka--travel-result nil
        eureka--surrounding-end-stack nil)
  (when (null (car sparse-tree))
    (setq eureka--indent-step -2))
  (eureka--treesit-traval-sparse-tree-1 language sparse-tree)
  eureka--travel-result)

(defun eureka--retrieve-buffer-skeleton (&optional buf)
  (let* ((buf (or buf (current-buffer)))
         (language (eureka--buffer-treesit-language buf))
         (parser (eureka--ensure-treesit-parser buf))
         (node-types (eureka--treesit-language-node-types language))
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
                (eureka--treesit-traval-sparse-tree language sparse-tree)))
            eureka--travel-result)
        (message "parser or node types are nil")
        nil))))

(defun eureka--retrieve-file-skeleton (filename)
  (eureka-with-file-open-temporarily
      filename t
      (eureka--retrieve-buffer-skeleton)))

(defun eureka--retrieve-file-content (filename)
  (eureka-with-file-open-temporarily
      filename t
      (buffer-substring-no-properties (point-min) (point-max))))

;;;; Repomap

(defcustom eureka-file-deps-function nil
  "A function to retrieve files the current file
depends on. The function takes one argument, current
file's name, and returns filenames as a list."
  :group 'eureka
  :type 'function)

(defcustom eureka-file-content-type nil
  "When creating file context, use file skeleton (see `eureka--file-skeleton')
or file full content (see `eureka--file-full-content').
It can be 'full or 'skeleton. nil means 'skeleton, t means 'full."
  :group 'eureka
  :type 'boolean)

;; There are four different kinds of context in eureka.
;; Here context means file skeleton or full file content (in
;; this case, it's because file skeleton cannot be got).

;; The first kind is for projects, and is added manually.
;; This kind is stored in `eureka-project''s context.

;; The second kind is for files and is added automatically.
;; This kind of context is files retrieved by calling
;; `eureka-file-deps-function'.

;; The third kind is also for files and is added manually.
;; This kind is stored in `eureka--file-context-manually'.

;; The forth kind is the current file's full content.

(cl-defstruct eureka-project
  "A structure that represents a eureka project.

ROOT is the project root, string.

CONTEXT is the context of this project, a list of filename.

PROVIDER is the llm provider for this project.

SESSION is the `eureka-session' for this project."
  root
  context
  provider
  session)

(cl-defstruct eureka-file-deps
  (filename)
  (deps)
  (timestamp))

(defvar eureka--file-deps (cache-make-cache #'eureka--file-cache-init-fn
                                            #'eureka--file-cache-test-fn
                                            #'ignore
                                            :test #'equal)
  "Buffers' deps.
Key is file name, value is of type `eureka-file-deps'.")


(defvar-local eureka--file-context-manually nil
  "File context which is added manually.
This is a list of filename.")

(defvar eureka--projects (make-hash-table :test #'equal)
  "Each project (represented by project root) and
its `eureka-project'.")

(defun eureka--retrieve-calls-by-lspce ()
  (cl-labels
      ((lsp--call-hierarchy (method item tag)
         (let (calls)
           (when-let* ((response (lspce--request method (list :item item))))
             (setq calls (seq-map (lambda (item)
                                    (gethash tag item))
                                  response)))
           calls)))
    (let (incomings
          outgoings)
      (when-let* ((root-items (when (lspce--server-capable-chain "callHierarchyProvider")
                                (lspce--request "textDocument/prepareCallHierarchy" (lspce--make-textDocumentPositionParams)))))
        (let ((method "callHierarchy/incomingCalls")
              (tag "from"))
          (cl-dolist (item root-items)
            (setq incomings (append incomings (lsp--call-hierarchy method item tag)))))
        (let ((method "callHierarchy/outgoingCalls")
              (tag "to"))
          (cl-dolist (item root-items)
            (setq outgoings (append outgoings (lsp--call-hierarchy method item tag))))))
      (list incomings outgoings))))

;; NOTE How `eureka--retrieve-file-deps-by-lspce-1' works:
;; - Open FILENAME.

;; - Use textDocument/documentSymbol to query desired symbols.
;;   Filter symbols with kind and get symbol's start position from selectionRange.

;; - Retrieve incoming calls and get files that directly depend on FILENAME.

;; - Retrieve outgoning calls and get files that FILENAME directly depends on.

;; 5 class
;; 6 method
;; 12 function
(defvar eureka-lsp-symbol-kinds '(5 6 12)
  "Symbol kinds that are used to filter symbols when retrieving file deps.")

;; - Merge and dedup filenames
(defun eureka--retrieve-file-deps-by-lspce (filename)
  (when (and (fboundp 'lspce-mode)
             (file-exists-p filename))
    (eureka-with-file-open-temporarily
        filename t
        (when lspce-mode
          (let (filenames
                symbols
                incomings
                outgoings
                calls
                deps)
            (when-let* ((response (lspce--request
                                   "textDocument/documentSymbol"
                                   (list :textDocument
                                         (lspce--textDocumentIdenfitier (lspce--uri))))))
              (cl-dolist (symbol response)
                (let ((kind (gethash "kind" symbol))
                      (children (gethash "children" symbol)))
                  (when (member kind eureka-lsp-symbol-kinds)
                    (push symbol symbols))
                  (when children
                    (cl-dolist (c children)
                      (when (member (gethash "kind" c) eureka-lsp-symbol-kinds)
                        (push c symbols)))))))
            (save-excursion
              (save-restriction
                (cl-dolist (s symbols)
                  (let* ((selectionRange (gethash "selectionRange" s))
                         (start (gethash "start" selectionRange))
                         (pos (lspce--lsp-position-to-point start)))
                    (goto-char pos)
                    (setq calls (eureka--retrieve-calls-by-lspce))
                    (setq incomings (append incomings (nth 0 calls))
                          outgoings (append outgoings (nth 1 calls)))))))
            (cl-dolist (item (append incomings outgoings))
              (when-let* ((uri (gethash "uri" item))
                          (path (eureka--uri-to-path uri)))
                (when (and (string-prefix-p lspce--root-uri uri)
                           (not (string-equal filename path)))
                  (push path deps))))
            (delete-dups deps))))))

(defun eureka--retrieve-file-deps (filename)
  "Get files depending on or depended by FILENAME."
  (let ((deps (cache-get filename eureka--file-deps filename)))
    (unless deps
      (setq deps (and eureka-file-deps-function
                      (functionp eureka-file-deps-function)
                      (funcall eureka-file-deps-function filename)))
      (when deps
        (cache-put filename deps eureka--file-deps filename)))
    deps))

;;;###autoload
(defun eureka-add-local-context ()
  "Add a file to local manual context."
  (interactive)
  (let ((project-root (eureka--project-root))
        filename)
    (setq filename (read-file-name "Add a file to local context: "))
    (when (and filename
               project-root
               (file-exists-p filename))
      (setq eureka--file-context-manually (delete-dups (push filename eureka--file-context-manually))))))

;;;###autoload
(defun eureka-remove-local-context ()
  "Remove a file from local manual context."
  (interactive)
  (let (filename)
    (setq filename (completing-read "Remove a file from local context: "
                                    eureka--file-context-manually))
    (when (and filename
               (file-exists-p filename))
      (setq eureka--file-context-manually (delete filename eureka--file-context-manually)))))

(defvar eureka--session-project-map (make-hash-table :test #'equal)
  "Map session-id to project-root.")

(defun eureka--project (project-root)
  "Get project with root PROJECT-ROOT from cache. Create it
if not existing."
  (when project-root
    (let ((project (gethash project-root eureka--projects))
          session)
      (unless project
        (setq session (eureka-new-session eureka-provider project-root))
        (setq project (make-eureka-project :root project-root :context nil :provider eureka-provider :session session))
        (puthash (eureka-session-id session) project-root eureka--session-project-map)
        (puthash project-root project eureka--projects))
      project)))

;;;###autoload
(defun eureka-project-refresh-session ()
  "Recreate session for current project."
  (interactive)
  (let* ((project-root (eureka--project-root))
         project
         old-session
         session)
    (when project-root
      (setq project (gethash project-root eureka--projects))
      (when project
        (setq old-session (eureka-project-session project))
        (setq session (eureka-new-session eureka-provider project-root))
        (setf (eureka-project-session project) session)
        (remhash (eureka-session-id old-session) eureka--session-project-map)
        (puthash (eureka-session-id session) project-root eureka--session-project-map)))))

;;;###autoload
(defun eureka-add-project-context ()
  "Add a file to project context."
  (interactive)
  (let ((project-root (eureka--project-root))
        filename
        context
        project
        session)
    (setq filename (read-file-name "Add a file to project context: "))
    (when (and filename
               project-root
               (file-exists-p filename))
      (setq project (eureka--project project-root))
      (when project
        (setq context (eureka-project-context project))
        (setq context (delete-dups (push filename context)))
        (setf (eureka-project-context project) context)
        (puthash project-root project eureka--projects)))))

;;;###autoload
(defun eureka-remove-project-context ()
  "Remove a file from project context."
  (interactive)
  (let ((project-root (eureka--project-root))
        filename
        context
        project)
    (setq project (eureka--project project-root))
    (when project
      (setq context (eureka-project-context project))
      (setq filename (completing-read "Remove a file from project context: " context))
      (when (and filename
                 (file-exists-p filename))
        (setf (eureka-project-context project) (delete filename context))))))

;;;###autoload
(defun eureka-list-files-in-file-context ()
  "List all files in current file's context.
The result will include project's context."
  (interactive)
  )

(defvar eureka--language-ids
  '(("py" . "python")
    ("el" . "elisp")
    ("js" . "javascript")
    ("ts" . "typescript"))
  "Map file extension to language id that can be used in markdown.")

(defun eureka--file-language-id (filename)
  "Get language id of FILENAME."
  (let (suffix
        language-id)
    (setq suffix (file-name-extension filename))
    (setq language-id (assoc-default suffix eureka--language-ids))
    (unless language-id
      (setq language-id suffix))
    language-id))

(defun eureka--format-file-context (filename content)
  "Format FILENAME and its CONTENT as context in the specified format."
  (let (context
        (language-id (eureka--file-language-id filename)))
    (concat "```\n"
            filename "\n"
            "```\n"
            "```" language-id "\n"
            content "\n"
            "```"
            "\n\n")))

(defun eureka--get-file-content (filename)
  (cond
   ((or (null eureka-file-content-type)
        (eq eureka-file-content-type 'skeleton))
    (eureka--file-skeleton filename))
   ((or (eq eureka-file-content-type t)
        (eq eureka-file-content-type 'full))
    (eureka--file-full-content filename))))

;; internal use
;; files in a file's or project's context
(cl-defstruct eureka--context-files
  automatic
  manual
  project
  )

(defun eureka--file-context (&optional automatic-p manual-p project-p)
  "Calculate context of current buffer file."
  (with-current-buffer (current-buffer)
    (let* ((filename (buffer-file-name))
           (project-root (eureka--project-root))
           (project (eureka--project project-root))
           file-content
           deps
           context)
      (when (and filename
                 project-root)
        (setq deps (list filename))
        (cond
         (automatic-p
          (setq deps (append deps (eureka--retrieve-file-deps filename))))
         (manual-p
          (setq deps (append deps eureka--file-context-manually)))
         ((and project-p
               project)
          (setq deps (append deps (eureka-project-context project)))))
        (setq deps (delete filename (delete-dups deps)))
        (setq context (eureka--format-file-context
                       filename
                       (buffer-substring-no-properties (point-min) (point-max))))
        (cl-dolist (dep deps)
          (setq file-content (eureka--get-file-content dep))
          (when file-content
            (setq context (concat context
                                  (eureka--format-file-context
                                   (eureka-file-content-filename file-content)
                                   (eureka-file-content-content file-content)))))))
      context)))

(defun eureka--project-context ()
  "Calculate context of current buffer's project."
  (with-current-buffer (current-buffer)
    (let* ((filename (buffer-file-name))
           (project-root (eureka--project-root))
           (project (eureka--project project-root))
           file-content
           deps
           context)
      (when (and filename
                 project)
        (setq deps (delete-dups (eureka-project-context project)))
        (setq context (eureka--format-file-context
                       filename
                       (buffer-substring-no-properties (point-min) (point-max))))
        (cl-dolist (dep deps)
          (setq file-content (eureka--get-file-content dep))
          (when file-content
            (setq context (concat context
                                  (eureka--format-file-context
                                   (eureka-file-content-filename file-content)
                                   (eureka-file-content-content file-content)))))))
      context)))

;;;; Helping functions
(defun eureka-latest-response ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let* ((project-root (eureka--project-root))
           response)
      (when project-root
        (when-let* ((project (eureka--project project-root))
                    (session (eureka-project-session project)))
          (setq response (eureka-session-response session))))
      (message "response: %s" response))))

(defun eureka-latest-prompt ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let* ((project-root (eureka--project-root))
           prompt)
      (when project-root
        (when-let* ((project (eureka--project project-root))
                    (session (eureka-project-session project)))
          (setq prompt (eureka-session-prompt session))))
      (message "prompt: %s" prompt))))

(provide 'eureka-coding)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; eureka-coding.el ends here

