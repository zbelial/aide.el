;;; aidee-coding.el  --- AI powered Development Environtment for Emacs. -*- lexical-binding: t; -*-

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
(require 'cl-lib)
(require 'aidee-core)
(require 'aidee-utils)
(require 'cache)

(defcustom aidee-coding-language "English"
  "Language for aidee coding."
  :group 'aidee
  :type 'string)

;; 4 placeholders: context, context format, user instrument and suffix.
(defcustom aidee-project-code-prompt-template "Context:\n%s\n%s\nBased on the above context, act as an expert software developer, %s. %s"
  "Prompt template for all project code task."
  :group 'aidee
  :type 'string)

(defcustom aidee-project-code-review-instruction-template "review the following code and make concise suggestions: \n```%s\n```"
  "Prompt template for `aidee-project-code-review'."
  :group 'aidee
  :type 'string)

(defcustom aidee-project-code-improve-instruction-template "enhance the following code: \n```%s\n```"
  "Prompt template for `aidee-project-code-improve'."
  :group 'aidee
  :type 'string)

(defcustom aidee-project-code-explain-instruction-template "explain the following code in a detailed way: \n```%s\n```"
  "Prompt template for `aidee-project-code-explain'."
  :group 'aidee
  :type 'string)

(defcustom aidee-project-code-context-format "Context at the begining contains one or more *CONTEXT block*,
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
  :group 'aidee
  :type 'string
  )

;; 1 placeholder(s): language
(defcustom aidee-project-code-edit-suffix-template "Always use best practices when coding.
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
1. The *FULL* file path alone on a line, verbatim. No bold asterisks, no quotes around it, no escaping of characters, etc.
2. The opening fence and code language, eg: ```python, here python should be replaced with the code language that is generated
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
  "Prompt template for `aidee-project-code-edit'."
  :group 'aidee
  :type 'string)

(defvar aidee--code-search-label "<<<<<<< SEARCH")
(defvar aidee--code-replace-label ">>>>>>> REPLACE")

(defconst aidee--project-code-search-replace-pattern
  (rx
   (seq
    ;;code fence start
    (minimal-match
     (zero-or-more anything) (literal "```") (one-or-more alpha) (+ (or "\n" "\r")))
    ;;SEARCH
    (minimal-match
     (eval aidee--code-search-label) (+ (or "\n" "\r")))
    (group (minimal-match
            (zero-or-more anything)))
    ;; =======
    (literal "=======") (+ (or "\n" "\r"))
    ;;REPLACE
    (group (minimal-match
            (zero-or-more anything)))
    (minimal-match
     (eval aidee--code-replace-label) (+ (or "\n" "\r")))
    ;;code fence end
    (literal "```")))
  )

(defconst aidee--project-code-edit-pattern
  (rx
   ;;filename fence start
   (minimal-match
    (literal "```") (zero-or-more alpha) (+ (or "\n" "\r")))
   ;;filename
   (group (minimal-match
           (one-or-more (not (any "\n" "\r")))))
   (+ (or "\n" "\r"))
   ;;filename fence end
   (zero-or-more (seq (literal "```") (+ (or "\n" "\r"))))
   (one-or-more
    (seq
     ;;code fence start
     (minimal-match
      (zero-or-more anything) (literal "```") (one-or-more alpha) (+ (or "\n" "\r")))
     ;;SEARCH
     (minimal-match
      (eval aidee--code-search-label) (+ (or "\n" "\r")))
     (group (minimal-match
             (zero-or-more anything)))
     ;; =======
     (literal "=======") (+ (or "\n" "\r"))
     ;;REPLACE
     (group (minimal-match
             (zero-or-more anything)))
     (minimal-match
      (eval aidee--code-replace-label) (+ (or "\n" "\r")))
     ;;code fence end
     (literal "```")))))

;; copied from s.el's s-match and modified
(defun aidee--match (regexp s &optional start)
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
          (message "match-data-list: %s" match-data-list)
          (while match-data-list
            (let* ((beg (car match-data-list))
                   (end (cadr match-data-list))
                   (subs (if (and beg end) (list (substring s beg end) beg end) nil)))
              (setq result (cons subs result))
              (setq match-data-list
                    (cddr match-data-list))))
          (nreverse result)))))

(cl-defstruct aidee--file-edit
  search
  replace)

(cl-defstruct aidee--file-action
  filename
  edits)

(defun aidee--count-substring (substring string)
  "Count the number of occurrences of SUBSTRING in STRING."
  (let ((count 0)
        (start 0))
    (while (string-match substring string start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(defun aidee--project-code-edit-parse-search-replace (text)
  (let ((edits nil)
        search replace
        edit
        start
        stop?
        match-result)
    (while (not stop?)
      (setq match-result (aidee--match aidee--project-code-search-replace-pattern text start))
      (if (and match-result
               (length= match-result 3))
          (progn
            (setq start (nth 2 (nth 0 match-result))
                  search (nth 0 (nth 1 match-result))
                  replace (nth 0 (nth 2 match-result)))
            (setq edit (make-aidee--file-edit :search search :replace replace))
            (push edit edits))
        (setq stop? t)))
    (nreverse edits)))

(defun aidee--project-code-edit-parse-response (text)
  (let ((actions nil)
        filename
        action
        match-result
        edits
        edit-text
        filename-end
        total-end
        start
        stop?)
    (if (= (aidee--count-substring aidee--code-search-label text)
           (aidee--count-substring aidee--code-replace-label text))
        (progn
          (while (not stop?)
            (progn
              (setq match-result (aidee--match aidee--project-code-edit-pattern text start))
              (if (and match-result
                       (length> match-result 2))
                  (progn
                    (setq total-end (nth 2 (nth 0 match-result))
                          filename-end (nth 2 (nth 1 match-result))
                          filename (nth 0 (nth 1 match-result)))
                    (setq edit-text (substring-no-properties text filename-end total-end))
                    (setq edits (aidee--project-code-edit-parse-search-replace edit-text))
                    (setq action nil)
                    (when (and filename
                               edits)
                      (setq action (make-aidee--file-action :filename filename
                                                            :edits edits))
                      (push action actions))
                    (setq start total-end))
                (setq stop? t))))
          (nreverse actions))
      (message "Ill-formed response."))))

(defun aidee--project-code-edit-done-callback (text &optional on-done)
  (let ((actions (aidee--project-code-edit-parse-response text)))
    (when actions
      (cl-dolist (action actions)
        (let ((filename (aidee--file-action-filename action))
              (edits (aidee--file-action-edits action))
              search
              replace)
          (aidee-with-file-open-temporarily
              filename t
              (cl-dolist (edit edits)
                (setq search (aidee--file-edit-search edit)
                      replace (aidee--file-edit-replace edit))
                (if (length= search 0)
                    (progn
                      (goto-char (point-max))
                      (insert replace))
                  (query-replace search replace nil (point-min) (point-max))))
              ))))))

(defun aidee--project-code-edit-done-callback (text &optional on-done)
  (let ((actions (aidee--project-code-edit-parse-response text)))
    (when actions
      (cl-dolist (action actions)
        (let ((filename (aidee--file-action-filename action))
              (edits (aidee--file-action-edits action))
              content
              oldbuf
              tmpbuf
              tmpcont
              search
              replace)
          (aidee-with-file-open-temporarily
              filename t
              (setq oldbuf (current-buffer)
                    content (buffer-substring-no-properties (point-min) (point-max)))
              (setq tmpcont content)
              (setq tmpbuf (generate-new-buffer " *temp* " t))
              (cl-dolist (edit edits)
                (setq search (aidee--file-edit-search edit)
                      replace (aidee--file-edit-replace edit))
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
                (insert tmpcont))
              (ediff-buffers oldbuf tmpbuf)))))))

;;;###autoload
(defun aidee-project-code-edit (task)
  "Do some coding edit in the project."
  (interactive "sWhat needs to be done: ")
  (let* ((root-uri (aidee--root-uri))
         (project (aidee--project root-uri))
         (context (aidee--file-context t t t))
         session
         session-file
         buffer
         text)
    (when project
      (setq session (aidee-project-session project))
      (setq buffer (aidee-session-buffer session))
      ;; if buffer has been destroyed, recreate it
      (unless (buffer-live-p buffer)
        (setq session-file (aidee-session-file session))
        (setq buffer (find-file-noselect session-file))
        (setf (aidee-session-buffer session) buffer))
      (aidee-stream
       (format
        aidee-project-code-prompt-template
        context
        aidee-project-code-context-format
        task
        (format aidee-project-code-edit-suffix-template aidee-coding-language))
       :provider aidee-provider
       :session session
       :buffer buffer
       :point (with-current-buffer buffer (goto-char (point-max)) (point))
       :on-done #'aidee--project-code-edit-done-callback))))

;;;###autoload
(defun aidee-project-code-explain ()
  "Explain code in selected region or current buffer."
  (interactive)
  (let ((text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max))))
        (context (aidee--file-context nil nil nil)))
    (aidee-instant (format
                    aidee-project-code-prompt-template
                    context
                    aidee-project-code-context-format
                    (format aidee-project-code-explain-instruction-template text)
                    "")
                   :provider aidee-provider)))

;;;###autoload
(defun aidee-project-code-review ()
  "Review code in selected region or current buffer."
  (interactive)
  (let ((text (if (region-active-p)
		  (buffer-substring-no-properties (region-beginning) (region-end))
		(buffer-substring-no-properties (point-min) (point-max))))
        (context (aidee--file-context nil nil nil)))
    (aidee-instant (format
                    aidee-project-code-prompt-template
                    context
                    aidee-project-code-context-format
                    (format aidee-project-code-review-instruction-template text)
                    "")
                   :provider aidee-provider)))

;;;###autoload
(defun aidee-project-code-improve ()
  "Improve selected code or the code in current buffer."
  (interactive)
  (let* ((root-uri (aidee--root-uri))
         (project (aidee--project root-uri))
         (context (aidee--file-context t t t))
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
      (setq session (aidee-project-session project))
      (setq buffer (aidee-session-buffer session))
      (unless (buffer-live-p buffer)
        (setq session-file (aidee-session-file session))
        (setq buffer (find-file-noselect session-file))
        (setf (aidee-session-buffer session) buffer))
      (aidee-stream
       (format
        aidee-project-code-prompt-template
        context
        aidee-project-code-context-format
        (format aidee-project-code-improve-prompt-template text)
        (format aidee-project-code-edit-suffix-template aidee-coding-language))
       :provider aidee-provider
       :session session
       :buffer buffer
       :point (with-current-buffer buffer (goto-char (point-max)) (point))
       :on-done #'aidee--project-code-edit-done-callback))))

;;;; File skeleton

(cl-defstruct aidee-file-skeleton
  (filename)
  (skeleton)
  (timestamp))

;; TODO explain queries
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

(defvar-local aidee--file-skeleton nil
  "Skeleton of current buffer's file.")

(defun aidee--file-modified-timestamp (filename)
  (let ((timestamp 0))
    (when (and filename
               (file-exists-p filename))
      (time-convert (file-attribute-modification-time (file-attributes filename)) 'integer))))

(defun aidee--file-skeleton-init-fn (data)
  "DATA is a filename."
  (aidee--file-modified-timestamp data))

(defun aidee--file-skeleton-test-fn (info data)
  "INFO is what `aidee--file-skeleton-init-fn' returns.
DATA is a filename."
  (> (aidee--file-modified-timestamp data) info))

(defvar aidee--file-skeletons (cache-make-cache #'aidee--file-skeleton-init-fn
                                                #'aidee--file-skeleton-test-fn
                                                #'ignore
                                                :test #'equal)
  "Buffers' skeleton.
Key is file name, value is of type `aidee-file-skeleton'.")

(defun aidee--file-skeleton (filename)
  "Get FILENAME's skeleton.
Return nil if no treesitter support for FILENAME."
  (let ((file-skeleton (cache-get filename aidee--file-skeletons filename))
        skeleton)
    (unless file-skeleton
      (progn
        (setq skeleton (aidee--retrieve-file-skeleton filename))
        (setq file-skeleton (make-aidee-file-skeleton :filename filename
                                                      :skeleton skeleton
                                                      :timestamp (aidee--current-timestamp)))
        (cache-put filename file-skeleton aidee--file-skeletons filename)))
    file-skeleton))


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

(defun aidee--retrieve-file-skeleton (filename)
  (aidee-with-file-open-temporarily
      filename t
      (aidee--retrieve-buffer-skeleton)))

;;;; Repomap

(defcustom aidee-file-deps-function nil
  "A function to retrieve files the current file
depends on. The function takes one argument, current
file's name, and returns filenames as a list."
  :group 'aidee
  :type 'function)


;; There are four different kinds of context in aidee.
;; Here context means file skeleton or file content (in
;; this case, it's because file skeleton cannot be got).

;; The first kind is for projects, and is added manually.
;; This kind is stored in `aidee-project''s session.

;; The second kind is for files and is added automatically,
;; this kind of context is files retrieved by calling
;; `aidee-file-deps-function'.
;; This kind is stored in `aidee--file-context-automatically'.

;; The third kind is also for files and is added manually.
;; This kind is stored in `aidee--file-context-manually'.

;; The forth kind is the current file, which is stored in
;; `aidee--file-skeleton'.

(cl-defstruct aidee-project
  "A structure that represents aidee project.

ROOT is the project root, string.

CONTEXT is the context of this project, a list of filename.

PROVIDER is the llm provider for this project.

SESSION is the `aidee-session' for this project."
  root
  context
  provider
  session
  )

(cl-defstruct aidee-file-deps
  (filename)
  (deps)
  (timestamp))

(defun aidee--file-deps-init-fn (data)
  "DATA is a filename."
  (aidee--file-modified-timestamp data))

(defun aidee--file-deps-test-fn (info data)
  "INFO is what `aidee--file-deps-init-fn' returns.
DATA is a filename."
  (> (aidee--file-modified-timestamp data) info))

(defvar aidee--file-deps (cache-make-cache #'aidee--file-deps-init-fn
                                           #'aidee--file-deps-test-fn
                                           #'ignore
                                           :test #'equal)
  "Buffers' deps.
Key is file name, value is of type `aidee-file-deps'.")


(defvar-local aidee--file-context-automatically nil
  "File context which is calculated automatically.
This is a list of filename.")

(defvar-local aidee--file-context-manually nil
  "File context which is added manually.
This is a list of filename.")

(defvar aidee--projects (make-hash-table :test #'equal)
  "Each project (represented by project root) and
its `aidee-project'.")

(defun aidee--retrieve-calls-by-lspce ()
  (cl-labels
      ((lsp--call-hierarchy (method item tag)
         (let (calls)
           (when-let* ((response (lspce--request method (list :item item))))
             (setq calls (seq-map (lambda (item)
                                    (gethash tag item))
                                  response)))
           calls)))
    (let ((root-uri lspce--root-uri)
          (lsp-type lspce--lsp-type)
          incomings
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

;; NOTE How `aidee--retrieve-file-deps-by-lspce-1' works:
;; - Open FILENAME.

;; - Use textDocument/documentSymbol to query desired symbols.
;;   Filter symbols with kind and get symbol's start position from selectionRange.

;; - Retrieve incoming calls and get files that directly depend on FILENAME.

;; - Retrieve outgoning calls and get files that FILENAME directly depends on.

;; - Merge and dedup filenames
(defun aidee--retrieve-file-deps-by-lspce-1 (filename)
  (when (and (fboundp 'lspce-mode)
             (file-exists-p filename))
    (aidee-with-file-open-temporarily
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
                  ;; FIXME add a defcustom
                  ;; 5 class
                  ;; 6 method
                  ;; 12 function
                  (when (member kind '(5 6 12))
                    (push symbol symbols))
                  (when children
                    (cl-dolist (c children)
                      (when (member (gethash "kind" c) '(5 6 12))
                        (push c symbols)))))))
            (save-excursion
              (save-restriction
                (cl-dolist (s symbols)
                  (let* ((selectionRange (gethash "selectionRange" s))
                         (start (gethash "start" selectionRange))
                         (pos (lspce--lsp-position-to-point start)))
                    (goto-char pos)
                    (setq calls (aidee--retrieve-calls-by-lspce))
                    (setq incomings (append incomings (nth 0 calls))
                          outgoings (append outgoings (nth 1 calls)))))))
            (cl-dolist (item (append incomings outgoings))
              (when-let* ((uri (gethash "uri" item))
                          (path (aidee--uri-to-path uri)))
                (when (and (string-prefix-p lspce--root-uri uri)
                           (not (string-equal filename path)))
                  (push path deps))))
            (delete-dups deps))))))

(defun aidee--retrieve-file-deps (filename)
  "Get files depending on or depended by FILENAME."
  (let ((deps (cache-get filename aidee--file-deps filename)))
    (unless deps
      (setq deps (and aidee-file-deps-function
                      (functionp aidee-file-deps-function)
                      (funcall aidee-file-deps-function filename)))
      (when deps
        (cache-put filename deps aidee--file-deps filename)))
    deps))

;;;###autoload
(defun aidee-add-local-context ()
  "Add a file to local manual context."
  (interactive)
  (let ((root-uri (aidee--root-uri))
        filename)
    (setq filename (read-file-name "Add a file to local context: "))
    (when (and filename
               root-uri
               (file-exists-p filename))
      (setq aidee--file-context-manually (delete-dups (push filename aidee--file-context-manually))))))

;;;###autoload
(defun aidee-remove-local-context ()
  "Remove a file from local manual context."
  (interactive)
  (let (filename)
    (setq filename (completing-read "Remove a file from local context: "
                                    aidee--file-context-manually))
    (when (and filename
               (file-exists-p filename))
      (setq aidee--file-context-manually (delete filename aidee--file-context-manually)))))

(defun aidee--project (root-uri)
  (when root-uri
    (let ((project (gethash root-uri aidee--projects))
          session)
      (unless project
        (setq session (aidee-new-session aidee-provider root-uri))
        (setq project (make-aidee-project :root root-uri :context nil :provider aidee-provider :session session))
        (puthash root-uri project aidee--projects))
      project)))

(defun aidee-project-refresh-session ()
  (interactive)
  (let* ((root-uri (lspce--root-uri))
         project
         session)
    (when root-uri
      (setq project (gethash root-uri aidee--projects))
      (when project
        (setq session (aidee-new-session aidee-provider root-uri))
        (setf (aidee-project-session project) session)))))

;;;###autoload
(defun aidee-add-project-context ()
  "Add a file to project context."
  (interactive)
  (let ((root-uri (aidee--root-uri))
        filename
        context
        project
        session)
    (setq filename (read-file-name "Add a file to local context: "))
    (when (and filename
               root-uri
               (file-exists-p filename))
      (setq project (aidee--project root-uri))
      (when project
        (setq context (aidee-project-context project))
        (setq context (delete-dups (push filename context)))
        (setf (aidee-project-context project) context)
        (puthash root-uri project aidee--projects)))))

;;;###autoload
(defun aidee-remove-project-context ()
  "Remove a file from project context."
  (interactive)
  (let ((root-uri (aidee--root-uri))
        filename
        context
        project)
    (setq project (aidee--project root-uri))
    (when project
      (setq context (aidee-project-context project))
      (setq filename (completing-read "Remove a file from project context: " context))
      (when (and filename
                 (file-exists-p filename))
        (setf (aidee-project-context project) (delete filename context))))))

;; TODO add more associations
(defvar aidee--language-ids
  '(("py" . "python")
    ("el" . "elisp")))

(defun aidee--file-language-id (filename)
  "Get language id of FILENAME."
  (let (suffix
        language-id)
    (setq suffix (file-name-extension filename))
    (setq language-id (assoc-default suffix aidee--language-ids))
    (unless language-id
      (setq language-id suffix))
    language-id))

(defun aidee--format-file-context (filename content)
  "Format FILENAME and its CONTENT as context in the specified format."
  (let (context
        (language-id (aidee--file-language-id filename)))
    (concat "```\n"
            filename "\n"
            "```\n"
            "```" language-id "\n"
            content "\n"
            "```"
            "\n\n")))

(defun aidee--file-context (&optional automatic-p manual-p project-p)
  "Calculate context of current buffer file."
  (with-current-buffer (current-buffer)
    (let* ((filename (buffer-file-name))
           (root-uri (aidee--root-uri))
           (project (aidee--project root-uri))
           file-skeleton
           deps
           context)
      (when (and filename
                 root-uri)
        (setq deps (list filename))
        (cond
         (automatic-p
          (setq deps (append deps (aidee--retrieve-file-deps filename))))
         (manual-p
          (setq deps (append deps aidee--file-context-manually)))
         ((and project-p
               project)
          (setq deps (append deps (aidee-project-context project)))))
        (setq deps (delete filename (delete-dups deps)))
        (setq context (aidee--format-file-context
                       filename
                       (buffer-substring-no-properties (point-min) (point-max))))
        (cl-dolist (dep deps)
          (setq file-skeleton (aidee--file-skeleton dep))
          (when file-skeleton
            (setq context (concat context
                                  (aidee--format-file-context
                                   (aidee-file-skeleton-filename file-skeleton)
                                   (aidee-file-skeleton-skeleton file-skeleton)))))))
      context)))

(defun aidee--project-context ()
  "Calculate context of current buffer's project."
  (with-current-buffer (current-buffer)
    (let* ((filename (buffer-file-name))
           (root-uri (aidee--root-uri))
           (project (aidee--project root-uri))
           file-skeleton
           deps
           context)
      (when (and filename
                 project)
        (setq deps (delete-dups (aidee-project-context project)))
        (setq context (aidee--format-file-context
                       filename
                       (buffer-substring-no-properties (point-min) (point-max))))
        (cl-dolist (dep deps)
          (setq file-skeleton (aidee--file-skeleton dep))
          (when file-skeleton
            (setq context (concat context
                                  (aidee--format-file-context
                                   (aidee-file-skeleton-filename file-skeleton)
                                   (aidee-file-skeleton-skeleton file-skeleton)))))))
      context)))

;;;; Helping functions
(defun aidee-latest-response ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let* ((root-uri (aidee--root-uri))
           response)
      (when root-uri
        (when-let* ((project (aidee--project root-uri))
                    (session (aidee-project-session project)))
          (setq response (aidee-session-response session))))
      (message "response: %s" response))))

(defun aidee-latest-prompt ()
  (interactive)
  (with-current-buffer (current-buffer)
    (let* ((root-uri (aidee--root-uri))
           prompt)
      (when root-uri
        (when-let* ((project (aidee--project root-uri))
                    (session (aidee-project-session project)))
          (setq prompt (aidee-session-prompt session))))
      (message "prompt: %s" prompt))))

(provide 'aidee-coding)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; aidee-coding.el ends here

