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
(require 'ellama)

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

