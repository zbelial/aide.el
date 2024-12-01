;;; aidee.el  --- AI powered Development Environtment for Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 zbelial zjyzhaojiyang@gmail.com

;; Author: zbelial zjyzhaojiyang@gmail.com

;; Maintainer: zbelial zjyzhaojiyang@gmail.com

;; Homepage: https://github.com/zbelial/aide.el.git
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
(require 'project)
(require 'ellama)
(require 'cache)

;;;; utils
(defvar ellama--load-path (file-name-directory load-file-name))

;;;; file skeleton

(cl-defstruct ellama-buffer-skeleton
  (bufname)
  (skeleton)
  (timestamp))

(setq ellama--treesit-language-queries
      '((python . (("class_definition" (class_definition body: (_) @context.end) @context)
                   ("function_definition" (function_definition parameters: (_) :anchor ":" @context.end) @context)))
        (go . (("function_declaration" (function_declaration body: (_) @context.end) @context)
               ("method_declaration" (method_declaration body: (_) @context.end) @context)))
        )
      )

(defun ellama--treesit-language-node-types (language)
  (let ((queries (assoc-default language ellama--treesit-language-queries))
        node-types)
    (when queries
      (setq node-types (mapcar 'car queries)))
    node-types))

(defun ellama--treesit-language-node-query (language node-type)
  (let ((queries (assoc-default language ellama--treesit-language-queries))
        result)
    (when queries
      (cl-dolist (query queries)
        (when (string-equal (car query) node-type)
          (setq result (append result (cdr query))))))
    result))

(defvar-local ellama--buffer-skeleton nil
  "Skeleton of current buffer.")

(defvar ellama--buffer-skeletons (make-hash-table :test #'equal)
  "Buffers' skeleton in current project.
Key is file name, value is of type `ellama-buffer-skeleton'.")

(defvar ellama-treesit-suffix-language-map (make-hash-table :test #'equal)
  "Use file name suffix to determine language.")

(defvar ellama-treesit-major-mode-language-map (make-hash-table :test #'equal)
  "Use buffer's major mode (in string form) to determine language.")

(defvar ellama--treesit-queries (make-hash-table :test #'equal)
  "Treesit queries for each language.")

(defun ellama--buffer-treesit-language (&optional buf)
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
      (setq language (or (gethash suffix ellama-treesit-suffix-language-map)
                         (gethash mm ellama-treesit-major-mode-language-map)))
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

(defvar-local ellama--treesit-parser nil
  "treesit parser of the current buffer.")

(defun ellama--ensure-treesit-parser (&optional buf)
  (let ((buf (or buf (current-buffer)))
        language
        parser)
    (with-current-buffer buf
      (setq parser ellama--treesit-parser)
      (unless parser
        (setq language (ellama--buffer-treesit-language))
        (if (and language
                 (treesit-language-available-p language))
            (progn
              (setq parser (treesit-parser-create language))
              (setq-local ellama--treesit-parser parser))
          (message "language: %s without parser available" language))))
    parser))

(defun ellama--treesit-traval-sparse-tree (language sparse-tree)
  )

(defun ellama--retrieve-buffer-skeleton (&optional buf)
  (let* ((buf (or buf (current-buffer)))
         (language (ellama--buffer-treesit-language buf))
         (parser (ellama--ensure-treesit-parser buf))
         (node-types (ellama--treesit-language-node-types language))
         root sparse-tree
         tags)
    (with-current-buffer buf
      (if (and node-types
               parser)
          (progn
            (setq root (treesit-parser-root-node parser))
            (when root
              (setq sparse-tree (treesit-induce-sparse-tree
                                 root
                                 (lambda (node)
                                   (member (treesit-node-type node) node-types))))
              (when sparse-tree
                ))
            sparse-tree)
        (message "parser or node types are nil")))
    )
  )

;;;; repomap


(provide 'aidee)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; aide.el ends here

