;;; eureka-utils.el  --- AI powered Development Environment for Emacs. -*- lexical-binding: t; -*-

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


;;;; Other routines
(defconst eureka--uri-path-allowed-chars
  (let ((vec (copy-sequence url-path-allowed-chars)))
    (aset vec ?: nil)
    vec)
  "Like `url-path-allows-chars' but more restrictive.")

(defun eureka--path-to-uri (path)
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
                   eureka--uri-path-allowed-chars)))
      (concat "file://"
              (url-hexify-string
               ;; Again watch out for trampy paths.
               (directory-file-name (file-local-name truepath))
               eureka--uri-path-allowed-chars)))))

(defun eureka--uri-to-path (uri)
  "Convert URI to a file path."
  (when (keywordp uri)
    (setq uri (substring (symbol-name uri) 1)))
  (let ((retval (url-unhex-string (url-filename (url-generic-parse-url uri)))))
    (if (eq system-type 'windows-nt)
        (substring retval 1)
      retval)))

(defun eureka--project-root ()
  (when-let* ((proj (project-current))
              (root (project-root proj)))
    root))

(defmacro eureka-with-file-open-temporarily (file close-after-body &rest body)
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

(defun eureka--current-timestamp ()
  (time-convert (current-time) 'integer))

(provide 'eureka-utils)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; eureka-utils.el ends here
