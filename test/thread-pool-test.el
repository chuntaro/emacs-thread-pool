;;; thread-pool-test.el --- Test for Thread Pool     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  chuntaro

;; Author: chuntaro <chuntaro@sakura-games.jp>

;; This program is free software; you can redistribute it and/or modify
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

;; Test code ported from `PThreads Programming'.
;; https://resources.oreilly.com/examples/9781565921153/
;; The result is the almost same as the C language version.

;; To run the test, press C-x C-e at the end of the following Lisp code.
;; (The new Emacs will start and run the test.)

;; (start-process "emacs" nil (file-truename (expand-file-name invocation-name invocation-directory)) "-Q" "-L" "../" "-l" "thread-pool-test.el" "-f" "tpool-test-main1")

;; (start-process "emacs" nil (file-truename (expand-file-name invocation-name invocation-directory)) "-Q" "-L" "../" "-l" "thread-pool-test.el" "-f" "tpool-test-main2")

;; Or, run the following command.

;; ~/thread-pool/test$ emacs -Q -L ../ -l thread-pool-test.el -f tpool-test-main1

;; ~/thread-pool/test$ emacs -Q -L ../ -l thread-pool-test.el -f tpool-test-main2

;;; Code:

(require 'thread-pool)

(defconst tpool-test-s1 ["STRING 0"
                         "STRING 1"
                         "STRING 2"
                         "STRING 3"
                         "STRING 4"
                         "STRING 5"
                         "STRING 6"
                         "STRING 7"
                         "STRING 8"
                         "STRING 9"
                         "STRING 10"
                         "STRING 11"
                         "STRING 12"
                         "STRING 13"
                         "STRING 14"
                         "STRING 15"
                         "STRING 16"
                         "STRING 17"
                         "STRING 18"
                         "STRING 19"])

(defun tpool-test-r1 (printstring)
  (let ((x 0))
    (princ (format "%s START\n" printstring))
    (dotimes (i 1000000)
      (cl-incf x i)
      (thread-yield))
    (princ (format "%s DONE\n" printstring))))

(defun tpool-test-main1 ()
  (let ((test-pool (tpool-init 10 20)))
    (sleep-for 5)
    (dotimes (i 5)
      (princ (format "tpool-add-work returned %s\n"
                     (tpool-add-work test-pool #'tpool-test-r1 (aref tpool-test-s1 i))))
      (sleep-for 1))
    (princ (format "main: all work queued\n"))
    (tpool-destroy test-pool t)
    (switch-to-buffer "*Messages*")))

;; Test Code Using the 国立国会図書館(National Diet Library, Japan) Search API.
;; (not part of `PThreads Programming')

(require 'url-http)
(require 'xml)
(require 'dom)

(defun tpool-test-get-text-first-tag (xml tag)
  "XML 内の TAG にマッチした最初のテキストを返す。"
  (decode-coding-string (dom-text (car (dom-by-tag xml tag)))
                        'utf-8))

(defun tpool-test-url-retrieve-synchronously (keyword)
  (with-current-buffer
      (url-retrieve-synchronously (concat "http://iss.ndl.go.jp/api/opensearch?title="
                                          (url-encode-url keyword)))
    (when (url-http-parse-headers)
      (search-forward-regexp "\n\\s-*\n" nil t)
      (tpool-test-get-text-first-tag (xml-parse-region) 'dc:title))))

(defun tpool-test-r2 (keyword)
  (with-current-buffer (get-buffer-create "*tpool-test*")
    (insert (print (tpool-test-url-retrieve-synchronously keyword)))))

(defun tpool-test-main2 ()
  (let ((test-pool (tpool-init 10 20)))
    (sleep-for 1)
    (dolist (keyword '("東京" "埼玉" "神奈川" "千葉" "茨城" "栃木" "群馬"))
      (princ (format "tpool-add-work returned %s\n"
                     (tpool-add-work test-pool #'tpool-test-r2 keyword)))
      (sleep-for 0.5))
    (princ (format "main: all work queued\n"))
    (tpool-destroy test-pool t)
    (switch-to-buffer "*tpool-test*")))

(provide 'thread-pool-test)
;;; thread-pool-test.el ends here
