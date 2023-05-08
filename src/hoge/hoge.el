;;; hoge.el --- Some summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'json)
(defvar json-data)
(defvar json-src-file "hoge.json")
(defvar json-dest-file "hogeout.json")

(defun read-json-from-file (file)
  "Read data from FILE."
  (let ((data))
    (setq data (json-read-file file))
    data))

(defun write-json-to-file (file data)
  "Write DATA to FILE."
  (let ((buffer "hoge-buffer"))
    (generate-new-buffer buffer)
    (princ (json-encode-plist data) (get-buffer buffer))
    (with-current-buffer buffer
      (json-pretty-print-buffer)
      (write-file file))
    t))

(defun set-value-to-key (data key value)
  "Set VALUE to KEY in DATA."
  (let ()
    (setq data (delq (assq key data) data))
    (add-to-list 'data (cons key value))
    (message "set-value-to-key: %s" data)
    data))

(setq json-data (read-json-from-file json-src-file))
(setq  json-data (set-value-to-key json-data 'hoge1 1660))
(message "main: %s" json-data)
(write-json-to-file json-dest-file json-data)

(provide 'hoge)
;;; hoge.el ends here.

