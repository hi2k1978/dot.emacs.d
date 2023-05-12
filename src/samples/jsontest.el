;;; hoge.el --- Some summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
;;(require 'uuid)
(require 'json)

;; uuid: http://xahlee.info/emacs/emacs/elisp_generate_uuid.html

(defvar json-data)
(defvar json-src-directory "datasets/")
(defvar json-src-file "datasets/json_data.json")
(defvar json-dest-directory "out/")
(defvar json-dest-file "out/hogeout.json")

(file-directory-p json-src-directory)
(defun read-json-from-file (file)
  "Read data from FILE."
  (let ((data))
    (cond ((file-directory-p json-src-directory) (progn
               (setq data (json-read-file file))
               data))
          (t nil))))

(defun write-json-to-file (file data)
  "Write DATA to FILE."
  (let ((buffer "jsontest-buffer"))
    (when (null (file-directory-p json-dest-directory))
      (make-directory json-dest-directory))
    (generate-new-buffer buffer)
    (princ (json-encode-plist data) (get-buffer buffer))
    (with-current-buffer buffer
      (json-pretty-print-buffer-ordered)
      (write-file file)
      )
    (kill-buffer (get-file-buffer file))
    t))

(defun alist-set-value-to-key (data key value)
  "Set VALUE to KEY in DATA."
  (let ()
    (setf (alist-get key data) value)
    (message "%s: %s" key value)
    data))

(catch 'main-return
  (setq json-data (read-json-from-file json-src-file))
  (when (null json-data)
    (throw 'main-return nil))
  (setq  json-data (alist-set-value-to-key json-data 'hoge1 1800))
  (setq  json-data (alist-set-value-to-key json-data 'hoge4 4800))
  (message "json-data: %s" json-data)
  (write-json-to-file json-dest-file json-data))

(provide 'hoge)
;;; hoge.el ends here.

