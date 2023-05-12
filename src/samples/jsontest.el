;;; hoge.el --- Some summary -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
;;(require 'uuid)
(require 'json)

;; uuid: http://xahlee.info/emacs/emacs/elisp_generate_uuid.html

(defvar json-src-directory "datasets/")
(defvar json-src-file "datasets/json_data.json")
(defvar json-dest-directory "out/")
(defvar json-dest-file "out/hogeout.json")

(defun read-data-from-json-file (file)
  "Read data from FILE."
  (let ((data))
    (cond ((file-directory-p json-src-directory)
           (progn
             (setq data (json-read-file file))
             data))
          (t nil))))

(defun write-data-to-json-file (file data)
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

(defun jsontest-main ()
  "Main."
  (let ((json-data))
    (catch 'main-return
      (setq json-data (read-data-from-json-file json-src-file))
      (when (null json-data)
        (throw 'main-return nil))
      (setq  json-data (alist-set-value-to-key json-data 'hoge1 1800))
      (setq  json-data (alist-set-value-to-key json-data 'hoge4 4800))
      (message "json-data: %s" json-data)
      (write-data-to-json-file json-dest-file json-data))))

(jsontest-main)

(provide 'jsontest)

;;; jsontest.el ends here.
