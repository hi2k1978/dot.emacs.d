;;; vlc.el --- Vlc control in emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:
(defgroup vlc nil
  "Vlc control in Emacs."
  :group 'lisp
  :prefix "vlc-")

(defvar vlc-repeat nil) ;; nil t
(defvar vlc-shuffle nil) ;; nil o 1
(defvar vlc-volume 50) ;; 0 - 100
(defvar vlc-max-depth 3)
(defvar vlc-data (make-hash-table :test 'equal))

(defun vlc ()
  "Vlc Mode."
  (interactive)
  (message "vlc-mode"))

(defun vlc-value-or-empty-string (val)
  "Return VAL when VAL is not Nil.  Else return ''(empty string)."
  (format "%s" (or val "")))

(defun vlc-set-one-record-data (&rest args)
  "Record data is created from ARTIST, ALBUM, TITLE, ARGS."
  (defvar vlc-one-record-hash (make-hash-table :test 'equal))
  (puthash "artist" (vlc-value-or-empty-string (plist-get args :artist)) vlc-one-record-hash)
  (puthash "album" (vlc-value-or-empty-string (plist-get args :album)) vlc-one-record-hash)
  (puthash "title" (vlc-value-or-empty-string (plist-get args :title)) vlc-one-record-hash)
  (puthash "year" (vlc-value-or-empty-string (plist-get args :year)) vlc-one-record-hash)
  vlc-one-record-hash)

(vlc-set-one-record-data :artist "higedan" :album "hige" :title "dan")
(vlc-set-one-record-data :artist "king-gnu" :title "gnu")

(defun vlc-get-sound-files-from-directory (directory depth)
  "Read music files in directory DIR, DEPTH."
  (let (sound-files
	(candidates (directory-files directory t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))
    (dolist (target candidates)
      (when (and (> depth 0) (file-directory-p target))
	(setq sound-files (vlc-get-sound-files-from-directory target (1- depth))))
      (when (file-readable-p target)
	(setq sound-files (append sound-files (list target)))))
    sound-files))

(vlc-get-sound-files-from-directory "." vlc-max-depth)

(puthash 0
	 (vlc-set-one-record-data :artist "higedan" :album "hige" :title "dan")
	 vlc-data)
(puthash 1
	 (vlc-set-one-record-data :artist "king-gnu" :title "gnu")
	 vlc-data)
vlc-data
(gethash "artist" (gethash 0 vlc-data))




