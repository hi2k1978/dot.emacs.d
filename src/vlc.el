;;; vlc.el --- Vlc control in emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;

(require 'cl-lib)
;;; Code:
(defgroup vlc nil
  "Vlc control in Emacs."
  :group 'lisp
  :prefix "vlc-")

(defvar vlc-prefs-repeat nil) ;; nil t
(defvar vlc-prefs-shuffle nil) ;; nil o 1
(defvar vlc-prefs-volume 50) ;; 0 - 100
(defvar vlc-max-depth 3)
(defvar vlc-sound-file-alist)
(defvar vlc-ignore-dirs "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")

(cl-defun vlc ()
  "Vlc Mode."
  (interactive)
  (message "vlc-mode"))

(cl-defun vlc-value-or-empty-string (val)
  "Return VAL when VAL is not Nil.  Else return ''(empty string)."
  (format "%s" (or val "")))

(cl-defun vlc-sound-file-data-alist (&key file artist album title &allow-other-keys)
  "Record data is created from FILE, ARTIST, ALBUM, TITLE."
  (list (cons 'file (vlc-value-or-empty-string file))
	(cons 'artist (vlc-value-or-empty-string artist))
	(cons 'album (vlc-value-or-empty-string album))
	(cons 'title (vlc-value-or-empty-string title))))

(vlc-sound-file-data-alist :file "file1" :artist "higedan" :album "hige" :title "dan" :year "2002")
(assoc-default 'artist (vlc-sound-file-data-alist :file "file1" :artist "higedan" :album "hige" :title "dan"))
(vlc-sound-file-data-alist :artist "king-gnu" :title "gnu")

(cl-defun vlc-get-sound-files-from-directory (directory depth)
  "Read music files in DIRECTORY recursively.  maxdepth is DEPTH."
  (let (results (candidates (directory-files directory t vlc-ignore-dirs)))
    (cond ((zerop depth) nil)
	  (t (dolist (target candidates)
	       (when (file-directory-p target)
		 (setq results (vlc-get-sound-files-from-directory target (1- depth))))
	       (when (file-readable-p target)
		 (setq results (append results (list target)))))
	     results))))

(vlc-get-sound-files-from-directory "." vlc-max-depth)
(vlc-get-sound-files-from-directory "." 1)

(puthash 0
	 (vlc-set-one-record-data :artist "higedan" :album "hige" :title "dan")
	 vlc-data)
(puthash 1
	 (vlc-set-one-record-data :artist "king-gnu" :title "gnu")
	 vlc-data)
vlc-data
(gethash "artist" (gethash 0 vlc-data))




