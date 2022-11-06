;;; vlc.el --- Vlc control in emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;

(require 'cl-lib)
;;; Code:
(defgroup vlc nil
  "Vlc control in Emacs."
  :group 'lisp
  :prefix "vlc-")

(defcustom vlc-media-file-alist ()
  "MEDIA FILES."
  :type 'list)

(defcustom vlc-prefs-repeat nil
  "Preferences: repeat or not."
  :type 'boolean)

(defcustom vlc-prefs-shuffle nil
  "Preferences: shuffle or not."
  :type 'boolean)

(defcustom vlc-prefs-volume 50
  "Preferences: volume."
  :type 'number)

(defcustom vlc-prefs-search-max-depth 3
  "Preferences: max depth of searching video or sound files."
  :type 'number)

;; AAC, OGG, MP3, OPUS, WAV, 3GP, FLV, MP4, WEBM
(defcustom vlc-media-file-extensions "\\(\\.aac\\|\\.ogg\\|\\.mp3\\|\\.wav\\|\\.flv\\|\\.mp4\\)$"
  "File extensions that can be allowed to read in vlc mode."
  :type 'string)

(defcustom vlc-ignore-dirs "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"
  "Ignore directories of current(.) and parent(..)."
  :type 'string)

(defcustom vlc-current-directory "."
  "Current directory"
  :type 'string)


(cl-defun vlc ()
  "Vlc Mode."
  (interactive)
  (message "vlc-mode"))

(cl-defun set-vlc-value-or-empty-string (val)
  "Return VAL when VAL is not Nil.  Else return ''(empty string)."
  (format "%s" (or val "")))

(cl-defun vlc-media-file-data-alist (&key file artist album title &allow-other-keys)
  "Record data is created from FILE, ARTIST, ALBUM, TITLE."
  (list (cons 'file (set-vlc-value-or-empty-string file))
	(cons 'artist (set-vlc-value-or-empty-string artist))
	(cons 'album (set-vlc-value-or-empty-string album))
	(cons 'title (set-vlc-value-or-empty-string title))))


(cl-defun vlc-get-media-files-from-directory (directory depth)
  "Read music files in DIRECTORY recursively.  maxdepth is DEPTH."
  (let (results (candidates (directory-files directory t vlc-ignore-dirs)))
    (cond ((zerop depth) nil)
	  (t (dolist (target candidates)
	       (when (file-directory-p target)
		 (setq results (vlc-get-media-files-from-directory target (1- depth))))
	       (when (file-readable-p target)
		 (when (string-match vlc-media-file-extensions target)
		   (setq results (append results (list target))))))
	     results))))

(cl-defun vlc-open-media-file (file)
  "Open FILE to read id3tag info."
  (let (artist album title)))


(cl-defun vlc-open-local-files ()
  "Open LOCAL FILES."
  (let (media-files)
    (setq media-files (vlc-get-media-files-from-directory vlc-current-directory vlc-prefs-search-max-depth))
    (dolist (target media-files)
      (setq media-info (shell-command-to-string
       (mapconcat #'shell-quote-argument
		  (list "id3v2" "-R" target)
		  " ")))
      (dolist (target (cdr (split-string media-info "\n")))
	(prin1 target))
      ;(princ (split-string result ":\n"))
      (princ (cdr (split-string media-info ":\n")))
    )))
(vlc-open-local-files)

(vlc-media-file-data-alist :file "file1" :artist "higedan" :album "hige" :title "dan" :year "2002")
(assoc-default 'artist (vlc-media-file-data-alist :file "file1" :artist "higedan" :album "hige" :title "dan"))
(vlc-media-file-data-alist :artist "king-gnu" :title "gnu")



;;; vlc.el ends here
