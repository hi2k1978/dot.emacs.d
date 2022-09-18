;; -*- coding: utf-8 -*-
;;;coding system is utf-8!!!

(require 'package)
;; package-archivesを上書き
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))


(package-initialize)
(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(global-linum-mode)
(global-whitespace-mode)
(global-hl-line-mode)

(auto-image-file-mode t)
(auto-compression-mode t)
(tool-bar-mode t)
(show-paren-mode t)
(setq visible-bell t)
(setq whitespace-style '(face tabs))
(display-time)
(setq visual-line-mode t)
(setq fill-column 60)
(setq-default auto-fill-mode t)
(setq truncate-lines t)
(setq truncate-partial-width-windows t)
(setq auto-image-file-mode t)
(setq next-line-add-newlines t)
(setq-default transient-mark-mode t)

(when window-system (progn
		      (set-frame-parameter nil 'alpha 93) 
		      (set-background-color "Dark Green")
		      (set-foreground-color "LightGray")
		      (set-cursor-color "Gray")
		      ))
;;(load-theme 'tango t)

;;;grobal-set-key
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-c\C-f" 'load-file)
(global-set-key "\C-c\C-g" 'goto-line)


(setq browse-url "chrome")

(require 'init-loader)
(init-loader-load "~/.emacs.d/loader")

