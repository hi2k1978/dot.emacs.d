;; org-modeの初期化
;; load-pathへの追加
;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

(require 'org-install)
;; キーバインドの設定
;(define-key global-map "\C-cl" 'org-store-link)
;(define-key global-map "\C-ca" 'org-agenda)
;(define-key global-map "\C-cr" 'org-remember)
;; 拡張子がorgのファイルを開いた時，自動的にorg-modeにする
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; org-modeでの強調表示を可能にする
(add-hook 'org-mode-hook 'turn-on-font-lock)
;; 見出しの余分な*を消す
;(setq org-hide-leading-stars t)
;; org-default-notes-fileのディレクトリ
(setq org-directory "~/.org/")
;; org-default-notes-fileのファイル名
(setq org-default-notes-file "notes.org")
(setq org-startup-with-inline-images t)
