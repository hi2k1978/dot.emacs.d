(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts[x]?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))


(add-hook 'web-mode-hook 
	  (lambda ()
	    
	    (prettier-mode)

	    ;; インデント設定
	    (setq web-mode-markup-indent-offset 2)
	    (setq web-mode-css-indent-offset 2)
	    (setq web-mode-code-indent-offset 2)
	    
	    ;; 要素のハイライト
	    (setq web-mode-enable-current-element-highlight t)
	    
	    ;; フォントの配色
	    (set-face-attribute 'web-mode-doctype-face nil :foreground "Pink3")
	    (set-face-attribute 'web-mode-html-tag-face nil :foreground "Green")
	    (set-face-attribute 'web-mode-html-attr-value-face nil :foreground "Yellow")
	    (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#0FF")
	    
	    ;; タグを自動で閉じる
	    (setq web-mode-enable-auto-pairing t)
	    (setq web-mode-enable-auto-closing t)
	    )
	  )
;; (use-package flymake-eslint
;;   :config
;;   (add-hook 'web-mode-hook
;;             (lambda ()
;;               (flymake-eslint-enable))))

;; (require 'prettier-js)
;; (use-package prettier
;;   :config
;;   (add-hook 'web-mode-hook
;;             (lambda ()
;;               (prettier-mode))))

;; (use-package web-mode
;;   :mode
;;   (("\\.js\\'" . web-mode)
;;    ("\\.vue\\'" . web-mode)
;;    ("\\.ts\\'" . web-mode))
;;   :custom
;;   (web-mode-markup-indent-offset 2)
;;   (web-mode-css-indent-offset 2)
;;   (web-mode-code-indent-offset 2)
;;   (web-mode-part-padding 0)
;;   )

;; (require 'prettier-js)
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.js[x]?$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts[x]?$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))

;; (defun my-web-mode-hook ()
;;   "Hooks for Web mode."
  
;;   ;; インデント設定
;;   (setq web-mode-markup-indent-offset 4)
;;   (setq web-mode-css-indent-offset 4)
;;   (setq web-mode-code-indent-offset 4)
  
;;   ;; 要素のハイライト
;;   (setq web-mode-enable-current-element-highlight t)
  
;;   ;; フォントの配色
;;   (set-face-attribute 'web-mode-doctype-face nil :foreground "Pink3")
;;   (set-face-attribute 'web-mode-html-tag-face nil :foreground "Green")
;;   (set-face-attribute 'web-mode-html-attr-value-face nil :foreground "Yellow")
;;   (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#0FF")
  
;;   ;; タグを自動で閉じる
;;   (setq web-mode-enable-auto-pairing t)
;;   (setq web-mode-enable-auto-closing t)
;;   )
;; (add-hook 'web-mode-hook 'my-web-mode-hook)
