(add-to-list 'load-path "/usr/share/emacs/site-lisp/ddskk")
(require 'skk-autoloads)
;; (global-set-key "\C-x\C-j" 'skk-mode)
;; (global-set-key "\C-xj" 'skk-auto-fill-mode)
;; (global-set-key "\C-xt" 'skk-tutorial)
					;
(global-set-key "\C-x\C-t" 'skk-mode)
(global-set-key "\C-xt" 'skk-auto-fill-mode)

(setq skk-kakutei-key "\C-t")
(global-set-key  "\C-t" 'skk-kakutei)

(setq skk-egg-like-new-line t)

