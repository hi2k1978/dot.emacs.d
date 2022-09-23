;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Naoya Yamashita

;; Author: Naoya Yamashita <conao3@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/{{pkg}}/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;;
;; BEGIN: ここにいっぱい設定を書く
;;
(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf leaf-manager
  :ensure t)

(leaf macrostep
  :ensure t
  :bind
  ("C-c e" . macrostep-expand))

(leaf zenburn-theme
  :doc "A low contrast color theme for Emacs."
  :url "http://github.com/bbatsov/zenburn-emacs"
  :added "2022-09-23"
  :ensure t)
;; (load-theme 'tango t)
;; (load-theme 'tango-dark t)
;;(load-theme 'misterioso t)              
(load-theme 'zenburn t)

;; emacsclient
(add-hook 'before-make-frame-hook
      #'(lambda ()
          (add-to-list 'default-frame-alist '(alpha . 93))))

;; !emacsclient
(leaf cus-start
  :when window-system
  :config
  (set-frame-parameter nil 'alpha 93))

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))
  :bind
  ("C-M-c" . comment-or-uncomment-region)
  ("C-c C-f" . load-file)
  ("C-h" . delete-backward-char)
  ("M-h" . backward-kill-word)
  ("M-ESC ESC" . c/redraw-frame)
  :custom
  (display-time-mode . t)
  (visible-bell . t)
  (visual-line-mode . t)
  (fill-column . 120)
  (truncate-lines . t)
  (truncate-partial-width-windows . t)
  (auto-image-file-mode . t)
  (next-line-add-newlines . t)
  (auto-fill-mode . t)
  (transient-mark-mode . t)
  (global-linum-mode . t)
  (global-whitespace-mode . nil)
  (global-hl-line-mode . t)
  (auto-image-file-mode . t)
  (auto-compression-mode . t)
  (auto-fill-mode . nil)
  (tool-bar-mode . t)
  (show-paren-mode . t)
  )

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf *minor-mode
  :config
  (leaf posframe
    :ensure t
    :when (version<= "26.1" emacs-version)
    :when window-system
    :config
    (leaf ivy-posframe
      :doc "Using posframe to show Ivy"
      :after ivy
      :ensure t
      :custom ((ivy-posframe-mode . t)
               (ivy-posframe-height-alist . '((swiper . 30) (t . 40)))
               (ivy-posframe-display-functions-alist
                . '((swiper . nil) (t . ivy-posframe-display-at-frame-center)))
               (ivy-posframe-parameters . '((left-fringe . 10)))))

    (leaf company-posframe
      :doc "Use a posframe as company candidate menu"
      :ensure t
      :after company
      :custom ((company-posframe-mode . t)))

    (leaf flycheck-posframe
      :ensure t
      :after flycheck
      :custom ((flycheck-posframe-mode . t)))

    (leaf which-key-posframe
      :ensure t
      :after which-key
      :custom ((which-key-posframe-mode . t)))

    (leaf ddskk-posframe
      :doc "Show Henkan tooltip for ddskk via posframe"
      :after skk
      :el-get conao3/ddskk-posframe.el
      :custom ((ddskk-posframe-mode . t))))

  ;; other minor-mode packages...
  )

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

(leaf cc-mode
  :doc "major mode for editing C and similar languages"
  :tag "builtin"
  :defvar (c-basic-offset)
  :bind
  ("C-c c" . compile)
  :mode-hook
  (c-mode-hook . ((c-set-style "bsd") (setq c-basic-offset 4)))
  (c++-mode-hook . ((c-set-style "bsd") (setq c-basic-offset 4))))

;; (leaf term
;;   :bind
;;   ("C-x o" . other-window))


(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

;; (leaf dimmer
;;   :doc "Visually highlight the selected buffer"
;;   :req "emacs-25.1"
;;   :tag "editing" "faces" "emacs>=25.1"
;;   :url "https://github.com/gonewest818/dimmer.el"
;;   :added "2022-09-23"
;;   :emacs>= 25.1
;;   :ensure t)

(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :custom ((kill-ring-max . 100)
           (kill-read-only-ok . t)
           (kill-whole-line . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil)))

(leaf beacon
  :ensure t
  ;; :diminish beacon-mode
  :require t
  :config
  (beacon-mode 1))

;; (leaf typescript-mode
;;   :ensure t
;;   :custom
;;   (typescript-indent-level . 2)
;;   )

(leaf rainbow-mode
  :ensure t
  :leaf-defer t
  :hook
  (web-mode-hook . rainbow-mode))

(leaf rainbow-delimiters
  :ensure t
  :leaf-defer t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(leaf fontawesome
  :ensure t)

(leaf codic
  :ensure t
  :leaf-defer t)


(leaf highlight-indent-guides
  :ensure t
  :require t
  ;; :diminish highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-method . 'character)
  (highlight-indent-guides-auto-character-face-perc . 20)
  (highlight-indent-guides-character . ?\|)
  :hook
  (prog-mode-hook . highlight-indent-guides-mode))

(leaf google-this
  :doc "A set of functions and bindings to google under point."
  :req "emacs-24.1"
  :tag "hypermedia" "convenience" "emacs>=24.1"
  :url "http://github.com/Malabarba/emacs-google-this"
  :added "2022-09-23"
  :emacs>= 24.1
  :ensure t
  :bind ("C-c C-g" . google-this))

(leaf neotree
  :ensure t
  :bind ("C-c C-t" . neotree-toggle))

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom
  (ivy-initial-inputs-alist . nil)
  (ivy-use-selectable-prompt . t)
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind
    ("C-s" . swiper))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :bind
    ("C-S-s" . counsel-imenu)
    ("C-x C-r" . counsel-recentf)
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode)

(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind
  ("M-n" . flycheck-next-error)
  ("M-p" . flycheck-previous-error)
  :global-minor-mode global-flycheck-mode)


;; (leaf flymake
;;   :doc "A universal on-the-fly syntax checker"
;;   :tag "builtin"
;;   :added "2022-09-22"
;;   :ensure t)

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2020-03-25"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers))

(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-26.1" "dash-2.18.0" "f-0.20.0" "ht-2.3" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=26.1"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :added "2022-09-23"
  :emacs>= 26.1
  :ensure t
  :after spinner markdown-mode lv)


(leaf ddskk
  :ensure t
  :bind
  ("C-x C-t" . skk-mode)
  :setq
  (skk-preload . t)
  (default-input-method . "japanese-skk"))

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "compat-28.1.1.2" "dash-20210826" "git-commit-20220222" "magit-section-20220325" "transient-20220325" "with-editor-20220318"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :url "https://github.com/magit/magit"
  :added "2022-09-21"
  :emacs>= 25.1
  :ensure t
  :after compat git-commit magit-section with-editor)

(leaf prettier
  :doc "Code formatting with Prettier"
  :req "emacs-26.1" "iter2-0.9" "nvm-0.2" "editorconfig-0.8"
  :tag "files" "languages" "convenience" "emacs>=26.1"
  :url "https://github.com/jscheid/prettier.el"
  :added "2022-09-22"
  :emacs>= 26.1
  :ensure t
  :hook
  (web-mode-hook . prettier-mode))

(leaf web-mode
  :doc "major mode for editing web templates"
  :req "emacs-23.1"
  :tag "languages" "emacs>=23.1"
  :url "https://web-mode.org"
  :added "2022-09-22"
  :emacs>= 23.1
  :ensure t
  :mode (("\\.js[x]?$" . web-mode)
         ("\\.ts[x]?$" . web-mode)
         ("\\.vue$" . web-mode)))

(leaf yaml-mode
  :doc "Major mode for editing YAML files"
  :req "emacs-24.1"
  :tag "yaml" "data" "emacs>=24.1"
  :url "https://github.com/yoshiki/yaml-mode"
  :added "2022-09-23"
  :emacs>= 24.1
  :ensure t)

(leaf markdown-mode
  :doc "Major mode for Markdown-formatted text"
  :req "emacs-26.1"
  :tag "itex" "github flavored markdown" "markdown" "emacs>=26.1"
  :url "https://jblevins.org/projects/markdown-mode/"
  :added "2022-09-23"
  :emacs>= 26.1
  :ensure t)

(leaf plantuml-mode
  :doc "Major mode for PlantUML"
  :req "dash-2.0.0" "emacs-25.0"
  :tag "ascii" "plantuml" "uml" "emacs>=25.0"
  :added "2022-09-23"
  :emacs>= 25.0
  :ensure t
  :custom
  (plantuml-jar-path . "/usr/share/plantuml/plantuml.jar")
  (plantuml-exec-mode quote jar))

(leaf w3m
  :doc "an Emacs interface to w3m"
  :tag "hypermedia" "www" "w3m"
  :added "2022-09-23"
  :ensure t)

;;
;; END: ここにいっぱい設定を書く
;;

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here

