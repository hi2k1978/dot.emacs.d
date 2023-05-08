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
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init))
    :ensure t)

;;
;; BEGIN: ここにいっぱい設定を書く
;;
(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left)))
    :ensure t)

(leaf leaf-manager
  :ensure t)

(leaf macrostep
  :bind
  ("C-c e" . macrostep-expand)
    :ensure t)

(leaf zenburn-theme
  :doc "A low contrast color theme for Emacs."
  :url "http://github.com/bbatsov/zenburn-emacs"
  :added "2022-09-23"
  :ensure t)
;; (load-theme 'tango t)
;; (load-theme 'tango-dark t)
;; (load-theme 'misterioso t)
(load-theme 'zenburn t)

;; emacsclient
(add-hook 'before-make-frame-hook
      #'(lambda ()
          (add-to-list 'default-frame-alist '(alpha . 93))))

;; supress warnings
(leaf warnings
  :custom
  (warning-suppress-types . '((comp))))

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
  (auto-image-file-mode . t)
  (auto-compression-mode . t)
  (auto-fill-mode . nil)
  ; (auto-fill-mode . t)
  (auto-image-file-mode . t)
  (column-number-mode . t)
  (display-time-mode . t)
  (electric-pair-mode . t)
  (fill-column . 120)
  (global-hl-line-mode . t)
  (global-linum-mode . t)
  (global-whitespace-mode . nil)
  (indent-tabs-mode . nil)
  (line-number-mode . t)
  (next-line-add-newlines . t)
  (right-click-context-mode . t)
  (show-paren-mode . t)
  (tool-bar-mode . t)
  (transient-mark-mode . t)
  (truncate-lines . t)
  (truncate-partial-width-windows . t)
  (visible-bell . t)
  (visual-line-mode . t)
  )

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf *minor-mode
  :config
  (leaf posframe
    :when (version<= "26.1" emacs-version)
    :when window-system
    :config
    (leaf ivy-posframe
      :doc "Using posframe to show Ivy"
      :after ivy
      :custom ((ivy-posframe-mode . t)
               (ivy-posframe-height-alist . '((swiper . 30) (t . 40)))
               (ivy-posframe-display-functions-alist
                . '((swiper . nil) (t . ivy-posframe-display-at-frame-center)))
               (ivy-posframe-parameters . '((left-fringe . 10))))
      :ensure t)

    (leaf company-posframe
      :doc "Use a posframe as company candidate menu"
      :after company
      :custom ((company-posframe-mode . t))
      :ensure t)

    (leaf flycheck-posframe
      :after flycheck
      :custom ((flycheck-posframe-mode . t))
      :ensure t)

    (leaf which-key-posframe
      :after which-key
      :custom ((which-key-posframe-mode . t))
      :ensure t)

    (leaf ddskk-posframe
      :doc "Show Henkan tooltip for ddskk via posframe"
      :after skk
      :el-get conao3/ddskk-posframe.el
      :custom ((ddskk-posframe-mode . t)))
    :ensure t)

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

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0.1))
  :global-minor-mode show-paren-mode)

(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :custom ((kill-ring-max . 100)
           (kill-read-only-ok . t)
           (kill-whole-line . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil)))

(leaf files
  :doc "file input and output commands for Emacs"
  :tag "builtin"
  :custom `((auto-save-timeout . 15)
            (auto-save-interval . 60)
            (auto-save-file-name-transforms . '((".*" ,(locate-user-emacs-file "backup/") t)))
            (backup-directory-alist . '((".*" . ,(locate-user-emacs-file "backup"))
                                        (,tramp-file-name-regexp . nil)))
            (version-control . t)
            (delete-old-versions . t)))

(leaf startup
  :doc "process Emacs shell arguments"
  :tag "builtin" "internal"
  :custom `((auto-save-list-file-prefix . ,(locate-user-emacs-file "backup/.saves-"))))

(leaf windmove
  :doc "directional window-selection routines"
  :tag "builtin"
  :added "2022-09-25"
  :config
  (windmove-default-keybindings 'meta))

(leaf beacon
  ;; :diminish beacon-mode
  :require t
  :config
  (beacon-mode 1)
  :ensure t)

;; (leaf typescript-mode
;;   :custom
;;   (typescript-indent-level . 2)
;;   :ensure t)

(leaf rainbow-mode
  :leaf-defer t
  :hook
  (web-mode-hook . rainbow-mode)
  :ensure t)

(leaf rainbow-delimiters
  :leaf-defer t
  :hook
  (prog-mode-hook . rainbow-delimiters-mode)
  :ensure t)

(leaf highlight-indentation
    :doc "Minor modes for highlighting indentation"
    :url "https://github.com/antonj/Highlight-Indentation-for-Emacs"
    :added "2023-03-04"
    :ensure t)

(leaf right-click-context
  :doc "Right Click Context menu"
  :req "emacs-24.3" "popup-0.5" "ordinal-0.0.1"
  :tag "rightclick" "menu" "mouse" "emacs>=24.3"
  :url "https://github.com/zonuexe/right-click-context"
  :added "2023-03-10"
  :emacs>= 24.3
  :after ordinal
  :ensure t)

(leaf fontawesome
  :ensure t)

(leaf codic
  :leaf-defer t
  :ensure t)


(leaf highlight-indent-guides
  :require t
  ;; :diminish highlight-indent-guides-mode
  :custom
  (highlight-indent-guides-method . 'character)
  (highlight-indent-guides-auto-character-face-perc . 20)
  (highlight-indent-guides-character . ?\|)
  :hook
  (prog-mode-hook . highlight-indent-guides-mode)
  :ensure t)

(leaf google-this
  :doc "A set of functions and bindings to google under point."
  :req "emacs-24.1"
  :tag "hypermedia" "convenience" "emacs>=24.1"
  :url "http://github.com/Malabarba/emacs-google-this"
  :added "2022-09-23"
  :emacs>= 24.1
  :bind ("C-c C-g" . google-this)
  :ensure t)

(leaf neotree
  :bind ("C-c t" . neotree-toggle)
  :ensure t)

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
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
    :bind
    ("C-s" . swiper)
    :ensure t)

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :blackout t
    :bind
    ("C-S-s" . counsel-imenu)
    ("C-x C-r" . counsel-recentf)
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t
    :ensure t)
  :ensure t)

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode
  :ensure t)

(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t
  :ensure t)

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :bind
  ("M-n" . flycheck-next-error)
  ("M-p" . flycheck-previous-error)
  :global-minor-mode global-flycheck-mode
  :ensure t)


(leaf flymake
  :doc "A universal on-the-fly syntax checker"
  :tag "builtin"
  :added "2022-09-22"
  :ensure nil)

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
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
  :global-minor-mode global-company-mode
  :ensure t)

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2020-03-25"
  :emacs>= 24.1
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers)
  :ensure t)

(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-26.1" "dash-2.18.0" "f-0.20.0" "ht-2.3" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=26.1"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :added "2022-09-23"
  :emacs>= 26.1
  ;; :hook
  ;; (prog-major-mode . lsp-prog-major-mode-enable)
  ;; :after spinner markdown-mode lv
  :ensure t)


(leaf ddskk
  :bind
  ("C-x C-t" . skk-mode)
  :setq
  (skk-preload . t)
  (default-input-method . "japanese-skk")
  :ensure t)

(leaf magit
  :doc "A Git porcelain inside Emacs."
  :req "emacs-25.1" "compat-28.1.1.2" "dash-20210826" "git-commit-20220222" "magit-section-20220325" "transient-20220325" "with-editor-20220318"
  :tag "vc" "tools" "git" "emacs>=25.1"
  :url "https://github.com/magit/magit"
  :added "2022-09-21"
  :emacs>= 25.1
  :after compat git-commit magit-section with-editor
  :ensure t)

(leaf prettier
  :doc "Code formatting with Prettier"
  :req "emacs-26.1" "iter2-0.9" "nvm-0.2" "editorconfig-0.8"
  :tag "files" "languages" "convenience" "emacs>=26.1"
  :url "https://github.com/jscheid/prettier.el"
  :added "2022-09-22"
  :emacs>= 26.1
  :hook
  (web-mode-hook . prettier-mode)
  :ensure t)

(leaf cmake-mode
  :doc "major-mode for editing CMake sources"
  :req "emacs-24.1"
  :tag "emacs>=24.1"
  :added "2023-01-02"
  :emacs>= 24.1
  :ensure t)

(leaf web-mode
  :doc "major mode for editing web templates"
  :req "emacs-23.1"
  :tag "languages" "emacs>=23.1"
  :url "https://web-mode.org"
  :added "2022-09-22"
  :emacs>= 23.1
  :mode
  ("\\.js[x]?$" . web-mode)
  ("\\.ts[x]?$" . web-mode)
  ("\\.vue$" . web-mode)
  :ensure t)

(leaf csharp-mode
  :doc "C# mode derived mode"
  :req "emacs-26.1"
  :tag "mode" "oop" "languages" "c#" "emacs>=26.1"
  :url "https://github.com/emacs-csharp/csharp-mode"
  :added "2022-09-24"
  :emacs>= 26.1
  :ensure t)

;; (leaf lsp-csharp
;;   :doc "description"
;;   :tag "out-of-MELPA"
;;   :added "2022-09-24"
;;   :el-get {{user}/lsp-csharp
;;   :require t)

(leaf go-mode
  :doc "Major mode for the Go programming language"
  :req "emacs-26.1"
  :tag "go" "languages" "emacs>=26.1"
  :url "https://github.com/dominikh/go-mode.el"
  :added "2022-09-24"
  :emacs>= 26.1
  :init
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq tab-width 4)
  :ensure t)

(leaf dockerfile-mode
  :doc "Major mode for editing Docker's Dockerfiles"
  :req "emacs-24"
  :tag "tools" "processes" "languages" "docker" "emacs>=24"
  :url "https://github.com/spotify/dockerfile-mode"
  :added "2022-12-25"
  :emacs>= 24
  :ensure t)

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

(leaf mermaid-mode
  :doc "major mode for working with mermaid graphs"
  :req "f-0.20.0" "emacs-25.3"
  :tag "processes" "tools" "graphs" "mermaid" "emacs>=25.3"
  :url "https://github.com/abrochard/mermaid-mode"
  :added "2022-11-23"
  :emacs>= 25.3
  :ensure t)


(leaf plantuml-mode
  :doc "Major mode for PlantUML"
  :req "dash-2.0.0" "emacs-25.0"
  :tag "ascii" "plantuml" "uml" "emacs>=25.0"
  :added "2022-09-23"
  :emacs>= 25.0
  :custom
  (plantuml-output-type . "png")
  (plantuml-jar-path . "/usr/share/plantuml/plantuml.jar")
  (plantuml-default-exec-mode . 'jar)
  ;;(plantuml-executable-path . "/usr/local/bin/plantuml")
  ;;(plantuml-default-exec-mode . 'executable)
  :ensure t)

(leaf csv-mode
  :doc "Major mode for editing comma/char separated values"
  :req "emacs-27.1" "cl-lib-0.5"
  :tag "convenience" "emacs>=27.1"
  :url "https://elpa.gnu.org/packages/csv-mode.html"
  :added "2022-11-23"
  :emacs>= 27.1
  :after
  (csv-align-fields)
  :ensure t)

(leaf w3m
  :doc "an Emacs interface to w3m"
  :tag "hypermedia" "www" "w3m"
  :added "2022-09-23"
  :ensure t)

(leaf vlc
  :doc "VideoLAN VLC Media Player Control"
  :req "emacs-25.1"
  :tag "tools" "emacs>=25.1"
  :url "https://github.com/xuchunyang/vlc.el"
  :added "2022-09-25"
  :emacs>= 25.1
  :ensure t)
;;
;; END: ここにいっぱい設定を書く
;;

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here

