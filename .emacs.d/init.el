;; Turn off menubar, toolbar, scollbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Load `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'diminish)

(setq use-package-always-ensure t)

;; Vim mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1))
;; travel windows by arrow  keys
(windmove-default-keybindings)


;; Load editor theme
(use-package gruvbox-theme
  :config (load-theme 'gruvbox t))

;; Display line number when programming
(add-hook 'prog-mode-hook 'linum-mode)
;; (global-linum-mode t)
;;(setq linum-format "%4d \u2502")

(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-current-symbol "")
  (setq linum-relative-format " %4s ")
  (linum-relative-on)
 )
;; (require 'linum-relative)

(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(global-visual-line-mode t)
;;show parent mode for () {}
(show-paren-mode 1)
;;indent setup
(defun my-setup-indent (n)
  ;; java/c/c++
  (setq-local c-basic-offset n)
  ;; web development
  (setq-local coffee-tab-width n) ; coffeescript
  (setq-local javascript-indent-level n) ; javascript-mode
  (setq-local js-indent-level n) ; js-mode
  (setq-local js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-local css-indent-offset n) ; css-mode
  )

(defun my-personal-code-style ()
  (interactive)
  (message "My personal code style!")
  ;; use space instead of tab
  (setq indent-tabs-mode nil)
  ;; indent 2 spaces width
  (my-setup-indent 2))
;; ivy
(use-package ivy :ensure t
  :diminish (ivy-mode . "") ; does not display ivy in the modeline
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :bind (:map ivy-mode-map  ; bind in the ivy buffer
         ("C-'" . ivy-avy)) ; C-' to ivy-avy
  :config
  (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks and …
  (setq ivy-height 20)               ; set height of the ivy window
  (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
	
  )

(use-package swiper
	:ensure t
  :after ivy
	)

(use-package ivy-rich
	:ensure t
  :after ivy
	:config (ivy-rich-mode 1)
	)
;; :custom
;; (ivy-virtual-abbreviate 'full
;;                         ivy-rich-switch-buffer-align-virtual-buffer t
;;                         ivy-rich-path-style 'abbrev)
;; :config
;; (ivy-set-display-transformer 'ivy-switch-buffer
;;                              'ivy-rich-switch-buffer-transformer))

(use-package counsel
	:ensure t
	:after ivy
	:config (counsel-mode)
)
(use-package counsel-projectile
  :ensure t
  :after counsel

 )
;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; comment code
(defun xah-comment-dwim ()
  "Like `comment-dwim', but toggle comment if cursor is not at end of line."
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let ((-lbp (line-beginning-position))
          (-lep (line-end-position)))
      (if (eq -lbp -lep)
          (progn
            (comment-dwim nil))
        (if (eq (point) -lep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region -lbp -lep)
            (forward-line )))))))
(global-set-key (kbd "C-c c") 'xah-comment-dwim)

;;neotree

(use-package neotree
  :ensure t
  :config
  (setq neo-smart-open t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-fixed-size nil)
 )

;; suggestion
(use-package company               
  :ensure t
  :config (global-company-mode t))

;;elm setup 
(setq
  dotspacemacs-configuration-layers
   '(
    (elm :variables
        elm-sort-imports-on-save t
        elm-format-on-save t
        elm-format-command "elm-format"
        elm-reactor-arguments '("--port" "8000")
        elm-interactive-command '("elm" "repl")
        elm-reactor-command '("elm" "reactor")
        elm-compile-command '("elm" "make")
        elm-package-command '("elm" "package"))
    )
)

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (origami php-mode web-mode exec-path-from-shell flycheck js2-refactor xref-js2 js2-mode counsel-projectile ivy-rich ivy linum-relative general which-key helm use-package monokai-theme gruvbox-theme evil diminish))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;neotree key
(with-eval-after-load 'neotree
    (add-hook 'neotree-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
                (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
                (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
                (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
                (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
                (define-key evil-normal-state-local-map (kbd "R") 'neotree-rename-node)
                (define-key evil-normal-state-local-map (kbd "D") 'neotree-delete-node)
                (define-key evil-normal-state-local-map (kbd "C") 'neotree-create-node)
                (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)
                (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
                (define-key evil-normal-state-local-map (kbd "S") 'neotree-enter-horizontal-split)
                )))
		

;; javascript

(defun eslint-fix-file ()
  (interactive)
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat "eslint --fix " (buffer-file-name))))

(defun eslint-fix-file-and-revert ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

;; (use-package js2-mode
;;   :ensure t
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;   (add-hook 'js2-mode-hook (lambda ()
;; 			     (company-mode)
;; 			     (web-mode)))
;;   (add-hook 'js2-mode-hook
;;           (lambda ()
;;             (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))
;; )

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (add-hook 'web-mode-hook (lambda ()
			     (company-mode)))
  (add-hook 'web-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))
  (add-hook 'web-mode-hook 'my-personal-code-style)
)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
)
(when (memq window-system '(mac ns))                                                                                                                      
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "SSH_CLIENT" "HOSTNAME"                                                                         
                                           "GTAGSCONF" "GTAGSLABEL" "RUST_SRC_PATH"                                                                         
                                           "HISTFILE" "HOME" "GOPATH" "GOROOT" "GOEXEC"))                                                                   
    (exec-path-from-shell-initialize)) 
(use-package add-node-modules-path
  :ensure t
  :after js2-mode
  :config
  (add-hook 'js2-mode-hook #'add-node-modules-path)
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (setq js2-strict-missing-semi-warning nil)
)
(use-package flycheck
  :ensure t
  :after exec-path-from-shell
  :config
  (add-hook 'flycheck-mode-hook 'add-node-modules-path)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)

)

(use-package php-mode
  :ensure t
  :after exec-path-from-shell
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(?:php\\|phtml\\)\\'" . php-mode))  
  (add-hook 'php-mode-hook 'php-enable-psr2-coding-style)
 )

(use-package origami
  ;; :disabled
  :ensure t
  :config
  ;;origami https://github.com/gregsexton/origami.el
  ;; (use-package origami)
  (global-origami-mode 1)

  (defun nin-origami-toggle-node ()
    (interactive)
    (if (equal major-mode 'org-mode)
	(org-cycle)
      (save-excursion ;; leave point where it is
	(goto-char (point-at-eol))             ;; then go to the end of line
	(origami-toggle-node (current-buffer) (point)))))                 ;; and try to fold

  (add-hook 'prog-mode-hook
	    (lambda ()
	      ;; parsers see in variable origami-parser-alist
	      (setq-local origami-fold-style 'triple-braces)
	      (origami-mode)
	      (origami-close-all-nodes (current-buffer))
	      ))
  ;; mapping works only in normal mode
  (evil-define-key 'normal prog-mode-map (kbd "<tab>") 'nin-origami-toggle-node)
  ;; (evil-define-key 'normal php-mode-map (kbd "TAB") 'nin-origami-toggle-node)
  ;; (evil-define-key 'normal php-mode-map (kbd "<tab>") 'nin-origami-toggle-node)

  (define-key evil-normal-state-map "za" 'origami-forward-toggle-node)
  (define-key evil-normal-state-map "zR" 'origami-close-all-nodes)
  (define-key evil-normal-state-map "zM" 'origami-open-all-nodes)
  (define-key evil-normal-state-map "zr" 'origami-close-node-recursively)
  (define-key evil-normal-state-map "zm" 'origami-open-node-recursively)
  (define-key evil-normal-state-map "zo" 'origami-show-node)
  (define-key evil-normal-state-map "zc" 'origami-close-node)
  (define-key evil-normal-state-map "zj" 'origami-forward-fold)
  (define-key evil-normal-state-map "zk" 'origami-previous-fold)
  (define-key evil-visual-state-map "zf"
    '(lambda ()
       "create fold and add comment to it"
       (interactive)
       (setq start (region-beginning))
       (setq end (region-end))
       (deactivate-mark)
       (and (< end start)
	    (setq start (prog1 end (setq end start))))
       (goto-char start)
       (beginning-of-line)
       (indent-according-to-mode)
       (if (equal major-mode 'emacs-lisp-mode)
	   (insert ";; ")
	 ;; (indent-according-to-mode)
	 (insert comment-start " "))

       ;; (insert comment-start " ")
       (setq start (point))
       (insert "Folding" " {{{")
       (newline-and-indent)
       (goto-char end)
       (end-of-line)
       (and (not (bolp))
	    (eq 0 (forward-line))
	    (eobp)
	    (insert ?\n))
       (indent-according-to-mode)
       (if (equal major-mode 'emacs-lisp-mode)
	   (insert ";; }}}")

	 (if (equal comment-end "")
	     (insert comment-start " }}}")
	   (insert comment-end "}}}")))
       (newline-and-indent)
       (goto-char start)
       ))
  )
;;KEY BINDDING
(use-package general
  :ensure t
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-SPC"
  "TAB" '(switch-to-prev-buffer :which-key "previous buffer")"TAB" '(switch-to-prev-buffer :which-key "previous buffer")
  "`" '(switch-to-next-buffer :which-key "next buffer")
  "SPC" '(counsel-M-x  :which-key "M-x")
  "pf"  '(counsel-projectile :which-key "find files in project")
  "/"  '(counsel-ag :which-key "global searching")
  ;; Buffers
  "bb"  '(ivy-switch-buffer :which-key "buffers list ivy")
  "bm" '(ivy-push-view :which-key "ivy push bookmark view")
  "rm" '(ivy-pop-view :which-key "ivy remove bookmark view")
  ;; Window
  "wl"  '(windmove-right :which-key "move right")
  "wh"  '(windmove-left :which-key "move left")
  "wk"  '(windmove-up :which-key "move up")
  "wj"  '(windmove-down :which-key "move bottom")
  "w/"  '(split-window-right :which-key "split right")
  "w-"  '(split-window-below :which-key "split bottom")
  "wx"  '(delete-window :which-key "delete window")
  "sth" '(counsel-load-theme :which-key "theme list")
  ;;code
  "cc"  '(xah-comment-dwim :which-key "toggle code comment")
  ;neotree
  "nf" `(neotree-toggle :which-key "toogle neotree")
  ;;
  "sc" `(ispell-buffer :which-key "spell check")
  ;;"cs" '((load-file "~/.emacs.d/init.el"))
  ;; Others
  "at"  '(ansi-term :which-key "open terminal")
  ))
;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
