;; TODO: this file needs some serious organization, maybe splitting in multiple
;; files should help?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add Melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming :)
(defun lena/set-font (name)
  (setq lena/font name)
  (set-face-attribute 'default nil :font name)
  (set-face-attribute 'variable-pitch nil :font name)
  (set-face-attribute 'fixed-pitch nil :font name))

(lena/set-font "JetBrainsMono NF ExtraLight 10")

(use-package doom-themes :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

;; Treat all themes as safe
(setq custom-safe-themes t)
(setq lena/themes '(doom-horizon doom-nord-light))
(setq lena/theme-index 0)

(defun lena/disable-all-themes ()
  "Run disable-theme for all loaded themes"
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defun lena/set-theme ()
  "Set theme to whatever theme is indexed by lena/theme-index in lena/themes"
  (lena/disable-all-themes)
  (load-theme (nth lena/theme-index lena/themes)))

(defun lena/cycle-theme ()
  "Increment (with wrap-around) lena/theme-index and call lena/set-theme"
  (interactive)
  (setq lena/theme-index (% (1+ lena/theme-index) (length lena/themes)))
  (lena/set-theme))

(lena/set-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous things

;; Ignore warnings and error in native compilation
(setq native-comp-async-report-warnings-errors nil)

;; Always use spaces (tabs are for losers buuahh)
(setq-default indent-tabs-mode nil)

;; Disable line wraping
(setq-default truncate-lines t)

;; Disable backups, fuck it
(setq make-backup-files nil)

;; Disable bell ringing
(setq ring-bell-function 'ignore)

;; Disables startup screen
(setq inhibit-startup-screen t)

;; Avoid using system (gui) dialog boxes
(setq use-dialog-box nil)

;; Min number of spaces reserved for line numbers
;; I typically don't edit files with more than 4-digits line numbers
;; so 4 is ideal
(setq display-line-numbers-width 4)

;; Make dired kill the current buffer before opening another
(setq dired-kill-when-opening-new-dired-buffer t)

;; Automatically complete pairs - like '(' with ')'
(electric-pair-mode 1)

;; Disable saving history of minibuffer (I don't care about it)
(savehist-mode 0)

;; Disable auto-saving of files
(auto-save-mode 0)

;; Remeber cursor position of closed buffers
(save-place-mode 1)

;; Eevert buffers when the file changes
(global-auto-revert-mode 1)

;; Typed text replaces the selection if the selection is active
(delete-selection-mode 1)

;; Display line numbers
(global-display-line-numbers-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Disable menu bar, tool bar and scroll bar
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Make yes-or-no into y-or-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make windmove use the Meta key
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

;; CUSTOM KEYBINDS
(require 'bind-key)

(unbind-key (kbd "<insert>"))
(unbind-key (kbd "C-x C-z"))
(unbind-key (kbd "C-z"))

(bind-key* (kbd "<escape>") 'keyboard-escape-quit)

(bind-key* (kbd "C-=") 'text-scale-increase)
(bind-key* (kbd "C--") 'text-scale-decrease)

(bind-key* [f7] (lambda () (interactive) (find-file user-init-file)))
(bind-key* (kbd "C-z") 'undo)
(bind-key* (kbd "C-d") 'duplicate-line)
(bind-key* (kbd "C-c t") 'neotree-toggle)
(bind-key* (kbd "C-q") 'kill-buffer-and-window)
(bind-key* (kbd "S-<delete>") 'kill-whole-line)
(bind-key* (kbd "C-c r") 'windresize)
(bind-key* (kbd "C-c a t") 'lena/cycle-theme)

(defun lena/open-terminal ()
  (interactive)
  (defun lena/split-window-below-70-30 ()
    (let ((proportion (* 6 0.1)))
      (split-window-below (round (* proportion (window-height))))))
  (lena/split-window-below-70-30)
  (other-window 1)
  (vterm))

(bind-key (kbd "C-'") 'lena/open-terminal)

(defun lena/custom-home-key ()
  "If cursor is already at the begging of the line jump back to indentation, otherwise go to beggining"
  (interactive)
  (when this-command-keys-shift-translated
    (set-mark-command nil))
  (if (bolp)
    (back-to-indentation)
    (beginning-of-line))
  (when this-command-keys-shift-translated
    (activate-mark)))

(defun lena/custom-c-backspace ()
  "Remove all whitespace if the character behind the cursor is whitespace, otherwise remove a word"
  (interactive)
  (if (looking-back "[ \n]")
      (progn
        (delete-horizontal-space 't)
        (while (looking-back "[ \n]")
          (backward-delete-char 1)))
    (backward-kill-word 1)))

(defun lena/custom-c-delete ()
  "Same as lena/custom-c-backspace, but instead of going backward it goes forward"
  (interactive)
  (if (looking-at "[ \n]")
      (progn
        (delete-horizontal-space)
        (while (looking-at "[ \n]")
        (delete-char 1)))
    (kill-word 1)))

(bind-key* (kbd "<home>") 'lena/custom-home-key)
(bind-key* (kbd "C-<backspace>") 'lena/custom-c-backspace)
(bind-key* (kbd "C-<delete>") 'lena/custom-c-delete)
(bind-key* (kbd "C-c <up>") 'lena/move-line-up)
(bind-key* (kbd "C-c <down>") 'lena/move-line-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXTERNAL PACKAGES
(setq use-package-always-ensure t)

(use-package helpful)
(global-set-key (kbd "C-h f") 'helpful-callable)
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-h x") 'helpful-command)

(use-package yasnippet)
(yas-global-mode 1)

(use-package markdown-mode)

(add-to-list 'load-path (concat user-emacs-directory "/external/lsp-bridge"))
(require 'lsp-bridge)
(global-lsp-bridge-mode)

(setq lsp-bridge-enable-hover-diagnostic t)
(define-key lsp-bridge-mode-map (kbd "C-c l f") 'lsp-bridge-code-format)
(define-key lsp-bridge-mode-map (kbd "C-c l a") 'lsp-bridge-code-action)
(define-key lsp-bridge-mode-map (kbd "C-c l r") 'lsp-bridge-rename)
(define-key lsp-bridge-mode-map (kbd "C-c l h") 'lsp-bridge-popup-documentation)
(define-key lsp-bridge-mode-map (kbd "C-c l d") 'lsp-bridge-find-def)
(define-key lsp-bridge-mode-map (kbd "C-c l i") 'lsp-bridge-find-impl)
(define-key lsp-bridge-mode-map (kbd "C-c l u") 'lsp-bridge-find-references)
(define-key lsp-bridge-mode-map (kbd "C-<tab>") 'lsp-bridge-popup-complete-menu)

(use-package multiple-cursors)
(global-set-key (kbd "C-c c r") 'mc/edit-lines)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
(global-unset-key (kbd "M-<down-mouse-1>"))

(use-package move-text)
(bind-key* (kbd "C-<prior>") 'move-text-up)
(bind-key* (kbd "C-<next>") 'move-text-down)

(use-package pulsar)
(setq pulsar-pulse t)
(setq pulsar-delay 0.055)
(setq pulsar-iterations 10)
(setq pulsar-face 'pulsar-magenta)
(setq pulsar-highlight-face 'pulsar-yellow)
(pulsar-global-mode 1)

(use-package rainbow-mode)
(rainbow-turn-on)

(use-package doom-modeline)
(setq doom-modeline-icon nil)
(setq doom-modeline-time-icon nil)
(setq display-time-default-load-average nil)
(setq display-time-format "%H:%M:%S")
(setq display-time-interval 1)
(display-time-mode 1)
(doom-modeline-mode 1)

(use-package smooth-scrolling)
(smooth-scrolling-mode 1)

(use-package ivy)
(use-package swiper)
(use-package counsel)

(setq enable-recursive-minibuffers t)
(setq ivy-use-virtual-buffers t)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)

(counsel-mode 1)
(ivy-mode 1)

(use-package which-key)
(which-key-mode)

(use-package markdown-mode)
(use-package f)

(use-package neotree)
(setq neo-theme 'ascii
      neo-confirm-create-file 'off-p
      neo-confirm-create-directory 'off-p
      neo-show-hidden-files t
      neo-window-fixed-size nil
      neo-show-updir-line t
      neo-window-position 'left
      neo-show-slash-for-folder nil
      neo-autorefresh nil)

(use-package windresize)
(use-package vterm)

(add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))

(use-package lua-mode)
(use-package rust-mode)
(use-package meson-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS
(defun lena/lua-mode-hook ()
  "Lua mode hook for setting custom indentation-related variables"
  (setq indent-tabs-mode nil)
  (setq lua-indent-level 2)
  (setq lua-indent-nested-block-content-align nil)
  (setq lua-indent-close-paren-align nil)
  (setq lua-electric-flag nil)
  (setq lua-indent-string-contents nil))

(defun lena/lua-at-most-one-indent (old-function &rest arguments)
  "Fix bad lua-mode formatting of nested indentation levels"
  ;; I think this need some explaining for future me:
  ;; By default lua mode do this
  ;; -- lua
  ;; banana('hello!', function()
  ;;       print('wtf?')
  ;;   end)
  ;;
  ;; This fix it to:
  ;; -- lua
  ;; banana('hello!', function()
  ;;   print('wtf?')
  ;; end)
  (let ((old-res (apply old-function arguments)))
    (if (> old-res 2) 2 old-res)))

(advice-add #'lua-calculate-indentation-block-modifier
            :around #'lena/lua-at-most-one-indent)

(add-hook 'lua-mode-hook 'lena/lua-mode-hook)

(defun lena/c++-mode-hook ()
  (c-set-style "my-style"))

;; https://github.com/axelf4/dotfiles/blob/master/.config/emacs/init.el
(c-add-style
 "my-style" ; C/C++ style that uses alignment less liberally
 `((c-comment-only-line-offset 0 . 0) ; Indent column-zero comment lines too
   (c-offsets-alist
    (substatement-open . 0)
    (innamespace . 0)
    (block-close . c-lineup-under-anchor)
    (statement c-lineup-conditional-test-clause 0)
    (statement-cont add c-lineup-conditional-test-clause +)
    (arglist-cont-nonempty . +)
    (arglist-close
     . ,(lambda (langelem) (if (eq (c-lineup-close-paren langelem) 0) 0 '+)))
    (label . 0) (substatement-label . 0))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style "my-style")
 '(lsp-bridge-python-lsp-server "pyright")
 '(lua-prefix-key "C-c")
 '(package-selected-packages
   '(meson-mode rust-mode lua-mode dashboard vterm windresize neotree which-key counsel swiper ivy smooth-scrolling doom-modeline rainbow-mode pulsar move-text multiple-cursors markdown-mode yasnippet helpful doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
