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
  (set-face-attribute 'default nil :width 'normal  :weight 'normal :slant 'normal :font lena/font))

(lena/set-font "Hack 10")

(use-package doom-themes :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t))

(setq custom-safe-themes t) ; Treat all themes as safe
(setq lena/themes '(doom-nord-light doom-one))
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
(setq-default indent-tabs-mode nil)  ; always use space!
(setq-default truncate-lines t)


(setq make-backup-files nil) ; disable backups, fuck it
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(setq use-dialog-box nil) ; don't show things like file dialog boxes
(setq history-length 30)
;; (setq savehist-file (concat user-emacs-directory "/.hist"))
(setq display-line-numbers-width 4)

(electric-pair-mode 1)
(auto-save-mode 0)
(save-place-mode 1)         ; remeber cursor position of closed buffers
(global-auto-revert-mode 1) ; revert buffers when the file changes
(savehist-mode 0)
(global-display-line-numbers-mode 1)
(delete-selection-mode 1) ; typed text replaces the selection if the selection is active

;; (global-hl-line-mode 0)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(display-fill-column-indicator-mode 1)
(setq display-fill-column-indicator-column 80)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'meta))

;; CUSTOM KEYBINDS
(require 'bind-key)

(unbind-key (kbd "<insert>"))
(unbind-key (kbd "C-x C-z"))

(bind-key* [f7] (lambda () (interactive) (find-file user-init-file)))
(bind-key* (kbd "C-z") 'undo)
(bind-key* (kbd "C-d") 'duplicate-line)
(bind-key* (kbd "C-c t") 'neotree-toggle)
(bind-key* (kbd "C-q") 'kill-buffer-and-window)
(bind-key* (kbd "S-<delete>") 'kill-whole-line)
(bind-key* (kbd "C-c r") 'windresize)
(bind-key* (kbd "C-c a t") 'lena/cycle-theme)

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

(defun lena/move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun lena/move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(bind-key* (kbd "<home>") 'lena/custom-home-key)
(bind-key* (kbd "C-<backspace>") 'lena/custom-c-backspace)
(bind-key* (kbd "C-<delete>") 'lena/custom-c-delete)
(bind-key* (kbd "C-c <up>") 'lena/move-line-up)
(bind-key* (kbd "C-c <down>") 'lena/move-line-down)
(bind-key* (kbd "C-<prior>") 'lena/move-line-up)
(bind-key* (kbd "C-<next>") 'lena/move-line-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXTERNAL PACKAGES
(use-package yasnippet :ensure t)
(yas-global-mode 1)

(use-package markdown-mode :ensure t)

(add-to-list 'load-path (concat user-emacs-directory "/external/lsp-bridge"))
(require 'lsp-bridge)
(global-lsp-bridge-mode)

(setq lsp-bridge-enable-hover-diagnostic t)

(define-key lsp-bridge-mode-map (kbd "C-c l f") 'lsp-bridge-code-format)
(define-key lsp-bridge-mode-map (kbd "C-c l a") 'lsp-bridge-code-action)
(define-key lsp-bridge-mode-map (kbd "C-c l r") 'lsp-bridge-rename)
(define-key lsp-bridge-mode-map (kbd "C-c l h") 'lsp-bridge-popup-documentation)
(define-key lsp-bridge-mode-map (kbd "C-c l d") 'lsp-bridge-find-def)
(define-key lsp-bridge-mode-map (kbd "C-c l D") 'lsp-bridge-find-def-other-window)
(define-key lsp-bridge-mode-map (kbd "C-c l i") 'lsp-bridge-find-impl)
(define-key lsp-bridge-mode-map (kbd "C-c l I") 'lsp-bridge-find-impl-other-window)
(define-key lsp-bridge-mode-map (kbd "C-<tab>") 'lsp-bridge-popup-complete-menu)

(use-package multiple-cursors :ensure t)
(global-set-key (kbd "C-c c r") 'mc/edit-lines)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
(global-unset-key (kbd "M-<down-mouse-1>"))

(use-package pulsar :ensure t)
(setq pulsar-pulse t)
(setq pulsar-delay 0.055)
(setq pulsar-iterations 10)
(setq pulsar-face 'pulsar-magenta)
(setq pulsar-highlight-face 'pulsar-yellow)
(pulsar-global-mode 1)

(use-package doom-modeline :ensure t)
(setq doom-modeline-icon nil)
(setq doom-modeline-time-icon nil)
(setq display-time-default-load-average nil)
(setq display-time-format "%H:%M:%S")
(setq display-time-interval 1)
(display-time-mode 1)
(doom-modeline-mode 1)

(use-package smooth-scrolling :ensure t)
(smooth-scrolling-mode 1)

(use-package ivy :ensure t)
(use-package swiper :ensure t)
(use-package counsel :ensure t)

(setq enable-recursive-minibuffers t)
(setq ivy-use-virtual-buffers t)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)

(counsel-mode 1)
(ivy-mode 1)

(use-package which-key :ensure t)
(which-key-mode)

(use-package markdown-mode :ensure t)
(use-package f :ensure t)

(use-package neotree :ensure t)
(setq neo-theme 'ascii
      neo-confirm-create-file 'off-p
      neo-confirm-create-directory 'off-p
      neo-show-hidden-files t
      neo-window-fixed-size nil
      neo-show-updir-line t
      neo-window-position 'left
      neo-show-slash-for-folder nil
      neo-autorefresh nil)

(use-package windresize :ensure t)
(use-package vterm :ensure t)

(setq lua-indent-level 2)
(setq lua-electric-flag nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS
(defun lena/lua-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq lua-indent-level 2)
  (setq lua-indent-nested-block-content-align nil)
  (setq lua-indent-close-paren-align nil)
  (setq lua-indent-string-contents nil))

(defun lena/lua-at-most-one-indent (old-function &rest arguments)
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
 '(custom-safe-themes
   '("8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a" "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350" "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec" "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7" "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d" "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100" "4825b816a58680d1da5665f8776234d4aefce7908594bea75ec9d7e3dc429753" "cc3a6dedbea7c54b185c691c70d2b0ad979fbfe116bc08cf9a35b220592a1634" "c0e8a59eb7d603ca4af5616b2c61b8be2fee031760af4d8c80fd2f21229ce462" "c38ca564fb26ae0414ed076b9a8462cdfbb1e20eb651001bfaa789e842fdbfdd" default))
 '(lsp-bridge-python-lsp-server "pyright")
 '(lua-prefix-key "C-c")
 '(package-selected-packages
   '(vterm windresize neotree which-key counsel swiper ivy smooth-scrolling doom-modeline pulsar multiple-cursors yasnippet markdown-mode doom-themes))
 '(typescript-indent-level 2))
 '(package-selected-packages
   '(ocaml-ts-mode zig-mode python-black fennel-mode fsharp-mode typescript-mode haskell-mode glsl-mode windresize emacs-neotree lua-mode go-mode rust-mode catppuccin catppuccin-theme ini-mode counsel swiper multiple-cursors meson-mode vterm ivy-prescient ivy-prescient-mode pulsar doom-themes ivy which-key doom-modeline smooth-scrolling))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
