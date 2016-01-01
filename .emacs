;----------------------;
;;; Package Settings ;;;
;----------------------;

;; set package archives
(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")))

;; Define packages needed
(setq package-list  '(auto-complete
                      autopair
                      company
                      elpy
                      fill-column-indicator
                      magit
                      nlinum
                      pos-tip))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;--------------------;
;;; Setup Packages ;;;
;--------------------;

;; Enable pos-tip
;(require 'pos-tip)
;;(pos-tip-show "Testing pos-tip!!")

;; ;; Auto complete
;; (require 'auto-complete)
;; (ac-config-default)
;; ;(ac-ropemacs-initialize)
;; (global-auto-complete-mode t)
;; ;; (setqpopup-use-optimized-column-computation nil)
;; (setq ac-quick-help-prefer-pos-tip t)

;; Company-mode
;(add-hook 'after-init-hook 'global-company-mode)

;; Fill column indicator
(require 'fill-column-indicator)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)
(setq-default fill-column 80)

;; Color theme
(add-to-list 'custom-theme-load-path
             "~/.emacs.d/elpa/color-theme-solarized-20150619.1734")
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((mode (if (display-graphic-p frame) 'light 'dark)))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-paramter frame 'background-mode mode))
            (enable-theme 'solarized)))

;; Ido mode
(require 'ido)
(ido-mode 1)

;; Python mode
(when (require 'elpy nil t)
  (elpy-enable))
(setq elpy-rpc-backend "jedi")

;; Highlight Current Line Settings
(global-hl-line-mode)
(set-face-background hl-line-face "gray13")

;; nlinum Settings
(add-hook 'nlinum-mode-hook
          (lambda ()
            (unless (boundp 'nlinum--width)
              (setq nlinum--width
                    (length (number-to-string
                            (count-lines (point-min) (point-max))))))))
(setq nlinum-format "%d ")
(global-nlinum-mode)
(global-set-key "(" 'skeleton-pair-insert-maybe)

;; Autopair
(autopair-global-mode)

;; Magit

;------------------;
;; Misc. Settings ;;
;------------------;

(tool-bar-mode -1) ;; Disable the toolbar
(menu-bar-mode -1) ;; Disable the menubar
(defalias 'yes-or-no-p 'y-or-n-p) ;; Use 'y' or 'n' for 'yes' or 'no'

;; Marking Text
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Keybindings
(global-set-key (kbd "RET") 'newline-and-indent)

;; Font
(set-default-font "Inconsolata 12")

;; Each line of text gets one line on the screen
(setq-default truncate-lines 1)
(setq truncate-partial-width-windows 1)

;; Always use spaces, not tabs, when indenting
(setq-default indent-tabs-mode nil)

;; Show the current line and column numberss in the stats bar
(line-number-mode 1)
(column-number-mode 1)

;; Don't blink the cursor
(blink-cursor-mode nil)

;; Ensure transient mark mode is enabled
(transient-mark-mode 1)

;; Highlight paretheses when the cursor is next to them
(require 'paren)
(show-paren-mode 1)

;; Disable backup
(setq backup-inhibited t)

;; Disable autosave
(setq auto-save-default nil)
