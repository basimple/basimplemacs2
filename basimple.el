;; using common lisp
(require 'cl)

;; set variables
;;;;;;;;;;;;;;;;
(setq emacs-dir (concat (getenv "HOME") "/basimplemacs/"))
(setq plugins-dir (concat emacs-dir "plugins/"))
(setq elpa-dir (concat emacs-dir "elpa/"))

;; set load path
(add-to-list 'load-path plugins-dir)

;; set builtin variable
(setq custom-file (concat emacs-dir "custom.el"))
(setq x-select-enable-clipboard t) ; os의 clipboard 공유하기
(setq font-use-system-font nil)
(setq tab-width 4)
(setq current-language-environment "UTF-8")
(setq default-input-method "korean-hangul")
(setq inhibit-startup-screen t) ; 시작화면 보이지 않기

;; key binding
(global-set-key (kbd "C-x C-m") 'execute-extended-command) ;execute-extend-command -> meta+m

;; set gui
;;;;;;;;;;;;;;;;
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)
(show-paren-mode 1)
(ido-mode -1)
(fringe-mode nil)
(scroll-bar-mode -1)


;; (setq truncate-partial-width-windows nil)
(setq truncate-lines nil) ; (아래나 위를 잘라서) 길이를 줄이다[짧게 하다]
(visual-line-mode -1)
(setq pop-up-frames nil)

;; set system dependent environment
;;;;;;;;;;;;;;;;
;; sh을 통해 실행되지 않을 경우엔 PASS가 적용되지 않는다.

    ;; (progn 
    ;;   (setenv "ANDROID_SDK"
    ;; 	      "/opt/android-sdk-linux_x86")
    ;;   (setenv "PATH"
    ;; 	      (concat
    ;; 	       (getenv "PATH") ":"
    ;; 	       "/usr/local/bin" ":"
    ;; 	       "/opt/android-sdk-linux_x86/tools" ":"
    ;; 	       "/opt/android-sdk-linux_x86/platform-tools"
    ;; 	       ))
    ;;   )



(global-set-key (kbd "S-SPC") 'toggle-korean-input-method)

;; Setting for font
(if (eq system-type 'darwin)
    (progn
      ;; (set-face-attribute 'default nil :height 100)
      (set-default-font "Monoco-10")
      (set-fontset-font (frame-parameter nil 'font)
			'hangul '("AppleGothic" . "unicode-bmp"))
      )
  )

(if (eq system-type 'darwin) ; mac os x의 경우
    (progn
      (setq mac-command-modifier 'meta) ;; Sets the command (Apple) key as Meta
      )
  )

(transient-mark-mode 1) ; highlight text selection
(delete-selection-mode 1) ; delete seleted text when typing
(global-font-lock-mode 1) ; turn on syntax coloring
(show-paren-mode 1) ; turn on paren match highlighting
(add-hook 'text-mode-hook (lambda () (hl-line-mode 1))) ; highlight current line
;; (global-set-key (kbd "<f7>") 'toggle-truncate-lines)

;; are we in aquamacs or carbon emacs?
(defvar *aquamacs-p* (boundp 'aquamacs-version))

;; turn off the toolbar, scrollbar, fringe; keep the tab bar
(when *aquamacs-p*
  (tabbar-mode 1)
  (setq special-display-regexps (remove "[ ]?\\*[hH]elp.*" special-display-regexps)) ; open *help* in current frame
  (smart-frame-positioning-mode nil)
  ;; (add-to-list 'default-frame-alist '(alpha . (90 80)))
  )
(setq default-frame-alist
      '(
	(background-color . "black")
	(foreground-color . "gray")
	;; (left . 0) (width . 141) (height . 44)
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basimple's function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (eq line-spacing nil)
      (setq-default line-spacing 0.5) ; add 0.5 height between lines
    (setq-default line-spacing nil)   ; no extra heigh between lines
    ))
(global-set-key (kbd "<f7>") 'toggle-line-spacing)

(defun maximize-frame () 
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1000 1000))

(global-set-key (kbd "<M-S-return>") 'maximize-frame)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autoinstall
;;;;;;;;;;;;;;;;

(require 'auto-install)
(setq auto-install-directory plugins-dir)

;; redo+
;;;;;;;;;;;;;;;;
(require 'redo+)
(define-key global-map (kbd "C-/") 'undo)
(define-key global-map (kbd "C-x C-/") 'redo)

;; xcscope
;;;;;;;;;;;;;;;;
;; (require 'xcscope)

;; CEDET
;;;;;;;;;;;;;;;;
(load-file (concat plugins-dir "cedet-1.0/common/cedet.el"))
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; ecb
;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat plugins-dir "ecb-2.40/"))
(require 'ecb)


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name (concat elpa-dir "package.el")))
  (package-initialize))

;; ELPA:Company Mode
;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat elpa-dir "company-0.5/"))
(autoload 'company-mode "company" nil t)

