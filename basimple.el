;; set system dependent environment
;;;;;;;;;;;;;;;;
;; using common lisp
(require 'cl)

;; set variables
;;;;;;;;;;;;;;;;
(setq home (getenv "HOME"))
(setq emacs-dir (concat (getenv "HOME") "/basimplemacs/"))
(setq plugins-dir (concat emacs-dir "plugins/"))
(setq elpa-dir (concat emacs-dir "elpa/"))
(setq org-directory (concat home "/org/"))
(setq conf-dir (concat emacs-dir "conf/"))

;; set load path
(add-to-list 'load-path plugins-dir)
(add-to-list 'load-path conf-dir)

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

;; shell
;;;;;;;;;;;;;;;;
(defalias 'sh 'eshell)
(global-set-key (kbd "<f5>") 'eshell)


;; set gui
;;;;;;;;;;;;;;;;
(when window-system
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode 1))
  (if (fboundp 'tool-bar-mode)
      (tool-bar-mode -1))
  (if (fboundp 'scroll-bar-mode)
      (scroll-bar-mode -1))
  (if (fboundp 'display-time-mode)
      (display-time-mode 1))
  (if (fboundp 'show-paren-mode)
      (show-paren-mode 1))
  (if (fboundp 'ido-mode)
      (ido-mode -1))
  (if (fboundp 'fringe-mode)
      (fringe-mode -1))
  (if (fboundp 'scroll-bar-mode)
      (scroll-bar-mode -1))
  )


;; Set Frame Title
;;;;;;;;;;;;;;;;
(setq frame-title-format '("%b:%f:%s"))

;; (setq truncate-partial-width-windows nil)
(setq truncate-lines nil) ; (아래나 위를 잘라서) 길이를 줄이다[짧게 하다]
(visual-line-mode -1)
(setq pop-up-frames nil)
(global-auto-revert-mode -1)
;; change from yes/no 2 y/n.
;;;;;;;;;;;;;;;;
(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "S-SPC") 'toggle-korean-input-method)

(if (eq system-type 'darwin)
    (progn 
      (setenv "ANDROID_SDK"
	      "/Users/basimple/Deveopment/SDK/android-sdk-mac_x86")
      (setenv "PATH"
	      (concat
	       (getenv "PATH") ":"
	       (getenv "ANDROID_SDK") ":"
	       (concat (getenv "ANDROID_SDK") "/tools/")
	       ))
      )
  (if (eq system-type 'gnu/linux)
      (setenv "ANDROID_SDK"
	      "/opt/android-sdk-linux_x86")
    (setenv "PATH"
	    (concat
	     (getenv "PATH") ":"
	     "/usr/local/bin" ":"
	     "/opt/android-sdk-linux_x86/tools" ":"
	     "/opt/android-sdk-linux_x86/platform-tools"
	     ))
    ;; (setq exec-path
    ;; 	'(
    ;; 	  "/opt/android-sdk-linux_x86/tools"
    ;; 	  "/opt/android-sdk-linux_x86/platform-tools"
    ;; 	  ))
    )
  )
  
;; (global-set-key (kbd "S-SPC") 'toggle-korean-input-method)

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
(when window-system
  (setq default-frame-alist
	'(
	  (background-color . "black")
	  (foreground-color . "gray")
	  ;; (left . 0) (width . 141) (height . 44)
	  ))
  ;; (when window-system
  ;;   (set-face-foreground 'default "gray")
  ;;   (set-face-background 'default "black")
  ;; )
  (setq term-default-bg-color "black")
  (setq term-default-fg-color "gray")
  )

;; TransparentEmacs
;;;;;;;;;;;;;;;;
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(set-frame-parameter (selected-frame) 'alpha '(90 80))
(add-to-list 'default-frame-alist '(alpha 90 80))


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

(eval-when-compile (require 'cl))
(defun basimple:toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(85 50))))
(global-set-key (kbd "C-c t") 'basimple:toggle-transparency)

;; ToggleWindowSplit
;; http://www.emacswiki.org/emacs/ToggleWindowSplit
;;;;;;;;;;;;;;;;
(defun basimple:toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))
(define-key ctl-x-4-map "t" 'basimple:toggle-window-split)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto Complete Mode
;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat plugins-dir "autocomplete/"))
(require 'auto-complete)
(global-auto-complete-mode 1)

;; (require 'auto-complete+)
;; (require 'auto-complete-extension)
;; (require 'auto-complete-etags)
;; (require 'auto-complete-octave)

;; http://www.emacswiki.org/emacs/AutoAsyncByteCompile
;;;;;;;;;;;;;;;;
;; (require 'auto-async-byte-compile)
;; ;; (setq auto-async-byte-compile-exclude-files-regexp "/junk/")
;; (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;; fullscreen with wmctrl
;;;;;;;;;;;;;;;;
(if (eq system-type 'gnu/linux)
    (progn
      (defun basimple:switch-full-screen ()
	(interactive)
	(shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
      (global-set-key [f11] 'basimple:switch-full-screen)
      )
  )

;; Anything
;;;;;;;;;;;;;;;;
(require 'anything)


;; Edit with Emacs, Chrome Extention
;;;;;;;;;;;;;;;;
;; (require 'edit-server)
;; (edit-server-start)


;; indent one line xml file
;;;;;;;;;;;;;;;;
(require 'iox)


;; Ediff
;;;;;;;;;;;;;;;;
;; This is what you probably want if you are using a tiling window
;; manager under X, such as ratpoison.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; To make ediff to be horizontally split use:
(setq ediff-split-window-function 'split-window-horizontally)


;; Unicad
;; http://www.emacswiki.org/cgi-bin/emacs/Unicad
;;;;;;;;;;;;;;;;
(require 'unicad)


;; WhiteSpace mode
;;;;;;;;;;;;;;;;
;; make whitespace-mode use just basic coloring
(setq whitespace-style (quote
			( spaces tabs newline space-mark tab-mark newline-mark)))
;; make whitespace-mode use “¶” for newline and “▷” for tab.
;; together with the rest of its defaults
(setq whitespace-display-mappings
      '(
	(space-mark 32 [183] [46]) ; normal space
	(space-mark 160 [164] [95])
	(space-mark 2208 [2212] [95])
	(space-mark 2336 [2340] [95])
	(space-mark 3616 [3620] [95])
	(space-mark 3872 [3876] [95])
	(newline-mark 10 [182 10]) ; newlne
	;; (tab-mark 9 [9655 9] [92 9]) ; tab
	(tab-mark 9 [187 9] [92 9]) ; tab
	))

;; MobileOrg
;;;;;;;;;;;;;;;;
(setq org-mobile-inbox-for-pull
      (concat org-directory "from-mobile.org"))
;; http://orgmode.org/manual/Pushing-to-MobileOrg.html
;; File names will be staged with paths relative to org-directory, so all files should be inside this directory
(setq org-mobile-directory "/media/private/org/")
(setq org-mobile-files (list "gtd.org"
			     "bugs.org"
			     ))
(setq org-mobile-force-id-on-agenda-items nil)


;; Org-mode
;;;;;;;;;;;;;;;;
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (setq org-log-done t)
;; (setq org-directory "~/org/")
;; (setq org-agenda-files
;;       (file-expand-wildcards
;;        (concat org-directory "*.org")))

;; (setq org-link-abbrev-alist
;;       '(("bugzilla" . "http://bug.thinkfree.com/show_bug.cgi?id=")
;; 	("google"   . "http://www.google.com/search?q=")))


;; BrowseUrl [[http://www.emacswiki.org/emacs/BrowseUrl#toc9]]
;;;;;;;;;;;;;;;;
;; On arch linux, the following command will allow emacs to use the chromium-browser PKGBUILD:
(if (eq system-type 'gnu/linux)
    (setq browse-url-browser-function 'browse-url-generic
	  browse-url-generic-program "google-chrome"
	  ;; browse-url-generic-program "firefox-trunk"
	  ;; browse-url-generic-program "firefox"
	  )
  (if (eq system-type 'darwin)
      (progn
	
	)
    ))

(defun browse-url-default-macosx-browser (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (if (and new-window (>= emacs-major-version 23))
      (ns-do-applescript
       (format (concat "tell application \"Safari\" to make document with properties {URL:\"%s\"}\n"
		       "tell application \"Safari\" to activate") url))
    (start-process (concat "open " url) nil "open" url)))



;; [[http://www.emacswiki.org/emacs/ClusterSSH:ccsh]]
;;;;;;;;;;;;;;;;
;; (add-hook 'dired-mode-hook
;;   	  '(lambda ()
;; 	     (define-key dired-mode-map "o" 'dired-open-mac)))
;; (define-key dired-mode-map "o" 'dired-open-mac)

;; (defun dired-open-mac ()
;;   (interactive)
;;   (let ((file-name (dired-get-file-for-visit)))
;;     (if (file-exists-p file-name)
;; 	(call-process "/usr/bin/open" nil 0 nil file-name))))

;; (require 'cssh)

;; Bongo
;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat plugins-dir "bongo/"))
(autoload 'bongo "bongo"
  "Start Bongo by switching to a Bongo buffer." t)


;; WindMove
;;;;;;;;;;;;;;;;
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings 'super))
;; (global-set-key (kbd "m-h") 'windmove-left)
;; (global-set-key (kbd "m-j") 'windmove-down)
;; (global-set-key (kbd "m-k") 'windmove-up)
;; (global-set-key (kbd "m-l") 'windmove-right)

;; Twittering
;;;;;;;;;;;;;;;;
(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(global-set-key "\C-xt" 'twitter-get-friends-timeline)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)


;; Show Paren Mode
;;;;;;;;;;;;;;;;
(setq show-paren-style 'mixed)

;; JabberEL
;;;;;;;;;;;;;;;;
;; (setq jabber-account-list
;;       '(("basimple@gmail.com" 
;; 	 (:network-server . "talk.google.com")
;; 	 (:connection-type . ssl))))

;; (defun basimple:jabber-google-groupchat-create ()
;;   (interactive)
;;   (let ((group (apply 'format "private-chat-%x%x%x%x%x%x%x%x-%x%x%x%x-%x%x%x%x-%x%x%x%x-%x%x%x%x%x%x%x%x%x%x%x%x@groupchat.google.com"
;; 		      (mapcar (lambda (x) (random x)) (make-list 32 15))))
;; 	(account (jabber-read-account)))
;;     (jabber-groupchat-join account group (jabber-muc-read-my-nickname account group) t)))

;; Android Mode
;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat plugins-dir "android-mode/"))
(require 'android-mode)
(setq android-mode-sdk-dir (getenv "ANDROID_SDK"))

;; autoinstall
;;;;;;;;;;;;;;;;
;; [[http://www.emacswiki.org/emacs/AutoInstall:auto-install]]
(require 'auto-install)
(setq auto-install-directory plugins-dir)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;; redo+
;;;;;;;;;;;;;;;;
(require 'redo+)
(define-key global-map (kbd "C-/") 'undo)
(define-key global-map (kbd "C-x C-/") 'redo)

;; xcscope
;;;;;;;;;;;;;;;;
;; (require 'xcscope)
;; cscope
;;;;;;;;;;;;;;;;
;; (require 'xcscope)
;; (add-hook 'java-mode-hook (function cscope:hook))

;; (setq cscope-database-regexps
;;       '(
;; 	( "^/users/jdoe/sources/proj1"
;; 	  ( t )
;; 	  ( "/users/jdoe/sources/proj2")
;; 	  ( "/users/jdoe/sources/proj3/mycscope.out")
;; 	  ( "/users/jdoe/sources/proj4")
;; 	  t
;; 	  ( "/some/master/directory" ("-d" "-I/usr/local/include") )
;; 	  )
;; 	( "^/users/jdoe/sources/gnome/"
;; 	  ( "/master/gnome/database" ("-d") )
;; 	  )
;; 	))

;; 5. If you intend to use xcscope.el often you can optionally edit your
;;    ~/.emacs file to add keybindings that reduce the number of keystrokes
;;    required.  For example, the following will add "C-f#" keybindings, which
;;    are easier to type than the usual "C-c s" prefixed keybindings.  Note
;;    that specifying "global-map" instead of "cscope:map" makes the
;;    keybindings available in all buffers:
;;
;; (define-key global-map [(control f3)]  'cscope-set-initial-directory)
;; (define-key global-map [(control f4)]  'cscope-unset-initial-directory)
;; (define-key global-map [(control f5)]  'cscope-find-this-symbol)
;; (define-key global-map [(control f6)]  'cscope-find-global-definition)
;; (define-key global-map [(control f7)]
;;   'cscope-find-global-definition-no-prompting)
;; (define-key global-map [(control f8)]  'cscope-pop-mark)
;; (define-key global-map [(control f9)]  'cscope-next-symbol)
;; (define-key global-map [(control f10)] 'cscope-next-file)
;; (define-key global-map [(control f11)] 'cscope-prev-symbol)
;; (define-key global-map [(control f12)] 'cscope-prev-file)
;; (define-key global-map [(meta f9)]  'cscope-display-buffer)
;; (define-key global-map [(meta f10)] 'cscope-display-buffer-toggle)

;; CEDET
;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path (concat plugins-dir "cedet-1.0/common/"))
;; (load-file (concat plugins-dir "cedet-1.0/common/cedet.el"))
;; (require 'cedet)
;; (global-ede-mode 1)                      ; Enable the Project management system
;; (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;; (global-srecode-minor-mode 1)            ; Enable template insertion menu

;; ecb
;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path (concat plugins-dir "ecb-2.40/"))
;; (require 'ecb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elpa
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-user-dir
      (concat emacs-dir "elpa/")
      )
(load "package")
(package-initialize)
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
;; (when
;;     (load
;;      (expand-file-name (concat elpa-dir "package.el")))
;;   (package-initialize))

;; ELPA:Company Mode
;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path (concat elpa-dir "company-0.5/"))
;; (autoload 'company-mode "company" nil t)

