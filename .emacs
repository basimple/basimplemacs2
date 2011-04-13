;; menu를 통해 option 변경시 저장되는 파일
(setq custom-file "~/custom.el")

;; sh을 통해 실행되지 않을 경우엔 PASS가 적용되지 않는다.
(when (eq system-type 'gnu/linux)
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

;; for using common lisp
(require 'cl)

(setq elisp-dir (expand-file-name "~/.emacs.d/elisp/"))

(add-to-list 'load-path elisp-dir)

(global-auto-revert-mode -1)

;; releation about system
(setq x-select-enable-clipboard t)
(setq font-use-system-font t)

(setq tab-width 4)

(setq current-language-environment "UTF-8")
(setq default-input-method "korean-hangul")
(setq tab-width 4)

(when window-system
  (set-face-foreground 'default "gray")
  (set-face-background 'default "black")
)

;; (setq term-default-bg-color "black")
;; (setq term-default-fg-color "gray")

;; execute-extend-command -> meta+m
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(setq inhibit-splash-screen t)		; Remove splash screen

;; (setq fringe-mode '(0 . 0))
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(display-time-mode 1)
(show-paren-mode 1)
(ido-mode -1)

;; shell
;;;;;;;;;;;;;;;;
(defalias 'sh 'eshell)
;; (global-set-key (kbd "<f5>") 'eshell)

;; ecb
;;;;;;;;;;;;;;;;
(require 'ecb)

;; Company Mode
;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "/path/to/company")
(autoload 'company-mode "company" nil t)

;; cscope
;;;;;;;;;;;;;;;;
(require 'xcscope)

(add-hook 'java-mode-hook (function cscope:hook))

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
(define-key global-map [(control f3)]  'cscope-set-initial-directory)
(define-key global-map [(control f4)]  'cscope-unset-initial-directory)
(define-key global-map [(control f5)]  'cscope-find-this-symbol)
(define-key global-map [(control f6)]  'cscope-find-global-definition)
(define-key global-map [(control f7)]
  'cscope-find-global-definition-no-prompting)
(define-key global-map [(control f8)]  'cscope-pop-mark)
(define-key global-map [(control f9)]  'cscope-next-symbol)
(define-key global-map [(control f10)] 'cscope-next-file)
(define-key global-map [(control f11)] 'cscope-prev-symbol)
(define-key global-map [(control f12)] 'cscope-prev-file)
(define-key global-map [(meta f9)]  'cscope-display-buffer)
(define-key global-map [(meta f10)] 'cscope-display-buffer-toggle)



;; TransparentEmacs
;;;;;;;;;;;;;;;;
;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(set-frame-parameter (selected-frame) 'alpha '(95 85))
(add-to-list 'default-frame-alist '(alpha 95 85))

(eval-when-compile (require 'cl))
(defun basimple:toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(85 50))))
(global-set-key (kbd "C-c t") 'basimple:toggle-transparency)

;; folding(http://git.savannah.gnu.org/cgit/emacs-tiny-tools.git/plain/lisp/other/folding.el?h=devel)
;;;;;;;;;;;;;;;;
;; (autoload 'folding-mode          "folding" "Folding mode" t)
;; (autoload 'turn-off-folding-mode "folding" "Folding mode" t)
;; (autoload 'turn-on-folding-mode  "folding" "Folding mode" t)

;; (setq folding-default-keys-function
;;      'folding-bind-backward-compatible-keys)

;; (if (load "folding" 'nomessage 'noerror)
;;     (folding-mode-add-find-file-hook))

;; company mode
;;;;;;;;;;;;;;;;
;; (add-to-list 'load-path "/path/to/company")
(autoload 'company-mode "company" nil t)

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

;; font
;;;;;;;;;;;;;;;;
(when window-system
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-7.2")
  ;; (set-frame-font "DejaVu Sans Mono-7.5")
  (set-fontset-font "fontset-default" 'hangul '("UnDotum" . "unicode-bmp"))
  (set-fontset-font "fontset-default" 'kana '("Kochi Gothic" . "unicode-bmp"))
  )
;; change from yes/no 2 y/n.
;;;;;;;;;;;;;;;;
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set Frame Title
;;;;;;;;;;;;;;;;
(setq frame-title-format '("%b:%f:%s"))

;; Android Mode
;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat elisp-dir "android-mode/"))
(require 'android-mode)
(setq android-mode-sdk-dir "/opt/android-sdk-linux_x86")

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

;; Show Paren Mode
;;;;;;;;;;;;;;;;
(setq show-paren-style 'mixed)

;; Twittering
;;;;;;;;;;;;;;;;
(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(global-set-key "\C-xt" 'twitter-get-friends-timeline)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

;; WindMove
;;;;;;;;;;;;;;;;
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(windmove-default-keybindings 'super)
(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-l") 'windmove-right)

;; [[http://www.emacswiki.org/emacs/AutoInstall:auto-install]]

(require 'auto-install)
(setq auto-install-directory elisp-dir)
(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)

;; Bongo
;;;;;;;;;;;;;;;;
(add-to-list 'load-path (concat elisp-dir "bongo/"))
(autoload 'bongo "bongo"
  "Start Bongo by switching to a Bongo buffer." t)

;; [[http://www.emacswiki.org/emacs/ClusterSSH:ccsh]]
;;;;;;;;;;;;;;;;
(require 'cssh)

;; BrowseUrl [[http://www.emacswiki.org/emacs/BrowseUrl#toc9]]
;;;;;;;;;;;;;;;;
;; On arch linux, the following command will allow emacs to use the chromium-browser PKGBUILD:
(setq browse-url-browser-function 'browse-url-generic
      ;; browse-url-generic-program "google-chrome"
      browse-url-generic-program "firefox-trunk"
      ;; browse-url-generic-program "firefox"
      )

;; Org-mode
;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-directory "~/org/")
(setq org-agenda-files
      (file-expand-wildcards
       (concat org-directory "*.org")))

(setq org-link-abbrev-alist
      '(("bugzilla" . "http://bug.thinkfree.com/show_bug.cgi?id=")
	("google"   . "http://www.google.com/search?q=")))

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

;; Unicad
;; http://www.emacswiki.org/cgi-bin/emacs/Unicad
;;;;;;;;;;;;;;;;
(require 'unicad)

;; Ediff
;;;;;;;;;;;;;;;;
;; This is what you probably want if you are using a tiling window
;; manager under X, such as ratpoison.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; To make ediff to be horizontally split use:
(setq ediff-split-window-function 'split-window-horizontally)

;; indent one line xml file
;;;;;;;;;;;;;;;;
(require 'iox)

;; Edit with Emacs, Chrome Extention
;;;;;;;;;;;;;;;;
(require 'edit-server)
(edit-server-start)

;; Anything
;;;;;;;;;;;;;;;;
(require 'anything)

;; fullscreen with wmctrl
;;;;;;;;;;;;;;;;
(defun basimple:switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,fullscreen"))
(global-set-key [f11] 'basimple:switch-full-screen)

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
(put 'narrow-to-region 'disabled nil)

;; workaround for globalmenu
;;;;;;;;;;;;;;;;
;; (defun menuupdate () (menu-bar-mode -1) (menu-bar-mode 1))
(defun menuupdate () 
  ;; (menu-bar-mode -1)(menu-bar-mode 1)
  )
(add-hook 'window-configuration-change-hook 'menuupdate)
















;; my function
(defun insert-standard-date ()
  "Inserts standard date time string." 
  (interactive)
  (insert (format-time-string "%c")))
;; (defun insert-date (format)
;;   "Wrapper around format-time-string." 
;;   (interactive "MFormat: ")
;;   (insert (format-time-string format)))

(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
		 ((not prefix) "%d.%m.%Y")
		 ((equal prefix '(4)) "%Y-%m-%d")
		 ((equal prefix '(16)) "%A, %d. %B %Y")))
	(system-time-locale "de_DE"))
    (insert (format-time-string format))))

;; InsertAnyDate
;; http://www.emacswiki.org/emacs/InsertAnyDate
(require 'calendar)

(defun insdate-insert-any-date (date)
  "Insert DATE using the current locale."
  (interactive (list (calendar-read-date)))
  (insert (calendar-date-string date)))

(defun insdate-insert-date-from (&optional days)
  "Insert date that is DAYS from current."
  (interactive "p*")
  (insert
   (calendar-date-string
    (calendar-gregorian-from-absolute
     (+ (calendar-absolute-from-gregorian (calendar-current-date))
	days)))))

