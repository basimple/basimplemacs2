;;; conf-window.el --- Configuration for window-system
;;
;; Part of the Basimplemacs
(print-buffer "for window-system")
(when (eq system-type 'darwin)
  (setenv "ANDROID_SDK"
	  "/Users/basimple/Deveopment/SDK/android-sdk-mac_x86")
  (setenv "PATH"
	  (concat
	   (getenv "PATH") ":"
	   (getenv "ANDROID_SDK") ":"
	   (concat (getenv "ANDROID_SDK") "/tools/")
	   ))
  )
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

;; Setting for font
;;;;;;;;;;;;;;;;
(if (eq system-type 'darwin)
    (progn
      ;; (set-face-attribute 'default nil :height 100)
      (set-default-font "Monoco-10")
      (set-fontset-font (frame-parameter nil 'font)
			'hangul '("AppleGothic" . "unicode-bmp"))))
(if (eq system-type 'gnu/linux)
    (progn
      ;; (set-face-attribute 'default nil :height 100)
      (set-default-font "Monoco-10")
      (set-fontset-font (frame-parameter nil 'font)
			'hangul '("AppleGothic" . "unicode-bmp"))
      ))
(if (eq system-type 'gnu/linux)
    (progn
      (set-face-attribute 'default nil :font "DejaVu Sans Mono-7.2")
      ;; (set-frame-font "DejaVu Sans Mono-7.5")
      (set-fontset-font "fontset-default" 'hangul '("UnDotum" . "unicode-bmp"))
      (set-fontset-font "fontset-default" 'kana '("Kochi Gothic" . "unicode-bmp"))
      )
  )


(provide 'starter-kit-defuns)
;;; starter-kit-defuns.el ends here