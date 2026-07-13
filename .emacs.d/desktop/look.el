;; --- Core UI & Behavior Modernization ---
(setq inhibit-startup-message t
      initial-scratch-message nil
      visible-bell 1
      split-width-threshold nil                 ; Prefer vertical splits
      large-file-warning-threshold 1000000000
      find-file-visit-truename t)

(global-visual-line-mode 1)
(desktop-save-mode 1)
(toggle-frame-maximized)

;; material You
;; 1. Use a clean, modern sans-serif or geometric monospace font
(set-face-attribute 'default nil :font "JetBrains Mono-11")

;; 2. Remove the rigid vertical line dividers for a flatter, Material look
(set-face-background 'vertical-border (face-background 'default))
(set-face-foreground 'vertical-border (face-background 'default))

;; 3. Make the line numbers subtle and flat
(set-face-foreground 'line-number "#6272a4") ; adjust based on your theme
(set-face-background 'line-number-current-line nil)
