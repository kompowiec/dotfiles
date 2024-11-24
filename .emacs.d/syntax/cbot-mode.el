;; cbot-mode.el
(defvar cbot-mode-hook nil)

(defvar cbot-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for CBOT major mode")

;; Define the keywords and their respective faces
(setq cbot-keywords
      '("point" "object" "produce" "cmdline" "ismovie" "repeat" "drive"))

(setq cbot-buildings
      '("Houston" "SpaceShip" "BotFactory" "ResearchCenter" "RadarStation"
        "ExchangePost" "RepairCenter" "DefenseTower" "AutoLab" "PowerStation"
        "PowerPlant" "NuclearPlant" "Converter" "Derrick" "PowerCaptor"
        "Vault" "StartArea" "GoalArea" "Target1" "AlienNest"))

(setq cbot-messages
      '("DisplayMessage" "DisplayWarning" "DisplayInfo" "DisplayError"))

(setq cbot-portable
      '("TitaniumOre" "UraniumOre" "Titanium" "PowerCell" "NuclearCell"
        "OrgaMatter" "BlackBox" "TNT" "KeyA" "KeyB" "KeyC" "KeyD"))

(setq cbot-robots
      '("PracticeBot" "TargetBot" "WheeledGrabber" "TrackedGrabber" "WingedGrabber"
        "LeggedGrabber" "WheeledSniffer" "TrackedSniffer" "WingedSniffer" "LeggedSniffer"
        "WheeledShooter" "TrackedShooter" "WingedShooter" "LeggedShooter"
        "WheeledOrgaShooter" "TrackedOrgaShooter" "WingedOrgaShooter" "LeggedOrgaShooter"
        "Subber" "recycler" "Shielder" "Thumper" "PhazerShooter" "WheeledBuilder"
        "TrackedBuilder" "WingedBuilder" "LeggedBuilder" "DrawerBot"))

(setq cbot-enemies
      '("AlienQueen" "AlienEgg" "AlienAnt" "AlienSpider" "AlienWasp" "AlienWorm"))

(setq cbot-misc
      '("Me" "Mine" "Barrier" "Wreck" "Ruin"))

(setq cbot-flags
      '("BlueFlag" "RedFlag" "GreenFlag" "YellowFlag" "VioletFlag" "WayPoint" "Target2"
        "EnergySpot" "TitaniumSpot" "UraniumSpot"))

(setq cbot-misc2
      '("ResearchOrgaShooter" "ResearchLegged" "ResearchTracked" "ResearchWinged"
        "ResearchShooter" "ResearchDefenseTower" "ResearchNuclearPlant" "ResearchSubber"
        "ResearchShielder" "ResearchThumper" "ResearchPhazerShooter"))

(setq cbot-bots
      '("detect" "radar" "radarall" "search" "direction" "distance" "distance2d" "wait"
        "move" "turn" "goto" "motor" "jet" "message" "retobject" "errmode" "abstime"
        "ipf" "pendown" "penup" "pencolor" "penwidth" "canresearch" "researched"))

(setq cbot-topo
      '("space" "flatspace" "topo" "flatground"))

(setq cbot-robots2
      '("grab" "drop" "sniff" "thump" "recycle" "shield" "fire" "aim" "build" "canbuild"
        "buildingenabled"))

(setq cbot-objects
      '("factory" "research" "takeoff" "busy"))

(setq cbot-exchange
      '("receive" "send" "testinfo" "deleteinfo"))

(setq cbot-classes
      '("class" "public" "private" "protected" "static" "synchronized" "new"
        "extends" "extern"))

(setq cbot-point
      '("this" "super"))

(setq cbot-str
      '("strlen" "strleft" "strright" "strmid" "strfind" "strval" "strupper" "strlower"))

(setq cbot-files
      '("open" "close" "writeln" "readln" "eof" "deletefile"))

(setq cbot-math
      '("rand" "pow" "sqrt" "floor" "ceil" "round" "trunc" "sin" "cos" "tan" "asin"
        "acos" "atan" "atan2"))

(setq cbot-repeat
      '("try" "catch"))

(setq cbot-error
      '("CBOTDEBUGDD" ".bmp"))

(setq cbot-font-lock-keywords
      (let* (
             (x-keywords-regexp (regexp-opt cbot-keywords 'words))
             (x-buildings-regexp (regexp-opt cbot-buildings 'words))
             (x-messages-regexp (regexp-opt cbot-messages 'words))
             (x-portable-regexp (regexp-opt cbot-portable 'words))
             (x-robots-regexp (regexp-opt cbot-robots 'words))
             (x-enemies-regexp (regexp-opt cbot-enemies 'words))
             (x-misc-regexp (regexp-opt cbot-misc 'words))
             (x-flags-regexp (regexp-opt cbot-flags 'words))
             (x-misc2-regexp (regexp-opt cbot-misc2 'words))
             (x-bots-regexp (regexp-opt cbot-bots 'words))
             (x-topo-regexp (regexp-opt cbot-topo 'words))
             (x-robots2-regexp (regexp-opt cbot-robots2 'words))
             (x-objects-regexp (regexp-opt cbot-objects 'words))
             (x-exchange-regexp (regexp-opt cbot-exchange 'words))
             (x-classes-regexp (regexp-opt cbot-classes 'words))
             (x-point-regexp (regexp-opt cbot-point 'words))
             (x-str-regexp (regexp-opt cbot-str 'words))
             (x-files-regexp (regexp-opt cbot-files 'words))
             (x-math-regexp (regexp-opt cbot-math 'words))
             (x-repeat-regexp (regexp-opt cbot-repeat 'words))
             (x-error-regexp (regexp-opt cbot-error 'words))
             )

        `(
          (,x-keywords-regexp . font-lock-type-face)
          (,x-buildings-regexp . font-lock-constant-face)
          (,x-messages-regexp . font-lock-constant-face)
          (,x-portable-regexp . font-lock-constant-face)
          (,x-robots-regexp . font-lock-constant-face)
          (,x-enemies-regexp . font-lock-constant-face)
          (,x-misc-regexp . font-lock-constant-face)
          (,x-flags-regexp . font-lock-constant-face)
          (,x-misc2-regexp . font-lock-constant-face)
          (,x-bots-regexp . font-lock-keyword-face)
          (,x-topo-regexp . font-lock-keyword-face)
          (,x-robots2-regexp . font-lock-keyword-face)
          (,x-objects-regexp . font-lock-keyword-face)
          (,x-exchange-regexp . font-lock-keyword-face)
          (,x-classes-regexp . font-lock-function-name-face)
          (,x-point-regexp . font-lock-preprocessor-face)
          (,x-str-regexp . font-lock-keyword-face)
          (,x-files-regexp . font-lock-keyword-face)
          (,x-math-regexp . font-lock-keyword-face)
          (,x-repeat-regexp . font-lock-keyword-face)
          (,x-error-regexp . font-lock-warning-face)
          )))

(define-derived-mode cbot-mode fundamental-mode "cbot"
  "Major mode for editing CBOT language files."

  ;; Code for syntax highlighting
  (setq font-lock-defaults '((cbot-font-lock-keywords)))

  ;; Clear memory. No longer needed
  (setq cbot-keywords nil)
  (setq cbot-buildings nil)
  (setq cbot-messages nil)
  (setq cbot-portable nil)
  (setq cbot-robots nil)
  (setq cbot-enemies nil)
  (setq cbot-misc nil)
  (setq cbot-flags nil)
  (setq cbot-misc2 nil)
  (setq cbot-bots nil)
  (setq cbot-topo nil)
  (setq cbot-robots2 nil)
  (setq cbot-objects nil)
  (setq cbot-exchange nil)
  (setq cbot-classes nil)
  (setq cbot-point nil)
  (setq cbot-str nil)
  (setq cbot-files nil)
  (setq cbot-math nil)
  (setq cbot-repeat nil)
  (setq cbot-error nil)
  (setq cbot-font-lock-keywords nil)
  )

;; Associate .cbot files with cbot-mode
(add-to-list 'auto-mode-alist '("\\.cbot\\'" . cbot-mode))

(provide 'cbot-mode)

