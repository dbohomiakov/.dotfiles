;;; my-doom-solarized-dark-theme.el --- inspired by VS Code Solarized Dark -*- no-byte-compile: t; -*-
(require 'doom-themes)

;;
(defgroup my-doom-solarized-dark-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom my-doom-solarized-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'my-doom-solarized-dark-theme
  :type 'boolean)

(defcustom my-doom-solarized-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'my-doom-solarized-dark-theme
  :type 'boolean)

(defcustom my-doom-solarized-dark-brighter-text nil
  "If non-nil, default text will be brighter."
  :group 'my-doom-solarized-dark-theme
  :type 'boolean)

(defcustom my-doom-solarized-dark-comment-bg my-doom-solarized-dark-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'my-doom-solarized-dark-theme
  :type 'boolean)

(defcustom my-doom-solarized-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'my-doom-solarized-dark-theme
  :type '(choice integer boolean))

;;
(def-doom-theme my-doom-solarized-dark
  "A dark theme inspired by VS Code Solarized Dark"

  ;; name        default   256       16
  (
   (bg         '("#001b26" "#001b26"       nil     ))
   (bg-alt     '("#00212B" "#00212B"       nil     ))
   (base0      '("#073642" "#073642"   "black"     ))
   (base1      '("#03282F" "#03282F" "brightblack" ))
   (base2      '("#00212C" "#00212C" "brightblack" ))
   (base3      '("#13383C" "#13383C" "brightblack" ))
   (base4      '("#56697A" "#56697A" "brightblack" ))
   (base5      '("#405A61" "#405A61" "brightblack" ))
   (base6      '("#96A7A9" "#96A7A9" "brightblack" ))
   (base7      '("#788484" "#788484" "brightblack" ))
   (base8      '("#626C6C" "#626C6C" "white"       ))
   (fg-alt     '("#657b83" "#657b83" "white"       ))
   (fg         (if my-doom-solarized-dark-brighter-text
		   '("#BBBBBB" "#BBBBBB" "brightwhite")
		   '("#839496" "#839496" "brightwhite")))

   (grey       base4)
   (red        '("#9c5060" "#9c5060" "red"          ))
   (orange     '("#9c5060" "#9c5060" "brightred"    ))
   (green      '("#859900" "#99bb66" "green"        ))
   (teal       '("#35a69c" "#33aa99" "brightgreen"  ))
   (yellow     '("#b58900" "#ECBE7B" "yellow"       ))
   (blue       '("#1a6091" "#1a6091" "brightblue"   ))
   (dark-blue  '("#3F88AD" "#2257A0" "blue"         ))
   (magenta    '("#d33682" "#c678dd" "magenta"      ))
   (violet     '("#6c71c4" "#a9a1e1" "brightmagenta"))
   (cyan       '("#2aa198" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#204052" "#5699AF" "cyan"         ))
   ;; Custom colors
   (soft-green '("#4f978b", "#4f978b", "soft-green"))
   (soft-blue '("#3583ab", "#3583ab", "soft-blue"))
   (soft-dark-blue '("#4f978b", "#4f978b", "soft-dark-blue"))
   ;; (soft-yellow     '("#92950C" "#9E9D01" "soft-yellow"))

   ;; face categories -- required for all themes
   (highlight      dark-blue)
   (vertical-bar   (doom-darken base1 0.5))
   (selection      dark-blue)
   (builtin        fg)
   (comments       (if my-doom-solarized-dark-brighter-comments blue base5))
   (doc-comments   grey)
   (constants      fg)
   (functions      soft-blue)
   (keywords       green)
   (methods        cyan)
   (operators      orange)
   (type           soft-blue)
   (strings        soft-green)
   (variables      fg)
   (numbers        fg)
   (region         base0)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    yellow)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright my-doom-solarized-dark-brighter-modeline)
   (-modeline-pad
    (when my-doom-solarized-dark-padded-modeline
      (if (integerp my-doom-solarized-dark-padded-modeline) my-doom-solarized-dark-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((company-tooltip-selection     :background dark-cyan)
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (helm-selection :inherit 'bold
                   :background selection
                   :distant-foreground bg
                   :extend t)

   (font-lock-comment-face
    :foreground comments
    :background (if my-doom-solarized-dark-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)
   (font-lock-keyword-face
    :weight 'bold
    :foreground keywords)
   (font-lock-constant-face
    :weight 'bold
    :foreground constants)

   ;; Centaur tabs
   (centaur-tabs-active-bar-face :background blue)
   (centaur-tabs-modified-marker-selected :inherit 'centaur-tabs-selected
                                          :foreground blue)
   (centaur-tabs-modified-marker-unselected :inherit 'centaur-tabs-unselected
                                            :foreground blue)
   ;; Doom modeline
   (doom-modeline-bar :background blue)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   (tooltip              :background bg-alt :foreground fg)
   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face    :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override)   :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)

   ;; outline (affects org-mode)
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground teal)
   ((outline-4 &override) :foreground (doom-darken blue 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.2))
   ((outline-7 &override) :foreground (doom-darken blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))

   ;; org-mode
   ((org-block &override) :background base0)
   ((org-block-begin-line &override) :foreground comments :background base0)
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))
  ;; --- extra variables ---------------------
  ;; ()
  )

;;; my-doom-solarized-dark-theme.el ends here
