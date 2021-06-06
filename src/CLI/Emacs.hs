{-# LANGUAGE QuasiQuotes #-}

module CLI.Emacs
  ( setThemeFile
  , reloadTheme
  ) where

import Data.Theme
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.String.Interpolate ( i )
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (takeDirectory)
import System.Process


setThemeFile :: Theme -> IO ()
setThemeFile theme = do
  let path = "/tmp/opdt/emacs/opdt-theme.el"
  createDirectoryIfMissing True $ takeDirectory path
  T.writeFile path (themeFile theme)

reloadTheme :: IO ()
reloadTheme = do
  let emacsCommand = "\"(progn (dolist (i custom-enabled-themes) (disable-theme i)) (load-theme 'opdt t))\""
  callCommand $ "emacsclient -e " ++ emacsCommand

themeFile :: Theme -> Text
themeFile (Theme primaryColors normalColors brightColors editorColors) =
  let
    brightBlack = black brightColors
    brightRed = red brightColors
    brightGreen = green brightColors
    brightYellow = yellow brightColors
    brightBlue = blue brightColors
    brightMagenta = magenta brightColors
    brightCyan = cyan brightColors
    brightWhite = white brightColors

    normalBlack = black normalColors
    normalRed = red normalColors
    normalGreen = green normalColors
    normalYellow = yellow normalColors
    normalBlue = blue normalColors
    normalMagenta = magenta normalColors
    normalCyan = cyan normalColors
    normalWhite = white normalColors

    dark0Soft' = dark0Soft editorColors
    dark1' = dark1 editorColors
    dark2' = dark2 editorColors
    dark3' = dark3 editorColors
    dark4' = dark4 editorColors
    gray' = gray editorColors
    light0Hard' = light0Hard editorColors
    light0' = light0 editorColors
    light1' = light1 editorColors
    light2' = light2 editorColors
    light3' = light3 editorColors
    light4' = light4 editorColors
    brightOrange' = brightOrange editorColors
    normalOrange' = normalOrange editorColors
    darkRed' = darkRed editorColors
    darkBlue' = darkBlue editorColors
    darkAqua' = darkAqua editorColors
    sienna' = sienna editorColors
    lightblue4' = lightblue4 editorColors
    burlywood4' = burlywood4 editorColors
    turquoise4' = turquoise4 editorColors
    currentDiffA' = currentDiffA editorColors
    currentDiffB' = currentDiffB editorColors
    currentDiffC' = currentDiffC editorColors
    currentDiffAncestor' = currentDiffAncestor editorColors
    fineDiffA' = fineDiffA editorColors
    fineDiffB' = fineDiffB editorColors
    fineDiffC' = fineDiffC editorColors
    fineDiffAncestor' = fineDiffAncestor editorColors

  in [i|
(require 'autothemer)

(autothemer-deftheme opdt "Dynamic theme controlled by opdt"

  ;; Specify the color classes used by the theme
  ((((class color) (min-colors \#xFFFFFF)))

  (bright-red "#{brightRed}")
  (bright-green "#{brightGreen}")
  (bright-yellow "#{brightYellow}")
  (bright-blue "#{brightBlue}")
  (bright-magenta "#{brightMagenta}")
  (bright-cyan "#{brightCyan}")
  (bright-orange "#{brightOrange'}")

  (normal-red "#{normalRed}")
  (normal-green "#{normalGreen}")
  (normal-yellow "#{normalYellow}")
  (normal-blue "#{normalBlue}")
  (normal-magenta "#{normalMagenta}")
  (normal-cyan "#{normalCyan}")
  (normal-orange "#{normalOrange'}")

  (dark0           "#{normalBlack}")
  (dark0_soft      "#{dark0Soft'}")
  (dark1           "#{dark1'}")
  (dark2           "#{dark2'}")
  (dark3           "#{dark3'}")
  (dark4           "#{dark4'}")

  (gray            "#{gray'}")

  (light0_hard     "#{light0Hard'}")
  (light0          "#{light0'}")
  (light1          "#{light1'}")
  (light2          "#{light2'}")
  (light3          "#{light3'}")
  (light4          "#{light4'}")

  (bright_red      bright-red)
  (bright_green    bright-green)
  (bright_yellow   bright-yellow)
  (bright_blue     bright-blue)
  (bright_purple   bright-magenta)
  (bright_aqua     bright-cyan)
  (bright_orange   bright-orange)

  (neutral_red      bright-red)
  (neutral_green    bright-green)
  (neutral_yellow   bright-yellow)
  (neutral_blue     bright-blue)
  (neutral_purple   bright-magenta)
  (neutral_aqua     bright-blue)
  (neutral_orange   bright-orange)

  (faded_red       normal-red)
  (faded_green     normal-green)
  (faded_yellow    normal-yellow)
  (faded_blue      normal-blue)
  (faded_purple    normal-magenta)
  (faded_aqua      normal-cyan)
  (faded_orange    normal-orange)

  (dark_red        "#{darkRed'}")
  (dark_blue       "#{darkBlue'}")
  (dark_aqua       "#{darkAqua'}")

  (delimiter-one   normal-blue)
  (delimiter-two   normal-magenta)
  (delimiter-three bright-blue)
  (delimiter-four  normal-orange)
  (white           "\#FFFFFF")
  (black           "\#000000")
  (sienna          "#{sienna'}")
  (lightblue4      "#{lightblue4'}")
  (burlywood4      "#{burlywood4'}")
  (aquamarine4     bright-blue)
  (turquoise4      "#{turquoise4'}")

  (ediff-current-diff-A        "#{currentDiffA'}")
  (ediff-current-diff-B        "#{currentDiffB'}")
  (ediff-current-diff-C        "#{currentDiffC'}")
  (ediff-current-diff-Ancestor "#{currentDiffAncestor'}")
  (ediff-fine-diff-A           "#{fineDiffA'}")
  (ediff-fine-diff-B           "#{fineDiffB'}")
  (ediff-fine-diff-C           "#{fineDiffC'}")
  (ediff-fine-diff-Ancestor    "#{fineDiffAncestor'}")

  (bg dark0)
  (bg_inactive dark0_soft)
  )


  ((default                                   (:background bg :foreground light0))
   (cursor                                    (:background light0))
   (mode-line                                 (:background dark3 :foreground light2 :box nil))
   (mode-line-inactive                        (:background dark1 :foreground light4 :box nil))
   (fringe                                    (:background bg))
   (hl-line                                   (:background dark1))
   (region                                    (:background dark2)) ;;selection
     (secondary-selection                       (:background dark1))
     (minibuffer-prompt                         (:background bg :foreground bright_green :bold t))
     (vertical-border                           (:foreground dark2))
     (internal-border                           (:background dark2))
     (window-divider                            (:foreground dark2))
     (link                                      (:foreground faded_blue :underline t))
     (shadow                                    (:foreground dark4))

     ;; Built-in syntax

     (font-lock-builtin-face                            (:foreground bright_orange))
     (font-lock-constant-face                           (:foreground bright_purple))
     (font-lock-comment-face                            (:foreground dark4))
     (font-lock-function-name-face                      (:foreground bright_yellow))
     (font-lock-keyword-face                            (:foreground bright_red))
     (font-lock-string-face                             (:foreground bright_green))
     (font-lock-variable-name-face                      (:foreground bright_blue))
     (font-lock-type-face                               (:foreground bright_purple))
     (font-lock-warning-face                            (:foreground bright_red :bold t))

     ;; Basic faces
     (error                                             (:foreground bright_red :bold t))
     (success                                           (:foreground bright_green :bold t))
     (warning                                           (:foreground bright_yellow :bold t))
     (alert-low-face                                    (:foreground bright_blue))
     (trailing-whitespace                               (:background bright_red))
     (escape-glyph                                      (:foreground bright_aqua))
     (header-line                                       (:background dark0 :foreground light3 :box nil :inherit nil))
     (highlight                                         (:background dark4 :foreground light0))
     (homoglyph                                         (:foreground bright_yellow))
     (match                                             (:foreground dark0 :background bright_blue))

     ;; Customize faces
     (widget-field                                      (:background dark3))
     (custom-group-tag                                  (:foreground bright_blue :weight 'bold))
     (custom-variable-tag                               (:foreground bright_blue :weight 'bold))

     ;; whitespace-mode

     (whitespace-space                          (:background bg :foreground dark4))
     (whitespace-hspace                         (:background bg :foreground dark4))
     (whitespace-tab                            (:background bg :foreground dark4))
     (whitespace-newline                        (:background bg :foreground dark4))
     (whitespace-trailing                       (:background dark1 :foreground bright_red))
     (whitespace-line                           (:background dark1 :foreground bright_red))
     (whitespace-space-before-tab               (:background bg :foreground dark4))
     (whitespace-indentation                    (:background bg :foreground dark4))
     (whitespace-empty                          (:background nil :foreground nil))
     (whitespace-space-after-tab                (:background bg :foreground dark4))

     ;; RainbowDelimiters

     (rainbow-delimiters-depth-1-face           (:foreground delimiter-one))
     (rainbow-delimiters-depth-2-face           (:foreground delimiter-two))
     (rainbow-delimiters-depth-3-face           (:foreground delimiter-three))
     (rainbow-delimiters-depth-4-face           (:foreground delimiter-four))
     (rainbow-delimiters-depth-5-face           (:foreground delimiter-one))
     (rainbow-delimiters-depth-6-face           (:foreground delimiter-two))
     (rainbow-delimiters-depth-7-face           (:foreground delimiter-three))
     (rainbow-delimiters-depth-8-face           (:foreground delimiter-four))
     (rainbow-delimiters-depth-9-face           (:foreground delimiter-one))
     (rainbow-delimiters-depth-10-face          (:foreground delimiter-two))
     (rainbow-delimiters-depth-11-face          (:foreground delimiter-three))
     (rainbow-delimiters-depth-12-face          (:foreground delimiter-four))
     (rainbow-delimiters-unmatched-face         (:background nil :foreground light0))


     ;; line numbers
     (line-number                               (:foreground dark4 :background dark1))
     (line-number-current-line                  (:foreground bright_orange :background dark2))
     (linum                                     (:foreground dark4 :background dark1))
     (linum-highlight-face                      (:foreground bright_orange :background dark2))
     (linum-relative-current-face               (:foreground bright_orange :background dark2))

     ;; Highlight indentation mode
     (highlight-indentation-current-column-face (:background dark2))
     (highlight-indentation-face                (:background dark1))

     ;; smartparens
     (sp-pair-overlay-face                      (:background dark2))
     (sp-show-pair-match-face                   (:background dark2)) ;; Pair tags highlight
     (sp-show-pair-mismatch-face                (:background bright_red)) ;; Highlight for bracket without pair
     ;;(sp-wrap-overlay-face                     (:inherit 'sp-wrap-overlay-face))
     ;;(sp-wrap-tag-overlay-face                 (:inherit 'sp-wrap-overlay-face))

     ;; elscreen
     (elscreen-tab-background-face              (:background bg :box nil)) ;; Tab bar, not the tabs
     (elscreen-tab-control-face                 (:background dark2 :foreground bright_red :underline nil :box nil)) ;; The controls
     (elscreen-tab-current-screen-face          (:background dark4 :foreground dark0 :box nil)) ;; Current tab
     (elscreen-tab-other-screen-face            (:background dark2 :foreground light4 :underline nil :box nil)) ;; Inactive tab

     ;; ag (The Silver Searcher)
     (ag-hit-face                               (:foreground bright_blue))
     (ag-match-face                             (:foreground bright_red))

     ;; Diffs
     (diff-changed                              (:background nil :foreground light1))
     (diff-added                                (:background nil :foreground bright_green))
     (diff-removed                              (:background nil :foreground bright_red))
     (diff-indicator-changed                    (:inherit 'diff-changed))
     (diff-indicator-added                      (:inherit 'diff-added))
     (diff-indicator-removed                    (:inherit 'diff-removed))

     ;; Ediff
     (ediff-even-diff-A                         (:background dark1))
     (ediff-even-diff-B                         (:background dark1))
     (ediff-even-diff-C                         (:background dark1))
     (ediff-even-diff-Ancestor                  (:background dark1))
     (ediff-odd-diff-A                          (:background dark2))
     (ediff-odd-diff-B                          (:background dark2))
     (ediff-odd-diff-C                          (:background dark2))
     (ediff-odd-diff-Ancestor                   (:background dark2))

     (ediff-fine-diff-A                         (:background ediff-fine-diff-A))
     (ediff-fine-diff-Ancestor                  (:background ediff-fine-diff-Ancestor))
     (ediff-fine-diff-B                         (:background ediff-fine-diff-B))
     (ediff-fine-diff-C                         (:background ediff-fine-diff-C))
     (ediff-current-diff-A                      (:background ediff-current-diff-A))
     (ediff-current-diff-Ancestor               (:background ediff-current-diff-Ancestor))
     (ediff-current-diff-B                      (:background ediff-current-diff-B))
     (ediff-current-diff-C                      (:background ediff-current-diff-C))

     (js2-warning                               (:underline (:color bright_yellow :style 'wave)))
     (js2-error                                 (:underline (:color bright_red :style 'wave)))
     (js2-external-variable                     (:underline (:color bright_aqua :style 'wave)))
     (js2-jsdoc-tag                             (:background nil :foreground gray  ))
     (js2-jsdoc-type                            (:background nil :foreground light4))
     (js2-jsdoc-value                           (:background nil :foreground light3))
     (js2-function-param                        (:background nil :foreground bright_aqua))
     (js2-function-call                         (:background nil :foreground bright_blue))
     (js2-instance-member                       (:background nil :foreground bright_orange))
     (js2-private-member                        (:background nil :foreground faded_yellow))
     (js2-private-function-call                 (:background nil :foreground faded_aqua))
     (js2-jsdoc-html-tag-name                   (:background nil :foreground light4))
     (js2-jsdoc-html-tag-delimiter              (:background nil :foreground light3))

     ;; popup
     (popup-face                                (:underline nil :foreground light1 :background dark1))
     (popup-menu-mouse-face                     (:underline nil :foreground light0 :background faded_green))
     (popup-menu-selection-face                 (:underline nil :foreground light0 :background faded_green))
     (popup-tip-face                            (:underline nil :foreground light2 :background dark2))

     ;; helm
     (helm-M-x-key                              (:foreground bright_orange ))
     (helm-action                               (:foreground light0_hard :underline t))
     (helm-bookmark-addressbook                 (:foreground bright_red))
     (helm-bookmark-directory                   (:foreground bright_purple))
     (helm-bookmark-file                        (:foreground faded_blue))
     (helm-bookmark-gnus                        (:foreground faded_purple))
     (helm-bookmark-info                        (:foreground turquoise4))
     (helm-bookmark-man                         (:foreground sienna))
     (helm-bookmark-w3m                         (:foreground bright_yellow))
     (helm-buffer-directory                     (:foreground white :background bright_blue))
     (helm-buffer-not-saved                     (:foreground faded_red))
     (helm-buffer-process                       (:foreground burlywood4))
     (helm-buffer-saved-out                     (:foreground bright_red))
     (helm-buffer-size                          (:foreground bright_purple))
     (helm-candidate-number                     (:foreground bright_green))
     (helm-ff-directory                         (:foreground bright_purple))
     (helm-ff-executable                        (:foreground turquoise4))
     (helm-ff-file                              (:foreground sienna))
     (helm-ff-invalid-symlink                   (:foreground white :background bright_red))
     (helm-ff-prefix                            (:foreground black :background bright_yellow))
     (helm-ff-symlink                           (:foreground bright_orange))
     (helm-grep-cmd-line                        (:foreground bright_green))
     (helm-grep-file                            (:foreground faded_purple))
     (helm-grep-finish                          (:foreground turquoise4))
     (helm-grep-lineno                          (:foreground bright_orange))
     (helm-grep-match                           (:foreground bright_yellow))
     (helm-grep-running                         (:foreground bright_red))
     (helm-header                               (:foreground aquamarine4))
     (helm-helper                               (:foreground aquamarine4))
     (helm-history-deleted                      (:foreground black :background bright_red))
     (helm-history-remote                       (:foreground faded_red))
     (helm-lisp-completion-info                 (:foreground faded_orange))
     (helm-lisp-show-completion                 (:foreground bright_red))
     (helm-locate-finish                        (:foreground white :background aquamarine4))
     (helm-match                                (:foreground bright_orange))
     (helm-moccur-buffer                        (:foreground bright_aqua :underline t))
     (helm-prefarg                              (:foreground turquoise4))
     (helm-selection                            (:foreground white :background dark2))
     (helm-selection-line                       (:foreground white :background dark2))
     (helm-separator                            (:foreground faded_red))
     (helm-source-header                        (:foreground light2))
     (helm-visible-mark                         (:foreground black :background light3))

     ;;hi-lock-mode
     (hi-black-b                                (:foreground black :weight 'bold))
     (hi-black-hb                               (:foreground black :weight 'bold :height 1.5))
     (hi-blue                                   (:foreground dark0 :background bright_blue))
     (hi-blue-b                                 (:foreground bright_blue :weight 'bold))
     (hi-green                                  (:foreground dark0 :background bright_green))
     (hi-green-b                                (:foreground bright_green :weight 'bold))
     (hi-pink                                   (:foreground dark0 :background bright_purple))
     (hi-red-b                                  (:foreground bright_red :weight 'bold))
     (hi-yellow                                 (:foreground dark0 :background faded_yellow))

     ;; company-mode
     (company-scrollbar-bg                      (:background dark1))
     (company-scrollbar-fg                      (:background dark0_soft))
     (company-tooltip                           (:background dark0_soft))
     (company-tooltip-annotation                (:foreground bright_green))
     (company-tooltip-annotation-selection      (:inherit 'company-tooltip-annotation))
     (company-tooltip-selection                 (:foreground bright_purple :background dark2))
     (company-tooltip-common                    (:foreground bright_blue :underline t))
     (company-tooltip-common-selection          (:foreground bright_blue :underline t))
     (company-preview-common                    (:foreground light0))
     (company-preview                           (:background lightblue4))
     (company-preview-search                    (:background turquoise4))
     (company-template-field                    (:foreground black :background bright_yellow))
     (company-echo-common                       (:foreground faded_red))

     ;; tool tips
     (tooltip                                   (:foreground light1 :background dark1))

     ;; term
     (term-color-black                          (:foreground dark2 :background dark1))
     (term-color-blue                           (:foreground bright_blue :background bright_blue))
     (term-color-cyan                           (:foreground bright_aqua :background bright_aqua))
     (term-color-green                          (:foreground bright_green :background bright_green))
     (term-color-magenta                        (:foreground bright_purple :background bright_purple))
     (term-color-red                            (:foreground bright_red :background bright_red))
     (term-color-white                          (:foreground light1 :background light1))
     (term-color-yellow                         (:foreground bright_yellow :background bright_yellow))
     (term-default-fg-color                     (:foreground light0))
     (term-default-bg-color                     (:background bg))

     ;; message-mode
     (message-header-to                         (:inherit 'font-lock-variable-name-face))
     (message-header-cc                         (:inherit 'font-lock-variable-name-face))
     (message-header-subject                    (:foreground bright_orange :weight 'bold))
     (message-header-newsgroups                 (:foreground bright_yellow :weight 'bold))
     (message-header-other                      (:inherit 'font-lock-variable-name-face))
     (message-header-name                       (:inherit 'font-lock-keyword-face))
     (message-header-xheader                    (:foreground faded_blue))
     (message-separator                         (:inherit 'font-lock-comment-face))
     (message-cited-text                        (:inherit 'font-lock-comment-face))
     (message-mml                               (:foreground faded_green :weight 'bold))

     ;; org-mode
     (org-hide                                  (:foreground dark0))
     (org-level-1                               (:foreground bright_blue))
     (org-level-2                               (:foreground bright_yellow))
     (org-level-3                               (:foreground bright_purple))
     (org-level-4                               (:foreground bright_red))
     (org-level-5                               (:foreground bright_green))
     (org-level-6                               (:foreground bright_aqua))
     (org-level-7                               (:foreground faded_blue))
     (org-level-8                               (:foreground bright_orange))
     (org-special-keyword                       (:inherit 'font-lock-comment-face))
     (org-drawer                                (:inherit 'font-lock-function-name-face))
     (org-column                                (:background dark0))
     (org-column-title                          (:background dark0 :underline t :weight 'bold))
     (org-warning                               (:foreground bright_red :weight 'bold :underline nil :bold t))
     (org-archived                              (:foreground light0 :weight 'bold))
     (org-link                                  (:foreground faded_aqua :underline t))
     (org-footnote                              (:foreground bright_aqua :underline t))
     (org-ellipsis                              (:foreground light4))
     (org-date                                  (:foreground bright_blue :underline t))
     (org-sexp-date                             (:foreground faded_blue :underline t))
     (org-tag                                   (:bold t :weight 'bold))
     (org-list-dt                               (:bold t :weight 'bold))
     (org-todo                                  (:foreground bright_red :weight 'bold :bold t))
     (org-done                                  (:foreground bright_aqua :weight 'bold :bold t))
     (org-agenda-done                           (:foreground bright_aqua))
     (org-headline-done                         (:foreground bright_aqua))
     (org-table                                 (:foreground bright_blue))
     (org-block                                 (:background dark0_soft))
     (org-block-begin-line                      (:background dark1))
     (org-block-end-line                        (:background dark1))
     (org-formula                               (:foreground bright_yellow))
     (org-document-title                        (:foreground faded_blue))
     (org-document-info                         (:foreground faded_blue))
     (org-agenda-structure                      (:inherit 'font-lock-comment-face))
     (org-agenda-date-today                     (:foreground light0 :weight 'bold :italic t))
     (org-scheduled                             (:foreground bright_yellow))
     (org-scheduled-today                       (:foreground bright_blue))
     (org-scheduled-previously                  (:foreground faded_red))
     (org-upcoming-deadline                     (:inherit 'font-lock-keyword-face))
     (org-deadline-announce                     (:foreground faded_red))
     (org-time-grid                             (:foreground faded_orange))
     (org-latex-and-related                     (:foreground bright_blue))

     ;; org-habit
     (org-habit-clear-face                      (:background faded_blue))
     (org-habit-clear-future-face               (:background bright_blue))
     (org-habit-ready-face                      (:background faded_green))
     (org-habit-ready-future-face               (:background bright_green))
     (org-habit-alert-face                      (:background faded_yellow))
     (org-habit-alert-future-face               (:background bright_yellow))
     (org-habit-overdue-face                    (:background faded_red))
     (org-habit-overdue-future-face             (:background bright_red))

     ;; elfeed
     (elfeed-search-title-face                  (:foreground gray  ))
     (elfeed-search-unread-title-face           (:foreground light0))
     (elfeed-search-date-face                   (:inherit 'font-lock-builtin-face :underline t))
     (elfeed-search-feed-face                   (:inherit 'font-lock-variable-name-face))
     (elfeed-search-tag-face                    (:inherit 'font-lock-keyword-face))
     (elfeed-search-last-update-face            (:inherit 'font-lock-comment-face))
     (elfeed-search-unread-count-face           (:inherit 'font-lock-comment-face))
     (elfeed-search-filter-face                 (:inherit 'font-lock-string-face))

     ;; smart-mode-line
     (sml/global                                (:foreground light4 :inverse-video nil))
     (sml/modes                                 (:foreground bright_green))
     (sml/filename                              (:foreground bright_red :weight 'bold))
     (sml/prefix                                (:foreground light1))
     (sml/read-only                             (:foreground bright_blue))
     (persp-selected-face                       (:foreground bright_orange))

     ;; powerline
     (powerline-active0                         (:background dark4 :foreground light0))
     (powerline-active1                         (:background dark3 :foreground light0))
     (powerline-active2                         (:background dark2 :foreground light0))
     (powerline-inactive0                       (:background dark2 :foreground light4))
     (powerline-inactive1                       (:background dark1 :foreground light4))
     (powerline-inactive2                       (:background dark0 :foreground light4))

     ;; isearch
     (isearch                                   (:foreground black :background bright_orange))
     (lazy-highlight                            (:foreground black :background bright_yellow))
     (isearch-fail                              (:foreground light0 :background bright_red))

     ;; markdown-mode
     (markdown-header-face-1                    (:foreground bright_blue))
     (markdown-header-face-2                    (:foreground bright_yellow))
     (markdown-header-face-3                    (:foreground bright_purple))
     (markdown-header-face-4                    (:foreground bright_red))
     (markdown-header-face-5                    (:foreground bright_green))
     (markdown-header-face-6                    (:foreground bright_aqua))

     ;; anzu-mode
     (anzu-mode-line                            (:foreground bright_yellow :weight 'bold))
     (anzu-match-1                              (:background bright_green))
     (anzu-match-2                              (:background faded_yellow))
     (anzu-match-3                              (:background aquamarine4))
     (anzu-replace-to                           (:foreground bright_yellow))
     (anzu-replace-highlight                    (:inherit 'isearch))

     ;; ace-jump-mode
     (ace-jump-face-background                  (:foreground light4 :background bg :inverse-video nil))
     (ace-jump-face-foreground                  (:foreground bright_red :background bg :inverse-video nil))

     ;; ace-window
     (aw-background-face                        (:foreground light1 :background bg :inverse-video nil))
     (aw-leading-char-face                      (:foreground bright_red :background bg :height 4.0))

     ;; show-paren
     (show-paren-match                          (:background dark3 :foreground bright_blue  :weight 'bold))
     (show-paren-mismatch                       (:background bright_red :foreground dark3 :weight 'bold))

     ;; ivy
     (ivy-current-match                         (:foreground light0_hard :weight 'bold :underline t))
     (ivy-minibuffer-match-face-1               (:foreground bright_orange))
     (ivy-minibuffer-match-face-2               (:foreground bright_yellow))
     (ivy-minibuffer-match-face-3               (:foreground faded_orange))
     (ivy-minibuffer-match-face-4               (:foreground faded_yellow))

     ;; ido
     (ido-only-match                            (:inherit 'success))
     (ido-first-match                           (:foreground light0_hard :weight 'bold :underline t))
     (ido-subdir                                (:inherit 'dired-directory))

     ;; magit
     (magit-bisect-bad                          (:foreground faded_red))
     (magit-bisect-good                         (:foreground faded_green))
     (magit-bisect-skip                         (:foreground faded_yellow))
     (magit-blame-heading                       (:foreground light0 :background dark2))
     (magit-branch-local                        (:foreground bright_blue))
     (magit-branch-current                      (:underline bright_blue :inherit 'magit-branch-local))
     (magit-branch-remote                       (:foreground bright_green))
     (magit-cherry-equivalent                   (:foreground bright_purple))
     (magit-cherry-unmatched                    (:foreground bright_aqua))
     (magit-diff-added                          (:foreground bright_green))
     (magit-diff-added-highlight                (:foreground bright_green :inherit 'magit-diff-context-highlight))
     (magit-diff-base                           (:background faded_yellow :foreground light2))
     (magit-diff-base-highlight                 (:background faded_yellow :foreground light0))
     (magit-diff-context                        (:foreground dark1  :foreground light1))
     (magit-diff-context-highlight              (:background dark1 :foreground light0))
     (magit-diff-hunk-heading                   (:background dark3 :foreground light2))
     (magit-diff-hunk-heading-highlight         (:background dark2 :foreground light0))
     (magit-diff-hunk-heading-selection         (:background dark2 :foreground bright_orange))
     (magit-diff-lines-heading                  (:background faded_orange :foreground light0))
     (magit-diff-removed                        (:foreground bright_red))
     (magit-diff-removed-highlight              (:foreground bright_red :inherit 'magit-diff-context-highlight))
     (magit-diffstat-added                      (:foreground faded_green))
     (magit-diffstat-removed                    (:foreground faded_red))
     (magit-dimmed                              (:foreground dark4))
     (magit-hash                                (:foreground bright_blue))
     (magit-log-author                          (:foreground bright_red))
     (magit-log-date                            (:foreground bright_aqua))
     (magit-log-graph                           (:foreground dark4))
     (magit-process-ng                          (:foreground bright_red :weight 'bold))
     (magit-process-ok                          (:foreground bright_green :weight 'bold))
     (magit-reflog-amend                        (:foreground bright_purple))
     (magit-reflog-checkout                     (:foreground bright_blue))
     (magit-reflog-cherry-pick                  (:foreground bright_green))
     (magit-reflog-commit                       (:foreground bright_green))
     (magit-reflog-merge                        (:foreground bright_green))
     (magit-reflog-other                        (:foreground bright_aqua))
     (magit-reflog-rebase                       (:foreground bright_purple))
     (magit-reflog-remote                       (:foreground bright_blue))
     (magit-reflog-reset                        (:foreground bright_red))
     (magit-refname                             (:foreground light4))
     (magit-section-heading                     (:foreground bright_yellow :weight 'bold))
     (magit-section-heading-selection           (:foreground faded_yellow))
     (magit-section-highlight                   (:background dark1))
     (magit-sequence-drop                       (:foreground faded_yellow))
     (magit-sequence-head                       (:foreground bright_aqua))
     (magit-sequence-part                       (:foreground bright_yellow))
     (magit-sequence-stop                       (:foreground bright_green))
     (magit-signature-bad                       (:foreground bright_red :weight 'bold))
     (magit-signature-error                     (:foreground bright_red))
     (magit-signature-expired                   (:foreground bright_orange))
     (magit-signature-good                      (:foreground bright_green))
     (magit-signature-revoked                   (:foreground bright_purple))
     (magit-signature-untrusted                 (:foreground bright_blue))
     (magit-tag                                 (:foreground bright_yellow))

     ;; cider
     (cider-debug-code-overlay-face             (:background dark2 :foreground light0))
     (cider-deprecated-face                     (:background dark2 :foreground bright_orange))
     (cider-enlightened-local-face              (:foreground bright_orange :weight 'bold))
     (cider-error-highlight-face                (:foreground bright_red :underline t :style 'wave))
     (cider-fringe-good-face                    (:foreground neutral_green))
     (cider-instrumented-face                   (:background dark1 :box (:line-width -1 :color bright_red)))
     (cider-result-overlay-face                 (:background dark2 :box (:line-width -1 :color bright_yellow)))
     (cider-test-error-face                     (:background faded_red))
     (cider-test-error-face                     (:background neutral_orange))
     (cider-test-success-face                   (:background bright_green))
     (cider-traced                              (:background bright_aqua))
     (cider-warning-highlight-face              (:foreground bright_yellow :underline t :style 'wave))

     ;; git-gutter
     (git-gutter:modified                       (:background faded_blue :foreground faded_blue))
     (git-gutter:added                          (:background faded_green :foreground faded_green))
     (git-gutter:deleted                        (:background faded_red :foreground faded_red))

     ;; git-gutter+
     (git-gutter+-modified                      (:foreground faded_blue :background faded_blue))
     (git-gutter+-added                         (:foreground faded_green :background faded_green))
     (git-gutter+-deleted                       (:foreground faded_red :background faded_red))

     ;; git-gutter-fringe
     (git-gutter-fr:modified                    (:inherit 'git-gutter:modified))
     (git-gutter-fr:added                       (:inherit 'git-gutter:added))
     (git-gutter-fr:deleted                     (:inherit 'git-gutter:deleted))

     ;; diff-hl
     (diff-hl-change (:background faded_blue :foreground faded_blue))
     (diff-hl-delete (:background faded_red :foreground faded_red))
     (diff-hl-insert (:background faded_green :foreground faded_green))

     ;; flyspell
     (flyspell-duplicate                        (:underline (:color light4 :style 'line)))
     (flyspell-incorrect                        (:underline (:color bright_red :style 'line)))

     ;; langtool
     (langtool-errline                          (:foreground dark0 :background bright_red))
     (langtool-correction-face                  (:foreground bright_yellow :weight 'bold))

     ;; latex
     (font-latex-bold-face                      (:foreground faded_green :bold t))
     (font-latex-italic-face                    (:foreground bright_green :underline t))
     (font-latex-math-face                      (:foreground light3))
     (font-latex-script-char-face               (:foreground faded_aqua))
     (font-latex-sectioning-5-face              (:foreground bright_yellow :bold t))
     (font-latex-sedate-face                    (:foreground light4))
     (font-latex-string-face                    (:foreground bright_orange))
     (font-latex-verbatim-face                  (:foreground light4))
     (font-latex-warning-face                   (:foreground bright_red :weight 'bold))
     (preview-face                              (:background dark1))

     ;; lsp
     (lsp-lsp-flycheck-warning-unnecessary-face (:underline (:color bright_orange :style 'wave)
                                                            :foreground burlywood4))
     (lsp-ui-doc-background                     (:background dark3))
     (lsp-ui-doc-header                         (:background faded_blue))
     (lsp-ui-peek-filename                      (:foreground bright_red))
     (lsp-ui-sideline-code-action               (:foreground bright_yellow))
     (lsp-ui-sideline-current-symbol            (:foreground faded_aqua))
     (lsp-ui-sideline-symbol                    (:foreground gray))

     ;; mu4e
     (mu4e-header-key-face                      (:foreground bright_green :weight 'bold ))
     (mu4e-unread-face                          (:foreground bright_blue :weight 'bold ))
     (mu4e-highlight-face                       (:foreground bright_green))

     ;; shell script
     (sh-quoted-exec                            (:foreground bright_purple))
     (sh-heredoc                                (:foreground bright_orange))

     ;; undo-tree
     (undo-tree-visualizer-active-branch-face   (:foreground light0))
     (undo-tree-visualizer-current-face         (:foreground bright_red))
     (undo-tree-visualizer-default-face         (:foreground dark4))
     (undo-tree-visualizer-register-face        (:foreground bright_yellow))
     (undo-tree-visualizer-unmodified-face      (:foreground bright_aqua))

     ;; widget faces
     (widget-button-pressed-face                (:foreground bright_red))
     (widget-documentation-face                 (:foreground faded_green))
     (widget-field                              (:foreground light0 :background dark2))
     (widget-single-line-field                  (:foreground light0 :background dark2))

     ;; dired+
     (diredp-file-name                          (:foreground light2))
     (diredp-file-suffix                        (:foreground light4))
     (diredp-compressed-file-suffix             (:foreground faded_blue))
     (diredp-dir-name                           (:foreground faded_blue))
     (diredp-dir-heading                        (:foreground bright_blue))
     (diredp-symlink                            (:foreground bright_orange))
     (diredp-date-time                          (:foreground light3))
     (diredp-number                             (:foreground faded_blue))
     (diredp-no-priv                            (:foreground dark4))
     (diredp-other-priv                         (:foreground dark2))
     (diredp-rare-priv                          (:foreground dark4))
     (diredp-ignored-file-name                  (:foreground dark4))

     (diredp-dir-priv                           (:foreground faded_blue  :background dark_blue))
     (diredp-exec-priv                          (:foreground faded_blue  :background dark_blue))
     (diredp-link-priv                          (:foreground faded_aqua  :background dark_aqua))
     (diredp-read-priv                          (:foreground bright_red  :background dark_red))
     (diredp-write-priv                         (:foreground bright_aqua :background dark_aqua))

     ;; diredfl
     (diredfl-autofile-name                     (:foreground light2))
     (diredfl-compressed-file-name              (:foreground light2))
     (diredfl-compressed-file-suffix            (:foreground faded_blue))
     (diredfl-date-time                         (:foreground bright_aqua))
     (diredfl-deletion                          (:foreground bright_red :bold t))
     (diredfl-deletion-file-name                (:foreground bright_red :bold t))
     (diredfl-dir-heading                       (:foreground bright_blue :bold t))
     (diredfl-dir-name                          (:foreground bright_blue))
     (diredfl-dir-priv                          (:foreground bright_blue :background dark_blue))
     (diredfl-exec-priv                         (:foreground bright_blue :background dark_blue))
     (diredfl-executable-tag                    (:foreground bright_green))
     (diredfl-file-name                         (:foreground light2))
     (diredfl-file-suffix                       (:foreground light4))
     (diredfl-symlink                           (:foreground bright_purple))
     (diredfl-flag-mark                         (:foreground bright_yellow :background dark3))
     (diredfl-flag-mark-line                    (:foreground bright_yellow :background dark2))
     (diredfl-ignored-file-name                 (:foreground dark4))
     (diredfl-link-priv                         (:foreground faded_purple))
     (diredfl-no-priv                           (:foreground light2))
     (diredfl-number                            (:foreground bright_yellow))
     (diredfl-other-priv                        (:foreground bright_purple))
     (diredfl-rare-priv                         (:foreground light2))
     (diredfl-read-priv                         (:foreground bright_yellow))
     (diredfl-write-priv                        (:foreground bright_red))
     (diredfl-tagged-autofile-name              (:foreground light4))

     ;; neotree
     (neo-banner-face                           (:foreground bright_purple :bold t))
     (neo-dir-link-face                         (:foreground bright_yellow))
     (neo-expand-btn-face                       (:foreground bright_orange))
     (neo-file-link-face                        (:foreground light0))
     (neo-header-face                           (:foreground bright_purple))
     (neo-root-dir-face                         (:foreground bright_purple :bold t))

     ;; eshell
     (eshell-prompt-face                         (:foreground bright_aqua))
     (eshell-ls-archive-face                     (:foreground light3))
     (eshell-ls-backup-face                      (:foreground light4))
     (eshell-ls-clutter-face                     (:foreground bright_orange :weight 'bold))
     (eshell-ls-directory-face                   (:foreground bright_yellow))
     (eshell-ls-executable-face                  (:weight 'bold))
     (eshell-ls-missing-face                     (:foreground bright_red :bold t))
     (eshell-ls-product-face                     (:foreground faded_red))
     (eshell-ls-readonly-face                    (:foreground light2))
     (eshell-ls-special-face                     (:foreground bright_yellow :bold t))
     (eshell-ls-symlink-face                     (:foreground bright_red))
     (eshell-ls-unreadable-face                  (:foreground bright_red :bold t))

     ;; tabbar
     (tabbar-default                             (:foreground light0 :background dark3 :bold nil :height 1.0 :box (:line-width -5 :color dark3)))
     (tabbar-separator                           (:foreground light0 :background dark3))
     (tabbar-highlight                           (:inherit 'highlight))
     (tabbar-button                              (:foreground dark3 :background dark3 :box nil :line-width 0))
     (tabbar-button-highlight                    (:inherit 'tabbar-button :inverse-video t))
     (tabbar-modified                            (:foreground bright_green :background dark3 :box (:line-width -5 :color dark3)))
     (tabbar-unselected                          (:inherit 'tabbar-default))
     (tabbar-unselected-modified                 (:inherit 'tabbar-modified))
     (tabbar-selected                            (:inherit 'tabbar-default :foreground bright_yellow))
     (tabbar-selected-modified                   (:inherit 'tabbar-selected))

     ;; wgrep
     (wgrep-delete-face                          (:strike-through bright_red))
     (wgrep-done-face                            (:foreground turquoise4))
     (wgrep-face                                 (:underline (:color bright_yellow :style 'line)))
     (wgrep-file-face                            (:inherit 'highlight))
     (wgrep-reject-face                          (:foreground bright_red :bold t))

     ;; hydra
     (hydra-face-red (:foreground bright_red :weight 'bold))
     (hydra-face-blue (:foreground bright_blue :weight 'bold))
     (hydra-face-amaranth (:foreground bright_yellow :weight 'bold))
     (hydra-face-pink (:foreground bright_purple :weight 'bold))
     (hydra-face-teal (:foreground bright_aqua :weight 'bold))

     ;; which-function-mode
     (which-func                                 (:foreground faded_blue))

     ;; auto-dim-other-buffers
     (auto-dim-other-buffers-face                (:background bg_inactive))

     ;; flycheck
     (flycheck-warning                          (:underline (:style 'wave :color bright_yellow)))
     (flycheck-error                            (:underline (:style 'wave :color bright_red)))
     (flycheck-info                             (:underline (:style 'wave :color bright_blue)))
     (flycheck-fringe-warning                   (:foreground bright_yellow))
     (flycheck-fringe-error                     (:foreground bright_red))
     (flycheck-fringe-info                      (:foreground bright_blue))
     (flycheck-error-list-warning               (:foreground bright_yellow :bold t))
     (flycheck-error-list-error                 (:foreground bright_red :bold t))
     (flycheck-error-list-info                  (:foreground bright_blue :bold t))

     ;; tab-bar
     (tab-bar-tab-inactive (:background bg :foreground light0))
     (tab-bar-tab (:background dark2 :foreground light0))
     (tab-bar (:background bg :foreground light0))

     ;; circe
     (circe-prompt-face               (:foreground turquoise4))
     (circe-fool                      (:foreground dark2))
     (circe-highlight-nick-face       (:foreground bright_yellow))
     (circe-server-face               (:foreground dark4))
     (circe-my-message-face           (:foreground bright_aqua))
     (lui-time-stamp-face             (:foreground bright_blue))

     ;; erc
     (erc-action-face            (:inherit 'erc-default-face))
     (erc-bold-face              (:weight 'bold))
     (erc-current-nick-face      (:foreground aquamarine4))
     (erc-dangerous-host-face    (:inherit 'font-lock-warning-face))
     (erc-default-face           (:inherit 'default))
     (erc-direct-msg-face        (:inherit 'erc-default-face))
     (erc-error-face             (:inherit 'font-lock-warning-face))
     (erc-fool-face              (:inherit 'erc-default-face))
     (erc-input-face             (:foreground turquoise4))
     (erc-my-nick-face           (:foreground turquoise4))
     (erc-nick-msg-face          (:inherit 'erc-default-face))
     (erc-notice-face            (:foreground dark4))
     (erc-timestamp-face         (:foreground neutral_green))
     (erc-underline-face         (:underline t))
     (erc-prompt-face            (:foreground turquoise4))
     (erc-pal-face               (:foreground neutral_yellow :weight 'bold))
     (erc-keyword-face           (:foreground bright_orange :weight 'bold))
     (erc-nick-default-face      (:weight 'regular))
     (erc-button                 (:weight 'bold  :underline t))

     ;; gnus
     (gnus-group-mail-1           (:weight 'bold :foreground light0))
     (gnus-group-mail-2           (:inherit 'gnus-group-mail-1))
     (gnus-group-mail-3           (:inherit 'gnus-group-mail-1))
     (gnus-group-mail-1-empty     (:foreground dark4))
     (gnus-group-mail-2-empty     (:inherit 'gnus-group-mail-1-empty))
     (gnus-group-mail-3-empty     (:inherit 'gnus-group-mail-1-empty))
     (gnus-group-news-1           (:inherit 'gnus-group-mail-1))
     (gnus-group-news-2           (:inherit 'gnus-group-news-1))
     (gnus-group-news-3           (:inherit 'gnus-group-news-1))
     (gnus-group-news-4           (:inherit 'gnus-group-news-1))
     (gnus-group-news-5           (:inherit 'gnus-group-news-1))
     (gnus-group-news-6           (:inherit 'gnus-group-news-1))
     (gnus-group-news-1-empty     (:inherit 'gnus-group-mail-1-empty))
     (gnus-group-news-2-empty     (:inherit 'gnus-group-news-1-empty))
     (gnus-group-news-3-empty     (:inherit 'gnus-group-news-1-empty))
     (gnus-group-news-4-empty     (:inherit 'gnus-group-news-1-empty))
     (gnus-group-news-5-empty     (:inherit 'gnus-group-news-1-empty))
     (gnus-group-news-6-empty     (:inherit 'gnus-group-news-1-empty))
     (gnus-group-mail-low         (:inherit 'gnus-group-mail-1 :weight 'normal))
     (gnus-group-mail-low-empty   (:inherit 'gnus-group-mail-1-empty))
     (gnus-group-news-low         (:inherit 'gnus-group-mail-1 :foreground dark4))
     (gnus-group-news-low-empty   (:inherit 'gnus-group-news-low :weight 'normal))
     (gnus-header-content         (:inherit 'message-header-other))
     (gnus-header-from            (:inherit 'message-header-other))
     (gnus-header-name            (:inherit 'message-header-name))
     (gnus-header-newsgroups      (:inherit 'message-header-other))
     (gnus-header-subject         (:inherit 'message-header-subject))
     (gnus-summary-cancelled      (:foreground bright_red :strike-through t))
     (gnus-summary-normal-ancient (:foreground dark4 :inherit 'italic))
     (gnus-summary-normal-read    (:foreground light0))
     (gnus-summary-normal-ticked  (:foreground bright_purple))
     (gnus-summary-normal-unread  (:foreground bright_green :inherit 'bold))
     (gnus-summary-selected       (:foreground bright_blue :weight 'bold))
     (gnus-cite-1                 (:foreground bright_purple))
     (gnus-cite-2                 (:foreground bright_purple))
     (gnus-cite-3                 (:foreground bright_purple))
     (gnus-cite-4                 (:foreground bright_green))
     (gnus-cite-5                 (:foreground bright_green))
     (gnus-cite-6                 (:foreground bright_green))
     (gnus-cite-7                 (:foreground bright_purple))
     (gnus-cite-8                 (:foreground bright_purple))
     (gnus-cite-9                 (:foreground bright_purple))
     (gnus-cite-10                (:foreground faded_orange))
     (gnus-cite-11                (:foreground faded_orange))
     (gnus-signature              (:foreground faded_orange))
     (gnus-x-face                 (:background dark4 :foreground light0))

     ;; web-mode
     (web-mode-doctype-face          (:foreground bright_blue))
     (web-mode-html-tag-bracket-face (:foreground bright_blue))
     (web-mode-html-tag-face         (:foreground bright_blue))
     (web-mode-html-attr-name-face   (:foreground bright_yellow))
     (web-mode-html-attr-equal-face  (:foreground bright_yellow))
     (web-mode-html-attr-value-face  (:foreground bright_green))

     ;; Coq
     (coq-solve-tactics-face      (:inherit 'font-lock-constant-face))
     (coq-cheat-face              (:box (:line-width -1 :color bright_red :style nil)
                                   :foreground bright_red))

     ;; Proof General
     (proof-active-area-face      (:underline t))
     (proof-tacticals-name-face   (:inherit 'font-lock-constant-face))
     (proof-tactics-name-face     (:inherit 'font-lock-constant-face))
     )


  (custom-theme-set-variables 'opdt
                             `(ansi-color-names-vector
                               [,dark1
                                ,bright_red
                                ,bright_green
                                ,bright_yellow
                                ,bright_blue
                                ,bright_purple
                                ,bright_aqua
                                ,light1])
                             `(pdf-view-midnight-colors '(,light0 . ,bg))))

  (provide-theme 'opdt)

                                   |]
