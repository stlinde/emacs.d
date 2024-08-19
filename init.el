;;; init.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;; User Variables
(defconst shl/emacs-d (file-name-as-directory user-emacs-directory)
  "Directory of emacs.d")

(defvar shl/theme 'modus-operandi-deuteranopia
  "Theme to load on startup.")

(defvar shl/font-weight 'semilight
  "Font weight to use.")

;;;; Elpaca
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;;; Startup
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode))

(use-package gcmh
  :ensure t
  :init
  (add-hook 'after-init-hook (lambda() (gcmh-mode)))
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024)))

;;;; Appearance
(setopt custom-safe-themes t)

;; spacious-padding handles padding around UI elements.
(use-package spacious-padding
  :ensure t
  :defer nil
  :config
  (setq spacious-padding-widths
      '( :internal-border-width 15
         :header-line-width 4
         :mode-line-width 6
         :tab-width 4
         :right-divider-width 30
         :scroll-bar-width 8
         :fringe-width 8))
  (setq spacious-padding-subtle-mode-line
      `( :mode-line-active 'default
         :mode-line-inactive vertical-border))
  (spacious-padding-mode 1))

;; Install theme packages
;; The themes from Prot are very legible and therefore preferred.
(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-to-toggle '(modus-vivendi-deuteranopia modus-operandi-deuteranopia)
        modus-themes-mixed-fonts t
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-variable-pitch-ui t
        modus-themes-slanted-constructs t
        modus-themes-org-blocks 'gray-background)
  (modus-themes-select shl/theme))

(use-package ef-themes
  :ensure t
  :defer nil
  :init
  (defun shl--ef-themes-mode-line ()
  "Tweak the style of the mode lines."
  (ef-themes-with-colors
    (custom-set-faces
     `(mode-line ((,c :background ,bg-active :foreground ,fg-main :box (:line-width 1 :color ,fg-dim))))
     `(mode-line-inactive ((,c :box (:line-width 1 :color ,bg-active)))))))

  (add-hook 'ef-themes-post-load-hook  #'shl--ef-themes-mode-line)
  :config
  (setopt ef-themes-variable-pitch-ui t
          ef-themes-mixed-fonts t))

(use-package fontaine
  :ensure t
  :if (display-graphic-p)
  :hook ((after-init . fontaine-mode)
         (after-init . (lambda ()
                         (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))))
  :config
  (setopt x-underline-at-descent-line nil)
    (setq fontaine-presets
        '((small
           :default-family "Iosevka Comfy Motion"
           :default-height 80
           :variable-pitch-family "Iosevka Comfy Duo")
          (regular) ; like this it uses all the fallback values and is named `regular'
          (medium
           :default-weight medium
           :default-height 115
           :bold-weight extrabold)
          (large
           :inherit medium
           :default-height 150)
          (live-stream
           :default-family "Iosevka Comfy Wide Motion"
           :default-height 150
           :default-weight medium
           :fixed-pitch-family "Iosevka Comfy Wide Motion"
           :variable-pitch-family "Iosevka Comfy Wide Duo"
           :bold-weight extrabold)
          (presentation
           :default-height 180)
          (jumbo
           :default-height 260)
          (t
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "Iosevka Comfy"
           :default-weight medium
           :default-slant normal
           :default-height 100

           :fixed-pitch-family "Iosevka Comfy"
           :fixed-pitch-weight nil
           :fixed-pitch-slant nil
           :fixed-pitch-height 1.0

           :fixed-pitch-serif-family nil
           :fixed-pitch-serif-weight nil
           :fixed-pitch-serif-slant nil
           :fixed-pitch-serif-height 1.0

           :variable-pitch-family "Iosevka Comfy Motion Duo"
           :variable-pitch-weight nil
           :variable-pitch-slant nil
           :variable-pitch-height 1.0

           :mode-line-active-family nil
           :mode-line-active-weight nil
           :mode-line-active-slant nil
           :mode-line-active-height 1.0

           :mode-line-inactive-family nil
           :mode-line-inactive-weight nil
           :mode-line-inactive-slant nil
           :mode-line-inactive-height 1.0

           :header-line-family nil
           :header-line-weight nil
           :header-line-slant nil
           :header-line-height 1.0

           :line-number-family nil
           :line-number-weight nil
           :line-number-slant nil
           :line-number-height 1.0

           :tab-bar-family nil
           :tab-bar-weight nil
           :tab-bar-slant nil
           :tab-bar-height 1.0

           :tab-line-family nil
           :tab-line-weight nil
           :tab-line-slant nil
           :tab-line-height 1.0

           :bold-family nil
           :bold-weight bold
           :bold-slant nil
           :bold-height 1.0

           :italic-family nil
           :italic-weight nil
           :italic-slant italic
           :italic-height 1.0

           :line-spacing nil)))

  (with-eval-after-load 'pulsar
    (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)))

;; Setup fonts - use weights defined in user varables.
(set-face-attribute 'default nil
                    :family "Iosevka Comfy"
                    :height 105
                    :weight shl/font-weight)
(set-face-attribute 'variable-pitch nil
		    :family "Iosevka Comfy Motion Duo"
		    :height 105
		    :weight shl/font-weight)
(set-face-attribute 'fixed-pitch nil
		    :family "Iosevka Comfy"
		    :height 105
		    :weight shl/font-weight)

;; Setup line numbers
(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'org-mode-hook 'display-line-numbers-mode))


;; Setup fill-column
(when (boundp 'display-fill-column-indicator)
  (setq-default indicate-buffer-boundaries 'left)
  (setq-default display-fill-column-indicator-character ?┊)
  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode))

;; Setup modeline
(setq display-time-format " %a %e %b, %H:%M ")
(display-time-mode)

;; Enable battery in modeline, if on laptop
(require 'battery)
(when (and battery-status-function
           (not (string-match-p "N/A"
                                (battery-format "%B"
                                                (funcall battery-status-function)))))
  (display-battery-mode 1))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1))

;;;; Defaults
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (or (memq window-system '(mac ns x pgtk))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
    (exec-path-from-shell-initialize)))

(setopt bookmark-default-file (locate-user-emacs-file ".bookmarks.el")  ;; Hide bookmarks.el, to not clutter user-emacs-dir
        use-short-answers t  ;; Use y and n instead of yes and no.
	buffer-menu-max-size 30
	case-fold-search t  ;; Ignore case while searching
	column-number-mode t  ;; Show column number in modeline
	indent-tabs-mode nil  ;; Ensure that all indentation is with spaces
	create-lockfiles nil  ;; Don't clutter directories with lock files
	auto-save-default nil ;; Don't autosave buffers
	make-backup-files nil  ;; Don't make backups
	vc-make-backup-files nil  ;; Don't make backups of version controlled files
	save-interprogram-paste-before-kill t  ;; Save existing clipboard text into kill ring before replacing.
	scroll-preserve-screen-position 'always  ;; Ensure that scrolling does not move point
        truncate-lines nil ;; Truncate lines when wider than buffer-width
        truncate-partial-width-windows nil)

;; Speed up font rendering for special characters
;; @see https://www.reddit.com/r/emacs/comments/988paa/emacs_on_windows_seems_lagging/
(setq inhibit-compacting-font-caches t)

;; GUI Frames
(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Savehist
(savehist-mode 1)

;; Turn of bell
(setq ring-bell-function 'ignore)

;; Ensure that splitting windows also moves focus
(use-package window
  :ensure nil
  :config
  (defun hsplit-last-buffer ()
    "Focus to the last created horizontal window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))

  (defun vsplit-last-buffer ()
    "Focus to the last created vertical window."
    (interactive)
    (split-window-vertically)
    (other-window 1))
  (global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
  (global-set-key (kbd "C-x 3") 'hsplit-last-buffer))

;;; Frame-isolated buffers
(use-package beframe
  :ensure t
  :hook (after-init . beframe-mode)

  :bind (;; Override the `set-fill-column' that I have no use for.
         ("C-x f" . other-frame-prefix)
         ;; Bind Beframe commands to a prefix key. Notice the -map as I am
         ;; binding keymap here, not a command.
         ("C-c b" . beframe-prefix-map)
         ;; Replace the generic `buffer-menu'.  With a prefix argument, this
         ;; commands prompts for a frame.  Call the `buffer-menu' via M-x if
         ;; you absolutely need the global list of buffers.
         ("C-x C-b" . beframe-buffer-menu)
         ;; Not specific to Beframe, but since it renames frames (by means
         ;; of `beframe-mode') it is appropriate to have this here:
         ("C-x B" . select-frame-by-name))
  :config
  (setopt beframe-functions-in-frames '(project-prompt-project-dir)))

;; Replace text while typing if a region is selected
(add-hook 'after-init-hook 'delete-selection-mode)

;; Automatically revert buffers when file changes on disk
(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
;; (with-eval-after-load 'autorevert
;;   (diminish 'auto-revert-mode))

;; Highlighted region is highlighted with the 'region' face
(add-hook 'after-init-hook 'transient-mark-mode)

;;;; Minibuffer
;; Vertico
(use-package vertico
  :ensure t
  :config
  (vertico-mode))

;; Embark
(use-package embark
  :ensure t
  :config
  (global-set-key (kbd "C-.") 'embark-act)
  (global-set-key (kbd "M-.") 'embark-dwim))

;; Consult
(use-package consult
  :ensure t
  :config
  (global-set-key [remap switch-to-buffer] 'consult-buffer)
  (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
  (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
  (global-set-key [remap goto-line] 'consult-goto-line))

(use-package embark-consult
  :ensure t
  :config
  (with-eval-after-load 'embark
    (require 'embark-consult)
    (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)))

;; Marginalia
(use-package marginalia
  :ensure t
  :defer 1
  :config
  (setq marginalia-max-relative-age 0)  ;; Use absolute time
  (marginalia-mode))
  

(use-package which-key
  :ensure t
  :config
  (add-hook 'after-init-hook 'which-key-mode)
  (setq-default which-key-idle-delay 0.3))

;;;; Editing

;; Ensure that opening parentheses are paired with closing
(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)

;; Newlines
(defun shl/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'shl/newline-at-end-of-line)

;; Subword-mode enables moving in CamelCase and snake_case
(add-hook 'after-init-hook 'global-subword-mode)

;; Expand Region makes for a nicer way to mark stuff
(use-package expand-region
  :ensure t
  :commands er/expand-region
  :bind ("M-h" . er/expand-region))


;; Use avy to jump around the screen
(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "M-j") 'avy-goto-char-timer))

(use-package browse-kill-ring
  :ensure t
  :config
  (setq browse-kill-ring-separator "\f")
  (global-set-key (kbd "M-Y") 'browse-kill-ring)

  (with-eval-after-load 'browse-kill-ring
    (define-key browse-kill-ring-mode-map (kbd "C-g") 'browse-kill-ring-quit)
    (define-key browse-kill-ring-mode-map (kbd "M-n") 'browse-kill-ring-forward)
    (define-key browse-kill-ring-mode-map (kbd "M-p") 'browse-kill-ring-previous))

  (with-eval-after-load 'page-break-lines
    (add-to-list 'page-break-lines-modes 'browse-kill-ring-mode)))

;;;; Development
;;; Terminal
(use-package eat
  :ensure '(eat :type git
                :host codeberg
                :repo "akib/emacs-eat"
                :files ("*.el" ("term" "term/*.el") "*.texi"
                        "*.ti" ("terminfo/e" "terminfo/e/*")
                        ("terminfo/65" "terminfo/65/*")
                        ("integration" "integration/*")
                        (:exclude ".dir-locals.el" "*-tests.el"))))

;;; Syntax
(use-package treesit-auto
  :ensure t
  :config
  (setopt treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;;; LSP
(use-package eglot
  :ensure nil
  :hook ((python-ts-mode . eglot-ensure)
         (c-ts-mode . eglot-ensure)
         (c++-ts-mode . eglot-ensure)))

;;; Completion
(setq tab-always-indent 'complete)

(use-package orderless
  :ensure t
  :config

  (with-eval-after-load 'vertico
    (require 'orderless)
    (setq completion-styles '(orderless basic)))

  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless))))
  (setq completion-cycle-threshold 4))



(use-package corfu
  :ensure t
  :config
  ;; Customize Corfu
  (setq global-corfu-modes '((not erc-mode
                                  circe-mode
                                  help-mode
                                  gud-mode
                                  eat-mode
                                  inferior-python-mode)
                             t)
        corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 1
        corfu-cycle t
        corfu-separator ?\s
        corfu-preselect 'first
        corfu-count 16
        corfu-max-width 120
        corfu-preview-current nil
        corfu-on-exact-match nil
        corfu-quit-at-boundary 'separator
        corfu-quit-no-match 'separator)
  (setq-default corfu-quit-no-match 'separator)
  (setq text-mode-ispell-word-completion nil)

  (global-corfu-mode 1)

  ;; Rebinding keys
  (with-eval-after-load 'corfu
    (define-key corfu-map (kbd "C-y") 'corfu-insert)
    (define-key corfu-map (kbd "RET") nil))

  ;; Corfu history
  (with-eval-after-load 'corfu
    (require 'corfu-history)
    (add-hook 'corfu-mode-hook 'corfu-history-mode)

    (with-eval-after-load 'savehist
      (add-to-list 'savehist-additional-variables 'corfu-history)))

  ;; Popopinfo
  (with-eval-after-load 'corfu
    (require 'corfu-popupinfo)
    (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
    (setq corfu-popupinfo-delay '(0.5 . 1.0))))

(use-package cape
  :ensure t
  :config
  (add-hook 'prog-mode-hook (defun shl/corfu-add-cape-file-h ()
                              (add-hook 'completion-at-point-function #'cape-file -10 t)))
  (add-hook 'org-mode-hook (defun shl/corfu-add-cape-elisp-block-h ()
                             (add-hook 'completion-at-point-functions #'cape-elisp-block 0 t))))

;;; Version Control
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch))

;; Good commit style
(use-package git-commit
  :ensure t)

;;; Misc
(use-package envrc
  :ensure t
  :config
  (with-eval-after-load 'envrc
    (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))
  (add-hook 'after-init-hook #'envrc-global-mode))

;;;; Languages
;;; Python
(defvar shl/ipython-command '("ipython" "-i" "--simple-prompt" "--no-color-info" "--InteractiveShell.display_page=True")
  "Command to initialize the IPython repl.")

(use-package pyvenv
  :ensure t)

(use-package python
  :ensure nil
  :config
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; REPL
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt --no-color-info --InteractiveShell.display_page=True")

  ;; Docstrings
  (setq python-fill-docstring-style 'django)

  (add-hook 'python-mode-hook #'flymake-mode)
  (add-hook 'python-ts-mode-hook #'flymake-mode)

  (setq python-check-command "NO_COLOR=1 rye check"))

(use-package pet
  :ensure t
  :config
  (add-hook 'python-ts-hook 'pet-mode -10))

;;;; Org Mode
(use-package org
  :ensure t
  :config

  ;; Keybindings
  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c c") 'org-capture)
  (define-key global-map (kbd "C-c a") 'org-agenda)

  (with-eval-after-load 'org
    (define-key org-mode-map [remap goto-line] 'consult-org-heading))

  (defvar shl/org-global-prefix-map (make-sparse-keymap)
    "A keymap for global access to org helpers.")

  (define-key shl/org-global-prefix-map (kbd "j") 'org-clock-goto)
  (define-key shl/org-global-prefix-map (kbd "l") 'org-clock-in-last)
  (define-key shl/org-global-prefix-map (kbd "i") 'org-clock-in)
  (define-key shl/org-global-prefix-map (kbd "o") 'org-clock-out)
  (define-key global-map (kbd "C-c o") shl/org-global-prefix-map)

  (setopt org-log-done 'time  ;; Log when task was done
          org-hide-emphasis-markers t  ;; Hide emphasis markers in org-mode documents
          org-catch-invisible-edits 'show
          org-export-coding-system 'utf-8
          org-html-validation-link ""  ;; Don't validate HTML links -- Must be a string
          org-export-kill-product-buffer-when-displayed t
          org-tags-column 80
          org-indent-mode t
          org-id-link-to-org-use-id t) ;; Create ID when linking



  (setopt org-directory "/home/slinde/data/notes/")
  (setopt org-default-notes-file (concat org-directory "inbox.org"))

  ;; Word wrapping is nice in org
  (add-hook 'org-mode-hook (lambda ()
                             (toggle-word-wrap)
                             (visual-line-mode)))


  (setopt org-capture-templates
          `(("t" "todo" entry (file+headline "" "Tasks")  ; "" => `org-default-notes-file'
             "* TODO %?\n%U\n" :clock-resume t)
            ("j" "journal" entry (file+headline "" "Journal")
             "* %T \n\n%?" :clock-resume t :prepend t)
            ("n" "note" entry (file+headline "" "Notes")
             "* %? :NOTE:\n%U\n%a\n" :clock-resume t)))

  ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
  (setopt org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))
          org-refile-use-cache nil
          org-refile-use-outline-path t
          org-outline-path-complete-in-steps nil
          org-refile-allow-creating-parent-nodes 'confirm)  ;; Allow refile to create parent tasks with confirmation

  ;; Ensure that we save all buffers after refiling.
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

  ;; Exclude DONE state tasks from refile targets
  (defun shl/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets."
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (setopt org-refile-target-verify-function 'shl/verify-refile-target)

  (with-eval-after-load 'org-agenda
    (add-to-list 'org-agenda-after-show-hook 'org-fold-show-entry))

  (setopt org-todo-keywords
          (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                  (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
                  (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
          org-todo-repeat-to-state "NEXT")

  (setopt org-todo-keyword-faces
          (quote (("NEXT" :inherit warning)
                  ("PROJECT" :inherit font-lock-string-face))))

  ;; Set Agenda Files
  (setopt org-agenda-files `(,org-directory))

  ;; Re-align tags when window shape changes
  (with-eval-after-load 'org-agenda
    (add-hook 'org-agenda-mode-hook
              (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))


  (setopt org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))

  ;; Views
  (let ((active-project-match "-INBOX/PROJECT"))

    (setq org-stuck-projects
          `(,active-project-match ("NEXT")))

    (setopt org-agenda-compact-blocks t
            org-agenda-sticky t
            org-agenda-start-on-weekday nil
            org-agenda-span 'day
            org-agenda-include-diary nil
            org-agenda-sorting-strategy
            '((agenda habit-down time-up user-defined-up effort-up category-keep)
              (todo category-up effort-up)
              (tags category-up effort-up)
              (search category-up))
            org-agenda-window-setup 'current-window
            org-agenda-custom-commands
            `(("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("g" "GTD"
               ((agenda "" nil)
                (tags "INBOX"
                      ((org-agenda-overriding-header "Inbox")
                       (org-tags-match-list-sublevels nil)))
                (stuck ""
                       ((org-agenda-overriding-header "Stuck Projects")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-tags-match-list-sublevels t)
                        (org-agenda-todo-ignore-scheduled 'future)))
                (tags-todo "-INBOX"
                           ((org-agenda-overriding-header "Next Actions")
                            (org-agenda-tags-todo-honor-ignore-options t)
                            (org-agenda-todo-ignore-scheduled 'future)
                            (org-agenda-skip-function
                             '(lambda ()
                                (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                    (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo ,active-project-match
                           ((org-agenda-overriding-header "Projects")
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-INBOX/-NEXT"
                           ((org-agenda-overriding-header "Orphaned Tasks")
                            (org-agenda-tags-todo-honor-ignore-options t)
                            (org-agenda-todo-ignore-scheduled 'future)
                            (org-agenda-skip-function
                             '(lambda ()
                                (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                                    (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "/WAITING"
                           ((org-agenda-overriding-header "Waiting")
                            (org-agenda-tags-todo-honor-ignore-options t)
                            (org-agenda-todo-ignore-scheduled 'future)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "/DELEGATED"
                           ((org-agenda-overriding-header "Delegated")
                            (org-agenda-tags-todo-honor-ignore-options t)
                            (org-agenda-todo-ignore-scheduled 'future)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-INBOX"
                           ((org-agenda-overriding-header "On Hold")
                            (org-agenda-skip-function
                             '(lambda ()
                                (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                    (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                ;; (tags-todo "-NEXT"
                ;;            ((org-agenda-overriding-header "All other TODOs")
                ;;             (org-match-list-sublevels t)))
                )))))

  (add-hook 'org-agenda-mode-hook 'hl-line-mode)

  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (with-eval-after-load 'org
    (org-clock-persistence-insinuate))
  (setopt org-clock-persist t
          org-clock-in-resume t)


  (setopt org-clock-into-drawer t  ;; Save clock data and notes in the LOGBOOK drawer
          org-log-into-drawer t  ;; Save state changes in the LOGBOOK drawer
          org-clock-out-remove-zero-time-clocks t  ;; Removes clocked tasks with 0:00 duration
          org-time-clocksum-format
          '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))   ;; Show clock sums as hours and minutes, not "n days" etc.
  
  ;; Show the clocked-in task - if any - in the header line
  (defun shl/show-org-clock-in-header-line ()
    (setq-default header-line-format '((" " org-mode-line-string " "))))

  (defun shl/hide-org-clock-from-header-line ()
    (setq-default header-line-format nil))

  (add-hook 'org-clock-in-hook 'shl/show-org-clock-in-header-line)
  (add-hook 'org-clock-out-hook 'shl/hide-org-clock-from-header-line)
  (add-hook 'org-clock-cancel-hook 'shl/hide-org-clock-from-header-line)

  (with-eval-after-load 'org-clock
    (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
    (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))

  ;;; Org babel
  (setopt org-confirm-babel-evaluate nil  ;; Confirm before evaluation
          org-src-window-setup 'current-window  ;; How the source code edit buffer should be evaluatated.
          org-edit-src-persistent-message nil  ;; Show persistent exit help message while editing src examples
          org-src-fontify-natively t  ;; Fontify code in code blocks
          org-src-preserve-indentation t  ;; Preserve leading whitespace characters on export
          org-src-tab-acts-natively t  ;; Use the languages's major-mode binding for TAB in code blocks
          org-edit-src-content-indentation 0)  ;; Don't indent code blocks


  ;; Load org-babel language syntax
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     (seq-filter
      (lambda (pair)
        (locate-library (concat "ob-" (symbol-name (car pair)))))
      '((R . t)
        (ditaa . t)
        (dot . t)
        (emacs-lisp . t)
        (gnuplot . t)
        (latex . t)
        (ledger . t)
        (octave . t)
        (plantuml . t)
        (python . t)
        (screen . nil)
        (shell . t)
        (sql . t)
        (sqlite . t))))))

;; Command that takes a URL from the clipboard and inserts an org mode link with a
;; title of a page found by the URL in the current buffer.
(use-package org-cliplink
  :ensure t)

;; Pomodoro
(use-package org-pomodoro
  :ensure t
  :config
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)))

;; Install org-modern
(use-package org-modern
  :ensure t
  :config
  ;; Settings
  (setopt org-auto-align-tags nil
	  org-tags-column 0
	  org-catch-invisible-edits 'show-and-error
	  org-special-ctrl-a/e t
	  org-insert-heading-respect-content t
	  org-hide-emphasis-markers t
	  org-pretty-entities t
	  org-ellipsis "…"
          org-modern-star 'replace)

  ;; Enable org-modern

  (global-org-modern-mode))

;;;; Misc

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))

;;; init.el ends here
