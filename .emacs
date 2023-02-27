(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open")))
 '(buffers-menu-show-status nil)
 '(column-number-mode 1)
 '(company-idle-delay 0.0)
 '(cursor-type 'bar)
 '(menu-bar-mode nil)
 '(menu-prompting nil)
 '(package-selected-packages
   '(impatient-mode pdf-tools fixmee neotree sql-indent auctex company-irony elpy company-c-headers flycheck magit vterm cython-mode multi-vterm projectile iedit yasnippet-snippets yasnippet auto-complete-c-headers auto-complete company))
 '(tab-width 4)
 '(tool-bar-mode nil))

;; To hide the started screen of emacs
(setq inhibit-startup-screen t)

(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "Source Code Pro SemiBold-10"))
;; Fontify current frame
(fontify-frame nil)
;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; To make more slower the mouse scroll
;;(setq mouse-wheel-progressive-speed nil)

;; To manipulate the idetnation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4) ; Assuming you want your tabs to be four spaces wide

(defvaralias 'c-basic-offset 'tab-width)
(setq c-guess-make-basic-offset t)
(setq c-guess-guessed-basic-offset-verbose nil)
(setq js-indent-level 4)
(setq sgml-basic-offset 4)
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

;; Desactivate the backup files
(setq make-backup-files nil)
(setq auto-save-default nil) ; stop creating #autosave# files
(put 'erase-buffer 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(scroll-bar ((t (:foreground "gray3")))))

;; Load the monokai theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'monokai t)

;; To autocomplete brackets
(electric-pair-mode)

;; start package.el with emacs
(require 'package)
;; add MELPA to repository list
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; initialize package.el
(package-initialize)

;; Adding vterm
(add-to-list 'load-path "~/.emacs.d/emacs-libvterm")
(require 'vterm)

;; To run the iedit
(global-set-key (kbd "C-c ;") 'iedit-mode)

(add-hook 'asm-mode-hook
          (lambda () (local-set-key (kbd "C-c ;") 'iedit-mode)))

;;(eval-after-load 'asm-mode
;;  '(define-key LaTeX-mode-map (kbd "C-c ;") 'iedit-mode))

;; To run flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

;; To start auto completeheaders
;; (require 'auto-complete)

;; Add the directories path to the auto complete c headers
;; (require 'auto-complete-c-headers)
;;  (add-to-list 'ac-sources 'ac-source-c-headers)
;;  (setq achead:include-directories
;;   (append '("/usr/include/c++/4.8"
;;             "/usr/include/x86_64-linux-gnu/c++/4.8"
;;             "/usr/include/c++/4.8/backward"
;;             "/usr/lib/gcc/x86_64-linux-gnu/4.8/include"
;;             "/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed"
;;             "/usr/include/x86_64-linux-gnu")
;;             achead:include-directories))


;; Create function autocomplete c headers
;; (defun my:ac-c-headers-init ()
;;   (require 'auto-complete-c-headers)
;;   (add-to-list 'ac-sources 'ac-source-c-headers))

;; (add-hook 'c++-mode-hook 'my:ac-c-headers-init)
;; (add-hook 'c-mode-hook 'my:ac-c-headers-ini)

;; To active company
(require 'company)
(require 'company-c-headers)
(add-hook 'after-init-hook 'global-company-mode)

;; To active compnay c headers
(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/")
(add-to-list 'company-c-headers-path-system "/usr/include/c++/12.2.0/")
(add-to-list 'company-c-headers-path-system "/usr/i686-w64-mingw32/include/")

;; Snippets
(require 'yasnippet)
(yas-global-mode 1)
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(global-set-key (kbd "M-z") 'tab-indent-or-complete)

;; Enable elpy
(elpy-enable)
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

;; Enable Flycheck with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; My Shortcuts
(global-set-key [f5] 'compile)          ; To compile the code
(global-set-key (kbd "C-c C-f") 'display-buffer-other-frame) ; To open a new frame

;; To eneable the neotree
(add-to-list 'load-path "/some/path/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; To start fixmee
(require 'fixmee)
(require 'button-lock)
(global-fixmee-mode 1)


;; To enable pdf-tools and auct
(pdf-tools-install)

;; To enable the column of numbers in tex files
(defun my-display-numbers-hook ()
  (display-line-numbers-mode 1)
  )
(add-hook 'prog-mode-hook 'my-display-numbers-hook)
(add-hook 'text-mode-hook 'my-display-numbers-hook)


;; Fortran 90 config
(setq f90-do-indent 4)
(setq f90-if-indent 4)
(setq f90-type-indent 4)
(setq f90-program-indent 4)
(setq f90-continuation-indent 4)
(setq f90-smart-end 'blink)

(add-hook 'f90-mode-hook
          (lambda ()
            ;; Use default global binding for M-f and M-b.
            (local-set-key (kbd "C-c ;") 'iedit-mode)))

(add-hook 'fortran-mode-hook
          (lambda ()
            ;; Use default global binding for M-f and M-b.
            (local-set-key (kbd "C-c ;") 'iedit-mode)))

;; Fortran settings
(setq fortran-continuation-string "&")
(setq fortran-do-indent 4)
(setq fortran-if-indent 4)
(setq fortran-structure-indent 4)

;; To include doxymacs
(require 'doxymacs)

(add-hook 'c-mode-common-hook 'doxymacs-mode)
(add-hook 'c++-mode-common-hook 'doxymacs-mode)

;; To start the highlight the doxy code
(require 'cc-mode)

(defface doxygen-verbatim-face
  '((default :inherit default))
  "Face used to show Doxygen block regions"
  :group 'font-lock-faces)

(defface doxygen-match-face
  '((default :inherit default)
    (t :underline t))
  "Face used to show Doxygen region start end commands"
  :group 'font-lock-faces)

(defconst custom-font-lock-doc-comments
  `(
    ;; Highlight Doxygen special commands,
    ;;   \cmd or @cmd
    ;; and the non [a-z]+ commands
    ;;   \\ \@ \& \# \< \> \% \" \. \| \-- \--- \~[LanguageId]
    (,(concat
       "\\(?:"
       "[\\@][a-z]+"     ;; typical word Doxygen special @cmd or \cmd
       "\\|"
       ;; non-word commands, e.g. \\ or @\
       "[\\@]\\(?:\\\\\\|@\\|&\\|#\\|<\\|>\\|%\\|\"\\|\\.\\|::\\||\\|---?\\|~[a-z]*\\)"
       "\\)")
     0 ,c-doc-markup-face-name prepend nil)
    ;; Highlight autolinks. These are referring to functions, so we use a different font face
    ;; from the Doxygen special commands.
    (,(concat
       "\\(?:"
       ;; function() or function(int, std::string&, void*) or more complex where we only
       ;; match the first paren, function(x->(), 2*(y+z)).
       "[A-Za-z_0-9]+(\\([A-Za-z_0-9:&*, ]*)\\)?"
       ;; ClassName::memberFcn or the destructor ClassName::~ClassName. Can also do unqualified
       ;; references, e.g. ::member. The parens are optional, ::member(int, int), ::member(a, b).
       ;; We only require matching of first paren to make cases like ::member(x->(), 2*(y+z))
       ;; work. We don't want \::thing to be highlighed as a function, hence reason to look for
       ;; class::member or space before ::member.  Note '#' can be used instead of '::'
       "\\|"
       "\\(?:[A-Za-z_0-9]+\\|\\s-\\)\\(?:::\\|#\\)~?[A-Za-z_0-9]+(?\\(?:[A-Za-z_0-9:&*, \t]*)\\)?"
       ;; file.cpp, foo/file.cpp, etc. Don't want to pickup "e.g." or foo.txt because
       ;; these are not autolinked so look for common C++ extensions.
       "\\|"
       "[A-Za-z_0-9/]+\\.\\(?:cpp\\|cxx\\|cc\\|c\\|hpp\\|hxx\\|hh\\|h\\)"
       "\\)")
     0 font-lock-function-name-face prepend nil)
    ;; Highlight URLs, e.g. http://doxygen.nl/autolink.html note we do this
    ;; after autolinks highlighting (we don't want nl/autolink.h to be file color).
    ("https?://[^[:space:][:cntrl:]]+"
     0 font-lock-keyword-face prepend nil)
    ;; Highlight HTML tags - these are processed by Doxygen, e.g. <b> ... </b>
    (,(concat "</?\\sw"
                "\\("
                (concat "\\sw\\|\\s \\|[=\n\r*.:]\\|"
                        "\"[^\"]*\"\\|'[^']*'")
                "\\)*>")
     0 ,c-doc-markup-face-name prepend nil)
    ;; E-mails, e.g. first.last@domain.com. We don't want @domain to be picked up as a Doxygen
    ;; special command, thus explicitly look for e-mails and given them a different face than the
    ;; Doxygen special commands.
    ("[A-Za-z0-9.]+@[A-Za-z0-9_]+\\.[A-Za-z0-9_.]+"
     0 font-lock-keyword-face prepend nil)
    ;; Quotes: Doxygen special commands, etc. can't be in strings when on same line, e.g.
    ;; "foo @b bar line2 @todo foobar" will not bold or create todo's.
    ("\"[^\"[:cntrl:]]+\""
     0 ,c-doc-face-name prepend nil)

    ("[^\\@]\\([\\@]f.+?[\\@]f\\$\\)"  ;; single line formula but an escaped formula, e.g. \\f[
     1 'doxygen-verbatim-face prepend nil)

    ;; Doxygen verbatim/code/formula blocks should be shown using doxygen-verbatim-face, but
    ;; we can't do that easily, so for now flag the block start/ends
    (,(concat
       "[^\\@]"  ;; @@code shouldn't be matched
       "\\([\\@]\\(?:verbatim\\|endverbatim\\|code\\|endcode\\|f{\\|f\\[\\|f}\\|f]\\)\\)")
     1 'doxygen-match-face prepend nil)

    ;; Here's an attempt to get blocks shown using doxygen-verbatim-face. However, font-lock doesn't
    ;; support multi-line font-locking by default and I'm not sure the best way to make these work.
    ;;
    ;; Doxygen special commands, etc. can't be in verbatim/code blocks
    ;;   @verbatim
    ;;      @cmd  -> not a Doxygen special command
    ;;   @endverbatim
    ;; so set verbatim/code to a different font.  Verbatim/code blocks spans multiple lines and thus
    ;; a refresh of a buffer after editing a verbatim/code block may be required to have the font
    ;; updated.
    ;;("[^\\@][\\@]\\(verbatim\\|code\\)\\([[:ascii:][:nonascii:]]+?\\)[\\@]end\\1"
    ;; 2 'doxygen-verbatim-face prepend nil)
    ;; Doxygen formulas are link verbatim blocks, but contain LaTeX, e.g.
    ;;("[^\\@][\\@]f.+[\\@f]\\$"  ;; single line formula
    ;; 0 'doxygen-verbatim-face prepend nil)
    ;; multi-line formula,
    ;;   \f[ ... \f]     or    \f{ ... \}
    ;;("[^\\@][\\@]f\\(?:{\\|\\[\\)\\([[:ascii:][:nonascii:]]+?\\)[\\@]f\\(?:}\\|\\]\\)"
    ;; 1 'doxygen-verbatim-face prepend nil)

    ))



;; Matches across multiple lines:
;;   /** doxy comments */
;;   /*! doxy comments */
;;   /// doxy comments
;; Doesn't match:
;;   /*******/
(defconst custom-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "/\\(//\\|\\*[\\*!][^\\*!]\\)"
            limit custom-font-lock-doc-comments)))))

(setq-default c-doc-comment-style (quote (custom)))

;; To be able to run webkit you need to install org-mode
;; https://git.savannah.gnu.org/git/emacs/org-mode.git
(add-to-list 'load-path (concat user-emacs-directory "org-mode/lisp/"))
(require 'org)


;; include emacs-webkit
;; https://github.com/akirakyle/emacs-webkit
;; (add-to-list 'load-path (concat user-emacs-directory "emacs-webkit/"))
;; (require 'webkit)

;; To run project tile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; To run the eaf 
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
(require 'eaf)
(require 'eaf-markdown-previewer)
(require 'eaf-camera)
(require 'eaf-browser)
(require 'eaf-file-browser)
(require 'eaf-org-previewer)
(require 'eaf-music-player)
(require 'eaf-video-player)
(require 'eaf-image-viewer)

(eaf-setq eaf-browser-enable-adblocker "true")
