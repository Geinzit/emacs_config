(require 'package)                   ; Bring in to the environment all package management functions

;; Don't get this line but here
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; A list of package repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")
			 ("gnu"  . "https://elpa.gnu.org/packages/")))

(package-initialize)                 ; Initializes the package system and prepares it to be used

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(unless (package-installed-p 'quelpa-use-package)
  (package-install 'quelpa-use-package))
(require 'quelpa-use-package)

(unless (package-installed-p 'ligature)
  (package-install 'ligature))
(require 'ligature)

(unless (package-installed-p 'pdf-tools)
  (package-install 'pdf-tools))
(require 'pdf-tools)

(unless (package-installed-p 'markdown-preview-mode)
  (package-install 'markdown-preview-mode))
(require 'markdown-preview-mode)

(unless (package-installed-p 'exec-path-from-shell)
  (package-install 'exec-path-from-shell))
(require 'exec-path-from-shell)

(unless (package-installed-p 'lsp-mode)
  (package-install 'lsp-mode))
(require 'lsp-mode)

(unless (package-installed-p 'lsp-ui)
  (package-install 'lsp-ui))
(require 'lsp-ui)

(unless (package-installed-p 'lsp-ivy)
  (package-install 'lsp-ivy))
(require 'lsp-ivy)

(unless (package-installed-p 'lsp-treemacs)
  (package-install 'lsp-treemacs))
(require 'lsp-treemacs)

(unless (package-installed-p 'company)
  (package-install 'company))
(require 'company)

(unless (package-installed-p 'company-box)
  (package-install 'company-box))
(require 'company-box)

(unless (package-installed-p 'flycheck)
  (package-install 'flycheck))
(require 'flycheck)

(unless (package-installed-p 'flycheck-pos-tip)
  (package-install 'flycheck-pos-tip))
(require 'flycheck-pos-tip)

(unless (package-installed-p 'haskell-mode)
  (package-install 'haskell-mode))
(require 'haskell-mode)



  



(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))









;; font
(set-face-attribute 'default nil :family "Fira Code" :height 120)
;; Enable the www ligature in every possible major mode
(ligature-set-ligatures 't '("www"))
;; Enable ligatures in programming modes                                                           
(ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                     ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                     "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                     "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                     "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                     "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                     "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                     "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                     "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                     "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
(global-ligature-mode 't)













;; custom-set-variables
(custom-set-variables
  '(custom-enabled-themes '(tsdh-dark))
  '(inhibit-startup-screen t)
   '(package-selected-packages
   '(chinese-py chinese-pyim company company-box copilot dash
		editorconfig exec-path-from-shell flycheck
		flycheck-haskell flycheck-pos-tip haskell-mode lsp-ivy
		lsp-mode lsp-treemacs lsp-ui markdown-mode
		markdown-preview-mode pdf-tools quelpa
		quelpa-use-package s tablist)))











;; backup related
(desktop-save-mode 't)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)










;; pdf related
(pdf-tools-install)











;; code indentation
(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.                    
   Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if        
  ;; neither, we use the current indent-tabs-mode                               
  (let ((space-count (how-many-region (point-min) (point-max) "^    "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))










;; Copilot related
(use-package copilot
  :quelpa (copilot :fetcher github
		   :repo "copilot-emacs/copilot.el"
		   :branch "main"
		   :files ("*.el")))
(add-hook 'prog-mode-hook 'copilot-mode)












;; lsp related
(use-package lsp-mode
  :ensure t
  :hook ((c-mode . lsp)
	 (c++-mode . lsp)
	 (python-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-keymap-prefix "C-c l")
  (define-key lsp-mode-map (kbd "C-c k") lsp-command-map)
  (setq lsp-file-watch-threshold 15000))

(use-package lsp-ui
  :ensure t
  :commands (lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-delay 0.5)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)
     












;; Company related
(use-package company
  :ensure t
  :bind ("M-/" . company-complete-common-or-cycle)
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-show-numbers          t
	company-minimum-prefix-length 1
	company-idle-delay            0.5
	company-backends
	'((company-files          
	   company-keywords
	   company-capf
	   company-yasnippet)
	  (company-abbrev company-dabbrev))))

(use-package company-box
  :ensure t
  :after company
  :hook(company-mode . company-box-mode))














;; flycheck setup
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-function
	#'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-indication-mode nil))

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck
  :config
  (flycheck-pos-tip-mode))


	       


	   











;; Python related
(add-hook 'python-mode-hook
  (lambda ()
    (setq indent-tabs-mode nil)
    (infer-indentation-style)))

;;(use-package lsp-pyright
;;  :ensure t
;;  :custom (lsp-pyright-langserver-command "pyright")
;;  :hook (python-mode . (lambda() (require'lsp-pyright) (lsp))))
	    










;; C/C++ related
(defun my-c-mode-common-hook()
  (c-set-offset 'substatement-open 0)
  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)
  1(setq c-indent-level 4)
  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4))
  
(add-hook 'c-mode-common-hook
  (lambda()
    (setq indent-tab-mode nil)
    (setq c-basic-offset 4)
    (setq c-indent-level 4)
    (infer-indentation-style)
    (my-c-mode-common-hook)))










;; Haskell related
(use-package haskell-mode
  :ensure t)
(setq haskell-process-type 'stack-ghci)










;; Markdown related
(require 'markdown-preview-mode)
(setq markdown-command "pandoc --standalone --from=markdown --to=html5 --mathjax --quiet")

(setq markdown-preview-stylesheets
      (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.9.0/github-markdown.min.css"
            "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/default.min.css" "
  <style>
   body { min-width: 80% !important; }

   .markdown-body {
     box-sizing: border-box;
     min-width: 200px;
     max-width: 980px;
     margin: 0 auto;
     padding: 45px;
   }

   @media (max-width: 767px) {
     .markdown-body {
       padding: 15px;
     }
   }
  </style>
"))
(setq markdown-preview-javascript
      (list "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js" "
  <script>
   $(document).on('mdContentChange', function() {
     $('pre code').each(function(i, block) {
       hljs.highlightBlock(block);
     });
   });
  </script>
"))
(setq markdown-preview-script-onupdate "MathJax.typeset()")


