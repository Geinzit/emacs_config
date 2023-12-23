(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(tsdh-dark))
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(copilot editorconfig dash s quelpa-use-package quelpa chinese-py chinese-pyim markdown-preview-eww markdown-preview-mode markdown-mode haskell-mode pdf-tools tablist)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; tramp
(setq tramp-verbose 10)

(require 'quelpa-use-package)
(require 'use-package)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; fonts
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



;; chinese input method
(prefer-coding-system 'utf-8)
(setq default-input-method "chinese-py")

(pdf-tools-install) ; Standard activation command

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

(add-hook 'python-mode-hook
  (lambda ()
    (setq indent-tabs-mode nil)
    (infer-indentation-style)))

;; c/c++

(defun my-c-mode-common-hook ()
   ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
   (c-set-offset 'substatement-open 0)
   ;; other customizations can go here

   (setq c++-tab-always-indent t)
   (setq c-basic-offset 4)                  ;; Default is 2
   (setq c-indent-level 4)                  ;; Default is 2

   (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
   (setq tab-width 4)
)

(add-hook 'c-mode-common-hook
  (lambda()
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 4)
    (setq c-indent-level 4)
    (infer indentation style)
    ;; (my-c-mode-common-hook)
    ))

;;markdown

(use-package markdown-mode
  :ensure t)
(require 'markdown-preview-eww)
(require 'markdown-preview-mode)
(setq markdown-command "pandoc --standalone --from=markdown  --to=html5 --mathjax  --quiet")

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
;; save desktop
(setq desktop-save-mode 1)

;; backup files managing

(setq backup-directory-alist `(("." . "~/.saves")))

(setq backup-by-copying t)

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; copilot setup
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el")))

(add-hook 'prog-mode-hook 'copilot-mode)
;; you can utilize :map :hook and :config to customize copilot
