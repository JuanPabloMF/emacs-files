(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) 
(package-initialize)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)



;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
(org-babel-load-file (expand-file-name "~/Dropbox/orgfiles/myinit.org"))
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" default)))
 '(org-directory "~/Dropbox/orgfiles")
 '(package-selected-packages
   (quote
    (dired-quick-sort dired-filter visual-regexp-steroids visual-regexp helm-org-rifle shell-here multishell ox-hugo org-plus-contrib org auto-org-md avy ein ace-mc exec-path-from-shell ob-ipython elpy projectile ivy counsel dashboard use-package try org-jira org-bullets org-ac magit leuven-theme hydra helm-descbinds))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'ob-ipython)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   ;; other languages..
   ))
