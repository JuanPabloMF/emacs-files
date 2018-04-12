(load-theme 'leuven t)
(setq auto-save-default nil)
(setq make-backup-files nil)

(defun shell-other-window ()
  "Open a `shell' in a new window."
  (interactive)
  (let ((buf (shell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

(use-package counsel 
  :ensure t
  :bind*                           ; load counsel when pressed
  (("M-x"     . counsel-M-x)       ; M-x use counsel
   ("C-x C-f" . counsel-find-file) ; C-x C-f use counsel-find-file
   ("C-x C-r" . counsel-recentf)   ; search recently edited files
   ("C-c l"   . counsel-locate))   ; search for files or else using locate
  )

(use-package helm
  :bind*
  ("M-y" . 'helm-show-kill-ring)
  ("C-x b" . 'helm-mini)
  ("C-x C-b" . 'helm-buffers-list))
(use-package helm-descbinds
  :ensure t
  :bind*
  ("C-c d" . 'helm-descbinds))

(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
        ))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-ac
  :ensure t
  :init (progn
	  (require 'org-ac)
	  (org-ac/config-default)
	  ))

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
	 ((agenda "")
	  (alltodo "")))))

(setq org-agenda-files (list "~/Dropbox/orgfiles/gcal.org"))

(setq org-capture-templates
      '(("a" "Appointment" entry (file  "~/Dropbox/orgfiles/gcal.org" )
	 "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
	("t" "To Do Item" entry (file+headline "~/Dropbox/orgfiles/todos.org" "To Do")
	 "* TODO %?\n%u" :prepend t)
	))

(setq org-confirm-babel-evaluate nil)   ;don't prompt me to confirm everytime I want to evaluate a block

;;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(add-to-list 'org-structure-template-alist
             '("p" "#+BEGIN_SRC ipython :session :exports both :results raw drawer\n?\n#+END_SRC"))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))

(use-package multiple-cursors
    :ensure t
    :bind*
    ("C-x C-l" . mc/edit-lines)
    ("C->" . mc/mark-next-like-this)
    ("C-<" . 'mc/mark-previous-like-this)
    :config
    (setq mc/always-run-for-all t)
)

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package ox-hugo
  :after ox)

(require 'cl)

(defun kill-buffer-force (buffer)
  (set-buffer buffer)
  (set-buffer-modified-p nil)
  (kill-buffer buffer)
  )

(defun with-current-buffer-list (commands)
  "Lets you run commands without leaving newly opened buffers"
  (save-excursion
    (setq pbuffers (mapcar (function buffer-name) (buffer-list)))
    (funcall commands)
    (setq nbuffers (mapcar (function buffer-name) (buffer-list)))
    (setq bdiff (set-difference nbuffers pbuffers))
    (mapcar (function kill-buffer-force) bdiff)
    )
  )

