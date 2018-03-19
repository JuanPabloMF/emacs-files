
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

(custom-set-variables
 '(org-directory "~/Dropbox/orgfiles")
 )

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

(defadvice org-capture-finalize 
    (after delete-capture-frame activate)  
  "Advise capture-finalize to close the frame"  
  (if (equal "capture" (frame-parameter nil 'name))  
      (delete-frame)))
(defadvice org-capture-destroy 
    (after delete-capture-frame activate)  
  "Advise capture-destroy to close the frame"  
  (if (equal "capture" (frame-parameter nil 'name))  
      (delete-frame)))  

(define-key org-mode-map (kbd "C-c >") (lambda () (interactive (org-time-stamp-inactive))))


(add-to-list 'org-structure-template-alist
             '("p" "#+BEGIN_SRC ipython :session :exports both :results raw drawer\n?\n#+END_SRC"))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

(setq org-confirm-babel-evaluate nil)   ;don't prompt me to confirm everytime I want to evaluate a block

;;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))

(use-package multiple-cursors
    :ensure t
    :bind*
    ("C-x C-e" . mc/edit-lines)
    ("C->" . mc/mark-next-like-this)
    ("C-<" . 'mc/mark-previous-like-this)
    :config
    (setq mc/always-run-for-all t)
)

(use-package ace-window
  :ensure t
  :init
  (progn
  (setq aw-scope 'frame)
  (global-set-key (kbd "C-x O") 'other-frame)
    (global-set-key [remap other-window] 'ace-window)
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0))))) 
    ))
