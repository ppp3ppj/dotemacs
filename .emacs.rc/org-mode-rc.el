(global-set-key (kbd "C-x a") 'org-agenda)
(global-set-key (kbd "C-c C-x j") #'org-clock-jump-to-current-clock)

;; Linux or Windows for org file 
(cond
 ((eq system-type 'gnu/linux)
  (setq org-agenda-files '("~/Documents/Agenda/")))

 ((eq system-type 'windows-nt)
  (setq org-agenda-files
        (list (expand-file-name "Documents/Agenda/" (getenv "USERPROFILE")))))
)
;; (setq org-agenda-files (list "~/Documents/Agenda/"))
