(use-package all-the-icons
  :straight t
  :ensure t)
(use-package doom-modeline
  :straight t
  :ensure t
  :hook (after-init . doom-modeline-mode))
(use-package doom-themes
  :straight t
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(set-face-attribute
 'default nil
 :family "FantasqueSansMono Nerd Font")
