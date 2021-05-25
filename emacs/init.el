(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default nil)
(setq integration-with-flycheck t)
(straight-use-package 'use-package)
;; Language server
(load "~/.emacs.d/lisp/lsp_mode.el")
;; Rust stuff
(load "~/.emacs.d/lisp/rust_packages.el")
;; Completion
(load "~/.emacs.d/lisp/completion.el")
;; Visual stuff
(load "~/.emacs.d/lisp/visual.el")
;; Extras
(load "~/.emacs.d/lisp/extras.el")
;; Project stuff
(load "~/.emacs.d/lisp/project.el")
