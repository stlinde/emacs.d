;;; early-init.el --- -*- lexical-binding: t -*-
;;; Commentary
;;; Code:

;; Disable package.el
(setq package-enable-at-startup nil)

(setq gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum)

(setq inhibit-startup-message t)

(setq default-frame-alist
      '((menu-bar-lines . 0)
	(tool-bar-lines . 0)
	(horizontal-scroll-bars)
	(vertical-scroll-bars)))

(provide 'early-init)
;;; early-init.el ends here
