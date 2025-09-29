;; init-eaf.el  -*- lexical-binding: t -*-

(add-to-list 'load-path "~/.config/emacs/straight/repos/emacs-application-framework/")
;; (add-to-list 'load-path "~/.emacs.d/straight/repos/popweb/extension/dict/")


(require 'eaf)
;; (require 'eaf-git)
;; (require 'eaf-browser)
(require 'eaf-pdf-viewer)
(require 'eaf-markdown-previewer)
;; (require 'eaf-markmap)
;; (require 'eaf-video-player)
(require 'eaf-image-viewer)
;; (require 'eaf-org-previewer)
;; (require 'eaf-jupyter)
;; (require 'eaf-music-player)
(require 'eaf-file-manager)
(require 'eaf-pyqterminal)
(require 'eaf-rss-reader)
(require 'eaf-map)

(use-package hide-mode-line
  :straight t
  :hook ((eaf-mode vterm-mode) . hide-mode-line-mode))

;;(require 'popweb-dict)

;; (setq eaf-enable-debug t)
;; (setq eaf-pdf-dark-mode "follow")
(setq eaf-browser-continue-where-left-off t)
(setq eaf-webengine-pc-user-agent "Mozilla/5.0 (X11; Linux x86_64; rv:136.0) Gecko/20100101 Firefox/136.0")
(setq eaf-webengine-default-zoom 1.5)
(setq eaf-webengine-font-size 18)
(setq eaf-webengine-fixed-font-size 18)
(setq eaf-webengine-serif-font-family "LXGW WenKai Screen")
(setq eaf-webengine-font-family "WenQuanYi Micro Hei Mono")
(setq eaf-webengine-fixed-font-family "WenQuanYi Micro Hei Mono")
(setq eaf-browser-enable-adblocker nil)
(setq eaf-jupyter-font-family "JetBrainsMono")
(setq eaf-pyqterminal-font-family "JetBrains Mono")
(setq eaf-pyqterminal-font-size 28)
;; (setq eaf-pyqterminal-cursor-type "hbar")
(setq eaf-jupyter-font-size 20)
(defalias 'browse-web #'eaf-open-browser)

(defun eaf-goto-left-tab ()
  (interactive)
  (centaur-tabs-backward))

(defun eaf-goto-right-tab ()
  (interactive)
  (centaur-tabs-forward))

;; (defun eaf-translate-text (text)
;;   (popweb-dict-youdao-input text))

(provide 'init-eaf)
