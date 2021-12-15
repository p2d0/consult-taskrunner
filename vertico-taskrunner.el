;;; vertico-taskrunner.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Andrew Cerkin
;;
;; Author: Andrew Cerkin <https://github.com/andrew>
;; Maintainer: Andrew Cerkin <cerkin-3@yandex.ru>
;; Created: December 15, 2021
;; Modified: December 15, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/andrew/vertico-taskrunner
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'taskrunner)
(require 'consult)
(require 's)
(require 'marginalia)

(defvar vertico-taskrunner-candidates-hash (make-hash-table :test 'equal :weakness nil))
(defvar vertico-taskrunner-last-run-command "")

(defun vertico-taskrunner--my-face-annotator (cand)
  ""
  (marginalia--fields ((gethash cand vertico-taskrunner-candidates-hash) :face 'marginalia-file-name))
  ;; (when-let (sym (intern-soft cand))
  ;;   (concat (propertize " " 'display '(space :align-to center))
  ;;           (propertize "The quick brown fox jumps over the lazy dog" 'face sym)))
  )

(add-to-list 'marginalia-annotator-registry
  '(vtaskrunner vertico-taskrunner--my-face-annotator marginalia-annotate-face builtin none))

(defun vertico-taskrunner--get-commands (items)
  (let ((kek (list)))
    (maphash (lambda (key val)
							 (appendq! kek (list key))) items)
    kek))

(defun vertico-taskrunner--get-candidates-hash (items)
  (maphash (lambda (key val)
						 (--each val
							 (puthash it  (symbol-name key) vertico-taskrunner-candidates-hash))) items))

(defun vertico-taskrunner--invalidate-cache ()
  (clrhash vertico-taskrunner-candidates-hash))


;;;###autoload
(defun vertico-taskrunner ()
  ""
  (interactive)
  (taskrunner-read-cache-file)
  (vertico-taskrunner--get-candidates-hash taskrunner-command-history-cache)
  (let* ((commands (vertico-taskrunner--get-commands vertico-taskrunner-candidates-hash))
					(result (consult--read commands
										:category 'vtaskrunner))
					(path (gethash result vertico-taskrunner-candidates-hash)))
		(when (file-remote-p path)
			(message "TRAMP AZAZA")
			(eshell-command (s-concat path " ls"))
			)
		(taskrunner-run-task result path) ;; NOTE add .projectile file to folder to detect project root
		(setq vertico-taskrunner-last-run-command result)
		))

;;;###autoload
(defun vertico-taskrunner-rerun-last-task ()
	(interactive)
	(vertico-taskrunner--get-candidates-hash taskrunner-command-history-cache)
	(taskrunner-run-task vertico-taskrunner-last-run-command (gethash vertico-taskrunner-last-run-command vertico-taskrunner-candidates-hash)))

;;;###autoload
(defun vertico-taskrunner-remove-task ()
	(interactive)
	(vertico-taskrunner--get-candidates-hash taskrunner-command-history-cache)
	(let* ((commands (vertico-taskrunner--get-commands vertico-taskrunner-candidates-hash))
					(result (consult--read commands
										:category 'vtaskrunner)))
		(puthash (intern (gethash result vertico-taskrunner-candidates-hash))
			(remove result (gethash (intern (gethash result vertico-taskrunner-candidates-hash)) taskrunner-command-history-cache))
			taskrunner-command-history-cache
			)
		)
	(taskrunner-write-cache-file)
	(vertico-taskrunner--invalidate-cache)
	(setq vertico-taskrunner-last-run-command ""))

(provide 'vertico-taskrunner)
;;; vertico-taskrunner.el ends here
