;;; consult-taskrunner.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Andrew Cerkin
;;
;; Author: Andrew Cerkin <https://github.com/andrew>
;; Maintainer: Andrew Cerkin <cerkin-3@yandex.ru>
;; Created: December 15, 2021
;; Modified: December 15, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/andrew/consult-taskrunner
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

(defvar consult-taskrunner-candidates-hash (make-hash-table :test 'equal :weakness nil))
(defvar consult-taskrunner-last-run-command "")

(defun consult-taskrunner--my-face-annotator (cand)
  ""
  (marginalia--fields ((gethash cand consult-taskrunner-candidates-hash) :face 'marginalia-file-name))
  ;; (when-let (sym (intern-soft cand))
  ;;   (concat (propertize " " 'display '(space :align-to center))
  ;;           (propertize "The quick brown fox jumps over the lazy dog" 'face sym)))
  )

(add-to-list 'marginalia-annotator-registry
  '(vtaskrunner consult-taskrunner--my-face-annotator marginalia-annotate-face builtin none))

(defun consult-taskrunner--get-commands (items)
  (let ((kek (list)))
    (maphash (lambda (key val)
							 (appendq! kek (list key))) items)
    kek))

(defun consult-taskrunner--get-candidates-hash (items)
  (maphash (lambda (key val)
						 (--each val
							 (puthash it  (symbol-name key) consult-taskrunner-candidates-hash))) items))

(defun consult-taskrunner--invalidate-cache ()
  (clrhash consult-taskrunner-candidates-hash))


;;;###autoload
(defun consult-taskrunner ()
  ""
  (interactive)
  (taskrunner-read-cache-file)
  (consult-taskrunner--get-candidates-hash taskrunner-command-history-cache)
  (let* ((commands (consult-taskrunner--get-commands consult-taskrunner-candidates-hash))
					(result (consult--read commands
										:category 'vtaskrunner))
					(path (gethash result consult-taskrunner-candidates-hash)))

		(when (file-remote-p path)
			(eshell-command (s-concat path " ls")))

		(taskrunner-run-task result path) ;; NOTE add .projectile file to folder to detect project root
		(setq consult-taskrunner-last-run-command result)
		))

;;;###autoload
(defun consult-taskrunner-rerun-last-task ()
	(interactive)
	(consult-taskrunner--get-candidates-hash taskrunner-command-history-cache)
	(taskrunner-run-task consult-taskrunner-last-run-command (gethash consult-taskrunner-last-run-command consult-taskrunner-candidates-hash)))

;;;###autoload
(defun consult-taskrunner-remove-task ()
	(interactive)
	(consult-taskrunner--get-candidates-hash taskrunner-command-history-cache)
	(let* ((commands (consult-taskrunner--get-commands consult-taskrunner-candidates-hash))
					(result (consult--read commands
										:category 'vtaskrunner)))
		(puthash (intern (gethash result consult-taskrunner-candidates-hash))
			(remove result (gethash (intern (gethash result consult-taskrunner-candidates-hash)) taskrunner-command-history-cache))
			taskrunner-command-history-cache
			)
		)
	(taskrunner-write-cache-file)
	(consult-taskrunner--invalidate-cache)
	(setq consult-taskrunner-last-run-command ""))

(provide 'consult-taskrunner)
;;; consult-taskrunner.el ends here
