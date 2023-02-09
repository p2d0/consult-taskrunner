;;; consult-taskrunner.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Andrew Cerkin
;;
;; Author: Andrew Cerkin <https://github.com/patriot720>
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
(require 'doom-lib)

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

(defun consult-taskrunner--invalidate-cache ()
  (clrhash consult-taskrunner-candidates-hash))


;;;###autoload
(defun consult-taskrunner ()
  ""
  (interactive)
  (consult-taskrunner-read-cache-file)
  (let* ((commands (consult-taskrunner--get-commands consult-taskrunner-candidates-hash))
	  (result (consult--read commands
		    :category 'vtaskrunner))
	  (path (gethash result consult-taskrunner-candidates-hash (projectile-project-root))))

    (when (and path (file-remote-p path) )
      (eshell-command (s-concat path " ls"))) ;; TODO map remote path to local

    (taskrunner-run-task result path) ;; NOTE add .projectile file to folder to detect project root
    (puthash result path consult-taskrunner-candidates-hash)
    (consult-taskrunner-write-cache-file)
    (setq consult-taskrunner-last-run-command result)))

;;;###autoload
(defun consult-taskrunner-rerun-last-task ()
  (interactive)
  (taskrunner-run-task consult-taskrunner-last-run-command (gethash consult-taskrunner-last-run-command consult-taskrunner-candidates-hash)))

;;;###autoload
(defun consult-taskrunner-remove-task ()
  (interactive)
  (let* ((commands (consult-taskrunner--get-commands consult-taskrunner-candidates-hash))
	  (task (consult--read commands
		  :category 'vtaskrunner)))
    (consult-taskrunner--remove-task task))
  (setq consult-taskrunner-last-run-command ""))

(defun consult-taskrunner--remove-task (task)
  (remhash task consult-taskrunner-candidates-hash)
  ;; (let* ((task-dir (gethash task consult-taskrunner-candidates-hash)))
  ;;   (when task-dir
  ;;     (puthash (intern task-dir)
  ;; 	(remove task (gethash (intern task-dir) taskrunner-command-history-cache))
  ;; 	taskrunner-command-history-cache)))
  (consult-taskrunner-write-cache-file))

(defun consult-taskrunner-write-cache-file ()
  "Save all tasks in the cache to the cache file in Emacs user directory."
  (let ((consult-taskrunner-cache-filepath (expand-file-name "consult-taskrunner-tasks.eld" user-emacs-directory)))
    (write-region (format "%s%s\n" taskrunner--cache-file-header-warning
                    (prin1-to-string consult-taskrunner-candidates-hash))
      nil
      consult-taskrunner-cache-filepath)))

(defun consult-taskrunner-read-cache-file ()
  "Read the task cache file and initialize the task caches with its contents."
  (with-temp-buffer
    (let ((taskrunner-cache-filepath (expand-file-name "consult-taskrunner-tasks.eld" user-emacs-directory))
           (file-tasks))
      (when (file-exists-p taskrunner-cache-filepath)
        (with-temp-buffer
          (insert-file-contents taskrunner-cache-filepath)
          (setq file-tasks (car (read-from-string (buffer-string))))
          ;; Load all the caches with the retrieved info
          (setq consult-taskrunner-candidates-hash  file-tasks))))))

(provide 'consult-taskrunner)
;;; consult-taskrunner.el ends here
