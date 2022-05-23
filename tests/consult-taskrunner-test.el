;;; packages/consult-taskrunner/tests/consult-taskrunner-test.el -*- lexical-binding: t; -*-

(describe "Consult taskrunner"
	(it "should save tasks that were run"
		(consult-taskrunner-read-cache-file)
		(consult-taskrunner--remove-task "ls")
		(expect (hash-table-count consult-taskrunner-candidates-hash) :to-be 1)
		(taskrunner-run-task "ls")
    (puthash "ls" (projectile-project-root) consult-taskrunner-candidates-hash)
		(expect (gethash "ls" consult-taskrunner-candidates-hash) :to-match "home")
		(expect (hash-table-count consult-taskrunner-candidates-hash) :to-be 2)
		(consult-taskrunner--remove-task "LS ")
		)
	(it "should save cache file"
		(puthash "ls" "~/home/andrew" consult-taskrunner-candidates-hash)
		(consult-taskrunner-write-cache-file)
		)
	)
