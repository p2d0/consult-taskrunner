;;; packages/consult-taskrunner/tests/consult-taskrunner-test.el -*- lexical-binding: t; -*-

(describe "Consult taskrunner"
  (it "should save tasks that were run"
    (consult-taskrunner--remove-task "LS ")
    (consult-taskrunner--get-candidates-hash taskrunner-command-history-cache)
    (expect (hash-table-count consult-taskrunner-candidates-hash) :to-be 2)
    (taskrunner-run-task "ls")
    (taskrunner-write-cache-file)
    (taskrunner-read-cache-file)
    (consult-taskrunner--get-candidates-hash taskrunner-command-history-cache)
    (expect (hash-table-count consult-taskrunner-candidates-hash) :to-be 3)
    (consult-taskrunner--remove-task "LS ")
    )
	(it "should save cache file"
		(puthash "ls" "~/home/andrew" consult-taskrunner-candidates-hash)
    (consult-taskrunner-write-cache-file)
		)
	)
