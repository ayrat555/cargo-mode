(defcustom cargo-path-to-bin
  (or (executable-find "cargo")
      "~/.cargo/bin/cargo")
  "Path to the cargo executable."
  :type 'file
  :group 'cargo-mode)


(defun cargo-mode--fetch-cargo-tasks (project-root)
  "Fetches list of cargo commands from shell for project in PROJECT-ROOT."
  (let* ((default-directory (or project-root default-directory))
         (cmd (concat (shell-quote-argument cargo-path-to-bin) " --list"))
         (tasks-string (shell-command-to-string cmd))
         (tasks (cdr (split-string tasks-string "\n"))))
    (print tasks)))
