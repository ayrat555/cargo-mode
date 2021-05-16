(defcustom cargo-path-to-bin
  (or (executable-find "cargo")
      "~/.cargo/bin/cargo")
  "Path to the cargo executable."
  :type 'file
  :group 'cargo-mode)

(defvar cargo-mode--last-command nil "Last cargo command.")

(define-derived-mode cargo-mode compilation-mode "Cargo"
  "Major mode for the Cargo buffer."
  (setq buffer-read-only t)
  (setq-local truncate-lines t))

(defun cargo-mode-last-command ()
  "Execute the last cargo-mode task."
  (interactive)
  (if cargo-mode--last-command
      (apply #'cargo-mode--start cargo-mode--last-command)
    (message "Last command is not found.")))

(defun cargo-mode--fetch-cargo-tasks (project-root)
  "Fetches list of raw commands from shell for project in PROJECT-ROOT."
  (interactive "P")
  (let* ((default-directory (or project-root default-directory))
         (cmd (concat (shell-quote-argument cargo-path-to-bin) " --list"))
         (tasks-string (shell-command-to-string cmd))
         (tasks (butlast (cdr (split-string tasks-string "\n")))))
    (delete-dups tasks)))

(defun cargo-mode--available-tasks (project-root)
  "Lists all available tasks in PROJECT-ROOT."
  (interactive "P")
  (let* ((raw_tasks (cargo-mode--fetch-cargo-tasks project-root))
        (result (mapcar #'cargo-mode--format-command raw_tasks)))
    (print result)))

(defun cargo-mode--format-command (raw-command)
  "Splits command and doc string in RAW-COMMAND."
  (let* ((command-words (split-string raw-command))
         (command (car command-words))
         (doc-words (cdr command-words))
         (doc (concat (mapconcat #'identity doc-words " "))))
    (cons command doc)))

(defun cargo-mode--start (name command project-root)
  "Start the cargo-mode process NAME with the cargo command COMMAND from PROJECT-ROOT.
Returns the created process."
  (let* ((buffer (concat "*cargo-mode " name "*"))
         (path-to-bin (shell-quote-argument cargo-path-to-bin))
         (cmd (concat path-to-bin " " command))
         (default-directory (or project-root default-directory)))
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (and project-root
                              buffer-file-name
                              (string-prefix-p project-root (file-truename buffer-file-name)))))
    (setq cargo-mode--last-command (list name cmd project-root))
    (compilation-start cmd 'cargo-mode (lambda(_) buffer))
    (get-buffer-process buffer)))

(defun cargo-mode--project-directory ()
  "Find the project directory."
  (let ((closest-path (or buffer-file-name default-directory)))
    (locate-dominating-file closest-path "Cargo.toml")))

(defun cargo-mode--execute-task ()
  "Select and execute cargo task."
  (interactive)
  (let* ((project-root (cargo-mode--project-directory))
         (available-commands (cargo-mode--available-tasks project-root))
         (test (print available-commands))
         (selected-command (completing-read "select cargo command: " available-commands)))
    (cargo-mode--start "execute" selected-command project-root)))
