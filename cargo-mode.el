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

(defun cargo-mode--fetch-cargo-tasks (project-root)
  "Fetch list of raw commands from shell for project in PROJECT-ROOT."
  (let* ((default-directory (or project-root default-directory))
         (cmd (concat (shell-quote-argument cargo-path-to-bin) " --list"))
         (tasks-string (shell-command-to-string cmd))
         (tasks (butlast (cdr (split-string tasks-string "\n")))))
    (delete-dups tasks)))

(defun cargo-mode--available-tasks (project-root)
  "List all available tasks in PROJECT-ROOT."
  (let* ((raw_tasks (cargo-mode--fetch-cargo-tasks project-root))
         (commands (mapcar #'cargo-mode--split-command raw_tasks)))
    (cargo-mode--format-commands commands)))

(defun cargo-mode--format-commands (commands)
  "Format and concat all COMMANDS."
  (let ((max-length (cargo-mode--max-command-length (car commands) (cdr commands))))
    (mapcar
     (lambda (command) (cargo-mode--concat-command-and-doc command max-length))
     commands)))

(defun cargo-mode--concat-command-and-doc (command-with-doc max-command-length)
  "Concat the COMMAND-WITH-DOC with calcutated.
Space between them is based on MAX-COMMAND-LENGTH."
  (let* ((command (car command-with-doc))
        (doc (cdr command-with-doc))
        (command-length (length command))
        (whitespaces-number (- (+ max-command-length 1) command-length))
        (whitespaces-string (make-string whitespaces-number ?\s)))
    (concat command whitespaces-string "# " doc)))


(defun cargo-mode--split-command (raw-command)
  "Split command and doc string in RAW-COMMAND."
  (let* ((command-words (split-string raw-command))
         (command (car command-words))
         (doc-words (cdr command-words))
         (doc (concat (mapconcat #'identity doc-words " "))))
    (cons command doc)))

(defun cargo-mode--max-command-length (first-arg more-args)
  "Recursively find the longest command.
The current element is FIRST-ARG, remaining args are MORE-ARGS."
  (if more-args
      (let ((max-rest (cargo-mode--max-command-length (car more-args) (cdr more-args)))
            (first-arg-length (length (car first-arg))))
	(if (> first-arg-length max-rest)
	    first-arg-length
	  max-rest))
    (length (car first-arg))))

(defun cargo-mode--start (name command project-root)
  "Start the cargo-mode process with NAME and return the created process.
Cargo command is COMMAND.
The command is  started from directory PROJECT-ROOT."
  (let* ((buffer (concat "*cargo-mode " name "*"))
         (path-to-bin (shell-quote-argument cargo-path-to-bin))
         (cmd (if (string-match-p path-to-bin command)
                  command
                (concat path-to-bin " " command)))
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
         (selected-command (completing-read "select cargo command: " available-commands))
         (command-without-doc (car (split-string selected-command))))
    (cargo-mode--start "execute" command-without-doc project-root)))

(defun cargo-mode-last-command ()
  "Execute the last cargo-mode task."
  (interactive)
  (if cargo-mode--last-command
      (apply #'cargo-mode--start cargo-mode--last-command)
    (message "Last command is not found.")))

(defvar cargo-mode-command-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "e") 'cargo-mode--execute-task)
    (define-key km (kbd "l") 'cargo-mode-last-command)
    km)
  "Cargo-mode keymap after prefix.")
(fset 'cargo-minor-mode-command-map cargo-mode-command-map)

(defvar cargo-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c d") 'cargo-minor-mode-command-map)
    map)
  "Cargo-map keymap.")

(defvar cargo-minor-mode nil)

(define-minor-mode cargo-minor-mode
  "Cargo minor mode. Used to hold keybindings for cargo-mode.
\\{cargo-minor-mode-map}"
  nil " cargo" cargo-minor-mode-map)

(provide 'cargo-mode)
;;; cargo-mode.el ends here
