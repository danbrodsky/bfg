;;; ~/.doom.d/modules/private/tools/bgf/autoload/gdb.el -*- lexical-binding: t; -*-

(defun gud-gdb-script-filter (proc string)
  ;; Here's where the actual buffer insertion is done
  (let (output process-window)
    (if (buffer-name (process-buffer proc))
        (if gud-filter-defer-flag
            ;; If we can't process any text now,
            ;; save it for later.
            (setq gud-filter-pending-text
                  (concat (or gud-filter-pending-text "") string))

          ;; If we have to ask a question during the processing,
          ;; defer any additional text that comes from the debugger
          ;; during that time.
          (let ((gud-filter-defer-flag t))
            ;; Process now any text we previously saved up.
            (if gud-filter-pending-text
                (setq string (concat gud-filter-pending-text string)
                      gud-filter-pending-text nil))
            (with-current-buffer (process-buffer proc)
              ;; If we have been so requested, delete the debugger prompt.
              (save-restriction
                (widen)
                (if (marker-buffer gud-delete-prompt-marker)
                    (let ((inhibit-read-only t))
                      (delete-region (process-mark proc)
                                     gud-delete-prompt-marker)
                      (comint-update-fence)
                      (set-marker gud-delete-prompt-marker nil)))
                ;; Save the process output, checking for source file markers.
                (setq output (gud-gdbmi-fetch-lines-filter string))
                ;; Check for a filename-and-line number.
                ;; Don't display the specified file
                ;; unless (1) point is at or after the position where output appears
                ;; and (2) this buffer is on the screen.
                (setq process-window
                      (and gud-last-frame
                           (>= (point) (process-mark proc))
                           (get-buffer-window (current-buffer)))))

              ;; Let the comint filter do the actual insertion.
              ;; That lets us inherit various comint features.
              (comint-output-filter proc output))

            ;; Put the arrow on the source line.
            ;; This must be outside of the save-excursion
            ;; in case the source file is our current buffer.
            (if process-window
                (with-selected-window process-window
                  (gud-display-frame))
              ;; We have to be in the proper buffer, (process-buffer proc),
              ;; but not in a save-excursion, because that would restore point.
              (with-current-buffer (process-buffer proc)
                (gud-display-frame))))

          ;; If we deferred text that arrived during this processing,
          ;; handle it now.
          (if gud-filter-pending-text
              (gud-filter proc ""))))))


(defun gud-gdb-script-completions (context command)
  "Completion table for gdb script from existing gdbmi process"
  ;; (process-send-string (get-buffer-process gud-comint-buffer)
  ;;                      (concat "complete " context command "\n"))))
  ;; (accept-process-output (get-buffer-process gud-comint-buffer))
  (defvar gud-gdb-fetch-lines-string)
  (defvar gud-gdb-fetch-lines-break)
  (defvar gud-gdb-fetched-lines)
  (defvar gud-gdb-fetch-lines-in-progress)
  (let ((gud-gdb-fetch-lines-in-progress t)
        (gud-gdb-fetch-lines-string nil)
        (gud-gdb-fetch-lines-break (length context))
        (gud-gdb-fetched-lines nil))
    (set-process-filter (get-buffer-process gud-comint-buffer) #'gud-gdb-script-filter)
    (with-current-buffer (gdb-get-buffer 'gdb-partial-output-buffer)
      (gdb-input (concat "complete " context command)
                 (lambda () (setq gud-gdb-fetch-lines-in-progress nil)))
      (while gud-gdb-fetch-lines-in-progress
        (accept-process-output (get-buffer-process gud-comint-buffer))))
    (set-process-filter (get-buffer-process gud-comint-buffer) #'gud-filter)
    (gud-gdb-completions-1 (append gud-gdb-script-history gud-gdb-fetched-lines))))

(defun gud-gdb-script-completion-at-point ()
  "same as `gud-gdb-completion-at-point' but read current line in buffer only"
  (let ((end (point))
        (start (line-beginning-position)))
    ;; (save-excursion
    ;;   (skip-chars-backward "^ " (comint-line-beginning-position))
    ;;   (point))))
    ;; FIXME: `gud-gdb-run-command-fetch-lines' has some nasty side-effects on
    ;; the buffer (via `gud-delete-prompt-marker'): it removes the prompt and
    ;; then re-adds it later, thus messing up markers and overlays along the
    ;; way (bug#18282).
    ;; We use an "insert-before" marker for `start', since it's typically right
    ;; after the prompt, which works around the problem, but is a hack (and
    ;; comes with other downsides, e.g. if completion adds text at `start').
    (list (copy-marker start t) end
          (completion-table-dynamic
           (apply-partially gud-gdb-completion-function
                            (buffer-substring (line-beginning-position)
                                              start))))))


(defun new-gdb-script-buffer ()
  "create a new gdb script buffer"
  (defvar target-gdb-path (concat target-file-parent-path "script.gdb"))
  (with-selected-window (with-current-buffer "*gud*" (split-window-horizontally))
    (progn
      (unless (file-exists-p target-gdb-path)
        (copy-file (concat (file-name-directory load-file-name) "script.gdb") target-gdb-path))
      (find-file target-gdb-path)
      (rename-buffer "*gdb-script*")
      (add-hook 'gud-mode-hook #'company-mode)
      (gdb-script-mode)
      (add-hook 'completion-at-point-functions #'gud-gdb-script-completion-at-point
                nil 'local)
      (set (make-local-variable 'gud-gdb-completion-function) 'gud-gdb-script-completions)
      (set (make-local-variable 'gud-gdb-script-history) (gdb-history-load))
      (set (make-local-variable 'gud-minor-mode) 'gdbmi))))

(defun send-buffer-to-gdb ()
  "send all commands in gdb script buffer to gdb"
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (send-line-to-gdb)
    (forward-line 1)))

(defun send-line-to-gdb ()
  "send line at cursor to gdb"
  (interactive)
  (let*
      ((start (line-beginning-position))
       (end (line-end-position))
       (command (buffer-substring-no-properties start end)))
    (write-region (concat command "\n") nil "~/.gdb_history" t)
    (process-send-string "*gud*" (concat command "\n"))
    (push gud-gdb-script-history command)))

;; (defun gdb-history-store (command)
;;   "save gdb command history for last command"
;;   )

(defun gdb-history-load ()
  "load gdb command history into gdb autocompletion var"
  (with-temp-buffer
    (insert-file-contents "~/.gdb_history")
    (split-string (buffer-string) "\n" t)))

(defun save-gdb-out ()
  "save output of gdb buffer to tmp file"
  ()
  )

(defun view-gdb-out ()
  "open buffer with all gdb output seen"
  ()
  )

(defun new-gef-instance ()
  "create new gef instance under gud mode"
  (gdb "gdb -i=mi"))

(defun buffer-pty-name (buffer-name)
  "Get the pty of an existing buffer"
  (process-tty-name (get-buffer-process buffer-name)))

(defun create-pty-buffer (name)
  "Hackish way to open a pretty buffer with a pty"
  (defvar pty-buffer-name (concat "*" name "*"))
  (unless (get-buffer pty-buffer-name)
    (ansi-term "/usr/bin/cat" name))
  (switch-to-buffer pty-buffer-name))

;;;###autoload
(defun bgf-run ()
  "Run exploit on target program in gef"
  (interactive)
  (process-send-string "*gud*" (concat "kill\n"))
  (sit-for 1)
  (with-current-buffer "*exploit*" (python-shell-send-buffer))
  (process-send-string "*gud*" (concat "target remote 0.0.0.0:9999\n")))

;;;###autoload
(defun bgf ()
  "Start configured gdb session"
  (interactive)
  (defvar target-file-path (read-file-name "Target: "))
  (defvar target-file-parent-path (file-name-directory target-file-path))
  (defvar target-exploit-path (concat target-file-parent-path "x.py"))
  (setq python-shell-interpreter "/home/esc/python2env/bin/ipython")
  (setq python-shell-completion-native-disabled-interpreters '("python"))
  (pyvenv-activate "/home/esc/python2venv/")
  (add-hook 'gud-mode-hook #'company-mode)
  (unless (get-buffer "*gud*")
    (new-gef-instance))
  (company-mode-on)
  ;; (with-selected-window (get-buffer-window "*doom*") (view-buffer "*gud*"))
  (with-selected-window (with-current-buffer "*gud*" (split-window-right))
    (create-pty-buffer "gef-output"))
  (with-selected-window (with-current-buffer "*gud*" (split-window-vertically))
    (progn
      (unless (file-exists-p target-exploit-path)
        (copy-file (concat (file-name-directory load-file-name) "x.py") target-exploit-path))
      (find-file (concat target-file-parent-path "x.py"))
      (rename-buffer "*exploit*")))
  (new-gdb-script-buffer)
  (run-python)
  (with-selected-window (with-current-buffer "*exploit*" (split-window-vertically))
    (switch-to-buffer "*Python*"))

  (start-process-shell-command
   "*socat*"
   "*socat*"
   (concat
    (executable-find "socat")
    " -v TCP-LISTEN:9998,reuseaddr,fork EXEC:\""
    (executable-find "gdbserver")
    " \\:9999 " target-file-path "\"" ))
  (process-send-string "*gud*" (concat "gef config context.redirect \"" (buffer-pty-name "*gef-output*") "\"\n"))
  (with-current-buffer "*exploit*" (python-shell-send-buffer))
  (process-send-string "*gud*" (concat "target remote 0.0.0.0:9999\n")))
