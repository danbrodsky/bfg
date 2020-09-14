;;; ~/.doom.d/modules/private/tools/bgf/autoload/gdb.el -*- lexical-binding: t; -*-

;; +---+
;; |GDB|
;; +---+

(defvar gdb-filter-function nil)
(put 'gdb-filter-function 'permanent-local t)
(defun gdb-filter-function (&rest args)
  (apply gdb-filter-function args))

(defun gud-gdb-script-filter (proc string)
  ;; Copy of `gud-filter' but uses hardcoded funcall for gdb script filter
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
                (setq output (gdb-filter-function string))
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


(defvar gud-gdb-fetch-lines-string)
(defvar gud-gdb-fetch-lines-break)
(defvar gud-gdb-fetched-lines)
(defvar gud-gdb-fetch-lines-in-progress)


(defun gud-gdb-script-completions (context command)
  "Completion table for gdb script from existing gdbmi process"
  ;; (process-send-string (get-buffer-process gud-comint-buffer)
  ;;                      (concat "complete " context command "\n"))))
  ;; (accept-process-output (get-buffer-process gud-comint-buffer))
  (let ((gud-gdb-fetch-lines-in-progress t)
        (gud-gdb-fetch-lines-string nil)
        (gud-gdb-fetch-lines-break (length context))
        (gud-gdb-fetched-lines nil)
        (gdb-filter-function #'gud-gdbmi-fetch-lines-filter))
    ;; hijack process filter because it uses local variables
    (set-process-filter (get-buffer-process gud-comint-buffer) #'gud-gdb-script-filter)
    (with-current-buffer (gdb-get-buffer 'gdb-partial-output-buffer)
      (gdb-input (concat "complete " context command)
                 (lambda () (setq gud-gdb-fetch-lines-in-progress nil)))
      (while gud-gdb-fetch-lines-in-progress
        (accept-process-output (get-buffer-process gud-comint-buffer))))
    ;; set process filter back to original
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
  (progn
    (unless (file-exists-p target-gdb-path)
      (copy-file (concat (file-name-directory (symbol-file 'bgf)) "script.gdb") target-gdb-path))
    (find-file target-gdb-path)
    (rename-buffer "*gdb-script*")
    (add-hook 'gud-mode-hook #'company-mode)
    (gdb-script-mode)
    (add-hook 'completion-at-point-functions #'gud-gdb-script-completion-at-point
              nil 'local)
    (set (make-local-variable 'gud-gdb-completion-function) 'gud-gdb-script-completions)
    (set (make-local-variable 'gud-gdb-script-history) (gdb-history-load))
    (set (make-local-variable 'gud-minor-mode) 'gdbmi)))


(defun gdb-shell-send-buffer ()
  "send all commands in gdb script buffer to gdb"
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (gdb-shell-send-line)
    (forward-line 1)))


(defun gdb-shell-send-region (start end)
  "send lines between start and end to gdb comint buffer"
  (interactive)
  (goto-char start)
  (while (not (<= (- end (point)) 0))
    (gdb-shell-send-line)
    (forward-line 1)))


(defun gdb-shell-send-line ()
  "send line at cursor to gdb"
  (interactive)
  (let*
      ((start (line-beginning-position))
       (end (line-end-position))
       (command (buffer-substring start end)))
    (unless (string= (string (char-after start)) "#")
    (progn
      (write-region (concat command "\n") nil "~/.gdb_history" t)
      (process-send-string "*gud*" (concat command "\n"))
      (process-send-string "*gef-output*" "\n")
      (bgf-buffers-down)
      (push command gud-gdb-script-history)))))


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


;; +------+
;; |PYTHON|
;; +------+

(defun python-shell-send-region--line ()
  "send current line at mark to python shell"
  (interactive)
  (python-shell-send-region (line-beginning-position) (line-end-position)))

(defun lispy-eval-region ()
  (interactive)
  (save-excursion)
  (goto-char (line-beginning-position))
  (lispy-eval-and-comment)
  (search-forward "=>")
  (setq num-lines 0)
  (while
      (= (char-after (line-beginning-position)) ?#)
    (setq num-lines (+ num-lines 1))
    (when (= num-lines 5)
      (set-mark (line-beginning-position)))
    (next-line))
  (previous-line)
  (when (region-active-p)
    (vimish-fold (region-beginning) (region-end))))

(defun lispy-py-conf-init ()
  (interactive)
  (require 'le-python)
  (require 'lispy)
  (setq-local
   outline-regexp "# ?\\*+"
   lispy-outline (concat "^" outline-regexp)
   outline-heading-end-regexp "\n"
   completion-at-point-functions '(lispy-python-completion-at-point t)
   lispy-no-space t
   lispy-python-init-file "~/.pyrc.py")
  (setq
   +evil-want-o/O-to-continue-comments nil
   lispy-left ""
   lispy-right ""
   lispy-outline-header "#")
  (if (process-live-p (lispy--python-proc))
      (let ((_res
             (progn
               (lispy--eval
                (format "import os; os.chdir('%s')" default-directory))))))
    (error "No process"))
  (setq-local company-backends '(company-capf company-lsp)))



;; +---+
;; |BGF|
;; +---+

;;;###autoload
(defun bgf (&optional target-file)
  "Start configured gdb session"
  (interactive)
  (bgf--cleanup)
  (bgf-key-map)
  (add-hook 'gdb-script-mode-hook 'bgf-gdb-conf)
  (add-hook 'python-mode-hook 'bgf-py-conf)
  (defvar target-file-path
    (if target-file
        (concat (file-name-directory target-file) target-file)
      (read-file-name "Target: ")))
  (defvar target-file-parent-path (file-name-directory target-file-path))
  (defvar target-exploit-path (concat target-file-parent-path "x.py"))
  (setq lsp-auto-guess-root t)
  (progn
    (unless (file-exists-p target-exploit-path)
      (copy-file (concat (file-name-directory (symbol-file 'bgf)) "x.py") target-exploit-path))
    (find-file (concat target-file-parent-path "x.py"))
    (rename-buffer "*exploit*")
    (lispy-py-conf-init)
    (setq python-shell-buffer-name "lispy-python-default"))

  (add-hook 'gud-mode-hook #'company-mode)
  (unless (get-buffer "*gud*")
    (new-gef-instance))
  (company-mode-on)
  (with-selected-window (with-current-buffer "*gud*" (split-window-right))
    (create-pty-buffer "gef-output"))
  (new-gdb-script-buffer)
  (start-process-shell-command
   "*socat*"
   "*socat*"
   (concat
    (executable-find "socat")
    " -v TCP-LISTEN:9998,reuseaddr,fork EXEC:\""
    (executable-find "gdbserver")
    " \\:9999 " target-file-path "\"" ))
  (process-send-string "*gud*" (concat "gef config context.redirect \"" (buffer-pty-name "*gef-output*") "\"\n"))
  (with-current-buffer "*exploit*"
    (python-shell-send-buffer))
  (process-send-string "*gud*" "gef-remote 0.0.0.0:9999\n")
  (bgf-window-setup)
  )


;;;###autoload
(defun bgf-run ()
  "Run exploit on target program in gef"
  (interactive)
  (process-send-string "*gud*" (concat "kill\n"))
  (sit-for 1)
  (with-current-buffer "*exploit*" (python-shell-send-buffer))
  (process-send-string "*gud*" (concat "target remote 0.0.0.0:9999\n")))


;; +-------------+
;; |CONFIGURATION|
;; +-------------+


(defun bgf--cleanup ()
  (setq-local kill-buffer-query-functions nil)
  (unless (not (get-buffer "*gud*")) (kill-buffer "*gud*"))
  (unless (not (get-buffer "*socat*")) (kill-buffer "*socat*"))
  (unless (not (get-buffer "*gef-output*")) (kill-buffer "*gef-output*"))
  (unless (not (get-buffer "*lispy-python-default*")) (kill-buffer "*lispy-python-default*"))
  (unless (not (get-buffer "*exploit*")) (kill-buffer "*exploit*"))
  (unless (not (get-buffer "*gdb-script*")) (kill-buffer "*gdb-script*")))


(defun bgf-gdb-conf ()
  (setq-local send-selected-area-function #'gdb-shell-send-region)
  (setq-local send-line-function #'gdb-shell-send-line)
  (map!
   :desc "send buffer to gdb"
   :mode (gdb-script-mode)
   :nvm "gL"
   #'gdb-shell-send-buffer))


(defun bgf-py-conf ()
  (setq-local send-selected-area-function #'lispy-eval-region)
  (setq-local send-line-function #'lispy-eval-region)
  (map!
   :desc "send buffer to python"
   :mode (python-mode)
   :nvm "gL"
   #'python-shell-send-buffer))


(defun bgf-key-map ()
  (map!
   :desc "send line or selected area to target function"
   :nvm "gl"
   #'send-area)
  (map!
   (:leader
     (:prefix "w"
       :desc "open ace-window"
       "SPC"
       #'ace-window))))


;; +------+
;; |WINDOW|
;; +------+

(defun bgf-window-setup ()
  "initial window setup for bgf"
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-below)
  (switch-to-buffer "*exploit*")
  (split-window-right)
  (evil-window-right 1)
  (switch-to-buffer "*gdb-script*")
  (evil-window-down 1)
  (switch-to-buffer "*lispy-python-default*")
  (shrink-window (round (* (window-height) 0.5)))
  (evil-window-right 1)
  (switch-to-buffer "*gef-output*")
  (split-window-below)
  (evil-window-down 1)
  (switch-to-buffer "*gud*")
  (shrink-window (round (* (window-height) 0.5)))
  )


(defun bgf-buffers-down ()
  "move all output buffers to most recent out"
  (interactive)
  (with-selected-window (get-buffer-window "*gud*")
    (goto-char (point-max)))
  (with-selected-window (get-buffer-window "*gef-output*")
    (goto-char (point-max)))
  (with-selected-window (get-buffer-window "*lispy-python-default*")
    (goto-char (point-max))))

;; +----+
;; |UTIL|
;; +----+

(defvar send-line-function nil)
(defvar send-selected-area-function nil)
(defun send-area (start end)
  "send selected line(s) to function"
  (interactive "r")
  (if (region-active-p)
      (funcall send-selected-area-function start end)
    (funcall send-line-function)))


(defun buffer-pty-name (buffer-name)
  "Get the pty of an existing buffer"
  (process-tty-name (get-buffer-process buffer-name)))


(defun create-pty-buffer (name)
  "Hackish way to open a pretty buffer with a pty"
  (defvar pty-buffer-name (concat "*" name "*"))
  (unless (get-buffer pty-buffer-name)
    (ansi-term "/usr/bin/cat" name))
  (switch-to-buffer pty-buffer-name))
