;;; mplayer-mode.el --- control mplayer, facilitating transcription and note-taking.

;; Copyright (C) 2011 Mark Hepburn

;; Author: Mark Hepburn (mark.hepburn@gmail.com)
;; Compatibility: Emacs20, Emacs21, Emacs22, Emacs23
;; Keywords: multimedia

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Owes a lot in initial idea to the emacs video editor gneve
;; (http://www.1010.co.uk/gneve.html).  This mode controls mplayer
;; directly, using its slave-mode (see
;; http://www.mplayerhq.hu/DOCS/tech/slave.txt), which accepts
;; commands on stdin.  The original motivation was to facilitate note
;; taking from videos; hence it is possible to pause, skip backwards
;; and forwards, and insert a timestamp of the current position.

;;; Use:

;;; Install:

;; Put something similar to the following in your ~/.emacs to use this file:
;;
;; (load "~/path/to/mplayer-mode.el")
;;

;;; Dependency:

;; mplayer

;;; TODO:
;; - Proper org-mode integration would probably be nice (eg, a link to the file)
;; - Error handling and clean-up

;;; Code:

(defgroup mplayer nil
  "Group used to store various mplayer-mode variables."
  :group 'multimedia)


(defcustom mplayer-executable "mplayer"
  "Name or path to the mplayer executable."
  :type 'file
  :group 'mplayer)

(defvar mplayer-mode-map nil
  "Local keymap for mplayer-mode")

;; This prefix is chosen for ergonomic accessibility; it does ignore
;; the recomendations about C-x being for global combinations, etc,
;; so change if it's inconvenient.
(defcustom mplayer-prefix-command "\C-x "
  "The prefix for all mplayer minor-mode commands. Default C-x SPC."
  :type 'key-sequence
  :group 'mplayer)

(defcustom mplayer-default-seek-step 10
  "The number of seconds that the skip command will use."
  :type 'integer
  :group 'mplayer)

(defcustom mplayer-default-speed-step 10
  "The increase/decrease of playback speed that the faster/slower commands will use (percent of standard playback speed)."
  :type 'integer
  :group 'mplayer)

(defcustom mplayer-resume-rewind 10
  "The number of seconds before previous position that playback will start when resuming a saved session."
  :type 'integer
  :group 'mplayer)

(defcustom mplayer-default-play-pause-rewind 1
  "The number of seconds of rewind applied when using the command `mplayer-toggle-pause-with-rewind'"
  :type 'integer
  :group 'mplayer)
(put 'mplayer-default-play-pause-rewind 'safe-local-variable 'integerp)

(defcustom mplayer-osd-level 3
  "OSD level used by mplayer.  3 (the default) means position/length."
  :type 'integer
  :group 'mplayer)

(defcustom mplayer-timestamp-format "%H:%M:%S"
  "Format used for inserting timestamps."
  :type 'string
  :group 'mplayer)

(defcustom mplayer-display-time-in-modeline t
  "If the current time and play/pause-status should be displayed in the modeline"
  :type 'boolean
  :group 'mplayer)

(defcustom mplayer-modeline-time-format "%H:%M:%S"
  "Defines the format of the time representation in the modeline."
  :type 'string
  :group 'mplayer)

(defcustom mplayer-modeline-play-symbol "►"
  "The symbol used for play in the modeline indication"
  :type 'string
  :group 'mplayer)

(defcustom mplayer-modeline-pause-symbol "◼"
  "The symbol used for play in the modeline indication"
  :type 'string
  :group 'mplayer)

(defcustom mplayer-try-org-properties-for-sessions t
  "Whether org mode properties should be used to store session
variables (filename, position, playback speed)"
  :type 'boolean
  :group 'mplayer)

;;; Internal variables
(defvar mplayer--osd-enabled nil)
(defvar mplayer--process nil)
(defvar mplayer--process-buffer nil)
(defvar mplayer--modeline "")
(put 'mplayer--modeline 'risky-local-variable t)
(defvar mplayer-timer nil)
(defvar mplayer--current-org-hl-bm-for-props nil)
(make-variable-buffer-local 'mplayer--current-org-hl-bm-for-props)
;;; Variables for storing the session
;;;###autoload
(defvar mplayer-file "" "File local variable intended to store the media file for mplayer-mode between sessions.")
;;;###autoload
(put 'mplayer-file 'safe-local-variable 'file-readable-p)
;;;###autoload
(defvar mplayer-position 0 "File local variable intended to store the position in `mplayer-file' between sessions.")
;;;###autoload
(put 'mplayer-position 'safe-local-variable 'numberp)
;;;###autoload
(defvar mplayer-playback-speed 1 "File local variable intended to store the playback speed of `mplayer-file' between sessions.")
;;;###autoload
(put 'mplayer-playback-speed 'safe-local-variable 'numberp)


;;; Interactive Commands:
;;;###autoload
(defun mplayer-resume-session ()
  "Resumes a transcription session (entering mplayer-mode) with
the options given by the file local variables `mplayer-file',
`mplayer-position', and `mplayer-playback-speed' or in org
properties with the same name if
`mplayer-try-org-properties-for-sessions' is non-nil and
properties are found."
  (interactive)
  (let* (orgprop
         (file (or (and mplayer-try-org-properties-for-sessions
                        (setq orgprop (org-entry-get-with-inheritance "mplayer-file")))
                   mplayer-file))
         (position (or (and mplayer-try-org-properties-for-sessions
                            (let ((mp (org-entry-get-with-inheritance "mplayer-position")))
                              (when mp (string-to-number mp))))
                       mplayer-position))
         (playback-speed (or (and mplayer-try-org-properties-for-sessions
                                  (let ((mps (org-entry-get-with-inheritance "mplayer-playback-speed")))
                                    (when mps (string-to-number mps))))
                             mplayer-playback-speed)))
    (if (file-readable-p file)
        (progn
          (when orgprop
            (setq mplayer--current-org-hl-bm-for-props
                  (cons "mplayer--current-org-hl-bm-for-props"
                        (save-excursion
                          (save-restriction
                            (widen) (goto-char org-entry-property-inherited-from)
                            (bookmark-make-record-default))))))
          (mplayer-find-file file)
          (if (numberp position)
              (mplayer-seek-position (- position mplayer-resume-rewind))
            (message "Can't reset to previous position: %s" position))
          (if (numberp playback-speed)
              (mplayer--send (format "speed_set %s" playback-speed))
            (message "Can't set playback speed to previous value: %s" playback-speed)))
      (message "Can't resume mplayer session, unreadable file: %s" file))))

;;;###autoload
(defun mplayer-find-file (filename)
  "Entry point to this mode.  Starts playing the file using
mplayer, and enables some keybindings to support it; see the
documentation for `mplayer-mode' for available bindings."
  (interactive "fOpen recording file: ")
  (set (make-local-variable 'mplayer--osd-enabled) nil)
  (set (make-local-variable 'mplayer--process-buffer) (generate-new-buffer "*mplayer*"))
  (set (make-local-variable 'mplayer--process)
       (start-process "mplayer" mplayer--process-buffer
                      mplayer-executable
					  "-af" "scaletempo" "-quiet" "-slave"
                      filename))
  (mplayer-mode))

(defun mplayer-toggle-pause ()
  "Pause or play the currently-open recording."
  (interactive)
  (mplayer--send "pause")
  (run-at-time 0.2 nil 'mplayer--update-modeline))

(defun mplayer-toggle-pause-with-rewind ()
  "Pause or play the currently open recording and skip back an amount of `mplayer-default-play-pause-rewind' seconds."
  (interactive)
  (if (mplayer--paused)
      (mplayer--send (format "pausing seek -%d 0" mplayer-default-play-pause-rewind)))
  (mplayer-toggle-pause))

(defun mplayer-seek-forward (seconds)
  "Skip forward in the recording.  By default this is
`mplayer-default-seek-step' seconds; it can also be specified as
a numeric prefix arg, or plain prefix args act as a
successive (linear) multipliers of `mplayer-default-seek-step'."
  (interactive "P")
  (let ((seconds (mplayer--parse-seconds seconds)))
    (mplayer--send (format "seek %d 0" seconds)))
  (run-at-time 0.2 nil 'mplayer--update-modeline))

(defun mplayer-seek-backward (seconds)
  "Skip backward in the recording.  By default this is
`mplayer-default-seek-step' seconds; it can also be specified as
a numeric prefix arg, or plain prefix args act as a
successive (linear) multipliers of `mplayer-default-seek-step'."
  (interactive "P")
  (let ((seconds (- (mplayer--parse-seconds seconds))))
    (mplayer--send (format "seek %d 0" seconds)))
  (run-at-time 0.2 nil 'mplayer--update-modeline))

(defun mplayer-faster (speedstep)
  "Increase playback speed. By default by `mplayer-default-speed-step' percentage points; it can also be set with a numeric prefix arg, or plain prefix args acts as successive multipliers (2,3,4...) of `mplayer-default-speed-step'"
  (interactive "P")
  (let ((speedstep (mplayer--parse-speedstep speedstep)))
    (mplayer--send (format "speed_incr %.2f" speedstep))))

(defun mplayer-slower (speedstep)
  "Decreaser playback speed. By default by `mplayer-default-speed-step' percentage points; it can also be set with a numeric prefix arg, or plain prefix args acts as successive multipliers (2,3,4...) of `mplayer-default-speed-step'"
  (interactive "P")
  (let ((speedstep (mplayer--parse-speedstep speedstep)))
    (mplayer--send (format "speed_incr -%.2f" speedstep))))

(defun mplayer-reset-speed ()
  "Reset playback speed."
  (interactive)
  (mplayer--send "speed_set 1"))

(defun mplayer-toggle-osd ()
  "Toggle on-screen display on or off.  See `mplayer-osd-level'
for the type of display."
  (interactive)
  (if mplayer--osd-enabled
      (mplayer--send "osd")
    (mplayer--send (format "osd %d" mplayer-osd-level)))
  (setq mplayer--osd-enabled (not mplayer--osd-enabled)))

(defun mplayer-insert-timestamp ()
  "Insert a time-stamp of the current recording position in the
buffer.  See `mplayer-timestamp-format' for the insertion
format."
  (interactive)
  (let ((time (mplayer--get-time)))
    (if time
        (insert (mplayer--format-time time))
      (message "MPlayer: couldn't detect current time."))))


(defun mplayer-insert-position ()
  "Insert the current recording position in seconds,
into the buffer."
  (interactive)
  (let ((time (mplayer--get-time)))
    (if time
        (insert time)
      (message "MPlayer: couldn't detect current time."))))


(defun mplayer-seek-position (position &optional alwaysplay)
  "Seek to some place in the recording given by seconds in
POSITION. If the optional ALWAYSPLAY is non-nil resume playback
if paused."
  (interactive "nEnter seek position: ")
  (let ((doplay (and alwaysplay (mplayer--paused 0.1))))
    (mplayer--send (format "seek %d 2" position))
    (when doplay ;;resume playback
      (mplayer--send "pause"))
    (mplayer--update-modeline)))

(defun mplayer-seek-timestamp ()
  "Seek to the time specified by the closest (backwards) timestamp.
This requires timestamps to contain a string like %H:%M:%S and
means brackets etc. can be added to the standard format but not
much more"
  ;;Doing the reverse of format-time-string is very complicated, we
  ;;have to make assumptions
  (interactive)
  (if (string-match "%H:%M:%S" mplayer-timestamp-format)
      (let (hour min sec position)
        (save-excursion
          (re-search-backward "\\([0-9][0-9]\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)")
          (setq hour (string-to-number (match-string 1)))
          (setq min (string-to-number (match-string 2)))
          (setq sec (string-to-number (match-string 3)))
          (setq position (+ sec (* 60 min) (* 3600 hour)))
          (message "Going to timestamp position: %02d:%02d:%02d = %d" hour min sec position)
          (mplayer-seek-position position)))
    (message "Timestamp format does not contain %%H:%%M:%%S")))

(defun mplayer-quit-mplayer ()
  "Quit mplayer and exit `mplayer-mode', possibly saving the mplayer session."
  (interactive)
  (when (process-live-p mplayer--process) ;just disable mode otherwise
    (mplayer--maybe-save-session)
    (mplayer--send "quit")
    (set-process-filter mplayer--process
                        (lambda (process output)
                          (kill-buffer mplayer--process-buffer))))
  (mplayer--kill-mplayer-processes) ;;extra cleanup
  (mplayer-mode -1))

;;; Helper functions
(defun mplayer--send (cmd)
  (process-send-string mplayer--process (concat cmd "\n")))

(defun mplayer--parse-seconds (seconds)
  (cond
   ((null seconds) mplayer-default-seek-step)
   ((numberp seconds) seconds)
   ((listp seconds)
    (* mplayer-default-seek-step (log (abs (car seconds)) 4)))))

(defun mplayer--parse-speedstep (speedstep)
  (cond
   ((null speedstep) (/ mplayer-default-speed-step 100.0))
   ((numberp speedstep) (/ speedstep 100.0))
   ((listp speedstep)
    (/ (* mplayer-default-speed-step (+ 1 (log (abs (car speedstep)) 4))) 100.0))))

(defun mplayer--format-time (time &optional format)
  "Return a formatted time string, using the format string
`mplayer-timestamp-format'.  The argument is in seconds, and
can be an integer or a string. Optionally, a format for
`format-time-string' can be passed."
  ;;(message "format-time: %s" time)
  (let ((format (or format mplayer-timestamp-format)))
    (setq time
          (if (stringp time)
              (round (string-to-number time))
            (round time)))
    ;;(message "time to format: %s" time)
    (format-time-string format `(0 ,time 0) t)))

(defun mplayer--get-time (&optional wait)
  "Return time in seconds.
Optional WAIT is the time to wait for output (default 0.3s). If
WAIT=0 just grab the last time from `mplayer--process-buffer'."
  (let (time
        (nofilter (and wait (= 0 wait)))
        (wait (or wait 0.3))
        (time-regexp "^ANS_TIME_POSITION=\\(.*\\)$"))
    (unless nofilter
      (set-process-filter
       mplayer--process
       (lambda (process output)
         (when (string-match time-regexp output)
           (setq time (match-string 1 output)))
         (set-process-filter process nil))))
    (mplayer--send "pausing_keep_force get_time_pos")
    (if nofilter
        (setq time (mplayer--get-previous-output-regexp time-regexp))
      (accept-process-output mplayer--process wait))
    time))

(defun mplayer--get-filename (&optional wait)
  "Return filename of currently playing file."
  (let (fn
        (wait (or wait 0.3)))
    (set-process-filter
     mplayer--process
     (lambda (process output)
       (when (string-match "^ANS_path=\\(.*\\)$" output)
		 (setq fn (match-string 1 output)))
       (set-process-filter process nil)))
    (mplayer--send "pausing_keep_force get_property path")
    (accept-process-output mplayer--process wait)
    (if (file-exists-p fn)
        (if (string-match "\\.\\./" (file-relative-name fn)) ;if file is above current dir
            fn
          (file-relative-name fn))
      nil)))

(defun mplayer--get-speed (&optional wait)
  "Return current playback speed"
  (let (speed
        (wait (or wait 0.3)))
    (set-process-filter
     mplayer--process
     (lambda (process output)
       (when (string-match "^ANS_speed=\\(.*\\)$" output)
		 (setq speed (match-string 1 output)))
       (set-process-filter process nil)))
    (mplayer--send "pausing_keep_force get_property speed")
    (accept-process-output mplayer--process wait)
    (if (= 0 (string-to-number (if (stringp speed) speed "0"))) ;TODO, stupid?
        nil
      speed)))

(defun mplayer--paused (&optional wait)
  "Return pause status: t if paused, nil if not, undef if there was an error.
Optional WAIT is the time to wait for output (default 0.3s). If
WAIT=0 just grab the last status from `mplayer--process-buffer'."
  (let (ps
        (nofilter (and wait (= 0 wait)))
        (wait (or wait 0.3))
        (pause-regexp "^ANS_pause=\\(.*\\)$"))
    (unless nofilter
      (set-process-filter
       mplayer--process
       (lambda (process output)
         (when (string-match pause-regexp output)
           (setq ps (match-string 1 output)))
         (set-process-filter process nil))))
    (mplayer--send "pausing_keep_force get_property pause")
    (if nofilter 
        (setq ps (mplayer--get-previous-output-regexp pause-regexp))
      (accept-process-output mplayer--process wait))
    (cond
     ((null ps) 'undef)
     ((string= ps "yes") t)
     ((string= ps "no") nil)
     (t 'undef))))

(defun mplayer--get-previous-output-regexp (regexp)
  "Search backward and return (match-string 1) for a regexp in the
last 10 lines in the buffer of `mplayer-process'"
  (with-current-buffer (process-buffer mplayer--process)
    (save-excursion
      (goto-char (point-max))
      (search-backward-regexp
       regexp
       (save-excursion (forward-line -11) (point))
       t)
      (match-string 1))))


(defun mplayer--update-modeline (&optional wait-for-correct)
  "Update modeline with current position, if modeline display is
enabled with `mplayer-display-time-in-modeline'. If optional
WAIT-FOR-CORRECT is non-nil. Make sure we get the current time
by waiting for process output"
  (when (and mplayer-display-time-in-modeline mplayer--process)
    (with-local-quit ;; so C-g works if process hangs
      (let* ((wait-time (if wait-for-correct 3 0))
             (paused (mplayer--paused wait-time))
             (time (mplayer--get-time wait-time))
             (ts (if time (mplayer--format-time time mplayer-modeline-time-format) "")))
        (setq mplayer--modeline
              (list "["
                    (cond (paused (concat mplayer-modeline-pause-symbol " "))
                          ((not paused) (concat mplayer-modeline-play-symbol " "))
                          (t ""))
                    ts "]" ))))))

(defun mplayer--maybe-save-session ()
  (let* (org-pom
         old-file
         old-bm
         (current-file (mplayer--get-filename))
         (save-fn
          (or (when (and mplayer-try-org-properties-for-sessions
                         (or (setq old-bm mplayer--current-org-hl-bm-for-props)
                             (setq old-file (org-entry-get-with-inheritance "mplayer-file"))))
                (setq org-pom ; this isn't very clean
                      (cond
                       (old-bm (save-excursion
                                 (save-restriction
                                   (widen)
                                   (bookmark-default-handler old-bm)
                                   (setq mplayer--current-org-hl-bm-for-props nil)
                                   (point)))) ;;TODO, testa här
                       (old-file
                        (if (string= old-file current-file)
                            org-entry-property-inherited-from
                          (mplayer--org-get-save-heading
                           'buffer
                           "Different file than saved session, choose headline for property: ")))))
                ;; save-fn as org-entry-put:
                (lambda (symb val) (org-entry-put org-pom (symbol-name symb) val)))
              (when (and file-local-variables-alist
                         (assoc 'mplayer-file file-local-variables-alist))
                #'add-file-local-variable)
              ;; ask for how to save
              (cond ((and mplayer-try-org-properties-for-sessions
                          (y-or-n-p "Save session in org properties?"))
                     (setq org-pom (mplayer--org-get-save-heading 'tree))
                     (lambda (symb val) (org-entry-put org-pom (symbol-name symb) val)))
                    ((y-or-n-p "Save session as file-local variables?")
                     #'add-file-local-variable)
                    (t nil)))))
    (when save-fn
      (let ((save-list `((mplayer-file ,current-file 
                                       "Couldn't save filename")
                         (mplayer-position ,(mplayer--get-time)
                                           "Couldn't save playback position")
                         (mplayer-playback-speed ,(mplayer--get-speed)
                                                 "Couldn't save playback speed"))))
        (dolist (x save-list)
          (if (cadr x)
              (progn (apply save-fn (butlast x)))
            (message (car (last x)))))))))

(defun mplayer--org-get-save-heading (&optional tree-or-buffer prompt)
  "Select the position of a headline to save properties in.

Presents a selection of headings in the outline path to the
current headline, or among all headings in buffer.

Optional TREE-OR-BUFFER when set to 'buffer means check whole buffer,
when set to 'tree only the outline path of the current headline and when
nil asks the user.
Optional PROMPT is the prompt for selection."
  (let (hlist
        case-fold-search
        (whole-buf (and (not (eq 'tree tree-or-buffer))
                        (or (eq 'buffer tree-or-buffer)
                            (y-or-n-p "Select among all headings for saving properties?"))))
        (list-push
         '(push (cons (org-trim (replace-regexp-in-string
                                 ;; Remove statistical/checkboxes cookies
                                 "\\[[0-9]+%\\]\\|\\[[0-9]+/[0-9]+\\]" ""
                                 (org-match-string-no-properties 4)))
                      (point))
                hlist)))
    (save-excursion
      (save-restriction
        (widen)
        (if whole-buf
            (progn (goto-char (point-min))
                   (while (re-search-forward org-complex-heading-regexp nil t)
                     (eval list-push)))
          (progn
            (push (progn (org-back-to-heading) (cons (org-get-heading) (point)))
                  hlist)
            (while (org-up-heading-safe)
              (when (looking-at org-complex-heading-regexp)
                (eval list-push)))))))
    (let* ((keylist (mapcar 'car hlist))
           (heading (completing-read (or prompt "Heading to file under: ") keylist nil t)))
      (cdr (assoc heading hlist)))))

(defun mplayer--kill-mplayer-processes ()
  (let ((allp (process-list)))
    (dolist (proc allp)
      (when (string-match "^mplayer.*" (process-name proc))
        (let ((buf (process-buffer proc))
              (kill-buffer-query-functions nil)) ;;workaround really
          (kill-process proc)
          (kill-buffer buf))))))

;;; Mode setup
(unless mplayer-mode-map
  (setq mplayer-mode-map (make-sparse-keymap)))

(let ((map (make-sparse-keymap)))
  ;; (define-key map (kbd "f")       'mplayer-find-file)
  (define-key map (kbd "SPC")     'mplayer-toggle-pause)
  (define-key map (kbd "RET")     'mplayer-toggle-pause-with-rewind)
  (define-key map (kbd "<right>") 'mplayer-seek-forward)
  (define-key map (kbd "<left>")  'mplayer-seek-backward)
  (define-key map (kbd "f")       'mplayer-faster)
  (define-key map (kbd "s")       'mplayer-slower)
  (define-key map (kbd "r")       'mplayer-reset-speed)
  (define-key map (kbd "p")       'mplayer-seek-position)
  (define-key map (kbd "g")       'mplayer-seek-timestamp)
  (define-key map (kbd "t")       'mplayer-insert-position)
  (define-key map (kbd "d")       'mplayer-toggle-osd)
  (define-key map (kbd "i")       'mplayer-insert-timestamp)
  (define-key map (kbd "q")       'mplayer-quit-mplayer)

  (define-key mplayer-mode-map mplayer-prefix-command map))

(define-minor-mode mplayer-mode
  "Control mplayer from within Emacs.  Mainly intended for
transcription purposes, so commands exist to pause, seek, set playback speed, and
insert the current time as a timestamp.  This mode should not be
invoked directly; see `mplayer-find-file' and
`mplayer-quit-mplayer' for the entry and exit points.

Key bindings:
\\{mplayer-mode-map}"
  nil                                   ; initial value
  " MPlayer"                            ; mode-line string
  mplayer-mode-map
  (when mplayer-display-time-in-modeline ; assume it is not changed
										; when in mplayer-mode
	(if mplayer-mode
		(progn
		  (unless global-mode-string (setq global-mode-string '("")))
		  (unless (memq 'mplayer--modeline global-mode-string)
			(setq global-mode-string (append global-mode-string
											 '(mplayer--modeline))))
          (setq mplayer-timer (run-at-time 5 1 #'mplayer--update-modeline)))
	  (progn
        (cancel-timer mplayer-timer)
		(when (and (listp global-mode-string) (memq 'mplayer--modeline global-mode-string))
		  (setq global-mode-string (delq 'mplayer--modeline global-mode-string))
		  (force-mode-line-update))))))

(provide 'mplayer-mode)

;;; mplayer-mode.el ends here
