;;; mplayer-mode.el --- control mplayer, facilitating transcription and note-taking.

;; Copyright (C) 2011 Mark Hepburn

;; Author: Mark Hepburn (mark.hepburn@gmail.com)
;; Compatibility: Emacs20, Emacs21, Emacs22, Emacs23

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
  "Group used to store various mplayer-mode variables.")


(defcustom mplayer-executable "mplayer"
  "Name or path to the mplayer executable."
  :type 'file
  :group 'mplayer)

(defvar mplayer-mode-map nil
  "Local keymap for mplayer-mode")

;;; This prefix is chosen for ergonomic accessibility; it does ignore
;;; the recomendations about C-x being for global combinations, etc,
;;; so change if it's inconvenient.
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

(defcustom mplayer-osd-level 3
  "OSD level used by mplayer.  3 (the default) means position/length."
  :type 'integer
  :group 'mplayer)

(defcustom mplayer-timestamp-format "%H:%M:%S"
  "Format used for inserting timestamps."
  :type 'string
  :group 'mplayer)

(defvar mplayer-file "" "File local variable intended to store the media file for mplayer-mode between sessions.")
(put 'mplayer-file 'safe-local-variable 'file-readable-p)
(defvar mplayer-position 0 "File local variable intended to store the position in `mplayer-file' between sessions.")
(put 'mplayer-position 'safe-local-variable 'numberp)
(defvar mplayer-playback-speed 1 "File local variable intended to store the playback speed of `mplayer-file' between sessions.")
(put 'mplayer-playback-speed 'safe-local-variable 'numberp)


;;; Utilities:

(defun mplayer--send (cmd)
  (process-send-string mplayer-process (concat cmd "\n")))

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

(defun mplayer--format-time (time)
  "Return a formatted time string, using the format string
`mplayer-timestamp-format'.  The argument is in seconds, and
can be an integer or a string."
  (message "format-time: %s" time)
  (if (stringp time)
      (setq time (round (string-to-number time))))
  (message "time to format: %s" time)
  (format-time-string mplayer-timestamp-format `(0 ,time 0) t))

(defun mplayer--get-time ()
  "Return time in seconds."
    (let (time)
      (set-process-filter
       mplayer-process
       ;; wait for output, process, and remove filter:
       (lambda (process output)
         (string-match "^ANS_TIME_POSITION=\\(.*\\)$" output)
         (setq time (match-string 1 output))
         (set-process-filter process nil)))
      ;; Then send the command:
      (mplayer--send "pausing_keep_force get_time_pos")
      (accept-process-output mplayer-process 0.3)
      time))

(defun mplayer--get-filename ()
  "Return filename of currently playing file."
  (let (fn)
    (set-process-filter
     mplayer-process
     (lambda (process output)
       (string-match "^ANS_path=\\(.*\\)$" output)
       (setq fn (match-string 1 output))
       (set-process-filter process nil)))
    (mplayer--send "pausing_keep_force get_property path")
    (accept-process-output mplayer-process 0.3)
    (if (file-exists-p fn)
        (if (string-match "\\.\\./" (file-relative-name fn)) ;if file is above current dir
            fn
          (file-relative-name fn))
      nil)))

(defun mplayer--get-speed ()
  "Return current playback speed"
  (let (speed)
    (set-process-filter
     mplayer-process
     (lambda (process output)
       (string-match "^ANS_speed=\\(.*\\)$" output)
       (setq speed (match-string 1 output))
       (set-process-filter process nil)))
    (mplayer--send "pausing_keep_force get_property speed")
    (accept-process-output mplayer-process 0.3)
    (if (= 0 (string-to-number (if (stringp speed) speed "0")))
        nil
      speed)))


;;; Interactive Commands:
(defun mplayer-session-resume ()
  "Resumes a transcription session (entering mplayer-mode) with the options given by the file local variables `mplayer-file', `mplayer-position', and `mplayer-playback-speed'"
  (interactive)
  (when (file-readable-p mplayer-file)
    (mplayer-find-file mplayer-file)
    (if (numberp mplayer-position)
        (mplayer-seek-position (- mplayer-position mplayer-resume-rewind))
      (message "Can't reset to previous position"))
    (if (numberp mplayer-playback-speed)
        (mplayer--send (format "speed_set %s" mplayer-playback-speed))
      (message "Can't set playback speed to previous value.")
        )


    ))


(defun mplayer-find-file (filename)
  "Entry point to this mode.  Starts playing the file using
mplayer, and enables some keybindings to support it; see the
documentation for `mplayer-mode' for available bindings."
  (interactive "fOpen recording file: ")
  (set (make-local-variable 'mplayer--osd-enabled) nil)
  (set (make-local-variable 'mplayer-process-buffer) (generate-new-buffer "*mplayer*"))
  (set (make-local-variable 'mplayer-process)
       (start-process "mplayer" mplayer-process-buffer
                      mplayer-executable
                      "-quiet" "-slave"
                      filename))
  (mplayer-mode t))

(defun mplayer-toggle-pause ()
  "Pause or play the currently-open recording."
  (interactive)
  (mplayer--send "pause"))

(defun mplayer-seek-forward (seconds)
  "Skip forward in the recording.  By default this is
`mplayer-default-seek-step' seconds; it can also be specified as
a numeric prefix arg, or plain prefix args act as a
successive (linear) multipliers of `mplayer-default-seek-step'."
  (interactive "P")
  (let ((seconds (mplayer--parse-seconds seconds)))
    (mplayer--send (format "seek %d 0" seconds))))

(defun mplayer-seek-backward (seconds)
  "Skip backward in the recording.  By default this is
`mplayer-default-seek-step' seconds; it can also be specified as
a numeric prefix arg, or plain prefix args act as a
successive (linear) multipliers of `mplayer-default-seek-step'."
  (interactive "P")
  (let ((seconds (- (mplayer--parse-seconds seconds))))
    (mplayer--send (format "seek %d 0" seconds))))

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


(defun mplayer-seek-position (position)
  "Seek to some place in the recording."
  ;; (interactive "P")
  (interactive "nEnter seek position: ")
  ;; (message "Seeking to position: %n" position)
    (mplayer--send (format "seek %d 2" position)))

(defun mplayer-seek-timestamp ()
  "Seek to the time specified by the closest (backwards) timestamp. This requires timestamps to contain a string like %H:%M:%S.
This means brackets etc. can be added to the standard format but not much more"
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
  "Quit mplayer and exit this mode."
  (interactive)
  (let ((save t) (already-session
                  (if file-local-variables-alist
                      (if (assoc 'mplayer-file file-local-variables-alist)
                          t
                        nil)
                    nil)))
    (unless already-session (setq save (y-or-n-p "Save session as file-local variables?")))
    (when save
      (let ((file (mplayer--get-filename))
            (time (mplayer--get-time))
            (speed (mplayer--get-speed)))
        (if file
            (add-file-local-variable-prop-line 'mplayer-file file)
          (message "Couldn't save filename."))
        (if time
            (add-file-local-variable-prop-line 'mplayer-position (string-to-number time))
          (message "Couldn't save playback position."))
        (if speed
            (add-file-local-variable-prop-line 'mplayer-playback-speed (string-to-number speed))
          (message "Couldn't save playback speed."))
        (hack-local-variables))))
  (mplayer--send "quit")
  (set-process-filter
   mplayer-process
   (lambda (process output)
     (kill-buffer mplayer-process-buffer)))
  (mplayer-mode nil))

;;; Mode setup:

(unless mplayer-mode-map
  (setq mplayer-mode-map (make-sparse-keymap)))

(let ((map (make-sparse-keymap)))
  ;; (define-key map (kbd "f")       'mplayer-find-file)
  (define-key map (kbd "SPC")     'mplayer-toggle-pause)
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
  mplayer-mode-map)

(provide 'mplayer-mode)
