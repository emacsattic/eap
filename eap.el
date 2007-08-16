;;; eap.el --- Emacs' AlsaPlayer - "Music Without Jolts"

;; Copyright (C) 2007
;; Sebastian Tennant

;; Author:     Sebastian Tennant <sebyte@gmail.com>
;; Maintainer: Sebastian Tennant <sebyte@gmail.com>
;; Version:    1.0
;; Keywords:   audio, player, mp3, ogg

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; This is NOT part of GNU Emacs.

;;; Installation:
;;;
;;; N.B. You must have alsaplayer (and specifically alsaplayer's *text*
;;;      interface installed) for EAP to work.  For instance, both of these
;;;      Debian packages must be installed:
;;;
;;;        alsaplayer-common
;;;        alsaplayer-text
;;;
;;;      Under Debian, a third package is also required but which one depends
;;;      upon your choice of audio output module.  For example, I use the
;;;      Advanced Linux Sound Architecture (ALSA), so I also have this Debian
;;;      package installed:
;;;
;;;        alsaplayer-alsa
;;;
;;;      As you may have guessed, ALSA is where alsaplayer gets it's name, but
;;;      you don't have to use this output module.  Many other output modules
;;;      exist; esd, jack, nas, oss... to name but a few.
;;;
;;;      
;;; Make sure this file eap.el is located somehwere in your 'load-path'.  For
;;; example, put this file in ~/elisp and then add this line to your ~/.emacs:
;;;
;;;   (add-to-list 'load-path "~/elisp")
;;;
;;; Next, put this line in your ~/.emacs to ensure EAP is loaded at startup:
;;;
;;;   (autoload 'eap "eap.el" "Emacs' AlsaPlayer - \"Music Without Jolts\"" t)
;;;
;;; Finally, tell EAP where your music is and where you would like to keep your
;;; playlist directories (a.k.a playdirs).  For example:
;;;
;;;   (setq eap-music-dir    "/home/bob/music"
;;;   (setq eap-playdirs-dir "/home/bob/eap-playdirs"
;;;
;;; Restart Emacs (or type the command 'M-x load-library <RET> eap <RET>')

;;; Commentary:
;;;
;;; Emacs' AlsaPlayer - "Music Without Jolts" - an mp3/ogg player for Emacs.
;;;
;;; Type:
;;;
;;;   M-x eap <RET>
;;;
;;; and you will be asked if you would like to continue where you left off?
;;; Obviously, if this is your first time, reply no and you will be taken to
;;; your music directory in Dired.

;;; Access to your music is exclusively through Dired buffers.  There are three
;;; key sequences available to you, each of which can be performed on a single
;;; file, a set of marked files, a single directory, or a set of marked
;;; directories.  I refer to these simply as 'marked' below.
;;;
;;;  M-pp	-	Start a new playlist consisting of marked (e.g., if
;;;			  point is on an album directory, EAP will start
;;;			  playing that album immediately)
;;;
;;;  M-pq	-	Add marked to the current playlist (a.k.a enqueuing)
;;;
;;;  M-ps	-	Add marked to named playlist
;;;
;;; Note: M-pp and M-pq can be prefixed with C-u to ensure that songs are
;;;       added in a random order (a.k.a. 'shuffle-mode').
;;;
;;; Finally, there are two EAP buffers; *EAP* and *EAP Playlist*.  *EAP*
;;; displays the alsaplayer process output and is usually just a couple of lines
;;; tall.  *EAP Playlist* displays the contents of the current playlist.
;;;
;;; Further commands are available from within these buffers (the same in each).
;;; To learn about them, type:
;;;
;;;  C-hm	OR	 M-x describe-mode <RET>
;;;  
;;; from within one of these buffers.


;;; code starts here...

;; named volumes
(defvar eap-volume-mute 0.01)
(defvar eap-volume-soft 0.2)
(defvar eap-volume-full 1.0)
;; volume knob
(defvar eap-volume-knob 1.0)
(defvar eap-volume-step 0.01)
;; fades
(defvar eap-volume-restore 1.0)
(defvar eap-volume-slow-fade-in-p nil)
(defvar eap-volume-slow-fade-out-p t)
(defvar eap-volume-fade-sleep-step 0.05)
;; songs
(defvar eap-playlist '())
(defvar eap-playdirs-dir "/media/l5_60Gb_ext3/EAP playdirs")
(defvar eap-music-dir "/media/l5_60Gb_ext3/Music")
;; state
(defvar eap-paused-p nil)
;; hooks
(defvar eap-startup-hook nil
"Hook run at startup.")


;;; derived modes
;;;
(define-derived-mode eap-mode comint-mode "eap"
  "
Emacs' AlsaPlayer - \"Music Without Jolts\"
========================================
Major mode (derived from Comint mode) for the control and display
of synchronous audio playback using alsaplayer.

N.B. The same commands are available in *EAP* and
     *EAP Playlist* buffers.

Buffer key sequence:    Action:			Global command:
-------------------     ------			--------------
<  OR  left arrow	Previous song		M-x ap<
>  OR  right arrow	Next song		M-x ap>
SPC			Pause/Play		M-x ap.
j			Jump to song		M-x apj

0 (zero)		Mute volume		M-x ap0
-			Soft volume		M-x ap-
=			Full volume		M-x ap=
up arrow		Increase volume
down arrow		Decrease volume

m			Show music directory	M-x apm
v			Show current song	M-x apv

p			Show current playlist   M-x app
q			Hide EAP buffers

s			Add current song	M-x aps
			  to named playlist

k			Keep window small

Q			Quit EAP		M-x eaq
"

  (setq comint-scroll-to-bottom-on-output t)
  (eap-define-common-keys eap-mode-map)
  (font-lock-mode))

(defvar eap-playlist-font-lock-keywords '(("^.*\\* ?$" . font-lock-keyword-face)))

(define-derived-mode eap-playlist-mode fundamental-mode "eap-playlist"
  "
Emacs' AlsaPlayer - \"Music Without Jolts\"
========================================
Major mode for the display of the current EAP playlist, also
providing control of EAP's synchronous audio playback using
alsaplayer.

N.B. The same commands are available in *EAP* and
     *EAP Playlist* buffers.

Buffer key sequence:    Action:			Global command:
-------------------     ------			--------------
<  OR  left arrow	Previous song		M-x ap<
>  OR  right arrow	Next song		M-x ap>
SPC			Pause/Play		M-x ap.
j			Jump to song		M-x apj

0 (zero)		Mute volume		M-x ap0
-			Soft volume		M-x ap-
=			Full volume		M-x ap=
up arrow		Increase volume
down arrow		Decrease volume

m			Show music directory	M-x apm
v			Show current song	M-x apv

p			Show current playlist   M-x app
q			Hide EAP buffers

s			Add current song	M-x aps
			  to named playlist

k			Keep window small

Q			Quit EAP		M-x eaq
"
  (eap-define-common-keys eap-playlist-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(eap-playlist-font-lock-keywords nil t))
  (font-lock-mode))

(defun eap-define-common-keys (mode)
  ;; non-volume state change keys
  (define-key mode [right] 'ap>) ; either this...
  (define-key mode ">"     'ap>) ; or this.
  (define-key mode [left]  'ap<) ; either this...
  (define-key mode "<"     'ap<) ; or this.
  (define-key mode " "     'ap.)
  (define-key mode "j"     'apj)
  (define-key mode "Q"     'apq)
  ;; fixed volume keys
  (define-key mode "0"     'ap0) ; All these 'ap*' functions began life anonymously
  (define-key mode "-"     'ap-) ; (defined here).  They were exposed later according to
  (define-key mode "="     'ap=) ; a simple naming convention and can be found at the
  ;; dired keys			   bottom of this file
  (define-key mode "m"     'apm)
  (define-key mode "v"     'apv)
  (define-key mode "s"     'aps)
  ;; view playlist
  (define-key mode "p"     'app)
  ;; functions only acccessible from within an EAP buffer
  (define-key mode "k"     'eap-keep-window-small)
  (define-key mode "q"
    (lambda () (interactive)
      (delete-windows-on "*EAP*") (bury-buffer "*EAP*")
      (when (get-buffer "*EAP Playlist*")
	(bury-buffer "*EAP*")
	(kill-buffer "*EAP Playlist*"))))
  (define-key mode [up]
    (lambda ()
      (interactive)
      (unless (eap-volume-at-full-p)
	(eap-volume-change '+)
	(setq eap-volume-restore eap-volume-knob))))
  (define-key mode [down]
    (lambda () (interactive)
      (unless (eap-volume-at-mute-p)
	(eap-volume-change '-)
	(setq eap-volume-restore eap-volume-knob)))))


;;; state control
;;;
(defun eap-call-alsaplayer (action &optional args msg)
  (setq action (concat "--" action))
  (if args (apply 'call-process "alsaplayer" nil nil nil action args)
    (call-process "alsaplayer" nil nil nil action))
  (when msg (message msg)))

(defun eap-state-change (change)
  (if (eap-running-p)
      (progn
	(unless (equal change 'togg) (eap-fade-out))
	(cond ((equal change 'next) (eap-call-alsaplayer "next"))
	      ((equal change 'prev) (eap-call-alsaplayer "prev"))
	      ((equal change 'togg)	;toggle (pause or resume)
	       (if eap-paused-p
		   (progn (eap-call-alsaplayer "start") (setq eap-paused-p nil) (eap-fade-in))
		 (progn (eap-fade-out) (eap-call-alsaplayer "pause") (setq eap-paused-p t))))
	      ((equal change 'quit) (eap-call-alsaplayer "quit") (setq eap-playlist '() eap-paused-p nil))
	      (t             (eap-call-alsaplayer "jump" (list change))))
	(unless (equal change 'togg) (eap-volume-change eap-volume-restore)))
    (message "EAP not running.")))


;;; volume control
;;;
(defun eap-volume-at-full-p ()
  (if (>= eap-volume-knob eap-volume-full)
      (progn (setq eap-volume-knob eap-volume-full) t)
    nil))

(defun eap-volume-at-mute-p ()
  (if (<= eap-volume-knob eap-volume-mute)
      (progn (setq eap-volume-knob eap-volume-mute) t)
    nil))

(defun eap-volume-at-restore-p ()
  (if (>= eap-volume-knob eap-volume-restore)
      (progn (setq eap-volume-knob eap-volume-restore) t)
    nil))

(defun eap-volume-change (n)
  (cond ((equal n '+) (setq eap-volume-knob (+ eap-volume-knob eap-volume-step)))
	((equal n '-) (setq eap-volume-knob (- eap-volume-knob eap-volume-step)))
	((numberp n) (setq eap-volume-knob n)))
  (call-process "alsaplayer" nil nil nil "--volume" (number-to-string eap-volume-knob)))

(defun eap-fade-out ()
  (setq eap-volume-restore eap-volume-knob)
  (while (not (eap-volume-at-mute-p))
    (eap-volume-change '-)
    (when eap-volume-slow-fade-out-p
      (sleep-for eap-volume-fade-sleep-step))))

(defun eap-fade-in ()
  (while (not (eap-volume-at-restore-p))
    (eap-volume-change '+)
    (when eap-volume-slow-fade-in-p
      (sleep-for eap-volume-fade-sleep-step))))


;;; file management
;;; 
(defun eap-check-file-suffix (str)
  (or (equal (substring str -4) ".mp3")
      (equal (substring str -4) ".MP3")
      (equal (substring str -4) ".ogg")
      (equal (substring str -4) ".OGG")))

(defun eap-marked-check (files)
  (apply 'nconc
	 (mapcar '(lambda (f)
		    (if (eap-check-file-suffix f)
			(list f)
		      (when (file-accessible-directory-p f)
			;; cddr removes '.' and '..' from the list
			(eap-marked-check (cddr (directory-files f t))))))
		 files)))
    
(defun eap-marked-randomise (files)
  (let (f l)
    (while files
      (setq f (elt files (random (length files)))
	    files (remove f files)
	    l (cons f l))) l))

(defun eap-running-p ()
  (equal (process-status "EAP") 'run))


;;; do what I mean - start a new alsaplayer process,
;;;		     add to existing playlist, or
;;;		     start a new playlist
(defun eap-dwim (files enqueue-p)
  (set-buffer (get-buffer-create "*EAP*"))
  (unless (equal major-mode "eap-mode") (eap-mode))
  ;; go alsaplayer go
  (if (not (eap-running-p))
      (progn
	(erase-buffer)
	(display-buffer
	 (apply 'make-comint-in-buffer "EAP" nil "alsaplayer" nil "-i" "text" files))
	(eap-keep-window-small))
    (progn
      (if enqueue-p
	  (eap-call-alsaplayer "enqueue" files "Added to play queue.")
	(progn
	  (eap-fade-out)
	  (eap-call-alsaplayer "replace" files "Started new play queue.")
	  (eap-volume-change eap-volume-restore))))))

;;; top level entry to eap
(defun eap ()
  (interactive)
  (if (eap-running-p)
      ;; just pop to EAP buffer if already running
      (progn (pop-to-buffer "*EAP*") (eap-keep-window-small))
    ;; else...
    (progn
      (run-hooks 'eap-startup-hook) ;; useful for mounting stuff
      (if (y-or-n-p "Continue where you left off ")
	  (progn
	    ;; set eap-playlist variable to file contents, and go...
	    (with-temp-buffer
	      (insert-file-contents "~/.alsaplayer/alsaplayer.m3u")
	      (setq eap-playlist (remove "" (split-string (buffer-string) "\n"))))
	    (eap-dwim nil nil))
	;; just bring up music directory in Dired
	(dired eap-music-dir)))))

;; appearance 
(defun eap-keep-window-small ()
  (interactive)
  (let ((w (get-buffer-window "*EAP*")))
    (when (and w (not (= (window-height w) 3)))
      (fit-window-to-buffer w 3 3))))


;;; status
;;;
(defun eap-alsaplayer-status-alist ()
  (mapcar '(lambda (s) (split-string s ": "))
    (remove "---------------- Session ----------------"
      (remove "-------------- Current Track ------------"
	(remove "-----------------------------------------"
	  (remove ""
	    ;; call-process-to-string is mine
	    (split-string (call-process-to-string "alsaplayer --status") "\n")))))))

(defun eap-alsaplayer-this-status (s)
  (cadr (assoc s (eap-alsaplayer-status-alist))))

(defun eap-alsaplayer-current-song ()
  (car (last (split-string (eap-alsaplayer-this-status "path") "/"))))


;;; eap to dired
;;;
(defun eap-dired-music-dir ()
  (interactive)
  (dired-other-window eap-music-dir))

(defun eap-dired-current-track ()
  (interactive)
  (when (eap-running-p)
    (let ((track (eap-alsaplayer-this-status "path")))
      (dired-other-window (file-name-directory track))
      (goto-char (point-max))
      (re-search-backward (file-name-nondirectory track)))))

(defun eap-symlink-current-track ()
  (interactive)
  (eap-dired-current-track)
  (dired-eap-symlink-to-playdir))


;;; dired to eap
;;;
(defun dired-eap-replace-marked (rand-p)
  (interactive "p")
  (let ((files (eap-marked-check (dired-get-marked-files))))
    (if (equal rand-p 4)
	(setq eap-playlist (eap-marked-randomise files))
      (setq eap-playlist files))
    (eap-dwim eap-playlist nil))) ;start new playlist
  
(defun dired-eap-enqueue-marked (rand-p)
  (interactive "p")
  (let ((files (eap-marked-check (dired-get-marked-files))))
    (if (equal rand-p 4)
	(setq eap-playlist (nconc eap-playlist (eap-marked-randomise files)))
      (setq eap-playlist (nconc eap-playlist files)))
    (eap-dwim files t))) ;add files to current playlist

(defun dired-eap-symlink-to-playdir ()
  (interactive)
  (let ((dired-dwim-target t))
    (dired-other-window eap-playdirs-dir)
    (other-window 1)
    (let ((default-directory eap-playdirs-dir))
      (dired-do-symlink))))


;;; view playlist
;;;
(defun eap-display-playlist ()
  (interactive)
  (if (eap-running-p)
      (progn
	(switch-to-buffer "*EAP Playlist*")
	(unless (equal major-mode "eap-playlist-mode") (eap-playlist-mode))
	(delete-other-windows)		;this is needed for some reason
	(when buffer-read-only (toggle-read-only))
	(erase-buffer) 
	(let ((qpos 1))
	  (mapc '(lambda (s)
		   (let* ((aas-list (last (split-string s "/") 3))
			  (artist (car aas-list))
			  (album (cadr aas-list))
			  (song (caddr aas-list)))
		     (insert (format "%28s | %28s | %52s | %3d\n"
				     (truncate-string-to-width artist 28 nil nil t)
				     (truncate-string-to-width album 28 nil nil t)
				     (truncate-string-to-width song 52 nil nil t)
				     qpos)))
		   (incf qpos))
		eap-playlist))
	(goto-char (point-min))
	(re-search-forward (eap-alsaplayer-current-song) nil nil)
	(end-of-line) (insert "*")
	;;   (beginning-of-line) (push-mark nil t) (end-of-line)
	;;   (facemenu-set-bold)
	(toggle-read-only)
	(pop-to-buffer "*EAP*")		;return to *EAP*
	(eap-keep-window-small))
    (message "EAP isn't running :-(")))


;;; global to eap
;;; non-volume state change functions
(defun ap> () (interactive) (eap-state-change 'next))
(defun ap< () (interactive) (eap-state-change 'prev))
(defun ap. () (interactive) (eap-state-change 'togg))
(defun apj () (interactive) (eap-display-playlist)
  (let ((n (string-to-number (read-from-minibuffer  "Jump to track: "))))
    (if (or (<= n 0) (> n (length eap-playlist)))
	(message "That number does not correspond to a queued track.")
      (eap-state-change (number-to-string n)))))
(defun apq () (interactive) (eap-state-change 'quit))
;;; fixed volume functions
(defun ap0 () (interactive) (eap-volume-change eap-volume-mute))
(defun ap- () (interactive) (eap-volume-change eap-volume-soft))
(defun ap= () (interactive) (eap-volume-change eap-volume-full))
;;; dired function aliae
(defalias 'apm 'eap-dired-music-dir)
(defalias 'apv 'eap-dired-current-track)
(defalias 'aps 'eap-symlink-current-track)
;;; playlist function alias
(defalias 'app 'eap-display-playlist)



;;; send synchronous process output to a string
;;; (not strictly part of EAP)
(defun call-process-to-string (process &optional sep)
  "Start the synchronous process PROCESS (via `call-process') and
return the results in a string.  PROCESS may be a list or a
string.  If PROCESS is a string, any SEP argument (which must
also be a string) will be used as the separator when converting
the string to a list (via `split-string') instead of the default
whitespace value."
  (let (process-list)
    (cond ((stringp process)
	   (setq process-list (if sep (split-string process sep) (split-string process))))
	  ((listp process)
	   (setq process-list process))
	  (t
	   (setq process-list nil)))
    (when process-list
      (with-output-to-string
	(with-current-buffer standard-output
	  (apply 'call-process (car process-list) nil t nil (cdr process-list)))))))

;;; add dired-eap-* key sequences to dired-mode-map
(eval-after-load "dired"
  '(progn
     (define-prefix-command 'dired-eap)           ;creates a sparse keymap
     (define-key dired-mode-map "\M-p" dired-eap) ;accessed by M-p

     (define-key dired-mode-map "\M-pp" 'dired-eap-replace-marked) ;new playlist
     (define-key dired-mode-map "\M-pq" 'dired-eap-enqueue-marked) ;add to current playlist
     (define-key dired-mode-map "\M-ps" 'dired-eap-symlink-to-playdir))) ;add to named playlist

