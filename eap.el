;;; eap.el --- Emacs' AlsaPlayer - "Music Without Jolts"
;;; Copyright (C) 2007,2008 Sebastian Tennant
;;;
;;; Author:     Sebastian Tennant <sebyte@gmail.com>
;;; Maintainer: Sebastian Tennant <sebyte@gmail.com>
;;; Version:    0.11
;;; Keywords:   audio, player, mp3, ogg
;;;
;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301, USA.
;;;
;;; eap.el is NOT part of GNU Emacs.

;;; INSTALLATION
;;; 
;;; 1. Install Alsaplayer
;;; 
;;; On a Debian GNU/Linux system a minimal working Alsaplayer requires
;;; that these two packages:
;;; 
;;;   alsaplayer-common alsaplayer-text
;;; 
;;; and one of these:
;;; 
;;;   alsaplayer-alsa alsaplayer-esd alsaplayer-jack alsaplayer-nas alsaplayer-oss
;;; 
;;; be installed.
;;; 
;;; Choose the last package according to your audio output
;;; interface. Alsaplayer is designed to work closely with the ALSA
;;; system whenever possible, i.e., it works perfectly well without
;;; it.
;;; 
;;; Test the text interface by issuing the command:
;;; 
;;;   $ alsaplayer -i text some-audio-file.ogg
;;; 
;;; 2. Install eap.el
;;; 
;;; Ensure that the file, eap.el, is located somehwere in your
;;; load-path. For example, put eap.el in ~/elisp and then add this
;;; line to your ~/.emacs:
;;; 
;;; (add-to-list 'load-path "~/elisp")
;;; 
;;; Tell EAP where your music is kept, and where you would like to
;;; keep your playlist directories (a.k.a. playdirs). For example, add
;;; these two lines to your ~/.emacs:
;;; 
;;;   (setq eap-music-dir    "/home/bob/music"
;;;         eap-playdirs-dir "/home/bob/eap-playdirs")
;;; 
;;; N.B. Do not make you playlist directory, eap-playdirs-dir, a
;;;      sub-directory of your music directory, eap-music-dir.
;;; 
;;; Ensure that EAP is always ready to go. Copy and paste these lines
;;; to your ~/.emacs:
;;; 
;;;   (autoload 'eap "eap.el" "Emacs' AlsaPlayer - Music Without Jolts" t)
;;; 
;;;   (eval-after-load "Dired"
;;;     '(progn
;;;        (define-prefix-command 'Dired-eap)
;;;        (define-key Dired-mode-map "\M-p" Dired-eap)
;;;        (define-key Dired-mode-map "\M-pp" 'Dired-eap-replace-marked)
;;;        (define-key Dired-mode-map "\M-pq" 'Dired-eap-enqueue-marked)
;;;        (define-key Dired-mode-map "\M-ps" 'Dired-eap-symlink-to-playdir)))
;;; 
;;; Restart Emacs.

;;; USAGE
;;; 
;;; If you've ever used Alsaplayer before, the first time you type:
;;; 
;;;   M-x eap RET
;;; 
;;; you may well be asked if you want to continue where you left off?
;;; Answer positively and Emacs' AlsaPlayer will proceed to play
;;; through the playlist created the last time you used Alsaplayer.
;;; 
;;; If you've never used Alsaplayer before you'll simply be taken to
;;; your music directory in Dired.
;;; 
;;; N.B. You don't have to start Alsaplayer this way. M-x eap RET is
;;;      simply a way to pick up where you left off, a short-cut to
;;;      your music directory, or a short cut to the EAP buffer if EAP
;;;      is running.
;;; 
;;; Access to your music is exclusively through Dired buffers. There
;;; are three key sequences available to you, each of which can be
;;; performed on a single file, a set of marked files, a single
;;; directory, or a set of marked directories. I refer to these simply
;;; as 'marked' below:
;;; 
;;;   M-pp   Start a new playlist consisting of marked (e.g., if point is
;;;          on an album directory, EAP will start playing that album)
;;;
;;;   M-pq   Add marked to the current playlist (a.k.a enqueuing)
;;;
;;;   M-ps   Add marked to named playlist
;;; 
;;; N.B. Prefix M-pp and M-pq with C-u to ensure that songs are added
;;;      to the current playlist (or replace the current playlist) in
;;;      a random order (a.k.a. 'shuffling').
;;; 
;;; Finally, there are two EAP buffers; *EAP* and *EAP Playlist*.
;;; *EAP* displays the Alsaplayer process output, and is never usually
;;; more than two lines tall. *EAP Playlist* displays the contents of
;;; the current playlist, with the currently playing song
;;; highlighted. This buffer is not self-refreshing so you may
;;; occasionally need to type p to update it.

;;; KEYS AND COMMANDS
;;; Key commands are the same in both buffers; the *EAP* buffer and the *EAP Playlist* buffer.
;;; 
;;; Playlist/song position commands
;;; -------------------------------
;;; Key          Action        Global command 
;;; < OR [left]  Previous song M-x ap<        
;;; > OR [right] Next song     M-x ap>        
;;; SPC          Pause/Play    M-x ap.        
;;; j            Jump to song  M-x apj        
;;; 
;;; Volume adjustment commands
;;; --------------------------
;;; Key Action      Global command 
;;; 0   Volume mute M-x ap0        
;;; -   Volume soft M-x ap-        
;;; =   Volume full M-x ap=        
;;; 
;;; Other commands
;;; --------------
;;; Key Action                              Global command
;;; p   Show current playlist               M-x app       
;;; m   Show music directory                M-x apm       
;;; v   Show current song                   M-x apv       
;;; s   Add current song to named playlist  M-x aps       
;;; i   Toggle fade-in functionality        M-x api       
;;; o   Toggle fade-out functionality       M-x apo       
;;; Q   Quit EAP                            M-x eaq       
;;; 
;;; Commands only available in EAP buffers
;;; --------------------------------------
;;; Key    Action         
;;; f      Seek forward (in song)  
;;; b      Seek backward (in song) 
;;; [up]   Volume up               
;;; [down] Volume down             
;;; k      Keep window small       
;;; q      Bury EAP buffers        


;;; =========================================== defvars
;;; volumes
(defvar eap-volume-mute 0.01)
(defvar eap-volume-soft 0.2)
(defvar eap-volume-full 1.0)

;;; volume knob
(defvar eap-volume-knob 1.0)
(defvar eap-volume-step 0.01)

;;; fades
(defvar eap-volume-restore 1.0)
(defvar eap-volume-fade-in-p nil)
(defvar eap-volume-fade-out-p t)
(defvar eap-volume-fade-step 0.05)

;;; songs
(defvar eap-playlist '())
(defvar eap-playdirs-dir "/media/l5_60Gb_ext3/EAP playdirs")
(defvar eap-music-dir "/media/l5_60Gb_ext3/Music")

;;; misc
(defvar eap-paused-p nil)
(defvar eap-startup-hook nil "Hook run at startup.")
(defvar eap-playlist-font-lock-keywords '((".*\\* ?$" . font-lock-keyword-face)
					  (".*Pos ?$" . font-lock-builtin-face)))
(defvar eap-header-line-format
  (mapconcat
   (lambda (k)
     (format "%s %s" (propertize (car k) 'face 'bold) (cdr k)))
   '((">" . "next,") ("<" . "previous,") ("SPC" . "pause,") ("j" . "jump,") ("p" . "playlist,") ("m" . "music,")
     ("v" . "view,") ("s" . "symlink,") ("0" . "mute,") ("-" . "soft,") ("=" . "full,") ("q" . "bury,") ("Q" . "quit"))
   " "))

;;; =========================================== modes
;;; mode map
(defvar eap-mode-map (make-keymap))
(suppress-keymap eap-mode-map)
(mapc (lambda (k) (define-key eap-mode-map (car k) (cdr k)))
      '(;; state change keys (not volume)
	([right] . ap>) (">" . ap>) ;next track
	([left]  . ap<) ("<" . ap<) ;previous track
	(" "     . ap.) ("j" . apj) ("Q" . apq) ;pause, jump & quit
	;; fixed volume keys
	("0"     . ap0) ("-" . ap-) ("=" . ap=) ;mute, soft & full volume
	;; eap-to-dired keys
	("m"     . apm) ("v" . apv) ("s" . aps) ;music-dir, view track & symlink track to playdir
	;; other keys
	("p"     . app) ("i" . api) ("o" . apo) ;playlist, toggle fade-in, toggle fade-out
	;; functions only acccessible from within an EAP buffer
	("b"     . (lambda ()
		     (interactive)
		     (eap-call-alsaplayer "relative" '("-2"))))
	("f"     . (lambda ()
		     (interactive)
		     (eap-call-alsaplayer "relative" '("2"))))
	([up]    . (lambda () ; volume up
		     (interactive)
		     (unless (eap-volume-at-full-p)
		       (eap-volume-change '+)
		       (setq eap-volume-restore eap-volume-knob))))
	([down]  . (lambda () (interactive) ; volume down
		     (unless (eap-volume-at-mute-p)
		       (eap-volume-change '-)
		       (setq eap-volume-restore eap-volume-knob))))
	("c"     . (lambda (dir) (interactive "sChange value of eap-music-dir to: ")
		     (if (file-accessible-directory-p dir)
			 (setq eap-music-dir dir)
		       (message "%s not accessible. eap-music-directory unchanged." dir))))
	("k"     . eap-shrink-window) ;used in code (below)
	("q"     . (lambda () (interactive) ; bury EAP buffers
		     (delete-windows-on "*EAP*") (bury-buffer "*EAP*")
		     (when (get-buffer "*EAP Playlist*")
		       (bury-buffer "*EAP*")
		       (kill-buffer "*EAP Playlist*"))))
	))

;;; EAP mode
(define-derived-mode eap-mode comint-mode "EAP"
  "Major mode for the control of Emacs' Alsaplayer.

Emacs' AlsaPlayer - \"Music Without Jolts\"
\\<eap-mode-map>
\\{eap-mode-map}"
  (set-face-attribute 'header-line nil :underline nil)
  (setq header-line-format eap-header-line-format)
  (setq comint-scroll-to-bottom-on-output t))

;;; EAP Playlist mode
(defun eap-playlist-mode ()
  "Major mode for the control of Emacs' AlsaPlayer and the
display of the current Emacs' AlsaPlayer playlist.

Emacs' AlsaPlayer - \"Music Without Jolts\"
\\<eap-mode-map>
\\{eap-mode-map}"
  (kill-all-local-variables)
  (use-local-map eap-mode-map)
  (setq major-mode 'eap-playlist-mode)
  (setq mode-name "EAP Playlist")
  (setq font-lock-defaults '(eap-playlist-font-lock-keywords t))
  (font-lock-mode 1)
  (set-face-attribute 'header-line nil :underline nil)
  (setq header-line-format eap-header-line-format))

;;; =========================================== state control
;;; call alsaplayer
(defun eap-call-alsaplayer (action &optional args msg)
  (setq action (concat "--" action))
  (if args (apply 'call-process "alsaplayer" nil nil nil action args)
    (call-process "alsaplayer" nil nil nil action))
  (when msg (message msg)))

;;; change song stae
(defun eap-state-change (change)
  (if (eap-running-p)
      (progn
	(unless (equal change 'togg)
	  (unless (not eap-volume-fade-out-p)
	    (eap-volume-fade-out)))
	(cond ((equal change 'next) (eap-call-alsaplayer "next"))
	      ((equal change 'prev) (eap-call-alsaplayer "prev"))
	      ((equal change 'togg)	;toggle (pause or resume)
	       (if eap-paused-p
		   (progn (eap-call-alsaplayer "start")
			  (setq eap-paused-p nil)
			  (if eap-volume-fade-in-p
			      (eap-volume-fade-in)
			    (eap-volume-change eap-volume-restore)))
		 (progn (unless (not eap-volume-fade-out-p)
			  (eap-volume-fade-out))
			(eap-call-alsaplayer "pause")
			(setq eap-paused-p t))))
	      ((equal change 'quit) (eap-call-alsaplayer "quit") (setq eap-playlist '() eap-paused-p nil))
	      (t             (eap-call-alsaplayer "jump" (list change))))
	(unless (equal change 'togg)
	  (if eap-volume-fade-in-p
	      (eap-volume-fade-in)
	    (eap-volume-change eap-volume-restore))))
    (message "EAP not running.")))


;;; =========================================== volume control
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

(defun eap-volume-fade-out ()
  (setq eap-volume-restore eap-volume-knob)
  (while (not (eap-volume-at-mute-p))
    (eap-volume-change '-)
    (sleep-for eap-volume-fade-step)))

(defun eap-volume-fade-in ()
  (while (not (eap-volume-at-restore-p))
    (eap-volume-change '+)
    (sleep-for eap-volume-fade-step)))


;;; =========================================== file management
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


;;; =========================================== do what I mean
;;; start a new alsaplayer process, add to an existing playlist or start a new playlist
(defun eap-dwim (files enqueue-p)
  (set-buffer (get-buffer-create "*EAP*"))
  (unless (equal major-mode "eap-mode") (eap-mode))
  ;; go alsaplayer go
  (if (not (eap-running-p))
      (progn
	(erase-buffer)
	(display-buffer
	 (apply 'make-comint-in-buffer "EAP" nil "alsaplayer" nil "-i" "text" files))
	(eap-shrink-window))
    (progn
      (if enqueue-p
	  (eap-call-alsaplayer "enqueue" files "Added to play queue.")
	(progn
	  (when eap-volume-fade-out-p (eap-volume-fade-out))
	  (eap-call-alsaplayer "replace" files "Started new play queue.")
	  (eap-volume-change eap-volume-restore))))))

;;; top level entry to eap

;;; it would be nice to stop if eap-startup-hook returns nil
;;; but as this code snippet demonstrates run-hooks always returns nil:
;;;
;;;   (setq test-hooks-hook '())
;;;   (add-hook 'test-hooks-hook (lambda () t))
;;;   (run-hooks 'test-hooks-hook)
;;;   => nil
;;;###autoload
(defun eap ()
  "Emacs' Alsaplayer - Music Without Jolts"
  (interactive)
  (if (eap-running-p)
      ;; just pop to EAP buffer if already running
      (progn (pop-to-buffer "*EAP*") (eap-shrink-window))
    ;; else...
    (progn
      (if (and (file-exists-p "~/.alsaplayer/alsaplayer.m3u")
	       (y-or-n-p "Continue where you left off "))
	  (progn
	    ;; set eap-playlist variable to file contents, and go...
	    (with-temp-buffer
	      (insert-file-contents "~/.alsaplayer/alsaplayer.m3u")
	      (setq eap-playlist (remove "" (split-string (buffer-string) "\n"))))
	    (eap-dwim nil nil))
	;; just bring up music directory in Dired
	(if (file-accessible-directory-p eap-music-dir)
	    (dired eap-music-dir)
	  (message "Music directory (%s) not found or inaccessible." eap-music-dir)
	)))))

(defun eap-shrink-window ()
  (interactive)
  (let ((w (get-buffer-window "*EAP*")))
    (when (and w (not (= (window-height w) 3)))
      (fit-window-to-buffer w 3 3))))


;;; =========================================== status
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


;;; =========================================== eap to dired
;;;###autoload
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


;;; =========================================== dired to eap
;;;###autoload
(defun dired-eap-replace-marked (rand-p)
  (interactive "p")
  (let ((files (eap-marked-check (dired-get-marked-files))))
    (if (equal rand-p 4)
	(setq eap-playlist (eap-marked-randomise files))
      (setq eap-playlist files))
    (eap-dwim eap-playlist nil))) ;start new playlist

;;;###autoload
(defun dired-eap-enqueue-marked (rand-p)
  (interactive "p")
  (let ((files (eap-marked-check (dired-get-marked-files))))
    (if (equal rand-p 4)
	(setq eap-playlist (nconc eap-playlist (eap-marked-randomise files)))
      (setq eap-playlist (nconc eap-playlist files)))
    (eap-dwim files t))) ;add files to current playlist

;;;###autoload
(defun dired-eap-symlink-to-playdir ()
  (interactive)
  (let ((dired-dwim-target t))
    ;; create the playdirs directory if necessary
    (or (file-accessible-directory-p eap-playdirs-dir)
	(make-directory eap-playdirs-dir t))
    ;; create a playdir if necessary
    (or (split-string
	 (shell-command-to-string
	  (format "find %s -mindepth 1 -type d -print"
		  (shell-quote-argument eap-playdirs-dir))))
	(make-directory (concat eap-playdirs-dir "/Favourite songs")))
    ;; go
    (dired-other-window eap-playdirs-dir)
    (other-window 1)
    (let ((default-directory eap-playdirs-dir))
      (dired-do-symlink))))


;;; =========================================== other
(defun eap-display-playlist ()
  (interactive)
  (if (eap-running-p)
      (progn
	(switch-to-buffer "*EAP Playlist*")
	(unless (equal major-mode "eap-playlist-mode") (eap-playlist-mode))
	(delete-other-windows)		;this is needed for some reason
	(when buffer-read-only (toggle-read-only))
	(erase-buffer)
	(insert (format "\n%28s | %28s | %52s | %3s\n" "Artist" "Album" "Track file name" "Pos"))
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
	;; now place '*' next to current song, remembering that
	;; file names longer than approx. 48 chars are truncated
	(let ((search-substring-length
	       (if (>= (length (eap-alsaplayer-current-song)) 49)
		   48
		 (1- (length (eap-alsaplayer-current-song))))))
	  (re-search-forward (substring (eap-alsaplayer-current-song) 0 search-substring-length) nil nil))
	(end-of-line) (insert "*")
	(toggle-read-only)
	(pop-to-buffer "*EAP*")		;return to *EAP*
	(eap-shrink-window))
    (message "Emacs' AlsaPlayer isn't running :-(")))

(defun eap-toggle-fade-in ()
  (interactive)
  (setq eap-volume-fade-in-p (not eap-volume-fade-in-p))
  (message "Volume fade-in now %s." (if eap-volume-fade-in-p "active" "inactive")))

(defun eap-toggle-fade-out ()
  (interactive)
  (setq eap-volume-fade-out-p (not eap-volume-fade-out-p))
  (message "Volume fade-out now %s." (if eap-volume-fade-out-p "active" "inactive")))


;;; =========================================== global to eap
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

;;; volume functions
(defun ap0 () (interactive) (eap-volume-change eap-volume-mute))
(defun ap- () (interactive) (eap-volume-change eap-volume-soft))
(defun ap= () (interactive) (eap-volume-change eap-volume-full))

(defalias 'api 'eap-toggle-fade-in)
(defalias 'apo 'eap-toggle-fade-out)

;;; dired function aliae
;;;###autoload
(defalias 'apm 'eap-dired-music-dir)
(defalias 'apv 'eap-dired-current-track)
(defalias 'aps 'eap-symlink-current-track)

;;; playlist function alias
(defalias 'app 'eap-display-playlist)

;;; =========================================== other
;;; send synchronous process output to a string
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
;;;###autoload
(eval-after-load "dired"
  '(progn
     (define-prefix-command 'dired-eap)           ;creates a sparse keymap
     (define-key dired-mode-map "\M-p" dired-eap) ;accessed by M-p
     (define-key dired-mode-map "\M-pp" 'dired-eap-replace-marked) ;new playlist
     (define-key dired-mode-map "\M-pq" 'dired-eap-enqueue-marked) ;add to current playlist
     (define-key dired-mode-map "\M-ps" 'dired-eap-symlink-to-playdir))) ;add to named playlist

