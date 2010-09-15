;;; eap-dired-keybindings.el - Emacs' AlsaPlayer - Music Without Jolts
;;; Copyright (C) 2007, 2008, 2009 Sebastian Tennant
;;;
;;; Author:     Sebastian Tennant <sebyte@gmail.com>
;;; Maintainer: Sebastian Tennant <sebyte@gmail.com>
;;; Version:    0.12
;;; Keywords:   audio, player, mp3, ogg
;;;
;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your option)
;;; any later version.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;;; Boston, MA 02110-1301, USA.
;;;
;;; This file is part of Emacs' AlsaPlayer, which is not (yet) part of GNU Emacs

;;; create a sparse keymap accessed by M-p and define p, q and s
(eval-after-load "dired"
  '(progn
     (define-prefix-command 'dired-eap)
     (define-key dired-mode-map "\M-p" dired-eap)
     (define-key dired-mode-map "\M-pp" 'dired-eap-replace-marked)
     (define-key dired-mode-map "\M-pq" 'dired-eap-enqueue-marked)
     (define-key dired-mode-map "\M-ps" 'dired-eap-symlink-to-playlist-library)))

(provide 'eap-dired-keybindings)
