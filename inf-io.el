;;; inf-io.el --- Run a Io process in a buffer

;; Copyright (C) 2013 Samuel El-Borai

;; Author: Samuel El-Borai aka dgellow <samuel.elborai@gmail.com>
;; URL: http://github.com/dgellow/inf-io
;; Created: 22 October 2013
;; Keywords: io programming language

;; Commentary: 
;;
;; Provides a REPL buffer running an Io proces.s
;;
;; Work based on the inf-ruby.el by Yukihiro Matsumoto and Nobuyoshi Nakada
;; https://github.com/nonsequitur/inf-ruby

(defvar inf-io-buffer nil "Current Io process buffer.")
(defvar inf-io-default-implementation "io")
(defvar inf-io-implementations
	'(("io" . "io")))


(defun run-io (&optional command name)
	"Run an inferior Io process, input and output via buffer *io*."

	(interactive)
	(setq command (or command (cdr (assoc inf-io-default-implementation
																				inf-io-implementations))))

	(setq name (or name "io"))
	
	(if (not (comint-check-proc inf-io-buffer))
			(let ((commandlist (split-string-and-unquote command)))
				(set-buffer (apply 'make-comint name (car commandlist)
													 nil (cdr commandlist)))))
	(pop-to-buffer (setq inf-io-buffer (format "*%s*" name))))











