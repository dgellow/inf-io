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


;; Vars
(defvar inf-io-buffer nil "Current Io process buffer.")

(defvar inf-io-default-implementation "io")

(defvar inf-io-implementations
  '(("io" . "io"))  
  "A list of the different implementations of Io interpreter. There is only one at the current time.")


;; Tools
(defun region-get-lines (start end)
  "Return a cons of a region, line by line."
  (goto-char start)
  (defun loop-on-lines (acc)
    (if acc
        (forward-line 1))

    (if (>= (line-end-position) end)
        (cons 
         (buffer-substring-no-properties 
          (line-beginning-position) end)
         acc)
      (loop-on-lines (cons 
                      (buffer-substring-no-properties 
                       (line-beginning-position)
                       (line-end-position))
                      acc))))

  (reverse (loop-on-lines '())))


;; Io process
(defun inf-io-process () 
  "Return the inf Io process embedded in *io* buffer."
  (get-buffer-process inf-io-buffer))

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

(defun io-send-region (start end)
  "Send the current region to the inferior Io process."
  (interactive "r")
  (let ((lines (mapcar (lambda (x) (concat x "\n")) 
                       (region-get-lines start end))))

    (comint-send-string (inf-io-process) "doString(\"\n")
    (mapc (lambda (x) (comint-send-string (inf-io-process) x)) lines)
    (comint-send-string (inf-io-process) "\"\n)\n")))

(defun io-send-region-and-go (start end)
  "Send the region to the Io process and select the *io* buffer."
  (interactive "r")
  (io-send-region start end)
  (pop-to-buffer inf-io-buffer)
  (end-of-buffer))
