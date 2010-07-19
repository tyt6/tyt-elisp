;; -*-encoding:utf-8; mode:Emacs-Lisp -*-

(require 'cl)
(require 'basic)
(defconst tyt:whitespace " \t")


(defun split-path (&optional path)
  (or path (setq path (buffer-file-name)))
  (funcs-call `(file-name-directory ,(compose file-name-nondirectory file-name-sans-extension) file-name-extension)
	      (expand-file-name path)))

(defun read-number (prompt &optional initial historyp)
  (string-to-number (read-string prompt initial historyp)))

(defun read-float (prompt &optional initial historyp)
  (float (read-number prompt initial historyp)))

(defun read-line (n &optional buffer)
  (save-excursion
    (goto-line n buffer)
    (buffer-line)))

(defun insert-text-with-overlay (text face beg)
  (insert text)
  (if face (overlay-put (make-overlay beg (+ beg (length text))) 'face face)))

(defun insert/overlay (text face)
  (let1 p (point)
    (insert text)
    (if face (overlay-put (make-overlay p (+ p (length text))) 'face face))))

(defun insert/overlay-bgfg (text background foreground)
  (insert/overlay text `((t (:background	,background
			     :foreground	,foreground)))))

(defun call-with-output-port (path *lcl-func*)
  (save-excursion
    (let ((port (find-file path)))
      (funcall *lcl-func* port)
      (save-buffer)
      (kill-buffer port))))

(defmacro output-port* (var+path+erase &rest body)
  (destructuring-bind (var path erase? . min?) var+path+erase
    `(save-excursion
       (let ((,var (find-file ,path)))
	 ,(and erase? '(erase-buffer))
	 ,(if (car min?) '(goto-char (point-min)) '(goto-char (point-max)))
	 (prog1
	     (progn
	       ,@body)
	   (save-buffer)
	   (kill-buffer ,var))))))

(defun string->file (str file)
  (output-port* (out file t) (princ str out)))

(defun put-contents (file contents)
  (string->file contents file))

(defun string->file/append (str file)
  (output-port* (out file nil) (princ str out)))

(defun string->file/append-before (str file)
  (output-port* (out file nil t) (princ str out)))

(defun append-contents (file contents)
  (string->file/append contents file))

;; input 
(defun port->string (port)
  (with-current-buffer port
    (buffer-substring-no-properties (point-min) (point-max))))

(defun file->string (path)
  (letl it (find-file-noselect path)
    (prog1 (port->string it) (kill-buffer it))))

(defun get-contents (filepath)
  (file->string filepath))

(defun insertn (x)
  (insert (prin1-to-string x)))

(defun printn (x)
  (insert (prin1-to-string x))
  (newline))

(defun print:tsv1 (sep lis)
  (and lis (insertn (pop lis)))
  (while lis
    (insert sep)
    (insertn (pop lis)))
  (newline))

(defun print-list (lis &optional printer)
  (option-init (printer 'printn))
  (for-each printer lis))

(defun print-table (ll &optional sep)
  (option-init (sep "\t"))
  (print-list ll (cut print:tsv1 sep <>)))

(defun princ-buffer (sexp)
  (princ sexp (current-buffer)))

(defun princf (fom &rest args)
  (let ((out (if (bufferp (last-car args)) (pop-tail! args) nil)))
    (princ (apply 'format (cons fom args)) out)))

(defun map-printf (fom args)
  (dolist (a args)
   (apply 'printf fom a)))

(defun buffer-line (&optional noprop trim)
  (if trim
      (buffer-line-trim-whitespace noprop)
    (letl (now beg end) (point-st)
      (buffer-substring-a beginning-of-line end-of-line noprop))))

(defun buffer-line-trim-whitespace (&optional noprop)
  (let* ((word (buffer-line noprop nil))
	 (i -1)
	 (end (length word)))
    (while (and (< (inc! i) end) (find (aref word   i) tyt:whitespace)))
    (while (and (> (dec! end) i) (find (aref word end) tyt:whitespace)))
    (substring word i (1+ end)))) ;; string or nil

(defun buffer-taginside-regexp (otag-reg ctag-reg &optional size)
  (option-init (size 50))
  (let ((pt (point)))
    (buffer-substring-a
     (lambda ()
       (safe:1+ (re-search-backward otag-reg (max (- pt size) (point-min)) t)))
     (lambda ()
       (safe:1- (re-search-forward  ctag-reg (min (+ pt size) (point-max)) t))))))

(defmacro x-point (f &rest args)
  `(begin (,f ,@args) (point)))

(defmacro buffer-substring-a (f-open f-close &optional noprop)
  `(let ((pos (point))
	 (beg (x-point ,f-open))
	 (end (x-point ,f-close)))
     (goto-char pos)
     (and beg end (if ,noprop (buffer-substring-no-properties beg end)
		    (buffer-substring beg end)))))

(defun current-sexp (&optional arg)
  (read-from-string (buffer-substring-a backward-sexp forward-sexp)))

(defun point-st ()
  (let ((now (point)))
    (prog1
	(list now (begin (beginning-of-line) (point))
	      (begin (end-of-line ) (point)))
      (goto-char now))))

(defmacro with-line-replace (fn)
  `(letm (now start2 end2) (point-st)
     (let ((str (buffer-substring start2 end2)))
       (goto-char start2)
       (delete-char (- end2 start2))
       (insert (funcall ,fn str)))))

(defun transform-line-by (f &optional line-map)
  (or line-map (setq line-map 'id))
  (letm (p a z) (point-st)
	(let ((line (buffer-substring a z)))
	  (beginning-of-line)
	  (delete-char (- z a))
	  (insert (funcall f (funcall line-map line))))))

(defun echo (lis)
  (dolist (e lis)
    (princ e (current-buffer))))

(defmacro string-as-file* (var-file+str &rest body)
  (letl (var str) var-file+str
  `(let ((,var (make-temp-file "string-as-file")))
     (put-contents ,var ,str)
     (prog1 ,@body
       (delete-file ,var)))))

(defun save-and-kill-buffer (out)
  (with-current-buffer out
    (save-buffer))
  (kill-buffer out))

(provide 'lbuffer)