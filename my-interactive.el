;; -*-encoding:utf-8; mode:Emacs-Lisp -*-

(require 'cl)
(require 'basic)

;; (require 'w3m)
;; (require 'yasnippet)
;; (require 'etags)
;; (require 'eldoc)
(defvar *newline* ?\n)

(mass-productor make:accel-function (command)
  `(defun ,(symbol-append "accel:"command) (&optional arg)
     (interactive "p")
     (if (eq last-command this-command) (inc! arg arg))
     (,command arg)))

(make:accel-function
 (enlarge-window)  (enlarge-window-horizontally)
 (shrink-window)   (shrink-window-horizontally))

(defun forward-paragraph-indent ()
  (interactive)
  (let ((beg (point))
	(line (count-lines (point-min) (point)))
	(end (progn (forward-paragraph 1)
		    (count-lines (point-min) (point)))))
    (unless buffer-read-only
      (goto-char beg)
      (dotimes (i (- end line))
	(indent-for-tab-command)
	(next-line 1)))))

(defun scroll-right-backward (&optional step)
  (interactive)
  (let ((len (- (point) (line-beginning-position)))
  	(sp (if (numberp step) step (/ (window-width) 6))))
    (scroll-right sp)
    (backward-char (if (< sp len) sp
    		     (beginning-of-line)
		     (skip-chars-forward " \t")
		     0))))

(defun scroll-left-forward (&optional step)
  (interactive)
  (let ((len (- (line-end-position) (point)))
	(sp (if (numberp step) step (/ (window-width) 6))))
    (scroll-left sp)
    (forward-char
     (if (< sp len) sp
       (end-of-line)
       (skip-chars-backward " \t")
       0))))

(defun scroll-up-next-line (&optional step)
  (interactive)
  (let ((sp (if (numberp step) step (/ (window-height) 8))))
    (scroll-up sp)
    (next-line  sp)))

(defun scroll-down-previous-line (&optional step)
  (interactive)
  (let ((sp (if (numberp step) step (/ (window-height) 8))))
    (scroll-up-next-line (- sp))))

(defun kill-ring-save-line (&optional arg)
  (interactive "P")
  (kill-ring-save   (point)
   (save-excursion
     (if arg
	 (forward-line (prefix-numeric-value arg))
       (if (eobp)
	   (signal 'end-of-buffer nil))
       (if (or (looking-at "[ \t]*$") (and kill-whole-line (bolp)))
			  (forward-line 1)
	 (end-of-line)))
     (point))))

(defun occur-at-point()
  (interactive)
  (occur (thing-at-point 'word)))

(defun next-page (&optional n)
  (interactive "P")
  (or n (setq n 1))
  (dotimes (i n)
    (search-forward "" (point-max))
    ))

(defun previous-page (&optional n)
  (interactive "P")
  (or n (setq n 1))
  (dotimes (i n)
    (search-backward "" (point-max))
    ))

(defun read-args (func args)
  (eval
   (car
    (read-from-string
     (concat
      "(list " (read-from-minibuffer (format "Input argument of %s : " (cons func args))) ")")))))

(defun eval-last-defun (&optional printp)
  (interactive "P")
  (let (ret)
    (save-excursion
      (backward-sexp)
      (letl (sym func args . body) (read (current-buffer))
	(if (eq sym 'defun)
	    (funcall (if printp 'princ-buffer (cutr message (format "%s" <>)))
		     (if (consp args)
			 (apply func (read-args func args))
		       (funcall func)))
	  (message "eval-last-defun: not eq defun")))) ))

(defun tags-search-at-point (&optional file)
  (interactive)
  (let ((file (read-string "search for: " (current-word))))
    (tags-search file)))

(defun kill-rectangle1-in-paragraph ()
  (interactive)
  (let* ((p (current-column))
	 (beg (point))
	 (end (begin
	       (forward-paragraph)
	       (previous-line 1)
	       (beginning-of-line)
	       (forward-char (1+ p))
	       (point))))
    (kill-rectangle beg end)
    (goto-char beg)))

(defun insert-pwd ()
  (interactive)
  (insert default-directory))

(defun my-other-window (&optional x)
  (interactive "p")
  (call-interactively 'other-window)
  (if (equal (buffer-name) "*sdic*")
      (call-interactively 'other-window)))

(defmacro system* (name &rest args)
  `(shell-command-to-string (string-join (list ,(->string name) ,@(mapcar '->string args)) " ")))

(defun touch-at-point (&optional i)
  (interactive)
  (let ((path (thing-at-point 'filename)))
    (shell-command (concat "touch " path))))

;;;###autoload
;;(defun my-ffap-w3m (&optional filename)
;;  (interactive (list (thing-at-point 'filename)))
;;  (if (rxmatch "\\.\\(jpeg\\|jpg\\|gif\\|png\\|tif\\|flv\\|pdf\\|mpeg\\|avi\\|vob\\|mp3\\|wav\\)$"filename)
;;      (w3m-external-view filename)
;;    (w3m-browse-url filename)))

(defmacro point* (&rest body)
  `(save-excursion ,@(mapcar (lambda (x) (if (symbolp x) (list x) x)) body)  (point)))

(defun shell-switch (&rest plist)
  (aif (cdr (completing-read-by-plist plist))
       (cond ((stringp it)
	      (shell-command (concat it " &")))
	     (t (eval it)))))

(defun insert-header-line-emacs(&optional a)
  (interactive)
  (let((comment (if (member mode-name '("Emacs-Lisp" "Lisp" "Scheme"))
		    (concat comment-start comment-start)
		    comment-start)))
    (printf "%s -*-encoding:utf-8; mode:%s -*-\n%s %s,v 0.0.0\n" comment mode-name comment comment  (buffer-file-name))))

(defun shell-switch-default ()
  (interactive)
  (let ((word (thing-at-point 'word))
		(file (file-name-nondirectory (buffer-file-name ))))
    (shell-switch
     'head '(insert-header-line-emacs)
	 'cd   (list 'find-file (concat "test/" file))
	 'diff `(my-tramp-ediff-102)
	 'file '(my-tramp-find-file-102)
	 'yas `(yas/reload-all)
	 )))

(defun shell-command* (fom &rest args)
  (shell-command (apply 'format fom args)))

(defun systemf (fom &rest args)
  (shell-command (apply 'format fom args)))

(defun eval-first-sexp ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "(")
	(progn
	  (backward-char 1)
	  (eval (read (current-buffer)))))))

(defun browse-url-firefox-2ch ()
  (interactive)
  (let ((url (thing-at-point 'line)))
    (browse-url-firefox
     (if (and (> (length url) 0) (= (aref url 0) ?t))
	 (concat "h" url)
       url))))

(defun find-file/gzip-hexl (&optional file)
  (interactive "fFile/gzip/hexl:")
  (let1 file (or file (read-string "File/gzip/hexl: " (thing-at-point 'file) 'file-name-history))
    (when (and file (file-exists-p file))
      (let1 file-gz (concat file ".gz")
	(copy-file file file-gz t)
	(find-file file-gz)
	(hexl-mode)))))

(defun save-buffer//gz ()
  (interactive)
  (save-buffer)
  (let1 strs (split-string  (buffer-file-name) ".gz")
    (copy-file (buffer-file-name) (car strs) t)))

(defun extract-rectangle* (start end &optional fill)
  (interactive "r\nP")
  (mapcar (cut printf "%s\n" <>) (extract-rectangle start end)))

(defun format-bit (fom &rest args)
  (let ((ret '()) (len 0) (ret-tmp 0))
    (dolist (e (mapcar 'identity fom))
      (let ((a (pop args)))
	(dotimes (i (- e 48))
	  (setq ret-tmp	(logior ret-tmp	(ash (logand 1 a) (mod len 8)))
		a (ash a -1))
	  (when (= (mod (inc! len) 8) 0)
	      (push ret-tmp ret)
	      (setq ret-tmp 0)))))
    (if (not (= (mod len 8) 0))
	(push ret-tmp ret))
    (nreverse ret)))

(defun make-file-cache-file (path)
  (list (file-name-nondirectory path)
	(file-name-directory path)))

(defvar my-file-cache-ignore-ht nil)
(defvar my-file-cache-special-list nil)
(defvar my-file-cache-alist-8 nil)

(defun my-file-cache-minibuffer-complete ()
  (interactive)
  (let* ((file-cache-alist my-file-cache-alist-8)
	 (ret (call-interactively 'file-cache-minibuffer-complete)))
    ret))

(defun completing-read-tags ()
  (interactive)
  (let* ((pattern (or (thing-at-point 'sexp)
		      (progn
			(backward-char 1)
			(let1 ret (thing-at-point 'sexp)
			      (backward-char 1)
			      ret))))
	 (init (point))
	 (beg (progn (backward-sexp 1) (point)))
	 (candi (if pattern (take (all-completions pattern (tags-lazy-completion-table)) 300)))
	 (len (length candi))
	 (str  (cond ((= len 0) nil)
		     ((= len 1) (car candi))
		     (t (completing-read-by-list candi pattern))))
	 )
    (if str
	(if (string= str pattern)
	    (goto-char init)
	  (delete-char (length pattern))
	  (insert str))
      (goto-char init))))

(defun map-region (proc)
  (interactive (list (read-string "proc or lambda: ")))
  (setq proc (if (stringp proc) (eval (read proc)) proc))
  (mapcar proc (string->sexps (region->string))))

(defun ln (&optional from to)
  (interactive (letl (dir file ext) (split-path (buffer-file-name))
		     (let1 it (read-file-name "link file from : " dir (if file (if ext (concat file "." ext) file)))
			   (list it
				 (if (and it (buffer-file-name) (string= it (buffer-file-name)))
				     (read-file-name "link file to : " )
				   (read-file-name "link file from : " dir (if file (if ext (concat file "." ext) file))))))))
  (if (and from to)
      (shell-command-to-string (format "ln -s '%s' '%s'" (expand-file-name from) (expand-file-name to)))))

(defvar *cl-tyt-system-dir* "~/share/lib/lisp/systems/")

(defun ln-asd-file (&optional from)
  (interactive)
  (shell-command (format "ln -s '%s' '%s'" (expand-file-name (buffer-file-name)) (expand-file-name *cl-tyt-system-dir*))))

(defun find-file-asd (&optional from)
  (interactive)
  (find-file  (letl (d f e)  (split-path (buffer-file-name))
		    (concat d "/" f ".asd"))))

;; windows.el
(defun grep-ext (&optional args)
  (interactive)
  (let ((thing (thing-at-point 'word))
	(cbuf (current-buffer)))
    (win-switch-to-window 1 9)
    (switch-to-buffer cbuf)
    (shell (get-buffer-create
	    (format "*shell%d*" (+ (length (fold (^ (knil buf) (aif (buffer-name buf)
								    (if (rxmatch "\\*shell[0-9]*\\*" it)
									(cons buf knil)
								      knil)
								    knil)) '() (buffer-list))) 1))))
    (printf "grep -nH -e \"%s\" *" thing)
    (end-of-line)))

(defun set-iswitchb-buffer-ignore ()
  (setq iswitchb-buffer-ignore
	'("^ "
	  "^\\*\\(anything\\|Mini\\|BU\\|\\.skk\\|候補\\|cl\\|s\\(ldb\\|kk\\|uikyo\\|dic\\)\\|Help\\|tips\\|hown\\|tramp\\|debug\\|info\\|uzm\\|Message\\|Shell C\\|Comp\\|Ediff\\|scra\\)"
	  "^\\*slime"
	  "^\\*info"
	  "^:" ;;twitter
	  "\\.howm$"
	  )))

(defvar iswitchb-buffer-ignore2 '("^[^.*]+$"
				  "^TAGS$"
				  ".tags$"
				  "\\..?$"
				  "\\.[^.<>]\\{4\\}[^.<>]+$"
				  ))
(defvar my-ignore-base '("\\.cl\\(<[^>]+>\\)?$" "\\.lisp\\(<[^>]+>\\)?$""\\.scm\\(<[^>]+>\\)?$"
			 "\\.css\\(<[^>]+>\\)?$" "\\.js\\(<[^>]+>\\)?$" "\\.asd\\(<[^>]+>\\)?$"
			 "\\.sql\\(<[^>]+>\\)?$" "\\.el\\(<[^>]+>\\)?$" ))

(defvar my-ignore2 '("\\.\\([^cC].\\|.[^lL]\\)$"
		     "\\..\\([^sSmM].\\|.[^dDlL]\\)$"
		     "\\...\\([^sS].\\|.[^Pp]\\)$"
		     ))

(defvar my-ignore3 '("\\...$" ;;
		     "\\..\\([^cC].\\|.[^mM]\\)$"
		     "\\.....$"))
(defvar my-ignore4 '("\\.\\([^jJ].\\|.[^sS]\\)$"
		     "\\..\\([^sS].\\|.[^sS]\\)$"
		     "\\.....$"))
(defvar my-ignore5 '("\\...$"
		     "\\..\\([^qQ].\\|.[^lL]\\)$"
		     "\\.....$"))

(defvar my-ignore8 '("\\.\\([^eE].\\|.[^lL]\\)$"
		     "\\....$" 
		     "\\.....$"))

(defun my-find-file-log (pwd buf)
  (if (and buf (buffer-file-name buf)
	   (not (rxmatch "^/tmp" (buffer-file-name buf)))
	   (not (rxmatch "\\.elc$" (buffer-file-name buf)))
	   (not (rxmatch "\\.zip$" (buffer-file-name buf)))
	   (not (rxmatch "\\.tgz$" (buffer-file-name buf)))
	   (not (rxmatch "#$" (buffer-file-name buf))))
      (shell-command (format "echo '%s' '%s'  >> ~/log/ffap.history" (expand-file-name pwd) (expand-file-name (buffer-file-name buf))))))

(defun my-pwd ()
  (cadr (split-string (pwd) " ")))

(defalias 'my-dired-advertised-find-file 'my-dired-find-file)
(defun my-dired-find-file ()
  (interactive)
  (let* ((find-file-run-dired t)
	 (pwd (my-pwd))
	 (buf (find-file (dired-get-file-for-visit))))
    (my-find-file-log pwd buf)
    buf))

(defun my-find-tag (tagname &optional next-p regexp-p)
  (interactive (find-tag-interactive "Find tag: "))
  (let* ((pwd (my-pwd))
	 (buf (find-tag-noselect tagname next-p regexp-p))
	 (pos (with-current-buffer buf (point))))
    (condition-case nil
	(switch-to-buffer buf)
      (error (pop-to-buffer buf)))
    (my-find-file-log pwd buf)
    (goto-char pos)))

(defun get-file-ignore ()
  (cond ((= win:current-config 2)
	 (append iswitchb-buffer-ignore2 my-ignore2))
	((= win:current-config 3)
	 (append iswitchb-buffer-ignore2 my-ignore3))
	((= win:current-config 4)
	 (append iswitchb-buffer-ignore2 my-ignore4))
	((= win:current-config 5)
	 (append iswitchb-buffer-ignore2 my-ignore5))
	((= win:current-config 8)
	 (append iswitchb-buffer-ignore2 my-ignore8))
	((= win:current-config 9)
	 iswitchb-buffer-ignore2) ;; all
	(t
	 (append iswitchb-buffer-ignore2 my-ignore-base))))

(defun file-name-history-debug ()
  (interactive)
  (dolist (path (take file-name-history 10))
    (dolist (reg (get-file-ignore))
      (printf "filter? %s reg %s path %s \n"
	      (if (not (rxmatch reg path)) "t" "nil")
	      reg path))))
  
(defun ffap-local (&optional x)
  (interactive "P")
  (let (ret buf1)
    (let* ((ignores (get-file-ignore))
	   (fst (car* file-name-history nil))
	   (file-name-history
	    (if x
		file-name-history
		(cons-if (^ (lis) (cond ((or (null lis)  (null (car lis)))
					 fst)
					((and fst (string= fst (car lis)))
					 nil)
					(t fst)))
			 fst
			 (remove-if (cutr rxmatch-any ignores <>) file-name-history))))
	   (fst (car* file-name-history nil))
	   (pwd (my-pwd))
	   (file0 (thing-at-point 'filename))
	   (file1 (if (vectorp file0) (aref file0 0) file0))
	   (buf (cond ((rxmatch "^//" file1)
		       (call-interactively 'find-file))
		      ((and (find ?: file1)
			    (save-excursion
			      (or (rxmatch "[ \t\n]" (format "%c" (following-char))) (re-search-forward "[ \t\n]" nil t) (goto-char (point-max)))
			      (re-search-backward file1)
			      (= (preceding-char) (aref "\n" 0))))
		       (find-file (car (split-string file1 ":"))))
		      ((save-excursion
			 (or (rxmatch "[ \t\n]" (format "%c" (following-char))) (re-search-forward "[ \t\n]" nil t) (goto-char (point-max)))
			 (re-search-backward file1)
			 (= (preceding-char) ?<))
		       (call-interactively 'find-file))
		      (t
		       (call-interactively 'ffap)))))
      (setq buf1 buf)
      (if (and (not fst) file-name-history)
	  (setq ret (car file-name-history)))
      (if (and fst file-name-history (not (equal fst (car file-name-history))))
	  (setq ret (car file-name-history)))
      (my-find-file-log pwd buf))
    (if ret
	(push ret file-name-history))
    buf1))

(defun get-my-buffer-ignore ()
  (if (= win:current-config 9)
      '("^ ")
      (set-iswitchb-buffer-ignore)
      (append
       (cond ((= win:current-config 2)
	      (remove "^\\*slime" iswitchb-buffer-ignore))
	     ((= win:current-config 7)
	      (remove "^\\*info" iswitchb-buffer-ignore))
	     (t iswitchb-buffer-ignore))
       (cond ((= win:current-config 2)
	      (append iswitchb-buffer-ignore2 my-ignore2))
	     ((= win:current-config 3)
	      (append iswitchb-buffer-ignore2 my-ignore3))
	     ((= win:current-config 4)
	      (append iswitchb-buffer-ignore2 my-ignore4))
	     ((= win:current-config 5)
	      (append iswitchb-buffer-ignore2 my-ignore5))
	     ((= win:current-config 8)
	      (append iswitchb-buffer-ignore2 my-ignore8))
;;	     ((= win:current-config 9)
;;	      ) ;; all iswitchb-buffer-ignore2
	     (t
	      (append iswitchb-buffer-ignore2 my-ignore-base))))))

(defun my-iswitchb-buffer-debug ()
  (interactive)
  (let ((iswitchb-buffer-ignore (get-my-buffer-ignore)))
    (dolist (b (mapcar 'window-buffer (window-list)))
      (dolist (reg iswitchb-buffer-ignore)
	(printf "%s %s %s\n" reg (rxmatch  reg (buffer-name b))  (buffer-name b))))
    ))

(defun my-iswitchb-buffer ()
  (interactive)
  (let ((iswitchb-buffer-ignore (get-my-buffer-ignore)))
    (call-interactively 'iswitchb-buffer)))

(defun my-window-switch-all ()
  (let* ((regs (get-my-buffer-ignore))
	 (win-lis (fold (^ (knil w)(aif (buffer-file-name(window-buffer w))
				      (cons (expand-file-name it) knil)
				    knil)) '() (window-list)))
	 (lis (filter (cutr not (rxmatch-any regs (if (bufferp <>) (buffer-name <0>) <0>)))
		      (buffer-file-nub (append (buffer-list) file-name-history) win-lis))))
    (walk-windows*
     (if (rxmatch-any regs  (buffer-name))
	 (aif (pop lis)
	   (if (bufferp it)
	       (switch-to-buffer it)
	       (find-file it)))))))

(defun my-dabbrev-expand (&rest args)
  (interactive)
  (multiple-value-bind (templates start end) (yas/current-key)
    (if templates
	(call-interactively 'yas/expand)
	(call-interactively 'dabbrev-expand))))

(defun get-my-file-name-history (&optional all not-rev?)
  (let ((command (cond ((eq t all)  "awk '{print $2}' ~/log/ffap.history")
		       ((numberp all) (format "tail -n %d ~/log/ffap.history | awk '{print $2}'" all))
		       (t "tail -n 1000 ~/log/ffap.history | awk '{print $2}'"))))
    (if not-rev?
	(split-string (shell-command-to-string command) "\n")
	(cdr(nreverse (split-string (shell-command-to-string command) "\n"))))))

(defvar *my-eldoc-indent-function-tmp-path* "~/share/el-indent-tmp.el")
(defvar *my-eldoc-indent-function-path* "~/share/el-indent.el")

(defun my-eldoc-indent-function ()
  (interactive)
  (let (fun args-noop  ix-rest)
    (condition-case err
	(and (eldoc-display-message-no-interference-p)
	     (if eldoc-documentation-function
		 (eldoc-message (funcall eldoc-documentation-function))
		 (let* ((current-symbol (eldoc-current-symbol))
			(current-fnsym  (eldoc-fnsym-in-current-sexp))
			(doc (cond
			       ((null current-fnsym)
				nil)
			       ((eq current-symbol (car current-fnsym))
				(or (apply 'eldoc-get-fnsym-args-string
					   current-fnsym)
				    (eldoc-get-var-docstring current-symbol)))
			       (t
				(or (eldoc-get-var-docstring current-symbol)
				    (apply 'eldoc-get-fnsym-args-string
					   current-fnsym)))))
			(sp (split-string doc ": "))
			(fun2 (intern (car sp)))
			(args (car (read-from-string (cadr sp))))
			(args-noop2 (remove '&optional args))
			(ix-rest2 (find-index (cut eq '&rest <>) args-noop2))
			)
		   (setq fun fun2
			  args-noop args-noop2
			  ix-rest ix-rest2))))
      (error (message "eldoc error: %s" err)))
    (when (and (apropos-macrop fun) (> ix-rest  0))
      (progn
	(string->file/append (format "(put '%s 'lisp-indent-function %d)\n" fun ix-rest)
			     *my-eldoc-indent-function-tmp-path*)
	(put fun 'lisp-indent-function ix-rest))
      (shell-command (format "sort %s | uniq > %s" *my-eldoc-indent-function-tmp-path* *my-eldoc-indent-function-path*))
      t)))

(defun my-minibuffer-switch ()
  (interactive)
  (walk-windows* 
   (if (rxmatch "^ \\*Minibuf-[0-9]+\\*$" (buffer-name))
       (switch-to-buffer (get-buffer-create "*scratch*")))))

(defun beginning-of-buffer-sexp (&optional ARG)
  (interactive)
  (goto-char (point-min))
  (forward-sexp 1)
  (backward-sexp 1))

(defun end-of-buffer-sexp (&optional ARG)
  (interactive)
  (goto-char (point-max))
  (backward-sexp 1)
  (forward-sexp 1))

(setq kill-message-buffers-buffers '("*Messages*" "*Compile-Log*"))
(defun kill-message-buffers ()
  (interactive)
  (walk-windows
   (lambda (w)
     (if (member (buffer-name (window-buffer w)) kill-message-buffers-buffers)
	 (kill-buffer (window-buffer w))))))

(provide 'lstd-util)