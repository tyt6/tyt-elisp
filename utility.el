(defmacro ^ (args &rest body)
  `(lambda ,args ,@body))

(defmacro letp (name vars &rest body)
  `(labels ((,name ,(mapcar 'car vars) .,body))
     (,name .,(mapcar #'cadr vars))))

(defmacro let1 (var val &rest body)
  `(let ((,var ,val)) ,@body))

(defmacro aif (pred x &rest y)
  `(let ((it ,pred))
     (if it ,x ,@y)))

;;;###autoload
(defun accum (end? term succ kons knil &rest args)
  (let (tmp)
    (while (not (some end? (setq tmp (mapcar term args))))
      (setq knil (apply kons knil tmp)
	    args (mapcar succ args)))
    knil))

(defun negative? (obj)
  (and (numberp obj) (< obj 0)))

(defun negativep (obj)
  (and (numberp obj) (< obj 0)))

;;;###autoload
(defun fold (fold::proc fold::knil fold::lis &rest fold::rest)
  (if fold::rest
      (if (cdr fold::rest) ;; n fold
	  (while fold::lis
	    (setq
	     fold::knil (apply fold::proc fold::knil (car fold::lis) (mapcar 'car fold::rest))
	     fold::lis (cdr fold::lis) fold::rest (mapcar 'cdr fold::rest)))
	(setq fold::rest (car fold::rest))  ;; 2 fold
	(while fold::lis
	  (setq fold::knil
		(funcall fold::proc fold::knil (car fold::lis) (car fold::rest))
		fold::lis  (cdr fold::lis) fold::rest (cdr fold::rest))))
    (while fold::lis ;; 1 fold
      (setq fold::knil (funcall fold::proc fold::knil (car fold::lis))
	    fold::lis  (cdr fold::lis))))
  fold::knil)

;;;###autoload
(defun pfold (pfold:term pfold:knil pfold:plis &rest pfold:rest)
  (while pfold:plis
    (setq pfold:knil (funcall pfold:term (car pfold:plis) (cadr pfold:plis) pfold:knil)
	  pfold:plis (cddr pfold:plis)))
  pfold:knil)

(defun %%macro-compose (procs sym)
  (fold (^ (knil x) (list x knil)) sym (reverse procs)))

(defun %%variable-tree-accessor (proc ret tree target-sym)
  (cond ((null tree) ret)
	((vectorp tree)
	 (let1 ix -1
	       (reduce (^(knil v)(%%variable-tree-accessor (cons `(lambda (x)(aref x ,(incf ix))) proc) knil v  target-sym))
		       tree
		       :initial-value ret)))
	((consp tree)
	 (%%variable-tree-accessor
	  (cons 'cdr proc)
	  (%%variable-tree-accessor (cons 'car proc) ret (car tree) target-sym)
	  (cdr tree) target-sym))
	(t (cons (list tree (%%macro-compose proc target-sym)) ret))))

;;;###autoload
(defmacro let-match1 (vars lis &rest forms)
  (cond ((null vars) (error "null patern"))
	((consp vars)
	 (let ((sym (gensym "tmp")))
	   `(let ((,sym ,lis))
	      (let ,(%%variable-tree-accessor nil nil vars sym)
		,@forms))))
	((vectorp vars)
	 (let ((sym (gensym "tmp")))
	   `(let ((,sym ,lis))
	      (let ,(%%variable-tree-accessor nil nil vars sym)
		,@forms))))
	(t `(let ((,vars ,lis)) ,@forms))))

;;;###autoload
(defalias 'letm 'let-match1)
(defalias 'letl 'let-match1)

(defmacro let-v (vars vec &rest body)
  (let ((%vec (gensym "let-v"))
	(c -1))
    `(let1 ,%vec ,vec
	   (let ,(mapcar (lambda (var) (list var (list 'aref %vec (incf c)))) vars)
	     ,@body))))

(defmacro let-a (vars vec &rest body)
  (let ((%vec (gensym "let-a"))
	(c -1))
    `(let1 ,%vec ,vec
	   (let ,(mapcar (lambda (var) `(,var (assoc-ref ,%vec ',var))) vars)
	     ,@body))))

(defmacro dolist* (vars &rest forms)
  (and (not (consp vars))   (not (consp (cdr vars)))
       (error "usage:(dolist* (var-patern list &optional ret) . forms)"))
  `(dolist (dolist*:elem ,(cadr vars) ,@(cddr vars))
     (letl ,(car vars) dolist*:elem ,@forms)))

;;;###autoload
(defmacro fold* (var-patern+lis ret+val+proc &rest forms)
  (if (not (and (consp var-patern+lis) (consp ret+val+proc)))
      (error "fold*: 1st argument or 2nd argument not pair.")
    (let ((%lis (gensym "fold*"))
	  (%car (gensym "fold*"))
	  (pforms (cond ((null forms) (error "fold*: null form."))
			((null (cdr forms)) (car forms))
			(t `(progn ,@forms)))))
      (letl (vars lis) var-patern+lis
	    (letl (ret val . proc) ret+val+proc
		  (let ((var-match (%%variable-tree-accessor nil nil vars %car)))
		    (if (not var-match) (setq %car vars))  ; non-nil and non-pair
		    `(let ((,%lis ,lis) (,ret ,val))
		       (while ,%lis
			 (let* ((,%car (car ,%lis)) ,@var-match)
			   (setq ,ret ,pforms
				 ,%lis (cdr ,%lis))))
		       ,(if proc (list (car proc) ret) ret))))))))

(defmacro map* (var+list &rest body)
  `(fold* ,var+list (knil '() nreverse) (cons ,@body knil)))

(defmacro lfold* (vars ret &rest body)
  `(fold* ,vars (,ret '() nreverse) ,@body))


(defmacro cut (func &rest forms)
  (let (args body)
    (dolist (elem forms)
      (cond ((eq elem '<>)
	     (let ((var (gensym "cut:")))
	       (push var args)
	       (push var body)))
	    (t (push elem body))))
    `(lambda ,(nreverse args) (,func ,@(nreverse body)))))

(defun *cutr::inline* (lis ret-var)
  (cond ((null lis)
	 (cons ret-var '()))
	((consp lis)
	 (letl (aret-var . aret) (*cutr::inline* (car lis) ret-var)
	       (letl (dret-var . dret) (*cutr::inline* (cdr lis) aret-var)
		     (cons dret-var (cons aret dret)))))
	((eq lis '<>)
	 (let ((it (gensym "cut:"))) 
	   (acons it ret-var it)))
	((symbolp lis)
	 (let ((str (symbol-name lis)))
	   (cond ((and
		   (> (length str) 2)
		   (= (aref str 0) ?<)
		   (= (aref str (1- (length str))) ?>)
		   (>= (aref str 1) ?0)
		   (<= (aref str 1) ?9))
		  (let* ((n (- (length ret-var) 1 (string-to-number (substring str 1))))
			 (var (nth n ret-var)))
		    (if (null var)
			(let ((sym (gensym "cutr:")))
			  (acons sym ret-var sym))
		      (cons ret-var var))))
		 (t (cons ret-var lis)))))
	(t (cons ret-var lis))))

(defmacro cutr (func &rest forms)
  (letl (vars . ret) (*cutr::inline* forms '())
	`(lambda ,(nreverse vars) (,func ,@ret))))

(defmacro $s (fom &rest args)
  `(funcall (cutr list ,@fom) ,@args))

(defmacro if$ (vars pred expr else-expr)
  `(lambda ,vars (if (,pred ,@vars) ,expr ,else-expr)))

(defun macroexpand-recursive (lis)
  (cond ((consp lis)
	 (macroexpand (cons (car lis) (mapcar #'macroexpand-recursive (cdr lis)))))
	(t lis)))

(defmacro option-init (&rest vars)
  `(progn ,@(fold* ((name val) vars) (ret '() nreverse)
		   (cons `(or ,name (setq ,name ,val)) ret))))

(provide 'utility)
