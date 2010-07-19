;; -*-encoding:utf-8; mode:Emacs-Lisp -*-

(require 'cl)
(require 'utility)

(setq running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(defmacro flipx* (f a b) `(,f ,b ,a))

(defun ref (pos obj)
  (let ((as `((consp ,(cut flipx* nth <> <>))
	      (arrayp aref))))
    (funcall (cadr (lookupBy (^ (x) (funcall (car x) obj)) as)) obj pos)))

(defvar undef '())
(defun car* (x &rest default)
  (if (consp x)
      (car x)
    (car* default undef)))
(defun cdr* (x &rest default)
  (if (consp x)
      (cdr x)
    (car* default undef)))

(defalias 'cons* (symbol-function 'list*))
(defsubst xcons   (a b)        (cons b a))
(defsubst assocdr (key alist)  (cdr (assoc key alist)))
(defun circular-list (val &rest args)
  (let ((ret (cons val args)))
    (setcdr (last ret) ret)
    ret))

(defun circular-list-p (first)
  (let ((next first)
	(ret nil))
    (while (setq next (cdr next))
      (and (eq next first) (setq ret t next nil)))
    ret))

(defsubst display (x)
  (insert (->string x)))

(defsubst display2 (&rest args)
  (labels ((lp (xs depth)
	       (cond ((consp xs)
		      (mapcar (lambda (x) (lp x (1+ depth))) xs))
		     (t
		      (dotimes (i depth)
			(insert  "  "))
		      (printf "%s\n" (->string xs))))))
    (mapcar (lambda (x) (lp x 0)) args)))

(defmacro with-gensym (tag vars &rest body)
  (let ((ret '()))
    (dolist (var  vars)
      (push (list var `(gensym ,tag)) ret))
    `(let ,ret ,@body)))

(defmacro begin (&rest forms)
  `(progn ,@forms))

(defun safe:1+ (i)
  (if (integerp i) (1+ i) i))

(defun safe:1- (i)
  (if (integerp i) (1- i) i))

(defmacro inc! (place &rest args)
  `(setq ,place ,(if args (list* (quote +) place args) `(1+ ,place))))

(defmacro dec! (place &rest args)
  `(setq ,place ,(if args (list* (quote -) place args) `(1- ,place))))

(defmacro times! (place &rest args)
  (if args `(setq ,place ,(list* (quote *) place args)) place))

(defmacro div! (place &rest args)
  (if args `(setq ,place ,(list* (quote /) place args)) `(setq ,place (/ 1 ,place))))

(defmacro inccar! (x &optional y)
  (list 'setcar x (if y `(+ (car ,x) ,y) `(1+ (car ,x)))))

(defmacro inccdr! (x &optional y)
  (list 'setcdr x (if y `(+ (cdr ,x) ,y) `(1+ (cdr ,x)))))

(defmacro pop! (place)             `(prog1 (car ,place) (setq ,place (cdr ,place))))

(defmacro push! (place val)        `(setq ,place (cons ,val ,place)))

(defmacro push-append! (place lis) `(setq ,place (append ,lis ,place)))

(defmacro push-tail! (place val)
  `(if ,place (setcdr (nthcdr (1- (length ,place)) ,place) (cons ,val nil))
     (setq ,place (cons ,val nil))))

(defmacro pop-tail! (place)
  (let ((pos (make-symbol "p")))
    `(let ((,pos (1- (length ,place))))
       (if (<= ,pos 0) (prog1 (car ,place) (setq ,place nil)) 
	 (prog1 (nth ,pos ,place) (setcdr (nthcdr (1- ,pos) ,place) nil))))))

;; マクロを作るマクロ 産業廃棄物
(defmacro mass-productor (name vars &rest forms)
  (with-gensym "mass-productor" (ret place places )
    (let ((let-args '()))
      (dotimes (i (length vars))
	(push `(,(pop! vars) (nth ,i ,place)) let-args))
      `(defmacro ,name (&rest ,places)
	 (let ((,ret (list 'progn)))
	   (dolist (,place ,places (nreverse ,ret))
	     (let ,let-args
	       (push `(progn ,,@forms) ,ret))))))))

(defmacro let-string (str-value-list &rest body)
  `(let ,(mapcar (lambda (sval)
		   (list (intern (symbol-value (car sval))) (cadr sval)))
		 str-value-list) ,@body))

(defmacro macro:alias (alias name)
  `(defmacro ,alias (&rest args)
     (macroexpand (append (list (quote ,name)) args))))

(defmacro defprop (obj prop vars &rest forms)
  `(put (quote ,obj) (quote ,prop)
	'(lambda ,vars
	   ,@forms)))
(defmacro prop (obj prop &rest args)
  `(funcall (get (quote ,obj) (quote ,prop)) ,@args))
(defmacro putq (sym prop val) `(put (quote ,sym) (quote ,prop) ,val))
(defmacro getq (sym prop) `(get (quote ,sym) (quote ,prop)))
(defun keywordp (x)  (and (symbolp x) (= (aref (symbol-name x) 0) ?:)))

(defun keyword->string (x)
  (if (keywordp x)
    (let ((n (symbol-name x)))
      (substring n 1 (length n)))))

(defun set-keywords (x &rest args)
  (while (and args
	      (consp args)
	      (keywordp (car args))
	      (consp (cdr args)))
    (put x (car args) (cadr args))
    (setq args (cddr args)))
  args)

(defun format-to-symbol (format &rest args)
  (make-symbol (apply format args)))

(defun symbol-append (&rest args)
  (make-symbol
   (mapconcat (if$ (x) symbolp (symbol-name x) x) args "")))

(defun last-car (x)  (car (last x)))

(defun betweenp (num a b)  (and (>= num a) (<= num b) t))

(defalias 'reverse! (symbol-function 'nreverse))

(defmacro dolist-do  (var+lis+ret vars &rest body)
  (destructuring-bind (var lis . rest) var+lis+ret
    (let ((%lis (gensym "dolist2")))
      `(do* ,(append vars (list `(,%lis ,lis (cdr ,%lis)) `(,var (car ,%lis) (car ,%lis))))
	    ((null ,%lis) ,@rest)
	 ,@body))))

(defun nub (lis)
  (dolist-do (e lis (nreverse ret))
      ((ret '() (if (member e ret) ret (cons e ret))))))

(defun unique (lis)
  (nub lis))

(defun elem-by (eq? y lis)
  (do ((lis lis (cdr lis)))
      ((or (null lis) (funcall eq? y (car lis)))
       lis)));; (consp lis)

(defun nub-by (eq? lis)
  (dolist-do (e lis (nreverse ret))
      ((ret '() (if (elem-by eq? e ret)
		    ret
		    (cons e ret))))))

(defun nub-term (term lis)
  (let ((ret1 '())
	(ret  '()))
    (dolist (e lis (nreverse ret))
      (let((x (funcall term e)))
	(unless (member x ret1)
	  (setq ret (cons e ret)
		ret1 (cons x ret1)))))))

(defun zip (&rest lis-of-lis)
  (let (ret)
   (while (car lis-of-lis)
     (setq ret (cons (mapcar 'car lis-of-lis) ret)
	   lis-of-lis (mapcar 'cdr lis-of-lis)))
   (reverse ret)))

(defun zip-assoc (lis1 lis2 &optional ret)
  (while (and lis1 lis2)
    (setq ret (acons (car lis1) (car lis2) ret)
	  lis1 (cdr lis1)
	  lis2 (cdr lis2)))
   (reverse ret))

(defun for-each (basic:pred &rest basic:args)
  (while (not (memq nil basic:args))
    (apply basic:pred (mapcar 'car basic:args))
    (setq  basic:args (mapcar 'cdr basic:args))))

(defun find-index (pred lis)
  (do ((lis lis (cdr lis))
       (i 0 (1+ i)))
      ((or (funcall pred (car lis)) (null lis))  i)))

(defun range (a z)
  (let (ret)
    (while (<= a z)
      (setq ret (cons z ret)
	    z (1- z)))
    ret))

(defun iota (n &optional a step)
  (setq step (or step 1))
  (do ((n n (- n 1))
       (ret '() (cons a ret))
       (a (or a 0) (+ a step)))
      ((<= n 0) (nreverse ret))))

(defun joinBy  (proc lis con)
  (let* ((ret (cons (funcall proc (car lis)) '()))
	 (tail ret))
    (while (setq lis (cdr lis))
      (let ((new-ret (cons (funcall proc (car lis)) '())))
	(setcdr tail (cons con new-ret))
	(setq tail new-ret)))
    ret))

(defun joinByConcat (proc lis con)
  (apply 'concat (joinBy proc lis con)))

(defun any (any::pred lis)
  (fold (lambda (knil x) (or (funcall any::pred x) knil))
	nil lis))

(defun lookupBy (basic::pred lis)
  (while (and lis (not (funcall basic::pred (car lis))))
    (setq lis (cdr lis)))
  (and lis (car lis)))

(defun lookup-by (basic::pred lis)
  (while (and lis (not (funcall basic::pred (car lis))))
    (setq lis (cdr lis)))
  (and lis (car lis)))


(defun funcs-call (funcs &rest funcs:args)
  (mapcar (lambda (funcs:f) (apply funcs:f funcs:args)) funcs))

(defun funcs-apply (funcs &rest funcs:args)
  (mapcar (lambda (funcs:f) (apply funcs:f (append (car funcs:args) (cdr funcs:args)))) funcs))

(defun funcs-mapcar (funcs &rest funcs:args)
  (mapcar (lambda (funcs:f) (mapcar funcs:f funcs:args)) funcs))

(defun funcs-map (funcs &rest funcs:args)
  (mapcar (lambda (funcs:f) (apply 'mapcar* funcs:f funcs:args)) funcs))

(defun funcs-fold (funcs knil)
  (while funcs
    (setq knil (funcall (car funcs) knil)
	  funcs (cdr funcs)))
  knil)

(defun partition (partition:pred collection)
  (let ((filter '())
	(remove '()))
    (dolist (elem  collection (list filter remove))
      (if (funcall partition:pred elem)
	  (push elem filter)
	(push elem remove)))))

(defun plist-remove-element (key plis)
  (pfold (lambda (x y knil) (if (equal x key) knil (list* x y knil))) nil plis))

(defun mapp (proc lis)
  (nreverse (pfold (lambda (k v knil)
		     (cons (funcall proc k v) knil)) '() lis)))

(defun plist-ref (lis key &optional default)
  (pfold (lambda (k v knil)
	   (if (equal k key) v knil)) default lis))

(defun plist-ref-as-string (lis key &optional default)
  (let1 keys (->string key)
    (pfold (lambda (k v knil)
	     (if (string= (->string k) keys) v knil)) default lis)))

(defun completing-read-by-list (lis input &rest args)
  (apply 'completing-read
   (concat (string-join (mapcar '->string lis) "|") "\n")
   (mapcar (lambda (x) (list (->string x))) lis)
   nil
   t
   input args))

(defun completing-read-by-plist (plist)
  (let* ((ks (mapp (lambda (k v) k) plist))
	 (input
	  (completing-read
	   (concat (string-join (mapcar '->string ks) "|") "\n")
	   (mapcar (lambda (x) (list (->string x))) ks)
	   nil
	   t)))
    (cons input (plist-ref-as-string plist input nil))))

(defun list-split (lis list-split:pred)
  (let ((lis lis)
	(before '())
	(ret '()))
    (dolist (key lis (nreverse (cons (nreverse before) ret)))
      (if (funcall list-split:pred key)
	  (progn (push (nreverse before) ret)
		 (setq before nil))
	(push key before)))))

(defalias 'system (symbol-function 'shell-command))

(defmacro with-coding-system (coding &rest body)
  `(let* ((coding-system ,coding)
	  (prefix nil)
	  (coding-system-for-read coding-system)
	  (coding-system-for-write coding-system)
	  (coding-system-require-warning t)
	  (current-prefix-arg prefix))
     ,@body))

(defun path:unix->cygwin (path)
  (replace-all
   (replace-all(replace-all path "c:" "/cygdrive/c") "d:" "/cygdrive/d")
   "e:" "/cygdrive/e"))

(defun upto (a &optional b)
  (and (null b) (setq b 0))
  (do ((b (min a b))
       (a (max a b) (1- a))
       (ret '() (cons a ret)))
      ((< a b) ret)))

(defun shuffle (lis)
  (let* ((ret (vconcat lis))
	 (num (length ret))
	 val rnd)
    (dotimes (x  num ret)
      (setq rnd (random num)
	    val (aref ret x))
      (aset ret x (aref ret rnd))
      (aset ret rnd val))))

(defun random-permutation (a &optional b)
  (shuffle (upto a b)))

(defun rand-perm (a &optional b)
  (random-permutation a b))

(defun rotate-numchar (char num)
  (if (setq char (char-to-dec char))
      (aref "0123456789012345678"
	    (+ (mod num 10) char))))

(defun hex-to-dec (str)
  (let ((len (length str))
	char (sum 0) (i -1))
    (while (< (inc! i) len)
      (setq char (hexchar-to-dec (aref str i)))
      (if char (setq sum (+ (lsh sum 4) char))
	(error "hex-to-dec(%s): wrong char at %d" str (1+ i))))
    sum))

(defun char-to-dec (char)
  (if (betweenp char ?0 ?9) (- char ?0)))

(defun hexchar-to-dec (char)
  (cond ((betweenp char ?0 ?9) (- char ?0))
	((betweenp char ?A ?F) (- char ?A -10))
	((betweenp char ?a ?f) (- char ?a -10))))

(defmacro odd-even-th (odd even lis &rest forms)
  (let ((n (make-symbol "n")))
    `(let ((,odd  '())
	   (,even '()))
       (let ((,n 0))
	 (dolist (elem ,lis)
	   (if (evenp (inc! ,n)) (push elem ,even) (push elem ,odd)))
	 (setq ,even (reverse ,even)
	       ,odd (reverse ,odd)))
       ,@forms)))

(mass-productor math:make-list-operator (op lop lsop)
  `(defun ,lop (&rest args)
     (apply 'mapcar* (quote ,op) args))
  `(defun ,lsop (lis &rest args)
     (mapcar (^ (x)  (apply (quote ,op) x args)) lis)))

(math:make-list-operator
 (+ l+ list+scalar) (- l- list-scalar) (* l* list*scalar) (/ l/ list/scalar))

(defmacro list-inc!   (x y) `(setq ,x (l+ ,x ,y)))
(defmacro list-dec!   (x y) `(setq ,x (l- ,x ,y)))
(defmacro list-times! (x y) `(setq ,x (l* ,x ,y)))
(defmacro list-dev!   (x y) `(setq ,x (l/ ,x ,y)))

(defun string-join (strs con)
  (let ((ret (car strs)))
    (while (setq strs (cdr strs))
      (setq ret (concat ret con (car strs))))
    ret))
(defun symstr (x)
  (if (symbolp x) (symbol-name x) x))
(defun symjoin (strs con)
  (let ((ret (symstr (car strs))))
    (while (setq strs (cdr strs))
      (setq ret (concat ret con (symstr (car strs)))))
    ret))

(defun string-search (str char start &optional back)
  (if back
      (while (and start (not (= (aref str start) char)) )
	(if (= start 0)
	    (setq start nil)
	  (setq start (1- start))))
    (let ((max (- (length str) 1)))
      (while (and start (not (= (aref str start) char)))
	(if (= start max)
	    (setq start nil)
	  (setq start (1+ start))))))
  start)

(defun split-string1 (str delim)
  (let ((n (string-match delim str)))
    (if n (list (substring str 0 n) (substring str (1+ n)))
      (list str ""))))

(defun suffix-strip (path)
  (let ((x (split-string path "\\.")))
    (pop-tail! x)
    (apply 'concat x)))

(defun string-tail (str &optional charp)
  (if charp (aref str (1- (length str)))
    (substring str (1- (length str)))))

(defun string-delete-tail (str)
  (substring str 0 (1- (length str))))

(defun string-number-inc (string)
  (number-to-string (1+ (string-to-number string))))

(defun string-trim-zero (str)
  (number-to-string (string-to-number str)))

(defun string-taginside-regexp (string otag-reg ctag-reg)
  (let ((beg (string-match otag-reg string)))
    (when beg
      (inc! beg)
      (let ((end (string-match ctag-reg string beg)))
	(and end (substring string beg end))))))

(defun string-or-function (string:str)
  (if (stringp string:str) string:str (funcall string:str)))

(defmacro dotimes2 (i len1 j len2 &rest form)
  `(do ((,i 0 (1+ ,i)))
       ((>= ,i ,len1))
     (do ((,j  0 (1+ ,j)))
	 ((>= ,j ,len2))
       ,@form)))

(defun alist->hash-table (alis &optional test)
  (or test (setq test 'equal))
  (dolist-do (e alis ret)
      ((ret (make-hash-table :test test)))
    (puthash (car e) (cdr e) ret)))

(defun list->hash-table-key (lis &optional test)
  (or test (setq test 'equal))
  (dolist-do (e lis ret)
      ((ret (make-hash-table :test test)))
    (puthash e t ret)))

(defun filter-by-hash-table (ht lis)
  (dolist-do (e lis (nreverse ret))
      ((ret '() (if (gethash e ht nil)
		    (cons e ret)
		    ret)))))

(defun remove-by-hash-table (ht lis)
  (dolist-do (e lis (nreverse ret))
      ((ret '() (if (gethash e ht nil)
		    ret
		    (cons e ret))))))

;; (require 'cl) make-hash-tablを使った方がいい
(defun make-hashtable (size &optional init)
  (make-vector size init))
(defmacro hashtable:set! (obary name val)
  (if (symbolp name)
      `(set (intern ,(symbol-name name) ,obary) ,val)
    `(set (intern ,name ,obary) ,val)))
(defmacro hashtable:get (obary name)
  (if (symbolp name)
      `(eval (intern-soft ,(symbol-name name) ,obary))
    `(eval (intern-soft ,name ,obary))))
(defmacro hashtable:call (obary name &rest args)
  (if (symbolp name)
      `(funcall (eval (intern-soft ,(symbol-name name) ,obary)) ,@args)
    `(funcall (eval (intern-soft ,name ,obary)) ,@args)))
(macro:alias hget hashtable:get)
(macro:alias hset! hashtable:set!)
(macro:alias hcall hashtable:call)

(defun hashtable:update-by-asc (obary asc)
  (while asc
    (if (symbolp (caar asc))
	(hashtable:set! obary (symbol-name (caar asc)) (cdar asc))
      (hashtable:set! obary (caar asc) (cdar asc)))
    (setq asc (cdr asc)))
  obary)

(defun assoc->hashtable (asc)
  (hashtable:update-by-asc (make-hashtable (* 3 (length asc))) asc))

(defun printf (fom &rest args)
  (insert (apply 'format fom args)))

(defun fprintf (file fom &rest args)
  (princ (apply 'format fom args) file))


(defun prin1cb (sexp)
  (prin1 sexp (current-buffer)))

(defun insert1 (sexp)
  (insert (prin1-to-string sexp)))

(defun japanese-alphabet-downcasep (c)
  (betweenp c ?ａ ?ｚ))

(defun japanese-alphabet-upcasep (c)
  (betweenp c ?Ａ ?Ｚ))

(defun japanese-aplabet-upcase-char (c)
  (if (japanese-alphabet-downcasep c)
      (+ c (- ?Ａ ?ａ))
    c))
(defun japanese-aplabet-downcase-char (c)
 (if (japanese-alphabet-upcasep c)
     (+ c (- ?ａ ?Ａ))
   c))

(defun japanese-alphabet-upcase-region (start end)
  (letl it (buffer-substring start end)
    (delete-char (- start end))
    (insert (concat (mapcar 'japanese-aplabet-upcase-char it)))))
(defun japanese-alphabet-downcase-region (start end)
  (let ((str (buffer-substring start end)))
    (delete-char (- start end))
    (insert (concat (mapcar 'japanese-aplabet-downcase-char str)))))

(defun japanese-string-p (jstr)
  (not (string-match "\\Cj" jstr)))
(defun japanese-hiragana-p (jstr)
  (not (string-match "\\CH" jstr)))
(defun japanese-katakana-p (jstr)
  (not (string-match "\\CK" jstr)))
(defun japanese-kanji-p (jstr)
  (not (string-match "\\Ck" jstr)))
(defun japanese-udan-p (jstr)
  (if (find (string-to-char jstr) "ぅうくぐすずつづぬふぶぷむゅゆるゥウクグスズツヅヌフブプムュユルヴ" ) t nil))

(defun suffix= (path suf)
  (if (string-match (concat "." suf "$") path) t nil))

(defun mode-group ()
  (let ((mg
	 (assoc mode-name 
		'(("C++" "C") ("ObjC" "C")
		  ("TeX" "LaTeX")
		  ("Lisp Interaction" "Lisp")  ("Emacs-Lisp" "Lisp")
		  ("www" "HTML-VIEW") ("w3m" "HTML-VIEW") ("Navi2ch Article" "HTML-VIEW")
		  ("XML" "SGML")
		  ("iGosh" "Scheme")
		  ("Shell-script" "Shell-script")
		  ("R" "iEss")))))
    (if mg (cadr mg) mode-name)))

(defun sub-group ()
  (cond ((equal mode-name "Shell-script")
	 (group:shell-script-identify))
	((or (equal mode-name "Fundamental")
	     (equal mode-name "Text"))
	 (group:text-identify))))

(defvar group:text-search-alist
  '(()))

(defun group:shell-script-identify ()
  nil)

(defun group:text-identify ()
  (save-excursion
    (cond 
     (t
      (let ((search-alist group:text-search-alist)
	    retval  elem)
	(while search-alist
	  (setq elem (pop search-alist))
	  (goto-char (point-min))
	  (if (re-search-forward (cadr elem) (min (point-max) 100) t)
	      (setq search-alist nil
		    retval       (car elem))))
	retval)))))


(defun mm:make-image (path)
  nil)

(defmacro bench-marking (n &rest forms)
  `(let ((ct (current-time)))
     (dotimes (i ,n) ,@forms)
     (l- (current-time) ct)))

(defun assq! (k asc v)
  (aif (assq k asc)
       (progn (setcdr it v) asc)
       (acons k v asc)))

(defun set!-assoc (k asc v)
  (aif (assoc k asc)
       (progn (setcdr it v) asc)
       (acons k v asc)))

(defun number->string (n)
  (number-to-string n))
(defun id (x) x)

(defun xlist (x knil)
  (list knil x))
(defmacro compose (&rest args)
  (let ((sym (gensym "*compose*")))
    `(lambda (,sym) ,(fold 'xlist sym  (reverse args)) )))
(defun flip (f)
  (lambda (a b) (funcall f b a)))

(defun assq-delete! (asc1 key)
  (cond ((null asc1) nil)
	 ((equal (caar asc1) key) (cdr asc1))
	 (t
	  (let ((prev asc1)
		(asc (cdr asc1)))
	    (while asc
	      (if (equal (caar asc) key)
		  (progn (setcdr prev (cdr asc)) (setq asc nil))
		(setq prev asc asc (cdr asc)))))
	  asc1)))

(defmacro definteractive (name func &rest args)
  `(defun ,name ()
     (interactive)
     (,func ,@args)))

(defun nth-or (n lis)
  (while (and (consp lis) (>= (dec! n) 0))
    (setq lis (cdr lis)))
  (and (consp lis) (car lis)))

(defun assoc-ref (alis elem &optional default)
  (aif (assoc elem alis)
       (cdr it) default))

(defun ->string (s &optional seps)
  (cond ((stringp s) s)
	((consp s) (concat (->string (car s) (cdr seps)) (car seps)
			   (->string (cdr s) (cdr seps))))
	((symbolp s) (symbol-name s))
	((numberp s) (number-to-string s))
	(s "t")
	(t "nil")))

(defun flist (&rest fn)
  (mapcar (lambda (f) `(lambda ,@f)) fn))

(defun assoc->list (asc c1 c2)
  (lfold* ( (a b) asc) ret
    (if (null ret) (cons* b c1 a ret)
      (cons* b c1 a c2 ret))))

(defun repeat (obj leng)
  (let (ret)
    (dotimes (i leng ret)
      (push obj ret))))

(defmacro rep* (obj leng)
  `(repeat ,obj ,(if (consp leng) (length leng) leng)))

(defun replace-all (str x y)
  (string-join (split-string str x) y))

(defun escape-string (x)
  (replace-all x "\"" "\\\""))

(defun end-of-filep ()
  (= (point) (point-max)))

(defun min-by (valuate lis)
  (let ((x nil)
	(val nil))
    (dolist (e lis x)
      (let1 val2 (funcall valuate e)
	(if (or (not val) (< val2 val))
	    (setq x e
		  val val2))))))

(defun max-by (valuate lis)
  (let ((x nil)
	(val nil))
    (dolist (e lis x)
      (let1 val2 (funcall valuate e)
	(if (or (not val) (> val2 val))
	    (setq x e
		  val val2))))))

(defun directory-fold (proc knil dir-name)
  (let ((dirs (directory-files dir-name t))
	(dirlen (length (expand-file-name dir-name)))
	elem)
    (while (setq elem (pop! dirs))
      (when (< dirlen (length elem))
	(setq knil (funcall proc knil elem))))
    knil))

(defun image-filep (x)
  (member (file-name-extension x) '("gif" "jpg" "bmp" "png")))

(defun regexp-replace (reg string substitution)
  (replace-regexp-in-string reg substitution string))
(defalias 'rxmatch (symbol-function 'string-match))

(defun lines (str)
  (split-string str "\n"))

(defun concat-map (term pair)
  (cond ((consp pair)
	 (append
	  (concat-map term (car pair))
	  (concat-map term (cdr pair))))
	((null  pair)
	 '())
	(t (list (funcall term pair)))))

(defun mapcros (proc2 lis &optional connproc)
  (mapcros2 proc2 lis lis connproc))

(defun mapcros2 (proc2 lis1 lis2 &optional connproc)
  (mapcar
   (lambda (x)
     (dolist (e lis2)
       (funcall proc2 x e))
     (if connproc (funcall connproc)))
   lis1))

(defmacro hs-const (&rest body)
  `(lambda (x) (,@body)))

(defmacro dolist-cros (var1+var2+lis+connproc+ret &rest body)
  (letl (var1 var2 lis . rest) var1+var2+lis+connproc+ret
   (macroexpand `(dolist-cros2 (,var1 ,var2 ,lis ,lis ,@rest) ,@body))))
 
(defmacro dolist-cros2 (var1+var2+lis+lis2+connproc+ret &rest body)
  (letl (var1 var2 lis1 lis2 . rest) var1+var2+lis+lis2+connproc+ret
    (let ((connproc (if (consp rest) (car rest) nil))
	  (ret (if (and (consp rest) (consp (cdr rest))) (cadr rest) nil)))
      `(let ((it (mapcros2 (lambda (,var1 ,var2) ,@body) ,lis1 ,lis2 ,connproc))) ,ret))))

(defun sum (lis) (fold '+ 0 lis))

(defmacro $C (&rest rest) (macroexpand `(compose ,@rest)))
(defmacro gen (&rest rest)
  (cond ((null rest) (circular-list 0))
        ((= (length rest)  1) `(range 0 ,(car rest)))
	((= (length rest)  2) `(range ,(car rest) ,(cadr rest)))))

(defmacro $map (proc &rest rest)
 `(mapcar ,proc (,@rest)))

(defun integer->list (x base)
  (let ((n (/ x base)) (m (mod x base)))
    (if (>= n 10)
    (cons m (integer->list n base))
    (list m n))))

(defun backward-word-point (&optional n)
  (let* ((it (point))
	 (p (begin (backward-word (or n 1)) (point))))
    (goto-char it)
    p))

(defun proper-listp (o)
  (while (consp o)
    (setq o (cdr o)))
  (not o))
   
(defun read-lines (filePath)
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string
     (buffer-string) "\n" t)) )

(defun take (lis n)
  (let ((ret '()))
    (while (and lis (> n 0))
      (setq ret (cons (car lis) ret)
	    lis (cdr lis)
	    n (- n 1) ))
    (nreverse ret)))

(defun directory.childdir (dir &rest options)
  (filter (lambda (x)
	    (and (file-directory-p x) (not (rxmatch "/\\.?\\.$" x))))
	  (apply 'directory-files dir t options)))

(defun region->string ()
  (buffer-substring-no-properties (min (point) (mark)) (max (point) (mark))))

(defun string-trimr (str)
  (aif (rxmatch "[\n \t]+\0" (concat  str "\0")) (substring str 0 it) str))

(defun string->sexps (str)
  (setq str (string-trimr str))
  (do ((ret '())
       (elem nil))
      ((string= str "") (nreverse ret))
    (condition-case ex
	(setq elem (read-from-string str)
	      str (substring str (cdr elem))
	      ret (cons (car elem) ret))
      ('error (setq str "")))
    ))

(defvar +length*-dot+ -1)
(defvar +length*-circular+ -2)

(defun string? (a)
  (stringp a))

(defun string>0 (str)
  (not (string= str "")))

(defun number? (a)
  (numberp a))

(defun keyword? (a)
  (keywordp a))

(defun pair? (a)
  (consp a))

(defun null? (a)
  (null a))

(defun length* (lis)
  (letp lp ((slow lis)
            (lis lis)
            (n 0))
    (cond ((consp lis)
           (incf n)
           (setf lis (cdr lis))
           (cond ((consp lis)
                  (if (eq lis slow)
                      +length*-circular+
                      (lp (cdr slow) (cdr lis) (- n 1))))
                 ((null lis)
                  n)
                 (t +length*-dot+)))
          ((null lis)
           n)
          (t +length*-dot+))))

(defun proper-list? (lis)
  (> (length* lis) 0))

(defun dotted-list? (lis)
  (= (length* lis) +length*-dot+))

(defun circular-list? (lis)
  (= (length* lis) +length*-circular+))

(defun drop (n lis)
  (nthcdr n lis))

(defun == (a b)
    (equal a b))
(defun /== (a b)
  (not (equal a b)))

(defun remove-by (pred seq)
  (remove-if pred seq))

(defun filter (pred seq)
  (remove-if-not pred seq))

(defmacro with-tsv (var+path+rest &rest delimiter+body)
  (with-gensym "with-tsv" (%line)
    (letl (var path . rest) var+path+rest
      (let ((delimiter "\t"))
	(if (stringp (car delimiter+body))
	    (setq delimiter (pop delimiter+body)))
	`(dolist (,%line (split-string (file->string ,path) "\n") ,@rest)
	   (let ((,var (split-string ,%line ,delimiter)))
	     ,@delimiter+body))))))

(defun rxmatch-any (reg-lis str)
  (any (^ (rx) (rxmatch rx str)) reg-lis))

(defun buffer-file-nub (lis &optional ret)
  (let (ret2)
    (dolist (e lis (nreverse ret2))
      (if (bufferp e)
	  (aif (and (buffer-file-name e) (expand-file-name (buffer-file-name e)))
	      (unless (member it ret)
		(push it ret)
		(push e ret2)))
	  (let1 it (expand-file-name e)
	    (unless (member it ret)
	      (push it ret)
	      (push e ret2)))))))

(defun take-while (take-while::pred take-while::lis)
  (do ((take-while::ret '() (cons (car take-while::lis) take-while::ret))
       (take-while::lis take-while::lis (cdr take-while::lis)))
      ((or (null take-while::lis) (not (funcall take-while::pred (car take-while::lis))))
       (nreverse take-while::ret))))

(defun totalize (seq &rest cl-keys)
  (cl-parsing-keywords ((:rehash-threshold 0.8) (:test 'equal) (:size 100) (:rehash-size 2.0)) ()
    (totalize! (make-hash-table :rehash-threshold cl-rehash-threshold :test cl-test :size cl-size :rehash-size cl-rehash-size) seq)))

(defun totalize! (ht seq)
  (dolist (el seq ht)
    (aif (gethash el ht nil)
	(puthash el (+ 1 it) ht)
      (puthash  el 1 ht))))

(defun hash-table->alist (ht)
  (let ((ret '()))
    (maphash (lambda (k v) (push (cons k v) ret)) ht)
    ret))

(defmacro make-compare (base-function term)
  (with-gensym "make-compare" (%a %b)
    `(lambda (,%a ,%b) (,base-function (,term ,%a) (,term ,%b)))))

(defun take% (seq &optional odds)
  (let* ((sum (length seq))
	 (sum% (round (* sum (or odds 0.8)))))
    (mapcar 'car (take-while (cutr >= (decf sum% (cdr <>)) 0)
			     (sort (hash-table->alist (totalize seq :test 'equal :size sum :rehash-size 2.0))
				   (make-compare > cdr))))))

(defun cons-if (pred x lis)
  (if (funcall pred lis)
      (cons x lis)
      lis))

(defmacro with-tags-file-name (path &rest body)
  `(let ((tags-file-name ,path)
	 (tags-table-list '()))
     (aif (any (lambda (b) (string= (buffer-name b) "TAGS")) (buffer-list))
	 (kill-buffer it))
     ,@body))

(defmacro walk-windows* (&rest body)
  (let ((i (gensym)))
    `(dotimes (,i (length (window-list)))
       ,@body
       (other-window 1))))


(provide 'basic)
