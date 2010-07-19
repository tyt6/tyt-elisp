(require 'cl)
(require 'utility)
(require 'basic)

(defmacro define-keys (key-map &rest args)
  (let ((ret '()))
    (dolist (elem args `(progn ,@ret))
      (letl (key funcs shift-funcs)
	  (if (consp elem) elem (list elem))
	(let ((templates
	       (list (cutr vector
			   (if (= (length (symbol-name <>)) 1)
			       (aref (symbol-name <0>) 0)
			       <0>))
		     (cutr vector (list 'control <>))
		     (cutr vector (list 'meta <>))
		     (cutr vector (list 'control 'meta <>))))
	      (shift-templates
	       (list (cutr vector (list 'shift <>))
		     (cutr vector (list 'shift   'control <>))
		     (cutr vector (list 'shift   'meta    <>))
		     (cutr vector (list 'shift   'control 'meta <>))))
	      (action
	       (lambda (funcs templates)
		 (if (consp funcs)
		     (push-append! ret
				   (delq nil
					 (mapcar*
					  (lambda (key-func func)
					    (if func `(define-key ,key-map
							  ,(funcall key-func key) (quote ,func))))
					  templates funcs)))
		     (push
		      `(define-key ,key-map
			   ,(funcall (car templates) key) (quote ,funcs)) ret))))
	      )
	  (and funcs       (funcall action funcs       templates))
	  (and shift-funcs (funcall action shift-funcs shift-templates)))))
    ))

;;(define-keys global-map
;; f1 C-f1 M-f1 C-M-f1
;;  (f1      (nil                   mode-info-describe-function nil  scheme-apropos)
;;    	   (manual-entry          mode-info-describe-variable)) ;;shift
;;  (f4      (browse-url-at-point nil kill-emacs)))


(provide 'my-emacs)