(require 'basic)
(defvar window:help-height 7)

;; windmoveが似た機能を持っている。

(mass-productor 
  window:set-place-create-macro (name docstring window-getter window-creator)
  `(defun ,name (buffer-name &optional size) ,docstring
     (let* ((old-buffer              (current-buffer))
	    (buffer   (get-buffer-create buffer-name))
	    (x-place-window    (or ,window-getter     ; nil,if only 1 window
				   ,window-creator)))
       (set-window-buffer x-place-window buffer)
       (when (eq (selected-window)  x-place-window)
	 (other-window 1)
	 (or (eq old-buffer buffer)
	     (set-window-buffer (selected-window) old-buffer)))
       x-place-window)))

(window:set-place-create-macro 
 (window:set-bottom-create 
  "not documented"
  (window:get-bottom)
  (split-window (selected-window)
		(- (window-height)
		   (or size window:help-height)) nil))
 (window:set-right-create 
  "no"
  (window:get-right)
  (split-window (selected-window)
		(- (window-width)
		   (or size (/ (window-width) 2))) t))
 )

(defun window:create-second-window (&optional name size)
  (option-init (name (buffer-name))
	       (size window:help-height))
  (let ((buf (get-buffer-create name)))
    (if (one-window-p)
	(set-window-buffer 
	 (split-window (selected-window) (- (window-height) size))
       buf))))

(defvar %%window-edges (if running-xemacs 'window-pixel-edges 'window-edges))

(defun window:get-place (get-specific-edge compare-for-direction)
  (let ((ret 1)  ret-win)
    (walk-windows
     (lambda (w)
       (if (window-minibuffer-p w) nil
	 (let ((val (funcall get-specific-edge (funcall %%window-edges w))))
	   (if (funcall compare-for-direction val ret)
	       (setq ret val   ret-win w)))))
     "non nil") ;; "no minibuffer!" xemacs? emacs? meadow?
    (if (< (funcall get-specific-edge (funcall %%window-edges ret-win)) 0)
	nil ret-win)))

(defun window:get-places (get-specific-edge compare-for-direction)
  (let ((ret 1)  (ret-win '()))
    (walk-windows
     (lambda (w)
       (if (window-minibuffer-p w) nil
	 (let ((val (funcall get-specific-edge (funcall %%window-edges w))))
	   (cond ((funcall compare-for-direction val ret)
		  (setq ret val   ret-win (list w)))
		 ((=  val ret)
		  (push  w ret-win)))
	     )))
     "non nil") ;; "no minibuffer!" xemacs? emacs? meadow?
    (if (< (funcall get-specific-edge (funcall %%window-edges (car ret-win))) 0)
	nil ret-win)))

(defun window:fit-window ()
  (interactive)
  (let ((ws (window:get-bottom-windows)))
    (dolist (w ws)
      (if (string-match "^*.+*$" (buffer-name (window-buffer w)))
	  (set-window-text-height w 6)))))

(defun window:get-bottom-windows ()
  (window:get-places 'cadr '>))
(defun window:get-top ()
  (window:get-place 'cadr '<))
(defun window:get-bottom ()
  (window:get-place 'cadr '>))
(defun window:get-left ()
  (window:get-place 'car '<))
(defun window:get-right ()
  (window:get-place 'car '>))

(defun compute-point ()
  (compute-motion (window-start) '(0 . 0) (point) '(1000 . 1000)
		  (window-width) (cons (window-hscroll) 0) (selected-window)))


;; windmoveを使った方がいい。
(defun move-window-by-direction (axis-edge-proc target-axis-edge-proc near-edge-proc motion)
  (let* ((current-window (selected-window))
	 (current-edges  (funcall %%window-edges current-window))
	 (current-axis   (funcall axis-edge-proc current-edges))
	 (add-comp (funcall (if (eq motion 'h) 'cadr 'caddr) (compute-point)))
	 (current-comp   (+ add-comp (funcall near-edge-proc current-edges)))
	 (largest -1)
	 (ret current-window))
    (walk-windows
     (lambda (w)
       (if (not (eq w current-window)) ; Not current-window
	 (let* ((edges       (funcall %%window-edges w))
		(target-axis (funcall target-axis-edge-proc edges))
		(target-comp (funcall near-edge-proc edges)))
	   (if (and (= current-axis target-axis)  ; same axis <- main
		    (<= target-comp current-comp) ; <- sub
		    (> target-comp largest))
	       (setq largest target-comp   ret  w)))))
     nil nil)
    (select-window ret)))


;;;###autoload
(defun left-window ()
  (interactive)
  (move-window-by-direction 'car 'caddr 'cadr 'v))

;;;###autoload
(defun right-window () 
  (interactive)
  (move-window-by-direction 'caddr 'car 'cadr 'v))

;;;###autoload
(defun up-window ()
  (interactive)
  (move-window-by-direction 'cadr 'cadddr 'car 'h))

;;;###autoload
(defun down-window ()
  (interactive)
  (move-window-by-direction 'cadddr 'cadr 'car 'h))

(provide 'window-controler)
