(require 'treemacs)
(require 'treemacs-treelib)
(require 'treemacs-rendering)
;; (require 'ein-ipynb-mode)
;; (require 'ein-jupyter)
(require 'ein-worksheet)
(require 'ein-log)
(require 'ein-process)
(require 'ein-notebooklist)
(require 'ein-cell)

(defun treemacs-list-ein-notebook()
  (let* (
		 (nblist-buffer
		  ;;暂且只考虑一个 booklist buffer的情况
		  (nth 0 (--filter
				  (s-starts-with? "*ein:notebooklist http://" (buffer-name it))
				  (buffer-list)
				  ))))
	(with-current-buffer nblist-buffer
	  (ein:notebooklist--order-data
	   (ein:$notebooklist-data ein:%notebooklist%)
	   ein:notebooklist-sort-field
	   ein:notebooklist-sort-order))))

(defvar-local ht-ein-chapters nil)

(defun treemacs-list-ein-items()
  (interactive)
  (when (not ht-ein-chapters)
	(setq ht-ein-chapters (make-hash-table)))  
  ;; (--map (format "%s" it) (treemacs-list-ein-buffer))
  (treemacs-list-ein-notebook)
  )

(defvar zyt-callback nil)
(defvar zyt-buffer nil)

(treemacs-define-entry-node-type TreemacsExt_EIN
  :label (propertize "EIN" 'face 'font-lock-keyword-face)
  :key 'TreemacsExt_EIN
  :open-icon (treemacs-get-icon-value 'list)
  :closed-icon (treemacs-get-icon-value 'list)
  :children
  (progn
	(unless (ein:jupyter-server-ready-p)
	  (ein:jupyter-server-start (executable-find ein:jupyter-server-command)
								"d:/Study/OpenCV/opencv-python-free-course-code/" t nil)
	  (while (not (ein:jupyter-server-ready-p))
		(sleep-for 100))
	  (ein:login (ein:notebooklist-ask-url-or-port)
				 (lambda (buffer _url-or-port) (pop-to-buffer buffer))
				 (when current-prefix-arg
				   (read-no-blanks-input "Cookie name: "))
				 (when current-prefix-arg
				   (read-no-blanks-input "Cookie content: "))
				 nil))
	(ein:notebooklist-open* "http://127.0.0.1:8888"
							nil
							nil
							(lambda (buffer _url-or-port)
							  (with-current-buffer zyt-buffer
								(funcall zyt-callback (treemacs-list-ein-notebook))
								)
							  ;; (switch-to-buffer buffer)
							  )
							)
	(setq zyt-callback callback)
	(setq zyt-buffer (current-buffer))
	)
  :child-type 'ein-item
  :async? t 
  )

(treemacs-define-expandable-node-type ein-item
  :closed-icon
  (cond
   ((and (plistp item) (equal "directory" (plist-get item :type)))
	(treemacs-icon-for-dir (plist-get item :path) 'closed))
   ((plistp item)
	(treemacs-icon-for-file (plist-get item :path)))
   ((object-of-class-p item ein:basecell)
	(ht-get treemacs-icons 'tag-leaf))
   (_ "• ")
   )
  :open-icon
  (cond
   ((and (plistp item) (equal "directory" (plist-get item :type)))
	(treemacs-icon-for-dir (plist-get item :path) 'open)
	)
   ((plistp item)
	(treemacs-icon-for-file (plist-get item :path)))
   ((object-of-class-p item ein:basecell)
	(ht-get treemacs-icons 'tag-leaf))
   (_ "+ ")
   )

  :label (if (plistp item)
			 (plist-get item :name)
		   (treemacs-ein-cell-to-string item)
		   )
  :key item
  :children
  (pcase (plist-get item :type)
	("directory"
	 (ein:notebooklist-open* "http://127.0.0.1:8888"
							 (plist-get item :path)
							 nil
							 (lambda (buffer _url-or-port)
							   (with-current-buffer zyt-buffer
								 (funcall zyt-callback (treemacs-list-ein-notebook))
								 )
							   ;; (switch-to-buffer buffer)
							   )
							 )
	 (setq zyt-callback callback)
	 (setq zyt-buffer (current-buffer))
	 )
	("notebook"
	 (let* (
			(path (plist-get item :path))
			)
	   (setq zyt-callback callback)
	   (setq zyt-buffer (current-buffer))
	   (ein:notebook-open
		"http://127.0.0.1:8888"
		path nil
		(lambda (notebook created)
		  (let ((buffer (ein:notebook-buffer notebook)))
			(with-current-buffer buffer
			  (ein:cell-goto (ein:worksheet-get-current-cell))
			  )
			(switch-to-buffer buffer)
			(condition-case err
				(funcall zyt-callback (ein:worksheet-get-cells (car (ein:$notebook-worksheets notebook))))
			  (t
			   (funcall zyt-callback (ein:worksheet-get-cells (car (ein:$notebook-worksheets notebook))))
			   )
			  )
			))
		)
	   (pop-to-buffer zyt-buffer '(display-buffer-in-previous-window . ()) t)
	   )
	 )
	(_
	 (funcall callback nil)
	 )
	)
  :child-type 'ein-item
  :more-properties `(:depth 0 :ein-file ,item :ein-cell ,item :ein-item  ,item)
  :ret-action #'jump-to-ein-item
  :async? t
  )

(defun ein-get-cells (depth &optional parent ein-buffer)
  (if parent
	  nil
	(with-current-buffer
		ein-buffer
	  (ein:worksheet-get-cells (ein:worksheet--get-ws-or-error))
	  )
	)
  )
(defun treemacs-ein-cell-to-string(cell)
  (with-current-buffer
	  ;; (car (treemacs-list-ein-buffer))
	  (ein:cell-buffer cell)
	(save-excursion
	  (let* (
			 ;; (cell item)
			 (start (condition-case var 
						(ein:cell-location cell :before-input)
					  (t (point-min))
					  )
					)
			 (end (condition-case var 
					  (ein:cell-location cell :after-input)
					(t (point-max))
					)
				  ))
		(goto-char start)
		(pcase (ein:cell-type cell)
		  ("markdown"
		   (progn
			 (next-line)
			 (setq start (point))
			 (if 
				 (ein:markdown-match-propertized-text 'ein:markdown-heading end)
				 (let* (
						(ret (match-string-no-properties 0))
						;; (ret "### <font color=\"green\">Read and display one frame </font>)")
						(regex "#*\s* \\(?:<.*?>\\)?\\([^<]*\\)\\(</.*>\\)?")
						)
				   (progn 
					 (string-match regex ret)
					 (or (match-string 1 ret) ret)
					 )
				   )
			   (buffer-substring-no-properties start (pos-eol))
			   )
			 )
		   )
		  ("code"
		   (progn
			 ;; (thing-at-point ')
			 (buffer-substring-no-properties start (pos-eol))
			 )
		   )
		  )
		;; (if (equal "markdown" (ein:cell-type cell))
		;; 	(progn
		;; 	  (next-line)
		;; 	  (setq start (point))
		;; 	  (ein:markdown-match-propertized-text 'ein:markdown-heading end)
		;; 	  (match-string-no-properties 0))
		;;   (buffer-substring-no-properties start (min end (+ start 20)))
		;;   )
		)
	  ))
  )
(treemacs-define-expandable-node-type ein-cell
  ;; :label (ein:cell-type item)
  :label
  (let ((cell item))
	(treemacs-ein-cell-to-string cell))
  :closed-icon
  (let (
		(children
		 (or
		  (gethash item ht-ein-chapters)
		  (progn 
			;; (print (format "not found %s rescan" item))
			(puthash item (or (ein-get-cells (1+ (treemacs-button-get (treemacs-current-button) :depth)) item (treemacs-button-get (treemacs-current-button) :ein-file))
							  'None)
					 ht-ein-chapters)
			)
		  )))
	(if (and children (not (eq children 'None)))
		"+ "
	  "• "))
  :open-icon
  (let (
		(children
		 (or
		  (gethash item ht-ein-chapters)
		  (progn 
			;; (print (format "not found %s rescan" item))
			(puthash item (or (ein-get-cells (1+ (treemacs-button-get (treemacs-current-button) :depth)) item (treemacs-button-get (treemacs-current-button) :ein-file))
							  'None)
					 ht-ein-chapters)
			)
		  )))
	(if (and children (not (eq children 'None)))
		"- "
	  "• "))
  :key item
  :children
  (let (
		(children
		 (or
		  (gethash item ht-ein-chapters)
		  (progn 
			;; (print (format "not found %s rescan" item))
			(puthash item (or (ein-get-cells (1+ (treemacs-button-get (treemacs-current-button) :depth)) item (treemacs-button-get (treemacs-current-button) :ein-file))
							  'None)
					 ht-ein-chapters)
			)
		  )))
	(if (eq children 'None)
		nil
	  children
	  )
	)
  :child-type 'ein-cell
  :more-properties
  `(:depth ,(1+ (treemacs-button-get (treemacs-current-button) :depth))
		   :ein-file ,(treemacs-button-get (treemacs-current-button) :ein-file))
  :ret-action #'jump-to-ein-cell
  )

(defun jump-to-ein-file (&optional _)
  (let* (
		 (info (treemacs-button-get (treemacs-current-button) :ein-file)) 
		 (path (plist-get info :path))
		 )
	(progn
	  (ein:notebook-open
	   "http://127.0.0.1:8888"
	   path nil
	   (lambda (notebook created)
		 (let ((buffer (ein:notebook-buffer notebook)))
		   (with-current-buffer buffer
			 (ein:cell-goto (ein:worksheet-get-current-cell))
			 )
		   (switch-to-buffer buffer)
		   ;; (pop-to-buffer buffer)
		   )))
	  )
	)
  )

(defun jump-to-ein-cell (&optional _)
  (let* (
		 ;; (ein-buf (car (treemacs-list-ein-buffer)))
		 (ein-buf (treemacs-button-get (treemacs-current-button) :ein-file))
		 (cell (treemacs-button-get (treemacs-current-button) :item))
		 (buffer (ein:cell-buffer cell))
		 ;; (ein-file (treemacs-button-get (treemacs-current-button) :ein-file))
		 )
	(switch-to-buffer buffer)
	;; (pop-to-buffer buffer)
	(ein:cell-goto cell)
	(message "After cell-goto")
	)
  )
(defun jump-to-ein-item (&optional _)
  (let*
	  (
	   (item (treemacs-button-get (treemacs-current-button) :ein-item))
	   )
	(cond
	 ((plistp item)
	  (jump-to-ein-file))
	 ((object-of-class-p item ein:basecell)
	  (jump-to-ein-cell)
	  ;; (print (format "jump to %s" item))
	  ;; (with-temp-buffer
	  ;; 	(message (format "jump to %s" item))
	  ;;  )
	  )
	 )
	)
  )

(defvar treemacs--project-of-extision-ein nil)
(setq treemacs--project-of-extision-ein (car (treemacs-workspace->projects (treemacs-current-workspace))))
(treemacs-enable-project-extension
 :extension 'TreemacsExt_EIN
 :position 'top
 ;; :predicate (lambda (_)t)
 ;; :predicate (lambda (project) (eq project (car (treemacs-workspace->projects (treemacs-current-workspace)))))
 :predicate (lambda (project) (eq project treemacs--project-of-extision-ein))
 )
;; (intern "treemacs-TreemacsExt_EIN-extension-instance")
;; (--map (print (car it)) treemacs--extension-registry)


(defun treemacs--current-tag-info ()
  Info-current-node
  )

(defun treemacs--current-tag-path-ein()
  (interactive)
  (let (
		(cur-node (format "(%s)%s" Info-current-file Info-current-node))
		(ret (list Info-current-node))
		(orig-window-start (window-start))
		(old-history Info-history)
		(old-history-list Info-history-list)
		(old-history-forward Info-history-forward)
		)
	(prog1 
		(save-excursion
		  (condition-case err
			  (while t
				(Info-up 'same-file)
				;; (print Info-current-node)
				(push Info-current-node ret)
				)
			(t nil);;(print "ZYT: Node has no Up"))
			)
		  (unless (equal cur-node (format "(%s)%s" Info-current-file Info-current-node))
			(Info-goto-node cur-node)
			(set-window-start (selected-window) orig-window-start)
			)
		  ;; (if (and (equal "dir" Info-current-file)
		  ;; 		   (equal '("Top") ret))
		  ;; 	  (list "c:" 'TreemacsExt_EIN)
		  ;; 	(append (list "c:" 'TreemacsExt_EIN (file-name-nondirectory Info-current-file)) (cdr ret))
		  ;; 	)
		  (cond
		   (
			(and (equal "dir" Info-current-file) (equal '("Top") ret))
			(list "c:" 'TreemacsExt_EIN)
			)
		   ((and (string-prefix-p "*" Info-current-file) (string-suffix-p "*" Info-current-file))
			;;对于比如通过 “M-x info-apropos”动态生成的临时Info文件，不支持定位，直接返回匹配失败
			nil
			)
		   (t (append (list "c:" 'TreemacsExt_EIN (file-name-nondirectory Info-current-file)) (cdr ret)))
		   )
		  )
	  (setq Info-history old-history)
	  (setq Info-history-list old-history-list)
	  (setq Info-history-forward old-history-forward)
	  )
	)
  )

;; (defvar-local matched-path-list nil)
(defun treemacs--tag-match-func-ein (path &optional current-tag-cache)
  (let ((current-tag-path (or current-tag-cache (treemacs--current-tag-path-ein))))
	(when
		(and
		 (listp path)
		 (eq 'TreemacsExt_EIN (nth 1 path))
		 ;; (not (memq path matched-path-list))
		 )
	  ;; (push path matched-path-list)
	  ;; (print (format "%s" path))
	  (cond
	   ((equal path current-tag-path)
		;; (print (format "Matched: %s" path))
		'matched)
	   ((and (<=(length path) (length current-tag-path)) (equal path (subseq current-tag-path 0 (length path))))
		;; (print "partial matched")
		;; (print (format "Parital Matched: %s to %s" path current-tag-path))
		'partial-matched
		;; 'partial-matched-ignore-following ;;暂未解决
		;;Info 的路径匹配和chm不同，部分匹配后，必定在此匹配路径的子节点中，
		;;返回'partial-matched-ignore-following，以告知treemacs--follow-tag-current-path略过后续忽略同级的其他节点
		;;直接进入此节点的展开搜索
		)
	   )
	  )
	)
  )
(provide 'TreemacsExt_EIN)
;; (treemacs-tag-follow-mode-add-ext ein
;; 								  ;; Info-mode
;; 								  ein-mode
;; 								  treemacs--tag-match-func-ein
;; 								  (treemacs--current-tag-path-ein)
;; 								  treemacs--project-of-extision-ein)

;; )
