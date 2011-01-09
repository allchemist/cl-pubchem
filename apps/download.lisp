(in-package :cl-pubchem)

(defun download-ftp-file (url &optional local-dest)
  (catch 'ftp-quit
    (ftp:with-ftp-connection (conn :hostname "ftp-private.ncbi.nlm.nih.gov"
				   :passive-ftp-p t)
      (ftp:send-cwd-command conn "pubchem/.fetch/")
      (let* ((filename (subseq url (1+ (position #\/ url :from-end t))))
	     (local (or local-dest (format nil "/tmp/~A" filename))))
	(ftp:retrieve-file conn filename local :if-exists :supersede)
	local))))

(defun download-for-id (id &key (parameter :smiles) (compression :none) filepath (time-thr 0.5))
  (let ((responce (pug (make-download-request (if (listp id) id (list id))
								  :parameter parameter
								  :compression compression))))
    (let ((request-id (parse-request-id responce)))
      (unless request-id
	(format t ";; Please wait~%")
	(progn (sleep time-thr)
	       (loop
		 (multiple-value-bind (xml proper-p)
		     (check-request request-id)
		   (cond ((not proper-p) (return :request-error))
			 ((not xml) (sleep time-thr))
			 (t (return (setf responce (pug (make-poll-request request-id))))))))))
      (format t ";; Request is complete~%")
      (let ((file (download-ftp-file (parse-download-url responce) filepath)))
	(format t ";; Requested file was downloaded successfully~%")
	(if (string= (subseq file (- (length file) 3)) ".gz")
	    (progn (format t ";; Uncompressing downloaded file~%")
		   (gzip-stream:gunzip file (subseq file 0 (- (length file) 3))))
	    (truename file))))))

;; read params from file

(defun read-txt (path)
  (let (params)
    (with-open-file (s path)
      (loop
	(let ((new-id (read s nil nil)))
	  (if new-id
	      (push (cons new-id (read-line s)) params)
	      (return)))))
    (nreverse params)))

(defun read-file (path)
  (ecase (intern (string-upcase (subseq path (1+ (position #\. path :from-end t)))) :keyword)
    (:txt (read-txt path))))

;; entire procedure

(defun ids->param (ids &key (parameter :smiles) (compression :none) (time-thr 0.5))
  (let ((file
	 (download-for-id ids
			  :parameter parameter
			  :compression compression
			  :time-thr time-thr)))
    (assert (not (eq file :request-error)) nil "Server signals for ill-formed request")
    (read-file (namestring file))))
