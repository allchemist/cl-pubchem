(in-package :cl-pubchem)

;; utils

(defun convert-to-string (arg)
  (typecase arg
    (string arg)
    (symbol (string arg))
    (t (write-to-string arg))))

(defun parse-xml (xml tag)
  (let (matches)
    (labels ((search-xml-tag (cur)
	       (cond ((null cur) nil)
		     ((atom cur) nil)
		     ((equal (car cur) tag) (push cur matches))
		      (t (progn (search-xml-tag (car cur))
				(search-xml-tag (cdr cur)))))))
      (search-xml-tag xml))
    (if (= (length matches) 1)
	(car matches)
	(nreverse matches))))

;; make requests

(defun make-poll-request (request-id)
  `("PCT-Data" NIL
    ("PCT-Data_input" NIL
     ("PCT-InputData" NIL
      ("PCT-InputData_request" NIL
       ("PCT-Request" NIL ("PCT-Request_reqid" NIL ,(convert-to-string request-id))
        ("PCT-Request_type" (("value" "status")))))))))

(defun make-download-request (ids &key (parameter :smiles) (compression :none))
  `("PCT-Data" NIL
    ("PCT-Data_input" NIL
     ("PCT-InputData" NIL
      ("PCT-InputData_download" NIL
       ("PCT-Download" NIL
        ("PCT-Download_uids" NIL
         ("PCT-QueryUids" NIL
          ("PCT-QueryUids_ids" NIL
           ("PCT-ID-List" NIL ("PCT-ID-List_db" NIL "pccompound")
            ("PCT-ID-List_uids" NIL
				,@(mapcar #'(lambda (id)
					     (list "PCT-ID-List_uids_E" NIL (convert-to-string id)))
					  (if (atom ids) (list ids) ids)))))))
	("PCT-Download_format" (("value" ,(string-downcase parameter))))
	,@(unless (eq compression :none)
	    `(("PCT-Download_compression" (("value" ,(string-downcase compression))))))))))))

(defun make-query-source-request (data)
  `("PCT-Data" NIL
    ("PCT-Data_input" NIL
     ("PCT-InputData" NIL
      ("PCT-InputData_query" NIL
       ("PCT-Query" NIL
        ("PCT-Query_type" NIL
         ("PCT-QueryType" NIL
          ("PCT-QueryType_css" NIL
           ("PCT-QueryCompoundCS" NIL
            ("PCT-QueryCompoundCS_query" NIL
             ("PCT-QueryCompoundCS_query_data" NIL ,(convert-to-string data)))
	    ("PCT-QueryCompoundCS_type" NIL
	     ("PCT-QueryCompoundCS_type_identical" NIL
	      ("PCT-CSIdentity" (("value" "same-stereo-isotope")) "5")))
	    ("PCT-QueryCompoundCS_results" NIL "1")))))))))))

(defun make-query-dest-request (key webenv &key (format :smiles) (compression :none))
  `("PCT-Data" NIL
    ("PCT-Data_input" NIL
     ("PCT-InputData" NIL
      ("PCT-InputData_download" NIL
       ("PCT-Download" NIL
        ("PCT-Download_uids" NIL
         ("PCT-QueryUids" NIL
          ("PCT-QueryUids_entrez" NIL
           ("PCT-Entrez" NIL ("PCT-Entrez_db" NIL "pccompound")
            ("PCT-Entrez_query-key" NIL ,(convert-to-string key))
            ("PCT-Entrez_webenv" NIL ,webenv)))))
        ("PCT-Download_format" (("value" ,(string-downcase format))))
        ,@(unless (eq compression :none)
	    `("PCT-Download_compression" (("value" ,(string-downcase compression)))))))))))

;; sending

(defun pug (xml)
  (let ((responce (trivial-http:http-post "http://pubchem.ncbi.nlm.nih.gov/pug/pug.cgi?" "text/xml"
					  (if (listp xml)
					      (xmls:toxml xml)
					      xml))))
    (with-open-stream (s (caddr responce))
       (assert (= (car responce) 200) nil
	       "Return code not success (200): ~A" (car responce))
      (xmls:parse s))))

(defun check-request (request-id)
  (let* ((responce (pug (make-poll-request request-id)))
	 (status (second (caadr (parse-xml responce "PCT-Status")))))
    (cond ((equal status "success")
	   (values responce t))
	  ((equal status "running")
	   (values nil t))
	  (t (values nil nil)))))

;; parse responces

(defun parse-request-id (xml) (third (parse-xml xml "PCT-Waiting_reqid")))
(defun parse-ids (xml) (mapcar #'third (parse-xml xml "PCT-ID-List_uids_E")))
(defun parse-download-url (xml) (third (parse-xml xml "PCT-Download-URL_url")))

(defun parse-query (xml)
  (values (third (car (parse-xml xml "PCT-Entrez_query-key")))
	  (third (car (parse-xml xml "PCT-Entrez_webenv")))))

;; 
