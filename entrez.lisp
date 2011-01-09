(in-package :cl-pubchem)

(defun entrez (term)
  (let ((responce (trivial-http:http-get
		   (format nil "http://www.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pccompound&term=~A" term))))
    (with-open-stream (s (caddr responce))
       (assert (= (car responce) 200) nil
	       "Return code not success (200): ~A" (car responce))
      (xmls:parse s))))

(defun term->cid (term)
  (third (parse-xml (entrez term) "Id")))
