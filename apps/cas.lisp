(in-package :cl-pubchem)

(defun cas->id-list (cas-list)
  (mapcar #'(lambda (cas) (term->cid cas)) cas-list))

(defun id->smiles-list (id-list)
  (ids->param id-list :parameter :smiles :compression :gzip))

(defun cas->smiles-list (cas-list)
  (id->smiles-list
   (prog1
       (cas->id-list cas-list)
     (format t ";; Successfully obtained compound id's~%"))))
