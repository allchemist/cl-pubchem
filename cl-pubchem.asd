(defpackage :cl-pubchem-system
    (:use :cl :asdf))

(in-package :cl-pubchem-system)

(defsystem cl-pubchem
  :name "cl-pubchem"
  :description "PubChem interaction tools"
  :author "Khokhlov Ivan"
  :licence "BSD"
  :depends-on (trivial-http xmls)
  :serial t
  :components
  ((:file "package")
   (:file "pug")
   (:file "entrez")
   (:module apps
	    :serial t
	    :components
	    ((:file "download")
	     (:file "cas")))))
