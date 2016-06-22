;;Created by Lucas Guerra and Eduardo Costa at NAIL (natural and artificial inteligence lab)
;;Feel free to use the code for free



(defparameter *node-size* 100)
(defparameter *n* 50)
(defparameter *list-size* 100)
(defparameter *empty* 0)



;; -----------------------Creations functions --------------------

(defun create (f-name)
  (if (stringp f-name)
      (progn(let ( (names (return-hst f-name)))
	      (handler-case  (progn(create-list-space (car names))
				   (createhash (cdr names) )
				   (format t "~A and ~A created with sucess" (car names) (cdr names)))
		(error (e) (format t "error : ~A" e))) ))
      (format t "not a string")))
      
      
(defun return-hst (f-name)
  (let ( ( hst-name (concatenate 'string f-name ".hst")))
    (cons f-name hst-name)))


;;function to create a hash
;;if the file already exist it pops an error
;;but dont break the execution



(defun createhash(fileName)
  (handler-case (with-open-file (stream fileName
					:direction :output
					:if-exists :error
					:if-does-not-exist :create)
		  "This loop create *N* * *nodeSize* free spaces"
		  (loop for i from 0 below (* *n* *node-size*)
		     do (format stream " "))
		  "This loop put 0 after *N* spaces"
		  (loop for i from 0 below *n* do
			   (file-position stream (* i *node-size*))
		       (format stream "~a" 0)))
    (error (e) (format t "error ~A" e))))

		   
;;Function to create a list
;;if the fileName already exist
;;Pops an error without breaking the code 

(defun create-list-space(fileName)
  (handler-case(with-open-file (stream fileName
				       :direction :output
				       :if-exists :error
				       :if-does-not-exist :create)
		 (loop for i from 0 to (* 3 *list-size*)
		    do (format stream " "))
		 (file-position stream 0)
		 (format stream "~a~%" 0)
		 (file-position stream (- *list-size* 2))
		 (format stream "~%"))
    (error (e) (format t "error ~A" e ))))

;;-------------------------------Manipulation functions -------------------------------

;;Take the first element in fileName that has nothing

(defun getFreePair(fileName)
  (handler-case  (with-open-file (stream fileName
					 :direction :io
					 :if-does-not-exist :error)
		   (file-position stream 0)
		   (let* ( (usedTop (read stream))
			  (top (+ usedTop 1))
			   (ptop (* top *list-size*)))
		     (file-position stream ptop)
		     (loop for i from ptop to (+ ptop *list-size*)
			do (format stream " "))
		     (file-position stream ptop)
		     (format stream "~a~%" '(0))
		     (file-position stream 0)
		     (format stream "~a~%" top)
		     top))
    (error (e) (format t "Erro ~A" e)) ))

;;Get the first free element in the filename and put xs and i

(defun fileCons(fileName xs i)
  (let* ( (free (getFreePair fileName))
	 (freePos (* free *list-size*)))
    " FreePos is the next free line in the file name"
    (handler-case(with-open-file (stream fileName
					 :direction :output
					 :if-does-not-exist :error)
		   (file-position stream freePos)
		   (format stream "~s" (list i xs)) 
		   (file-position stream (+ freePos -2  *list-size*))
		   (format stream "~%"))
      (error (e) (format t "~A" e)))
    free))

(defun prtPair(key stream i)
  (cond ( (equal i 0) (terpri))
	(t (file-position stream (* i *list-size*))
	   (let ((xs (read stream)))
	     (when  (equal (car (cadr xs)) key)
	       (print xs) (terpri))
             (prtPair key stream (car xs))) )))

;;print the key value inside the file db 

(defun file-prt-list(key fileName i)
  (handler-case (with-open-file (stream fileName
					:direction :input
					:if-does-not-exist :error)
		  (prtPair key stream i))
    (error (e) (format t "~a" e )) ))

;;xs has the format (key (value ))
;;(file2hash '("john" ("john@gmail.com") ) "lucas")


(defun file2hash(xs f-name)
  (let* ( (key (car xs))
	 (iHashTable (mod (sxhash key) *n*))
	  (hs-table (cdr (return-hst f-name))))
    (handler-case (with-open-file (stream hs-table
					  :direction :io
					  :if-does-not-exist :error)
		    (file-position stream (* iHashTable *node-size*))
		    (let* ( (hashIndex (read stream))
			   (listPos (fileCons f-name xs hashIndex)))
		      (file-position stream (* iHashTable *node-size*))
		      (format stream "~a" listPos)
		      listPos))
      (error (e) (format t"~A" e)) )))

		 
;;Print the value associate with key in the hashTable 
(defun prt-hash-value(key diskLIst)
  (let ((hashtable (cdr (return-hst disklist)) ))
    (handler-case (with-open-file (stream hashTable
					  :direction :input
					  :if-does-not-exist :error)
		    (let ( (iHashTable (mod (sxhash key) *n*)) )
		      (file-position stream (* iHashTable *node-size*))
		      (let* ( (hashIndex (read stream)))
			(file-prt-list key diskList hashIndex))) )
      (error (e) (format t "~A" e))) ))
	     
;;an error without breaking the code 

(defun create-list-space(fileName)
  (handler-case(with-open-file (stream fileName
				       :direction :output
				       :if-exists :error
				       :if-does-not-exist :create)
		 (loop for i from 0 to (* 3 *list-size*)
		    do (format stream " "))
		 (file-position stream 0)
		 (format stream "~a~%" 0)
		 (file-position stream (- *list-size* 2))
		 (format stream "~%"))
    (error (e) (format t "error ~A" e ))))

;;-------------------------------FUNÇÕES DE MANIPULAÇÃO -------------------------------

;;Take the first element in fileName that has nothing

(defun getFreePair(fileName)
  (handler-case  (with-open-file (stream fileName
					 :direction :io
					 :if-does-not-exist :error)
		   (file-position stream 0)
		   (let* ( (usedTop (read stream))
			  (top (+ usedTop 1))
			   (ptop (* top *list-size*)))
		     (file-position stream ptop)
		     (loop for i from ptop to (+ ptop *list-size*)
			do (format stream " "))
		     (file-position stream ptop)
		     (format stream "~a~%" '(0))
		     (file-position stream 0)
		     (format stream "~a~%" top)
		     top))
    (error (e) (format t "Erro ~A" e)) ))

;;Get the first free element in the filename and put xs and i

(defun fileCons(fileName xs i)
  (let* ( (free (getFreePair fileName))
	 (freePos (* free *list-size*)))
    " FreePos is the next free line in the file name"
    (handler-case(with-open-file (stream fileName
					 :direction :output
					 :if-does-not-exist :error)
		   (file-position stream freePos)
		   (format stream "~s" (list i xs)) 
		   (file-position stream (+ freePos -2  *list-size*))
		   (format stream "~%"))
      (error (e) (format t "~A" e)))
    free))

(defun prtPair(key stream i)
  (cond ( (equal i 0) (terpri))
	(t (file-position stream (* i *list-size*))
	   (let ((xs (read stream)))
	     (when  (equal (car (cadr xs)) key)
	       (print xs) (terpri))
             (prtPair key stream (car xs))) )))

;;print the key value inside the file db 

(defun file-prt-list(key fileName i)
  (handler-case (with-open-file (stream fileName
					:direction :input
					:if-does-not-exist :error)
		  (prtPair key stream i))
    (error (e) (format t "~a" e )) ))

;;xs has the format (key (value ))
;;(file2hash '("john" ("john@gmail.com") ) "lucas")


(defun file2hash(xs f-name)
  (let* ( (key (car xs))
	 (iHashTable (mod (sxhash key) *n*))
	  (hs-table (cdr (return-hst f-name))))
    (handler-case (with-open-file (stream hs-table
					  :direction :io
					  :if-does-not-exist :error)
		    (file-position stream (* iHashTable *node-size*))
		    (let* ( (hashIndex (read stream))
			   (listPos (fileCons f-name xs hashIndex)))
		      (file-position stream (* iHashTable *node-size*))
		      (format stream "~a" listPos)
		      listPos))
      (error (e) (format t"~A" e)) )))

		 
;;Print the value associate with key in the hashTable 
(defun prt-hash-value(key diskLIst)
  (let ((hashtable (cdr (return-hst disklist)) ))
    (handler-case (with-open-file (stream hashTable
					  :direction :input
					  :if-does-not-exist :error)
		    (let ( (iHashTable (mod (sxhash key) *n*)) )
		      (file-position stream (* iHashTable *node-size*))
		      (let* ( (hashIndex (read stream)))
			(file-prt-list key diskList hashIndex))) )
      (error (e) (format t "~A" e))) ))
	     
;;(create "test-file")
;; > test-file and test-file.hst created with sucess
;;------  Adding to the database --------
;;(file2hash '("lucas" ("lu.guerra7508@gmail.com")) "test-file")
;; > 1
;;(file2hash '("john" ("john@gmail.com" 21 "uberlândia")) "test-file")
;;2
;;----- Searching into database ---------
;;(prt-hash-value "peter norvig" "test-file"
;;(0 ("peter norvig" ("pn@gmail.com" 42 "WE LOVE LISP and AI")))
;;(prt-hash-value "lucas" "test-file")
;;(0 ("lucas" ("lu.guerra7508@gmail.com"))) 

