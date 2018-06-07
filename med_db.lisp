; Запись к врачам в поликлинике
(DEFSTRUCT RECORD ID NAME WHEN ROOM)
"	
  id    int
  name  string
  when  timestamp
  room  int
"
(SETF (GET 'REGISTRY' ID_COUNTER) 5)
"PRE INSERT DATABASE"
(SETQ REGISTRY
  (CONS (MAKE-RECORD :ID 1 :NAME 'ERMAN	:WHEN  3735587520 :ROOM 1)
  (CONS (MAKE-RECORD :ID 2 :NAME 'SMITH :WHEN  3735587520 :ROOM 1)
  (CONS (MAKE-RECORD :ID 3 :NAME 'GREYS	:WHEN  3735587520 :ROOM 2)
  (CONS (MAKE-RECORD :ID 4 :NAME 'ZHILY :WHEN  3735587520 :ROOM 2)
  (CONS (MAKE-RECORD :ID 5 :NAME 'ERMAN	:WHEN  3735587520 :ROOM 3) 
  NIL))))))

(defconstant *day-names*
    '("Monday" "Tuesday" "Wednesday"
      "Thursday" "Friday" "Saturday"
      "Sunday"))
; *DAY-NAMES*

(DEFUN DATE2STR (WHEN)
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
        (decode-universal-time WHEN)
    (format t "When: ~2,'0d:~2,'0d:~2,'0d of ~a, ~d/~2,'0d/~d (GMT~@d) ~%"
              hour
              minute
              second
              (nth day-of-week *day-names*)
              month
              date
              year
              (- tz))))

(DEFUN MAIN ()
  (SETQ choice (PRINT_MENU))
  (COND
    ((EQ choice 0) T)
	((EQ choice 1)
	  (PRINT_REGISTRY REGISTRY)
	  (MAIN))
	((EQ choice 2)
	  (ADD_RECORD)
	  (MAIN))
	((EQ choice 3)
	  (FORMAT T "***** Edit record *****~%")
	  (FORMAT T "Input record ID: ")
	  (SETQ _id (READ))
	  (SETQ ed_record (SEARCH_RECORD REGISTRY (LAMBDA (n) (IF (EQ (RECORD-ID n) _id) T NIL))))
	  (IF (NULL ed_record)
	    (FORMAT T "The record not found~%")
		(PROGN (EDIT_RECORD (CAR ed_record))
		 (FORMAT T "The record has been edited.~%")))
	  (MAIN))
	((EQ choice 4)
	  (FORMAT T "***** Delete record *****~%")
      (FORMAT T "Input record ID: ")
	  (SETQ _id (READ))
	  (SETQ del_record (SEARCH_RECORD REGISTRY (LAMBDA (n) (IF (EQ (RECORD-ID n) _id) T NIL))))
	  (IF (NULL del_record)
	    (FORMAT T "The record not found~%")
		(PROGN (DELETE_RECORDS del_record)
		 (FORMAT T "The record has been deleted.~%")))
	  (MAIN))
	((EQ choice 5)
	  (SETQ choice (PRINT_SEARCH_MENU))
	  (COND
	    ((EQ choice 1)
		  (FORMAT T "Input ID: ")
		  (SETQ _id (READ))
		  (PRINT_REGISTRY 
		    (SEARCH_RECORD 
			  REGISTRY 
			  (LAMBDA (n) (IF (EQ (RECORD-ID n) _id) T NIL)))))
		((EQ choice 2) 
		  (FORMAT T "Input name: ")
		  (SETQ _name (READ))
		  (PRINT_REGISTRY 
		    (SEARCH_RECORD 
			  REGISTRY 
			  (LAMBDA (n) (IF (EQ (RECORD-NAME n) _name) T NIL)))))
		((EQ choice 3) 
		  (FORMAT T "Input when (timestamp): ")
		  (SETQ _when (READ))
		  (PRINT_REGISTRY 
		    (SEARCH_RECORD 
			  REGISTRY 
			  (LAMBDA (n) (IF (EQ (RECORD-WHEN n) _when) T NIL)))))
    ((EQ choice 4) 
		  (FORMAT T "Input room number: ")
		  (SETQ _room (READ))
		  (PRINT_REGISTRY 
		    (SEARCH_RECORD 
			  REGISTRY 
			  (LAMBDA (n) (IF (EQ (RECORD-ROOM n) _room) T NIL)))))    
		)
		(MAIN))
	(T
	  (MAIN))))
	  
  

(DEFUN PRINT_MENU ()
  (FORMAT T "***** Registry *****~%")
  (FORMAT T "1. Show Registry~%")
  (FORMAT T "2. Add record~%")
  (FORMAT T "3. Edit record~%")
  (FORMAT T "4. Delete record~%")
  (FORMAT T "5. Search record~%")
  (FORMAT T "0. Exit~%")
  (FORMAT T ">>")
  (READ))
  
(DEFUN PRINT_SEARCH_MENU ()
  (FORMAT T "***** Search record *****~%")
  (FORMAT T "1. Search by ID~%")
  (FORMAT T "2. Search by name~%")
  (FORMAT T "3. Search by when (timestamp)~%")
  (FORMAT T "4. Search by room number~%")
  (FORMAT T ">>")
  (READ))
  
(DEFUN PRINT_RECORD (n)
  (COND
    ((NULL n) NIL)
    (T 
	    (FORMAT T "***** RECORD #~A *****~%" (RECORD-ID n))
      (FORMAT T "Name: ~A~%" (RECORD-NAME n))
      (DATE2STR (RECORD-WHEN n))
      (FORMAT T "Room: ~A~%" (RECORD-ROOM n))
		)))
  
(DEFUN PRINT_REGISTRY (SEQ)
  (COND
    ((NULL SEQ) T)
	(T
	  (PRINT_RECORD (CAR SEQ))
	  (TERPRI)
	  (PRINT_REGISTRY (CDR SEQ)))))
  
(DEFUN ADD_RECORD ()
  (SETQ new_record (MAKE-RECORD))
  (FORMAT T "***** Add record *****~%")
  (FORMAT T "Input name: ")
  (SETF (RECORD-NAME new_record) (READ))
  (FORMAT T "Input when (timestamp): ")
  (SETF (RECORD-WHEN new_record) (READ))
  (FORMAT T "Input room number: ")
  (SETF (RECORD-ROOM new_record) (READ))
  (SETF (RECORD-ID new_record) (INCF (GET 'REGISTRY' ID_COUNTER)))
  (SETQ REGISTRY (APPEND REGISTRY (CONS new_record NIL)))
  (FORMAT T "The record has been added.~%"))
  
(DEFUN SEARCH_RECORD (SEQ CRITERIA)
  (COND
    ((NULL SEQ) NIL)
	(T
	  (IF (FUNCALL CRITERIA (CAR SEQ)) 
	    (CONS (CAR SEQ) (SEARCH_RECORD (CDR SEQ) CRITERIA))
		(SEARCH_RECORD (CDR SEQ) CRITERIA)))))
		
(DEFUN DELETE_RECORDS (RECORDS)
  (COND
    ((NULL RECORDS) NIL)
	(T
	  (SETQ REGISTRY (REMOVE (CAR RECORDS) REGISTRY))
	  (DELETE_RECORDS (CDR RECORDS)))))
	
(DEFUN EDIT_RECORD (n)
  (FORMAT T "Input name: ")
  (SETF (RECORD-NAME n) (READ))
  (FORMAT T "Input when (timestamp): ")
  (SETF (RECORD-WHEN n) (READ))
  (FORMAT T "Input room number: ")
  (SETF (RECORD-ROOM n) (READ))
)

(main)	