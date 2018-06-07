; Телефонный справочник организации
(DEFSTRUCT NOTE ID NAME PHONE)
(SETF (GET 'PHONEBOOK' ID_COUNTER) 5)
(SETQ PHONEBOOK
  (CONS (MAKE-NOTE :ID 1 :NAME 'NAME1 :PHONE 79210000000)
    (CONS (MAKE-NOTE :ID 2 :NAME 'NAME2 :PHONE 79210000001)
      (CONS (MAKE-NOTE :ID 3 :NAME 'NAME3 :PHONE 79530000000)
        (CONS (MAKE-NOTE :ID 4 :NAME 'NAME1 :PHONE 79640000000)
          (CONS (MAKE-NOTE :ID 5 :NAME 'NAME1 :PHONE 79210000002) NIL))))))

(DEFUN MAIN ()
  (SETQ choice (PRINT_MENU))
  (COND
    ((EQ choice 0) T)
	((EQ choice 1)
	  (PRINT_PHONEBOOK PHONEBOOK)
	  (MAIN))
	((EQ choice 2)
	  (ADD_NOTE)
	  (MAIN))
	((EQ choice 3)
	  (FORMAT T "***** Edit note *****~%")
	  (FORMAT T "Input note ID: ")
	  (SETQ _id (READ))
	  (SETQ ed_note (SEARCH_NOTE PHONEBOOK (LAMBDA (n) (IF (EQ (NOTE-ID n) _id) T NIL))))
	  (IF (NULL ed_note)
	    (FORMAT T "The note not found~%")
		(PROGN (EDIT_NOTE (CAR ed_note))
		 (FORMAT T "The note has been edited.~%")))
	  (MAIN))
	((EQ choice 4)
	  (FORMAT T "***** Delete note *****~%")
      (FORMAT T "Input note ID: ")
	  (SETQ _id (READ))
	  (SETQ del_note (SEARCH_NOTE PHONEBOOK (LAMBDA (n) (IF (EQ (NOTE-ID n) _id) T NIL))))
	  (IF (NULL del_note)
	    (FORMAT T "The note not found~%")
		(PROGN (DELETE_NOTES del_note)
		 (FORMAT T "The note has been deleted.~%")))
	  (MAIN))
	((EQ choice 5)
	  (SETQ choice (PRINT_SEARCH_MENU))
	  (COND
	    ((EQ choice 1)
		  (FORMAT T "Input ID: ")
		  (SETQ _id (READ))
		  (PRINT_PHONEBOOK 
		    (SEARCH_NOTE 
			  PHONEBOOK 
			  (LAMBDA (n) (IF (EQ (NOTE-ID n) _id) T NIL)))))
		((EQ choice 2) 
		  (FORMAT T "Input name: ")
		  (SETQ _name (READ))
		  (PRINT_PHONEBOOK 
		    (SEARCH_NOTE 
			  PHONEBOOK 
			  (LAMBDA (n) (IF (EQ (NOTE-NAME n) _name) T NIL)))))
		((EQ choice 3) 
		  (FORMAT T "Input phone number: ")
		  (SETQ _phone (READ))
		  (PRINT_PHONEBOOK 
		    (SEARCH_NOTE 
			  PHONEBOOK 
			  (LAMBDA (n) (IF (EQ (NOTE-PHONE n) _phone) T NIL)))))
		((EQ choice 4)
		  (FORMAT T "Input phone code: ")
		  (SETQ _code (READ))
		  (PRINT_PHONEBOOK 
		    (SEARCH_NOTE 
			  PHONEBOOK 
			  (LAMBDA (n) (IF (EQ (PHONE_CODE (NOTE-PHONE n)) _code) T NIL))))))
		(MAIN))
	(T
	  (MAIN))))
	  
  

(DEFUN PRINT_MENU ()
  (FORMAT T "***** Phonebook *****~%")
  (FORMAT T "1. View phonebook~%")
  (FORMAT T "2. Add note~%")
  (FORMAT T "3. Edit note~%")
  (FORMAT T "4. Delete note~%")
  (FORMAT T "5. Search note~%")
  (FORMAT T "0. Exit~%")
  (FORMAT T ">>")
  (READ))
  
(DEFUN PRINT_SEARCH_MENU ()
  (FORMAT T "***** Search note *****~%")
  (FORMAT T "1. Search by ID~%")
  (FORMAT T "2. Search by name~%")
  (FORMAT T "3. Search by phone number~%")
  (FORMAT T "4. Search by phone code~%")
  (FORMAT T ">>")
  (READ))
  
(DEFUN PRINT_NOTE (n)
  (COND
    ((NULL n) NIL)
    (T 
	  (FORMAT T "***** NOTE #~A *****~%" (NOTE-ID n))
      (FORMAT T "Name: ~A~%" (NOTE-NAME n))
      (FORMAT T "Phone number: ~A~%" (NOTE-PHONE n)))))
  
(DEFUN PRINT_PHONEBOOK (SEQ)
  (COND
    ((NULL SEQ) T)
	(T
	  (PRINT_NOTE (CAR SEQ))
	  (TERPRI)
	  (PRINT_PHONEBOOK (CDR SEQ)))))
  
(DEFUN ADD_NOTE ()
  (SETQ new_note (MAKE-NOTE))
  (FORMAT T "***** Add note *****~%")
  (FORMAT T "Input name: ")
  (SETF (NOTE-NAME new_note) (READ))
  (FORMAT T "Input phone number: ")
  (SETF (NOTE-PHONE new_note) (READ))
  (SETF (NOTE-ID new_note) (INCF (GET 'PHONEBOOK' ID_COUNTER)))
  (SETQ PHONEBOOK (APPEND PHONEBOOK (CONS new_note NIL)))
  (FORMAT T "The note has been added.~%"))
  
(DEFUN SEARCH_NOTE (SEQ CRITERIA)
  (COND
    ((NULL SEQ) NIL)
	(T
	  (IF (FUNCALL CRITERIA (CAR SEQ)) 
	    (CONS (CAR SEQ) (SEARCH_NOTE (CDR SEQ) CRITERIA))
		(SEARCH_NOTE (CDR SEQ) CRITERIA)))))
		
(DEFUN DELETE_NOTES (NOTES)
  (COND
    ((NULL NOTES) NIL)
	(T
	  (SETQ PHONEBOOK (REMOVE (CAR NOTES) PHONEBOOK))
	  (DELETE_NOTES (CDR NOTES)))))
	
(DEFUN EDIT_NOTE (n)
  (FORMAT T "Input name: ")
  (SETF (NOTE-NAME n) (READ))
  (FORMAT T "Input phone number: ")
  (SETF (NOTE-PHONE n) (READ)))
  
(DEFUN PHONE_CODE (PHONE)
  (FLOOR PHONE 10000000))
	