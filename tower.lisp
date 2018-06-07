(DEFUN get_solution (graph graph_part final_rod)
  (COND
    ((NOT (LISTP graph)) nil)
    ((NOT (LISTP graph_part)) nil)
    ((EQ (LENGTH graph_part) 1) 
      (move_disk (CAR graph_part) final_rod) 
      (CONS (get_state graph) nil))
    (T
      (SETQ sol (get_solution graph (CDR graph_part) (get_temp_rod (CADAR graph_part) final_rod)))
      (move_disk (CAR graph_part) final_rod)
      (SETQ sol (APPEND sol (CONS (get_state graph) nil)))
      (SETQ sol (APPEND sol (get_solution graph (CDR graph_part) final_rod)))
      sol)))

    
(DEFUN move_disk (disk final_rod)
  (COND
    ((EQ (CADR disk) final_rod) nil)
    (T (RPLACA (CDR disk) final_rod)
    disk)))
    
(DEFUN get_temp_rod (initial_rod final_rod)
  (COND
    ((EQ initial_rod 1) (IF (EQ final_rod 2) 3 2))
    ((EQ initial_rod 2) (IF (EQ final_rod 1) 3 1))
    ((EQ initial_rod 3) (IF (EQ final_rod 1) 2 1))
    (T nil)))
    
(DEFUN get_state (graph)
  (COND
    ((NOT (LISTP graph)) nil)
    ((EQ (LENGTH graph) 0) nil)
    ((EQ (LENGTH graph) 1) (CONS (CADAR graph) nil))
    (T
      (CONS
        (CADAR graph)
        (get_state (CDR graph))))))
        
(DEFUN print_solution (solution)
  (COND
    ((NOT (LISTP solution)) nil)
    ((EQ (LENGTH solution) 0) nil)
    ((EQ (LENGTH solution) 1) nil)
    (T
      (print_step (CAR solution) (CADR solution))
      (print_solution (CDR solution)))))
      
(DEFUN print_step (before after)
  (SETQ diff (get_difference before after '1))
  (FORMAT T "Disk ~A to ~A~%" (CAR diff) (CAR (CDR diff))))
  
(DEFUN get_difference (before after index)
  (COND
    ((EQ (LENGTH before) 1)
      (IF (NOT (EQ (CAR before) (CAR after)))
        (CONS index (CONS (CAR after) nil))
        nil))
    (T
      (IF (NOT (EQ (CAR before) (CAR after)))
        (CONS index (CONS (CAR after) nil))
        (get_difference (CDR before) (CDR after) (INCF index))))))
        
(DEFUN solve (graph final_rod)
  (SETQ solution (APPEND (CONS (get_state graph) nil) (get_solution graph graph final_rod)))
  (PRINT "Rods states sequence:")
  (PRINT solution)
  (PRINT "Moves sequence:")
  (TERPRI)
  (print_solution solution))
  
  
 // Run 
 (SETQ graph '((D1 1 (D2))
 (D2 1 (D1 D3))
 (D3 1 (D2))))