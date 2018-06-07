(defun carSlice (l k)
 "Get sublist, which contains first $k elements"
    (if (>= k 0)
        (cons 
            (car l)
            (carSlice (cdr l) (- k 1) )
        )
        NIL
    )
)
(defun cdrSlice (l n)
"Get sublist, which contains without first $k elements"
 (if (> n 1) ; n + 1
    ( cdrSlice (cdr l) (- n 1))
    l
 )
)
