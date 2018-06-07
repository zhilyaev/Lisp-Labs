 (DEFUN cutevens (lst)
  (COND
    ((NULL lst) lst)
    (T (CONS
      (CAR lst)
      (cutevens (CDDR lst))))))
