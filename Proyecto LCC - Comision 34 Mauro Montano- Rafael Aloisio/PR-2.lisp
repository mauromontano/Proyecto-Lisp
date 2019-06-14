;Funcion trans 
(DEFUN trans (M)
    (COND
        ((NULL M) NIL )
        (   T    (trans2 M 0 (1- (longitudLista(CAR M)))))
    )
)

(DEFUN trans2 (M CP CU)
    (COND
        (
            (= CP CU)
            (LIST (matrizColumna M CP))
        )
        (   T          (CONS (matrizColumna M CP) (trans2 M (1+ CP) CU)))
    )
)

(DEFUN longitudLista (L)
    (COND
        (
            (NULL L)
            0
        )
        (   T       (+ 1 (longitudLista(CDR L))))
    )
)

(DEFUN elemLista (L P)
    (COND
        (
            (= P 0)
            (CAR L)
        )
        ( T      (elemLista (CDR L) (1- P)))
    )
)

(DEFUN matrizColumna (M CP)
    (COND
        (
            (NULL (CDR M))
            (LIST (elemLista (CAR M) CP))
        )
        ( T    (CONS (elemLista (CAR M) CP) (matrizColumna (CDR M) CP)))
    )
)

(DEFUN matrizElem (M F C)
    (elemLista (elemLista M F) C)
)


;Funcion sumaPrimos 
(DEFUN sumaPrimos (N)
		(COND
			(( or (> N 1000 ) (< N 0))  
					'(Ingresar un numero N valido)
			)
			( T				(sumaPrimos2 N))
		)
)

(DEFUN sumaPrimos2 (N)
		(COND
			((= N 0)  0)
			((LET ((X 2)) (esPrimo N X))
				(+ N (sumaPrimos2 (- N 1)))
			)
			(T 				(sumaPrimos2 (- N 1)))
		)
)

(DEFUN esPrimo (N X)
	(COND
		((= N 1) 		NIL)
		((< X (/ (+ N 1) 2))
			(COND
				(( = (MOD N X) 0) 
					NIL
				)
				(T 		(esPrimo N (+ X 1)))
			)
		)
		(T         T)
	)
)



;Funcion permLex
(DEFUN permLex (L)
    (COND
        (
            (NULL L)
            `()
        )
        (
            (permLex2 L `() 0 (1- (longitudLista L)))
        )
    )
)

(DEFUN permLex2 (L LAux P Ult)
    (COND
        (
            (= P Ult)
            (unir L LAux P Ult)
        )
        (
            (< P Ult)
            (APPEND (unir L LAux P Ult) (permLex2 L LAux (1+ P) Ult)) 
        )
    )
)

(DEFUN unir (L LAux P Ult)
    (COND
        (
            (NULL (CDR L))
            (LIST (APPEND LAux (LIST (CAR L)))) 
        )
        (
            (permLex2
                (eliminar L P)
				(APPEND LAux (LIST (elemLista L P)))
                0
                (1- Ult)
            )
        )
    )
)

(DEFUN eliminar (L P)
    (COND
        (
            (= 0 P)
            (CDR L) 
        )
        (   T     (CONS (CAR L) (eliminar (CDR L) (1- P))))
    )
)

