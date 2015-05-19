; Antonio Lopes 73721

(in-package :user)
(load (compile-file "procura.lisp"))


(defun resolve-problema (estado-inicial procura-str)
  (let* ((operadores (list #'coloca-rainha))
         (solucao nil)
         (problema 
          (cria-problema estado-inicial operadores :objectivo? #'objectivo :heuristica #'heuristica)
          ;(cria-problema estado-inicial operadores :objectivo? #'objectivo)
          )
         )
    (setf solucao (procura problema procura-str))
    solucao
    )
  )


;casas vazias
(defun heuristica (estado)
  (let* ((last_queen 0)
         (slots_livres 0)
         (dimensions (array-dimensions estado))
         (rows (car dimensions))
         (columns (car (cdr dimensions)))
         )
    (if (objectivo estado)
        (return-from heuristica 0)
      (progn
        (loop for row from 0 to (- rows 1) do
              (if (loop for column from 0 to (- columns 1) thereis (equal (aref estado row column) "t"))
                  (setf last_queen row)
                )
              )
        (loop for row from (+ last_queen 1) to (- rows 1) do
              (loop for column from 0 to (- columns 1) do
                    (if (equal (aref estado row column) nil)
                        (incf slots_livres))
                    )
              )
        (return-from heuristica slots_livres)
        )
      )
    )
  )


(defun objectivo (estado)
  ;nenhuma rainha ataca outra rainha
  ;como so' e' posta uma rainha numa posicao valida
  ;implica que se estiverem as n rainhas no tabuleiro,
  ;entao e' um estado final
  (let* ((numero_rainhas 0)
         (dimensions (array-dimensions estado))
         (rows (car dimensions))
         (columns (car (cdr dimensions)))
         )
    (loop for row from 0 to (- rows 1) do
          (progn 
            (if (and (> row 0) (< numero_rainhas row))
                (return-from objectivo nil)
              )
            (if (loop for column from 0 to (- columns 1) thereis (equal (aref estado row column) "t"))
                (progn 
                  (setf numero_rainhas (1+ numero_rainhas)) 
                  )
              )
            )
          )
    (return-from objectivo (eql numero_rainhas rows))
    )
  )


(defun estado= (estado1 estado2)
  ;Estado= verifica que todos os elementos sao iguais
  (let* ((dimensions (array-dimensions estado1))
         (rows (car dimensions))
         (columns (car (cdr dimensions)))
         )
    (dotimes (row rows)
      (dotimes (column columns) 
        (if (not (equal (aref estado1 row column) (aref estado2 row column)))
            (return-from estado= nil)
          )
        )
      )
    (return-from estado= t)
    )
  )

(defun propaga-ataque-linha (estado-copia row column) 
  (let* ((dimensions (array-dimensions estado-copia))
         (columns (car (cdr dimensions)))
         )
    ;ataques para a direita
    (loop for column-ataque from (+ column 1) to (- columns 1) do
          (setf (aref estado-copia row column-ataque) "a")
          )
    ;ataques para a esquerda
    (loop for column-ataque from 0 to (- column 1) do
          (setf (aref estado-copia row column-ataque) "a")
          )
    t
    )
  )

(defun propaga-ataque-coluna (estado-copia row column) 
  (let* ((dimensions (array-dimensions estado-copia))
         (rows (car dimensions))
         ;(columns (car (cdr dimensions)))
         )
    ;ataques para a direita
    (loop for row-ataque from (+ row 1) to (- rows 1) do
          (setf (aref estado-copia row-ataque column) "a")
          )
    ;ataques para a esquerda
    (loop for row-ataque from 0 to (- row 1) do
          (setf (aref estado-copia row-ataque column) "a")
          )
    t
    )
  )

(defun propaga-ataque-diagonais (estado-copia row column)
  (let* ((dimensions (array-dimensions estado-copia))
         (rows (car dimensions))
         (columns (car (cdr dimensions)))
         )
    ;diagonal direita baixo
    (loop for row-ataque from (+ row 1) to (- rows 1) do
          (if (< (+ column (- row-ataque row)) columns)
              (setf (aref estado-copia row-ataque (+ column (- row-ataque row))) "a")
            )
          )
    ;diagonal direita cima
    ;(loop for row-ataque from 0 to (- row 1) do
     ;     (if (< (+ column (- row row-ataque)) columns)
      ;        (setf (aref estado-copia row-ataque (+ column (- row row-ataque))) "a")
       ;     )
        ;  )
    ;diagonal esquerda baixo
    (loop for row-ataque from (+ row 1) to (- rows 1) do
          (if (>= (- column (- row-ataque row)) 0)
              (setf (aref estado-copia row-ataque (- column (- row-ataque row))) "a")
            )
          )
    ;diagonal esquerda cima
    ;(loop for row-ataque from 0 to (- row 1) do
     ;     (if (>= (- column (- row row-ataque)) 0)
      ;        (setf (aref estado-copia row-ataque (- column (- row row-ataque))) "a")
       ;     )
        ;  )
    t
    )
  )

(defun coloca-rainha (estado)
  ;para cada linha coloca iterativamente uma rainha numa posicao que satisfaz as constricoes
  (let* ((sucessores '())
         ;(last_queen 0)
         (dimensions (array-dimensions estado))
         (rows (car dimensions))
         (columns (car (cdr dimensions)))
         (pos_next_insert nil)
         )
    (setf pos_next_insert
      (loop named outer for row from 0 to (- rows 1) do
            (if (loop for column from 0 to (- columns 1) always (equal (aref estado row column) "a"))
                (return-from coloca-rainha sucessores)
              )
            ;(if (loop for column from 0 to (- columns 1) thereis (equal (aref estado row column) "t"))
             ;   (incf last_queen)
              ;)
            (if (loop for column from 0 to (- columns 1) thereis (equal (aref estado row column) nil))
                (return-from outer row)
              )
            )
      )
    (if (equal pos_next_insert nil)
        nil
      (let* ((row_insert_q pos_next_insert))
        (loop for column from 0 to (- columns 1) do
              (let* ((estado-copia nil))
                (if (eql (aref estado row_insert_q column) nil)
                    (progn 
                      (setf estado-copia (copy-array estado))
                      (setf (aref estado-copia row_insert_q column) "t")
                      (propaga-ataque-linha estado-copia row_insert_q column)
                      (propaga-ataque-coluna estado-copia row_insert_q column)
                      (propaga-ataque-diagonais estado-copia row_insert_q column)
                      (setf sucessores (cons estado-copia sucessores))
                      )
                  )
                )
              )
        (return-from coloca-rainha sucessores)
        )
      )
    )
  )

