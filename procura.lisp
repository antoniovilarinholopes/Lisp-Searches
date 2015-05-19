;;;
;;; Copyright (C) Joao Cachopo <jcachopo@gia.ist.utl.pt>
;;;
;;; File: procura-info.lisp
;;; Created on: Fri Apr 19 17:02:22 1996
;;;  change 001, Nov 2004 

(in-package :user)

(defun copy-array (array)
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims :displaced-to array)
     dims)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Algumas funcoes de utilizacao geral muito uteis.
;;;

;;; So' funciona para listas e nao para sequencias em geral, mas...
(defun filtra (predicado lista)
  "Devolve a lista dos elementos que satisfazem o predicado (pela
  ordem em que aparecem inicialmente)."  
  (let ((resultado nil))
    (dolist (elem lista)
      (when (funcall predicado elem)
	(push elem resultado)))
    (nreverse resultado)))


;;; Inspiracoes de Dylan
(defun always (value)
  #'(lambda (&rest args)
      (declare (ignore args))
      value))


;;; A funcao delete do Common Lisp e' generica demais para o tipo de
;;; operacao que pretendemos aqui.
;;; Esta versao e' mais eficiente.
(defun delete-first-eq-from-list (item list)
  (cond ((null list) nil)
	((eq item (car list)) (cdr list))
	(t (do ((prev list (cdr prev))
		(curr (cdr list) (cdr curr)))
	       ((null curr) list)
	     (when (eq (car curr) item)
	       (setf (cdr prev) (cdr curr))
	       (return list))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variaveis dinamicas utilizadas para estatisticas
;;; 

(defvar *nos-gerados*)
(defvar *nos-expandidos*)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Definicao da estrutura que permite representar um problema de
;;; procura.
;;; 
;;; Um problema, de acordo com o paradigma do espaco de estados, e'
;;; caracterizado por estados, por uma funcao que dado um estado gera
;;; os seus sucessores de acordo com determinados operadores de
;;; transiccao e por um predicado que identifica o estado objectivo.
;;;
;;; No nosso caso, para tornar os algoritmos de procura mais
;;; genericos, sao necessarias mais funcoes, em particular o teste de
;;; igualdade entre estados.
;;;
;;; Para usar algoritmos de procura informados e' necessaria ainda uma
;;; funcao de avaliacao (heuristica).
;;;

(defstruct problema
  estado-inicial
  operadores
  objectivo?
  custo
  heuristica
  hash
  estado=)


(defun cria-problema (estado-inicial operadores 
		      &key estado-final
			   objectivo?
			   custo
			   heuristica
			   (hash #'sxhash)
			   (estado= #'eql))
  
  (let ((obj? (cond ((functionp objectivo?) objectivo?)
		    (estado-final
		     #'(lambda (estado) 
			 (funcall estado= estado estado-final)))
		    (t (always t)))))
    
  (make-problema :estado-inicial estado-inicial
		 :operadores operadores
		 :objectivo? obj?
		 :custo (or custo (always 1))
		 :heuristica (or heuristica (always 0))
		 :hash hash
		 :estado= estado=)))


(defun problema-gera-sucessores (problema estado)
  (let ((sucessores nil))
    (dolist (operador (problema-operadores problema))
      (setf sucessores
	(nconc (funcall operador estado)
	       sucessores)))
    (incf *nos-expandidos*)
    (incf *nos-gerados* (length sucessores))
    sucessores))


(defun problema-da-hash (problema estado)
  (funcall (problema-hash problema) estado))

;;;
;;; Isto e' suficientemente generico para os problemas que vamos
;;; tratar.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Definicao da estrutura de dados "queue" (Norvig) que permite
;;; adicionar elementos ao fim muito facilmente.
;;;

(declaim (inline queue-contents make-queue enqueue dequeue
		 front empty-queue-p queue-nconc delete-from-queue))

(defun queue-contents (q)
  (cdr q))

(defun make-queue ()
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (item q)
  (setf (car q)
    (setf (rest (car q))
      (cons item nil)))
  q)

(defun dequeue (q)
  (pop (cdr q))
  (if (null (cdr q))
    (setf (car q) q))
  q)

(defun front (q)
  (first (queue-contents q)))

(defun empty-queue-p (q)
  (null (queue-contents q)))

(defun queue-nconc (q list)
  (unless (null list)
    (setf (car q)
      (last (setf (rest (car q))
	      list))))
  q)



(defun delete-from-queue (item q)
  "Delete the first ocurrence of an EQ item from the queue."
  ;; Empty queues don't have items
  (cond ((null (cdr q)) q)
	;; When the item is at the front, dequeue it (this handles
	;; proper handling of the "last" pointer)
	((eq item (car (cdr q))) (dequeue q))
	;; otherwise, it is not the first one, search for it in the
	;; rest
	(t (do ((prev (cdr q) (cdr prev))
		(curr (cdr (cdr q)) (cdr curr)))
	       ((null curr) q)
	     (when (eq (car curr) item)
	       (setf (cdr prev) (cdr curr))
	       ;; When we delete the last one, we need to change the
	       ;; pointer to the "last" cons cell
	       (if (null (cdr curr))
		 (setf (car q) prev))
	       (return q))))))


;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Definicao da estrutura de dados que serve para representar os nos
;;; do espaco de procura (grafo).
;;;


;;;
;;; Primeiro para o caso de procuras cegas (nao-informadas).
;;; Consideramos que nao existem custos associados (o que interessa
;;; e' o comprimento do caminho solucao).
;;;

(defstruct no
  "Estrutura usada por todos os algoritmos de procura para representar
  a informacao referente aos nos." 

  estado
  pai)

;;;
;;; Funcao de interface para a criacao de uma estrutura do tipo no.
;;; Continuamos a utilizar os selectores da estrutura normalmente.
;;;
(defun cria-no (estado pai)
  "Cria um no correspondente a um estado do espaco do problema."
  
  (make-no :estado estado 
	   :pai pai))



;;;
;;; Para o caso das procuras informadas e, em particular, para o A* o
;;; no' precisa de mais informacao.
;;;
;;; Para alem da informacao do no definido acima, estes nos devem
;;; guardar informacao sobre o custo de chegar ate' ao no' (g) e o
;;; custo estimado deste no' (f').
;;;

(defstruct (no-a* (:include no))
  "Estrutura usada pelo A* para representar a informacao referente aos
  nos."

  g
  f)


(defun calcula-novo-g (estado no-pai custo-transicao)
  "Calcula o novo valor de g para estado, sendo no-pai o seu pai."
  (declare (ignore estado))
  
  (if (null no-pai)
    0
    (+ (no-a*-g no-pai) custo-transicao)))


;;;
;;; Funcao de interface para a criacao de uma estrutura do tipo no.
;;; Continuamos a utilizar os selectores da estrutura normalmente.
;;;
(defun cria-no-a* (estado pai custo-transicao heuristica)
  "Cria um no correspondente a um estado do espaco do problema."
  
  (let ((g (calcula-novo-g estado pai custo-transicao)))
    (make-no-a* :estado estado 
		:pai pai
		:g g
		:f (+ g (funcall heuristica estado)))))


(defun no-a*-h (no)
  (- (no-a*-f no)
     (no-a*-g no)))

;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Definicao da estrutura de dados que serve para representar os nos
;;; ja gerados e ja expandidos para a procura num grafo.
;;;

;;;
;;; A estrutura guarda informacao acerca do problema a ser resolvido,
;;; os nos gerados mas nao expandidos (abertos) e os nos expandidos
;;; (fechados).
;;;
;;; abertos e fechados sao implementados como listas.
;;;
;;; Para podermos utilizar a mesma estrutura para o algoritmo de
;;; procura em largura primeiro e para o A* (ou para outros que venham
;;; a ser difinidos mais tarde), vamos criar varias funcoes para
;;; juntar os nos gerados aos abertos, dependendo da estrategia
;;; implementada, permitindo assim retira-los sempre da frente da
;;; lista.
;;;
;;; Guardamos na estrutura a funcao a utilizar para fazer a insercao
;;; nos abertos, que e' definida quando se cria um novo espaco.
;;; Assim, podemos utilizar sempre a mesma funcao (junta-nos-gerados)
;;; para juntar os novos nos aos abertos.
;;;
;;; Para alem disso, uma outra funcao que varia com a estrategia e' a
;;; que cria os nos, uma vez que as estruturas a utilizar sao
;;; diferentes de caso para caso.
;;; Assim, tambem a funcao a utilizar para criar os nos varia com a
;;; procura.
;;;
;;; Definimos diferentes funcoes <novo-espaco-XX> para cada tipo de
;;; procura de forma a inicializar o novo espaco com as funcoes
;;; adequadas.
;;;


;;; A estrutura espaco (de estados) precisa de guardar informacao
;;; acerca dos nos ja' gerados mas ainda nao expandidos.
;;;
;;; Isso normalmente e' feito mantendo uma lista de nos, a que
;;; chamamos normalmente de "lista de abertos".
;;; Acontece que, conforme o tipo de procura que pretendemos realizar,
;;; a gestao dessa "lista de abertos" e' feita de forma diferente.
;;; 
;;; Para podermos optimizar o acesso `a "lista de abertos", em certos
;;; casos, devemos utilizar outro tipo de representacao que nao a das
;;; listas de Lisp simples.
;;;
;;; Isso, no entanto, depende da forma como se vai fazer o acesso aos
;;; abertos.
;;; Por isso vamos definir uma outra estrutura auxiliar que representa
;;; os abertos.
;;;
;;; Esta estrutura mantem um campo para guardar os nos propriamente
;;; ditos, e os restantes campos sao as funcoes que devem ser
;;; utilizadas para manipular essa estrutura.
;;;
;;; As funcoes que vamos definir para esta estrutura apenas fazem o
;;; "dispatch" para as funcoes guardadas nestes campos.

(defstruct abertos
  nos			       ; Estrutura com os nos
  hash-table		       ; Permite procuras muito mais rapidas
  hash-fn                      ; A funcao que da' a chave do estado
  proximo-fn		       ; Funcao que devolve o proximo no
  insere-fn		       ; Funcao que insere uma lista de nos
  remove-fn		       ; Funcao que remove um no
  vazio-fn)		       ; Predicado que identifica ausencia de nos


(defun cria-abertos (valor-inicial &key proximo-fn hash-fn
					insere-fn remove-fn 
					vazio-fn)
  (make-abertos :nos valor-inicial
		:hash-table (make-hash-table)
		:hash-fn hash-fn
		:proximo-fn proximo-fn
		:insere-fn insere-fn
		:remove-fn remove-fn
		:vazio-fn vazio-fn))

(defun abertos-proximo-no (abertos)
  (funcall (abertos-proximo-fn abertos)
	   (abertos-nos abertos)))

(defun abertos-insere-nos (abertos lista-nos)
  ;; primeiro actualiza a hash-table colocando la' todos os nos
  (let ((table (abertos-hash-table abertos))
	(hash (abertos-hash-fn abertos)))
    (dolist (no lista-nos)
      (push no (gethash (funcall hash (no-estado no))
			table))))
  
  ;; agora actualiza a estrutura que mantem a ordem
  (setf (abertos-nos abertos)
    (funcall (abertos-insere-fn abertos)
	     (abertos-nos abertos)
	     lista-nos)))

(defun abertos-remove-no (abertos no)
  (let ((valor-hash (funcall (abertos-hash-fn abertos)
			     (no-estado no))))
    (setf (gethash valor-hash (abertos-hash-table abertos))
      (delete-first-eq-from-list no 
				 (gethash valor-hash (abertos-hash-table abertos)))))
  (setf (abertos-nos abertos)
    (funcall (abertos-remove-fn abertos)
	     ;; Ponho o no como primeiro argumento porque e' a
	     ;; convencao utilizada pelo Common Lisp em funcoes como
	     ;; remove e delete, por exemplo
	     no
	     (abertos-nos abertos))))

(defun abertos-vazio-p (abertos)
  (funcall (abertos-vazio-fn abertos)
	   (abertos-nos abertos)))


(defun abertos-procura-estado (abertos estado estado=)
  (find estado (gethash (funcall (abertos-hash-fn abertos)
				 estado)
			(abertos-hash-table abertos))
	:key #'no-estado
	:test estado=))




(defstruct espaco
  problema
  estado=
  abertos
  fechados
  em-arvore?
  cria-no)


;;; 
;;; Funcoes a utilizar para a procura em largura-primeiro.
;;;

(defun junta-no-fim (abertos nos)
  "Junta os nos no fim da lista.  Estrategia em largura-primeiro."
  (queue-nconc abertos nos))


(defun cria-no-procura-largura (espaco estado no-pai)
  "Cria um novo no para o caso da largura-primeiro, mas apenas se isso
  for necessario (ainda nao foi considerado o estado pela procura).
  Caso contrario devolve NIL."
  
  (if (and (not (espaco-em-arvore? espaco))
	   (or (da-no-dos-abertos espaco estado)
	       (da-no-dos-fechados espaco estado)))
    nil
    (cria-no estado no-pai)))



;;; 
;;; Funcoes a utilizar para a procura em largura-primeiro.
;;;

(defun junta-ordenados (abertos nos-a*)
  "Junta os nos por ordem crescente do seu valor de f.  
Estrategia A*."
  
  ;; O < do CL aceita multiplos argumentos, por isso gera imenso lixo
  ;; no &rest.  Definindo uma funcao especifica para dois argumentos
  ;; permite ao compilador optimizar a chamada `a funcao <.
  (flet ((menor (n1 n2)
	   (< n1 n2)))
    (merge 'list (sort nos-a* #'menor :key #'no-a*-f) abertos
	   #'menor :key #'no-a*-f)))



(defun cria-no-procura-a* (espaco estado no-pai)
  "Cria um novo no para a procura A*, mas apenas se isso for
  necessario.  Pode fazer alteracoes na lista de fechados e de abertos
  para actualizar nos existentes."
  
  (flet ((mantem-melhor-no (no estado no-pai custo-transicao)
	   (flet ((actualiza-custo-do-no (no diferenca)
		    (decf (no-a*-g no) diferenca)
		    (decf (no-a*-f no) diferenca)
		    (setf (no-pai no) no-pai)
		    no))
	     
	     (let ((diferenca-de-custo 
		    (- (no-a*-g no)
		       (calcula-novo-g estado no-pai custo-transicao))))
	       (if (> diferenca-de-custo 0)
		 (actualiza-custo-do-no no diferenca-de-custo)
		 nil)))))
  
    (let* ((problema (espaco-problema espaco))
	   (heuristica (problema-heuristica problema))
	   (custo-transicao (funcall (problema-custo problema) estado)))
      
      (if (espaco-em-arvore? espaco)
	;; Se o espaco e' uma arvore o no' nao pode ter sido gerado
	;; anteriormente
	(cria-no-a* estado no-pai custo-transicao heuristica)
      
	;; Se o espaco e' um grafo, o no' pode ja' ter sido gerado, e
	;; este ser um caminho pior
	(let ((melhor-no nil))
	  ;; Primeiro verificamos se esta' em fechados
	  (let ((no-fechado (da-no-dos-fechados espaco estado)))
	    (when no-fechado
	      (setf melhor-no
		(mantem-melhor-no no-fechado estado no-pai custo-transicao))
	      (when melhor-no
		(remove-no-de-fechados espaco melhor-no)))
	    
	    ;; Se nao estava em fechados pode estar em abertos...
	    (unless no-fechado
	      (let ((no-aberto (da-no-dos-abertos espaco estado)))
		(when no-aberto
		  (setf melhor-no 
		    (mantem-melhor-no no-aberto estado no-pai custo-transicao))
		  (when melhor-no
		    (remove-no-de-abertos espaco melhor-no)))
		
		;; Se nao existe ainda vamos cria-lo!
		(unless (or melhor-no no-aberto no-fechado)
		  (setf melhor-no
		    (cria-no-a* estado no-pai custo-transicao heuristica))))))
	  melhor-no)))))


;;;
;;; Vamos usar uma funcao novo-espaco diferente para cada tipo de
;;; procura.
;;;

(defun novo-espaco-largura (problema &key espaco-em-arvore?)
  (novo-espaco problema
	       (cria-abertos (make-queue)
			     :hash-fn (problema-hash problema)
			     :proximo-fn #'front
			     :insere-fn #'junta-no-fim
			     :remove-fn #'delete-from-queue
			     :vazio-fn #'empty-queue-p)
	       #'cria-no-procura-largura
	       espaco-em-arvore?))

(defun novo-espaco-a* (problema &key espaco-em-arvore?)
  (novo-espaco problema 
	       (cria-abertos nil
			     :hash-fn (problema-hash problema)
			     :proximo-fn #'first
			     :insere-fn #'junta-ordenados
			     :remove-fn #'delete-first-eq-from-list
			     :vazio-fn #'null)
	       #'cria-no-procura-a*
	       espaco-em-arvore?))


(defun novo-espaco (problema abertos cria-no espaco-em-arvore?)
  (make-espaco 
   :problema problema
   :estado= (problema-estado= problema)
   :abertos abertos
   :fechados (make-hash-table)
   :em-arvore? espaco-em-arvore?
   :cria-no cria-no))


(defun espaco-vazio? (espaco)
  "Predicado que devolve 't' quando nao existem nos gerados e nao
  expandidos."
  (abertos-vazio-p (espaco-abertos espaco)))


(defun espaco-proximo-no (espaco)
  "Devolve o proximo no a ser expandido."
  (abertos-proximo-no (espaco-abertos espaco)))


(defun junta-nos-gerados (espaco lista-nos)
  "Junta os nos na lista aos nos gerados do espaco."
  (abertos-insere-nos (espaco-abertos espaco) lista-nos))


(defun remove-no-de-abertos (espaco no)
  "Remove o no da lista de abertos."
  (abertos-remove-no (espaco-abertos espaco) no))




(defun remove-no-de-fechados (espaco no)
  "Remove o no da lista de fechados."
  (let ((fechados (espaco-fechados espaco))
	(hash-value (problema-da-hash (espaco-problema espaco)
				      (no-estado no))))
    (setf (gethash hash-value fechados)
      (delete-first-eq-from-list no (gethash hash-value fechados)))))


(defun junta-no-a-fechados (espaco no)  
  (push no (gethash (problema-da-hash (espaco-problema espaco)
				      (no-estado no))
		    (espaco-fechados espaco))))
    
    
(defun junta-no-expandido (espaco no)
  "Junta o no aos nos expandidos do espaco."
  ;; O no, se foi expandido, deixa de estar nos abertos ...
  (remove-no-de-abertos espaco no)
  ;; ... e passa para os fechados, se o espaco nao for uma arvore
  (unless (espaco-em-arvore? espaco)
    (junta-no-a-fechados espaco no)))



(defun espaco-expande-no (espaco no)
  "Expande o no recebido no espaco, actualizando a estrutura do
  espaco."
  ;; Comecamos por gerar todos os sucessores do estado correspondente
  ;; ao no recebido
  (let ((sucessores (problema-gera-sucessores (espaco-problema espaco)
					      (no-estado no))))
    ;; O no ja foi expandido, por isso passa para os expandidos
    (junta-no-expandido espaco no)
    
    ;; Finalmente, juntamos aos abertos os nos cujos estados ainda nao
    ;; existem no espaco (os nos mais recentes vao para o fim da
    ;; lista)
    (junta-nos-gerados espaco
		       (cria-nos-sucessores espaco no sucessores))))



(defun cria-nos-sucessores (espaco no-pai estados-sucessores)
  (let ((nos nil))
    (dolist (estado estados-sucessores)
      (let ((no (funcall (espaco-cria-no espaco)
			 espaco estado no-pai)))
	(when no
	  (push no nos))))
    nos))


(defun da-no-dos-abertos (espaco estado)
  (abertos-procura-estado (espaco-abertos espaco)
			  estado
			  (espaco-estado= espaco)))


(defun da-no-dos-fechados (espaco estado)
  (find estado (gethash (problema-da-hash (espaco-problema espaco)
					  estado)
			(espaco-fechados espaco))
	:key #'no-estado
	:test (espaco-estado= espaco)))


;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun a* (problema &key espaco-em-arvore?)
  (let ((espaco (novo-espaco-a* problema
				:espaco-em-arvore? espaco-em-arvore?)))
    (junta-nos-gerados espaco
		       (list (cria-no-a* (problema-estado-inicial problema)
					 nil  ; O pai do estado inicial nao existe
					 0    ; e o custo e' 0 (zero)
					 (problema-heuristica problema))))
    (procura-com-espaco problema espaco)))



(defun largura-primeiro (problema &key espaco-em-arvore?)
  "Funcao que implementa o algoritmo de procura em largura primeiro."

  (let ((espaco (novo-espaco-largura problema
				     :espaco-em-arvore? espaco-em-arvore?)))
    (junta-nos-gerados espaco
		       (list (cria-no (problema-estado-inicial problema)
				      nil)))
    (procura-com-espaco problema espaco)))
  

    
(defun procura-com-espaco (problema espaco)
  
  (let ((objectivo? (problema-objectivo? problema)))
    (loop
      
      ;; Quando nao temos mais nos e porque ja exploramos todo o
      ;; espaco e nao encontramos a solucao (nao existe)
      (when (espaco-vazio? espaco)
	(return nil))
      
      ;; Vamos considerar o no gerado mais antigo para termos uma
      ;; procura em largura primeiro
      (let ((proximo-no (espaco-proximo-no espaco)))

	;; Se atingimos a solucao paramos e devolvemos os estados no
	;; caminho 
	(when (funcall objectivo? (no-estado proximo-no))
	  (return (da-caminho proximo-no)))
	
	;; Caso contrario, devemos expandir o no
	(espaco-expande-no espaco proximo-no)))))
  

(defun da-caminho (no)
  "Devolve a lista com os estados no caminho que levam ao no dado."

  ;; Usa uma funcao local para implementar um processo iterativo,
  ;; fazendo os "conses" estritamente necessarios para construir a
  ;; solucao
  (labels ((constroi-caminho (no caminho)
	     (if (null no)
	       caminho
	       (constroi-caminho (no-pai no) 
				 (cons (no-estado no) caminho)))))
    
    (constroi-caminho no nil)))





(defun ida* (problema &key espaco-em-arvore?)
  (let ((estado= (problema-estado= problema))
	(heur (problema-heuristica problema))
	(fun-custo (problema-custo problema))
	(objectivo? (problema-objectivo? problema)))
    
    (labels ((esta-no-caminho? (estado caminho)
	       (unless espaco-em-arvore?
		 (member estado caminho :test estado=)))
	     
	     (prof (estado custo-max custo-caminho caminho)
	       (block prof
		 (if (esta-no-caminho? estado caminho)
		   nil
		   (let ((custo (+ custo-caminho (funcall heur estado))))
		     (cond ((> custo custo-max) custo)
			   ((funcall objectivo? estado) (list estado))
			   (t
			    (let ((min-custo most-positive-fixnum))
			      (dolist (suc (problema-gera-sucessores
					    problema estado))
				(let ((solucao (prof suc 
						     custo-max 
						     (+ custo-caminho
							(funcall fun-custo suc))
						     (or espaco-em-arvore?
							 (cons estado
							       caminho)))))
				  (if (numberp solucao)
				    (setf min-custo (min min-custo
							 solucao))
				    (if solucao
				      (return-from prof (cons estado
							      solucao))))))
			      min-custo))))))))
      
      (let ((custo-max 0))
	(loop
	  (let ((solucao (prof (problema-estado-inicial problema)
			       custo-max
			       0
			       nil)))
	    (if (numberp solucao)
	      (if (> solucao custo-max)
		(setf custo-max solucao)
		(return nil))
	      (return solucao))))))))
				      
			      
			  


;;;
;;;              Procura em profundidade primeiro  
;;;


(defun profundidade-primeiro (problema profundidade-maxima) 
  "Algoritmo de procura em profundidade primeiro."

  (let ((estado= (problema-estado= problema))
	(objectivo? (problema-objectivo? problema)))

    (labels ((esta-no-caminho? (estado caminho)
	       (member estado caminho :test estado=))
	     
	     (procura-prof (estado caminho prof-actual)
	       (block procura-prof
		 
		 ;; base da recursao:
		 ;; 1. quando comecamos a repetir estados pelos quais ja
		 ;;    passamos no caminho que esta a ser percorrido
		 ;;    (para evitar caminhos infinitos)
		 ;; 2. quando atingimos o objectivo
		 ;; 3. quando ultrapassamos a profundidade limite ate
		 ;;    onde se deve efectuar a procura
		 (cond ((funcall objectivo? estado) (list estado))
		       ((= prof-actual profundidade-maxima) nil)
		       ((esta-no-caminho? estado caminho) nil)
		       (t 
			(dolist (suc (problema-gera-sucessores problema
							       estado))
			  ;; avancamos recursivamente, em profundidade,
			  ;; para cada sucessor
			  (let ((solucao (procura-prof suc 
						       (cons estado caminho)
						       (1+ prof-actual))))
			    (when solucao
			      (return-from procura-prof (cons estado
							      solucao))))))))))
      
      (procura-prof (problema-estado-inicial problema) nil 0))))


;;;
;;;              Procura em profundidade iterativa  
;;;

(defun profundidade-iterativa (problema profundidade-maxima)
  "Algoritmo de procura em profundidade iterativa."
  (block profundidade-iterativa
    (dotimes (prof (1+ profundidade-maxima))
      (let ((solucao (profundidade-primeiro problema prof)))
	(when solucao 
	  (return-from profundidade-iterativa solucao))))))



;;;
;;; Funcao de interface que permite condensar todos os tipos de
;;; procura numa so funcao.
;;;


(defun procura (problema tipo-procura
		&key (profundidade-maxima most-positive-fixnum)
		     (espaco-em-arvore? nil))
  "Dado um problema e um tipo de procura devolve uma lista com: a
  solucao para o problema (a lista de estados desde o estado inicial
  ate' ao estado final), ou nil caso nao encontre a solucao; tempo
  gasto na procura (em internal-time-units); numero de nos expandidos;
  numero de nos gerados."

  (flet ((faz-a-procura (problema tipo-procura 
			 profundidade-maxima espaco-em-arvore?)
	   ;; Usamos cond em vez de case porque nao sabemos de que
	   ;; package veem os simbolos (o string-equal funciona com o
	   ;; symbol-name do simbolo e e' "case-insensitive")
	   
	   ;; Actualmente, apenas a procura em largura, o A* e o IDA*
	   ;; estao a aproveitar a informacao do espaco de estados ser
	   ;; uma arvore
	   (cond ((string-equal tipo-procura "largura")
		  (largura-primeiro problema 
				    :espaco-em-arvore? espaco-em-arvore?))
		 ((string-equal tipo-procura "profundidade")
		  (profundidade-primeiro problema profundidade-maxima))
		 ((string-equal tipo-procura "profundidade-iterativa")
		  (profundidade-iterativa problema profundidade-maxima))
		 ((string-equal tipo-procura "a*")
		  (a* problema :espaco-em-arvore? espaco-em-arvore?))
		 ((string-equal tipo-procura "ida*")
		  (ida* problema :espaco-em-arvore? espaco-em-arvore?)))))

    (let ((*nos-gerados* 0)
	  (*nos-expandidos* 0)
	  (tempo-inicio (get-internal-run-time)))
      (let ((solucao (faz-a-procura problema tipo-procura
				    profundidade-maxima
				    espaco-em-arvore?)))
	(list solucao 
	      (- (get-internal-run-time) tempo-inicio)
	      *nos-expandidos*
	      *nos-gerados*)))))
