(in-package :user)
(load (compile-file "procura.lisp"))
(load (compile-file "job-shop-problemas-modelos.lisp"))
(load (compile-file "job-shop-problemas-v1.lisp"))


(defun calendarizacao (job-shop-problem procura-str) 
  (let ((internal-problema nil)
		(result nil))
    (cond ((string-equal procura-str "ILDS") (setf internal-problema (converte-para-estado-interno job-shop-problem #'heuristica-tempo-final-v2))
					     (setf result (ilds internal-problema 100)))
	  ((string-equal procura-str "sondagem.iterativa") (setf internal-problema (converte-para-estado-interno job-shop-problem #'heuristica-tempo-final-v2))
							   (setf result (sondagem-iterativa internal-problema)))
	  ((string-equal procura-str "abordagem.alternativa") (setf internal-problema (converte-para-estado-interno job-shop-problem #'heuristica-tempo-final-v2))
							      (setf result (beam-search internal-problema #'is-better-solution-job-shop 10 25)))
	  ;FIXME
	  ((string-equal procura-str "melhor.alternativa") (setf internal-problema (converte-para-estado-interno job-shop-problem #'heuristica-tempo-final-v2))
							   (setf result (beam-search internal-problema 10 25)))
	  ;FIXME
	  ((string-equal procura-str "a*.melhor.heuristica") (setf internal-problema (converte-para-estado-interno job-shop-problem #'heuristica-tempo-final-v2))
							     (setf result (procura internal-problema "a*" :espaco-em-arvore? T)))
	  (t		;FIXME
	    	 (setf internal-problema (converte-para-estado-interno job-shop-problem #'heuristica-tempo-final-v2))
	    	 (setf result (procura internal-problema "a*" :espaco-em-arvore? T))))
    (if (not (null result))
    	(if (not (listp (car result)))
    		(converte-para-visualizacao (car result))
    		(converte-para-visualizacao (car (last (car result)))))
    	nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Job Shop aux
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defstruct job-schedule 
;;   job-shop-problem
;;   best-schedule)

 ;Used to propagate time constraint from task precendence and machine precendence
(defstruct job-task-w-constr
  job-task
  virtual-time)
  
(defstruct state-job-schedule
	machine-times
	non-allocated-tasks
	number-tasks
	jobs-tasks-space
	funcao-heuristica)

(defstruct task-info
	job-id
	task-id
	task-duration)

(defun converte-para-visualizacao (estado-interno)
	(let*  ((jobs-tasks (state-job-schedule-jobs-tasks-space estado-interno))
			(dimensions (array-dimensions jobs-tasks))
       	 	(rows (car dimensions)) ;numero de jobs igual ao numero de linhas
       	 	(columns (cadr dimensions))
       	 	(return-list nil))
		(dotimes (i rows)
			(dotimes (j columns)
				(setf return-list (append return-list (list (job-task-w-constr-job-task (aref jobs-tasks i j)))))))
		return-list))

(defun converte-para-estado-interno (job-shop-prob funcao-heuristica) 
  (let ((operadores (list #'inicia-task))
		(n.jobs (job-shop-problem-n.jobs job-shop-prob))
		(estado-inicial nil)
		(state-job nil)
		(max-num-tasks 0)
		(n-total-tasks 0)
		(jobs (job-shop-problem-jobs job-shop-prob))
		(tasks-to-assign nil))

	(dotimes (job n.jobs)
		(setf n-total-tasks (+ n-total-tasks (list-length (job-shop-job-tasks (nth job jobs)))))
		(if (> (list-length (job-shop-job-tasks (nth job jobs))) max-num-tasks)
	      	(setf max-num-tasks (list-length (job-shop-job-tasks (nth job jobs))))))

	(setf estado-inicial (make-array (list n.jobs max-num-tasks)))
	(dotimes (job n.jobs)
	  (let* ((job-tasks (job-shop-job-tasks (nth job jobs))))

	    (dolist (task job-tasks)
	      (let* ((job-task-wrapper (make-job-task-w-constr :job-task (copy-job-shop-task task) :virtual-time 0))
		      	 (task-nr (job-shop-task-task.nr task)))

		    (setf tasks-to-assign (append tasks-to-assign (list (make-task-info :job-id job
		    																	:task-id task-nr
		    																	:task-duration (job-shop-task-duration task)))))
		    (setf (aref estado-inicial job task-nr) job-task-wrapper)))))

	 ;inicializacao de precedencias
	(loop for job from 0 to (- n.jobs 1) do
	 	(loop for task from 1 to (- (list-length (job-shop-job-tasks (nth job jobs))) 1) do
		    (let*  ((job-w-constr (aref estado-inicial job task))
			   		(job-w-constr-before (aref estado-inicial job (- task 1)))
			   		(job-task-before (job-task-w-constr-job-task job-w-constr-before))
			   		(virtual-time-before (job-task-w-constr-virtual-time job-w-constr-before))
			   		(duration-before (job-shop-task-duration job-task-before)))
	      		(setf (job-task-w-constr-virtual-time job-w-constr) (+ virtual-time-before duration-before)))))

	(setf state-job (make-state-job-schedule :machine-times (make-array (list 2 (job-shop-problem-n.machines job-shop-prob)))
											 :non-allocated-tasks tasks-to-assign
											 :number-tasks n-total-tasks
											 :jobs-tasks-space estado-inicial
											 :funcao-heuristica funcao-heuristica))

	(cria-problema state-job operadores :objectivo? #'estado-objectivo :hash #'hash-generator :estado= #'estados-iguais :heuristica funcao-heuristica)))
  

(defun proxima-tarefa (estado job)
  (let* ((dimensions (array-dimensions estado))
		 (columns (cadr dimensions)))

    (loop for task from 0 to (- columns 1) do 
      (if (null (job-shop-task-start.time (job-task-w-constr-job-task (aref estado job task))))
	  	(return-from proxima-tarefa task)))

    (return-from proxima-tarefa nil)))
  
 
;operador para gerar sucessores
;operador assume que as tasks estao ordenadas por ordem crescente
;estado interno e' um array de job-task-w-constr
(defun inicia-task (state-job)
  (let* ((sucessores '())
       	 (estado (state-job-schedule-jobs-tasks-space state-job))
       	 (dimensions (array-dimensions estado))
       	 (rows (car dimensions)) ;numero de jobs igual ao numero de linhas
       	 (tempos-maquinas (state-job-schedule-machine-times state-job))
       	 (tarefas-a-atribuir (state-job-schedule-non-allocated-tasks state-job))
       	 (funcao-heuristica-estado (state-job-schedule-funcao-heuristica state-job))) 

    (loop for job from 0 to (- rows 1) do
      (let* ((prox-task (proxima-tarefa estado job))
	    	 (novo-estado nil)
	   	 	 (job-task nil)
	    	 (nr.maquina nil)
	    	 (last-start-time nil)
	    	 (task-duration nil)
	    	 (copia-task nil)
	    	 (tempo-maquina-actual nil)
	    	 (tempos-maquinas-actual nil)
	    	 (tasks-to-assign-actual nil)
	    	 (min-state-value most-positive-fixnum)
	    	 (sucessor-list nil))

	    (if (not (null prox-task))
			(progn

			  ;novo-estado sucessor 
			  (setf novo-estado (copy-array estado))

			  ;o job-shop-task
			  (setf job-task (job-task-w-constr-job-task (aref estado job prox-task)))
			  (setf copia-task (copy-job-shop-task job-task))

			  ;o numero da maquina
			  (setf nr.maquina (job-shop-task-machine.nr job-task))

			  ;actualizar o tempo de inicio do job
			  (setf (job-shop-task-start.time copia-task) (job-task-w-constr-virtual-time (aref estado job prox-task)))
			  (setf last-start-time (job-shop-task-start.time copia-task))
			  (setf task-duration (job-shop-task-duration job-task))
			  (setf (aref novo-estado job prox-task) (make-job-task-w-constr :job-task copia-task :virtual-time (job-shop-task-start.time copia-task)))

			  ;actualizar os tempos possiveis de inicio de tasks que usam a mesma maquina ou que se seguem na lista de tasks do job
			  (propaga-restr-tempo-task novo-estado prox-task job last-start-time task-duration)
			  (propaga-restr-tempo-maquina novo-estado nr.maquina job last-start-time task-duration)

			  ;actualizar tempos gastos nas maquinas
			  (setf tempos-maquinas-actual (copy-array tempos-maquinas))
			  (setf tempo-maquina-actual (aref tempos-maquinas-actual 0 nr.maquina))
			  (if (null tempo-maquina-actual)
			  	(setf tempo-maquina-actual task-duration)
			  	(setf tempo-maquina-actual (+ tempo-maquina-actual task-duration)))
			  (setf (aref tempos-maquinas-actual 0 nr.maquina) tempo-maquina-actual)
			  (setf (aref tempos-maquinas-actual 1 nr.maquina) task-duration)

			  ;actualizar lista de tasks por atribuir
			  (dolist (task tarefas-a-atribuir)
			  	(let ((task-nr (task-info-task-id task)) 
			  		  (job-nr (task-info-job-id task)))
			  		(if (not (and (= job job-nr) (= prox-task task-nr)))
			  			(setf tasks-to-assign-actual (append tasks-to-assign-actual (list task))))))

			  (setf sucessores (append sucessores (list (make-state-job-schedule :machine-times tempos-maquinas-actual
																				 :non-allocated-tasks tasks-to-assign-actual
																				 :number-tasks (state-job-schedule-number-tasks state-job)
																				 :jobs-tasks-space novo-estado
																				 :funcao-heuristica funcao-heuristica-estado))))))))
	
	;(dolist (sucessor sucessores)
	;	(let ((state-value (funcall funcao-heuristica-estado sucessor))
	;		  (max-sucessor-number (min (max 3 (round (/ (list-length sucessores) 2))) 15)))
	;		(if (< (list-length sucessor-list) max-sucessor-number)
	;			(progn
	;				(setf sucessor-list (append sucessor-list (list sucessor)))
	;				(if (< state-value min-state-value)
	;					(setf min-state-value state-value)))
	;			)))

    sucessores))

(defun propaga-restr-tempo-task (estado task-nr job-nr last-start-time task-duration)
	(let* ((dimensions (array-dimensions estado))
		 	;(rows (car dimensions))
			(columns (cadr dimensions))
			(virtual-time-inc (+ last-start-time task-duration)))
		
			(loop for task from (+ task-nr 1) to (- columns 1) do
				(if (null (aref estado job-nr task))
					(return-from propaga-restr-tempo-task))

				(let* ((job-w-constr (aref estado job-nr task))
			    	   (job-task-actual (copy-job-shop-task (job-task-w-constr-job-task (aref estado job-nr task))))
				  	   ;(nr.maquina (job-shop-task-machine.nr job-task-actual))
				  	   ;(duration (job-shop-task-duration job-task-actual))
				  	   ;(start.time (job-shop-task-start.time job-task-actual))
				  	   (virt-time (job-task-w-constr-virtual-time job-w-constr))
				  	   (job-task-w-const-copia nil))

						;(setf job-task-w-const-copia (make-job-task-w-constr :job-task job-task-actual :virtual-time (+ virt-time task-duration)))
						(setf job-task-w-const-copia (make-job-task-w-constr :job-task job-task-actual :virtual-time (max virt-time virtual-time-inc)))
			      		(setf (aref estado job-nr task) job-task-w-const-copia)))))	


(defun propaga-restr-tempo-maquina (estado maquina job-nr last-start-time task-duration)
  (let* ((virtual-time-inc (+ last-start-time task-duration))
		 (dimensions (array-dimensions estado))
		 (rows (car dimensions))
		 (columns (cadr dimensions)))

     ;para cada maquina que ainda nao tenha start time, propaga o tempo virtual, actualizando o anterior
     (loop for job from 0 to (- rows 1) do
     	(if (not (= job job-nr))
			(loop named inner for task from 0 to (- columns 1) do

		  		;nao e' garantido que todos os jobs tenham o mesmo numero de tarefas
		  		(if (null (aref estado job task)) 
		      		(return-from inner)

				    ;Caso exista
				    (let* ((job-w-constr (aref estado job task))
				    	   (job-task-actual (copy-job-shop-task (job-task-w-constr-job-task (aref estado job task))))
					  	   (nr.maquina (job-shop-task-machine.nr job-task-actual))
					  	   ;(duration (job-shop-task-duration job-task-actual))
					  	   (start.time (job-shop-task-start.time job-task-actual))
					  	   (virt-time (job-task-w-constr-virtual-time job-w-constr))
					  	   (job-task-w-const-copia nil))

				      ;caso seja a maquina escolhida no operador que gera os sucessores e nao tenha sido ainda escolhida,
				      ;actualiza o valor virtual de inicio

				      (if (and (= maquina nr.maquina) (null start.time))
				      	(progn
				      		;(setf job-task-w-const-copia (make-job-task-w-constr :job-task job-task-actual :virtual-time (+ virt-time virtual-time-inc)))
				      		(setf job-task-w-const-copia (make-job-task-w-constr :job-task job-task-actual :virtual-time (max virt-time virtual-time-inc)))
				      		(setf (aref estado job task) job-task-w-const-copia)
				      		(propaga-restr-tempo-task estado task job last-start-time task-duration))))))))))



(defun hash-generator (job-state)
        (let*  ((answer "")
				(estado (state-job-schedule-jobs-tasks-space job-state))
				(dimensions (array-dimensions estado))
				(rows (car dimensions))
				(columns (cadr dimensions)))
		    (loop for job from 0 to (- rows 1) do
				(loop for task from 0 to (- columns 1) do
			  		(let* ((task-constr (aref estado job task)))
			    		(if (not (null  task-constr))
							(setf answer (concatenate 'string answer (write-to-string (job-task-w-constr-virtual-time task-constr))))))))
	        (return-from hash-generator answer)))


(defun estado-objectivo (state-job)
	(let*  ((estado (state-job-schedule-jobs-tasks-space state-job))
			(dimensions (array-dimensions estado))
		 	(rows (car dimensions))
		 	(columns (cadr dimensions)))
  		(loop for job from 0 to (- rows 1) do
  			(if (loop for task from 0 to (- columns 1) thereis (null (job-shop-task-start.time (job-task-w-constr-job-task (aref estado job task)))))
  					(return-from estado-objectivo nil)))
  		t))

  		
;heuristica que mede o tempo que falta para os trabalhos acabarem
(defun heuristica-tempo-falta (job-state)
	(let ((tarefas-nao-alocadas (state-job-schedule-non-allocated-tasks job-state))
		  (heuristica-value 0))
		(dolist (task tarefas-nao-alocadas)
			(setf heuristica-value (+ heuristica-value (task-info-task-duration task))))
		heuristica-value)) 		

;heuristica que faz um peso entre o racio da paralelizacao e o tempo que falta para terminar a atribuicao
(defun heuristica-tempo-final-v2 (job-state)
	(let* ((tarefas-nao-alocadas (state-job-schedule-non-allocated-tasks job-state))
		   (estado (state-job-schedule-jobs-tasks-space job-state))
		   (n-tarefas-nao-alocadas (list-length tarefas-nao-alocadas))
		   (tempos-maquinas (state-job-schedule-machine-times job-state))
		   (n-maquinas (cadr (array-dimensions tempos-maquinas)))
		   (total-number-tasks (state-job-schedule-number-tasks job-state))
		   (machines-time-working 0)
		   (estimativa-fim-temporal 0)
		   (estimativa-max-tempo-trabalho 0)
		   (heuristica-value 0)
		   (parallel 0)
		   (peso1 0.35)
		   (peso2 0.7)
		   (peso3 0.05))
		   
		;estimativa do fim temporal do problema
		(dolist (task tarefas-nao-alocadas)
		    (let* ((task-id (task-info-task-id task))
				   (job-id (task-info-job-id task))
				   (virtual-task-time-start (job-task-w-constr-virtual-time (aref estado job-id task-id)))
				   (task-duration (task-info-task-duration task))
				   (estimativa (+ virtual-task-time-start task-duration)))
			(when (>  estimativa estimativa-fim-temporal)
			  (setf estimativa-fim-temporal estimativa))))

		;estimativa do tempo maximo que falta para acabar problema
		(dolist (task tarefas-nao-alocadas)
			(setf estimativa-max-tempo-trabalho (+ estimativa-max-tempo-trabalho (task-info-task-duration task))))
		
		;estimativa do quao boa esta a ser a paralelizacao
		(dotimes (nr-maquina n-maquinas)
			(let ((tempo-actual (aref tempos-maquinas 0 nr-maquina)))
				(if (null tempo-actual)
					(setf machines-time-working (+ machines-time-working 0))
			  		(setf machines-time-working (+ machines-time-working tempo-actual)))))
		(if (= 0 machines-time-working)
			(setf parallel 0)
			(setf parallel (/ (- total-number-tasks n-tarefas-nao-alocadas) machines-time-working)))
		

		;calcular valor da heuristica no estado
		(setf heuristica-value (* n-tarefas-nao-alocadas (+ (* peso1 estimativa-fim-temporal) 
															(* peso2 parallel)
															(* peso3 estimativa-max-tempo-trabalho)
															(* 0.55 (- (maquina-gastou-mais-tempo job-state) 1)))))
		heuristica-value))


;heuristica que faz um peso entre o racio da paralelizacao e de quanto falta para terminar
(defun heuristica-tempo-final (job-state)
	(let* ((tarefas-nao-alocadas (state-job-schedule-non-allocated-tasks job-state))
		   (estado (state-job-schedule-jobs-tasks-space job-state))
		   (n-tarefas-nao-alocadas (list-length tarefas-nao-alocadas))
		   (tempos-maquinas (state-job-schedule-machine-times job-state))
		   (n-maquinas (cadr (array-dimensions tempos-maquinas)))
		   (total-number-tasks (state-job-schedule-number-tasks job-state))
		   (machines-time-working 0)
		   (estimativa-fim-temporal 0)
		   (heuristica-value 0)
		   (parallel 0)
		   (peso1 0.4)
		   (peso2 0.6))
		   
		;estimativa do fim temporal do problema
		(dolist (task tarefas-nao-alocadas)
		    (let* ((task-id (task-info-task-id task))
				   (job-id (task-info-job-id task))
				   (virtual-task-time-start (job-task-w-constr-virtual-time (aref estado job-id task-id)))
				   (task-duration (task-info-task-duration task))
				   (estimativa (+ virtual-task-time-start task-duration)))
			(when (>  estimativa estimativa-fim-temporal)
			  (setf estimativa-fim-temporal estimativa))))
		
		;estimativa do quao boa esta a ser a paralelizacao
		(dotimes (nr-maquina n-maquinas)
			(let ((tempo-actual (aref tempos-maquinas 0 nr-maquina))
			      (last-task-duration (aref tempos-maquinas 1 nr-maquina)))
				(if (null tempo-actual)
					(setf machines-time-working (+ machines-time-working 0))
			  		(setf machines-time-working (+ machines-time-working tempo-actual)))))
		(if (= 0 machines-time-working)
			(setf parallel 0)
			(setf parallel (/ (- total-number-tasks n-tarefas-nao-alocadas) machines-time-working)))
		

		;calcular valor da heuristica no estado
		(setf heuristica-value (* n-tarefas-nao-alocadas (+ (* peso1 estimativa-fim-temporal) (* peso2 parallel))))
		heuristica-value))
		
		
; Heuristica Tempo Desperdicado (NaoAlocadas / (NMaquinas * TotalTarefas)) *(MaximoTempoMaquinas + SumDurationNonAllocated)
(defun heuristica-tempo-desperdicado (estado)
	(let* ((tarefas-nao-alocadas (state-job-schedule-non-allocated-tasks estado))
		   (n-tarefas-nao-alocadas (list-length tarefas-nao-alocadas))
		   (tempos-maquinas (state-job-schedule-machine-times estado))
		   (n-maquinas (cadr (array-dimensions tempos-maquinas)))
		   (total-number-tasks (state-job-schedule-number-tasks estado))
		   (remaining-task-durations 0)
		   (max-time-machine 0)
		   (heuristica-value 0))

		;calcular duracao das tarefas que faltam realizar
		(dolist (task tarefas-nao-alocadas)
			(setf remaining-task-durations (+ remaining-task-durations (task-info-task-duration task))))

		;determinar qual o tempo de funcionamento maximo de uma maquina ate agora
		(dotimes (nr-maquina n-maquinas)
			(let ((tempo-actual (aref tempos-maquinas 0 nr-maquina))
				  (last-task-duration (aref tempos-maquinas 1 nr-maquina)))
				(if (and (not (null tempo-actual)) (> (- tempo-actual last-task-duration) max-time-machine))
					(setf max-time-machine tempo-actual))))
					;(setf max-time-machine (- tempo-actual last-task-duration)))))

		;calcular valor da heuristica no estado
		(setf heuristica-value (* (/ n-tarefas-nao-alocadas (* n-maquinas total-number-tasks)) (+ max-time-machine 
																								  remaining-task-durations
																								  (* 0.9 (- (maquina-gastou-mais-tempo estado) 1)))))
		heuristica-value))


; Funcao de Custo associado a um estado
(defun maquina-gastou-mais-tempo (estado)
	(let* ((tempos-maquinas (state-job-schedule-machine-times estado))
		   (n-maquinas (cadr (array-dimensions tempos-maquinas)))
		   (max-time-machine 0))
		(dotimes (nr-maquina n-maquinas)
			(let ((tempo-actual (aref tempos-maquinas 0 nr-maquina))
				  (last-task-duration (aref tempos-maquinas 1 nr-maquina)))
				(if (and (not (null tempo-actual)) (> (- tempo-actual last-task-duration) max-time-machine))
					;(setf max-time-machine tempo-actual))))
					(setf max-time-machine (- tempo-actual last-task-duration)))))
		max-time-machine))

;Funcao de comparacao de estados
(defun estados-iguais (job-estado1 job-estado2)
	(let*  ((estado1 (state-job-schedule-jobs-tasks-space job-estado1))
			(estado2 (state-job-schedule-jobs-tasks-space job-estado2))
			(dimensions (array-dimensions estado1))
           	(rows (car dimensions))
           	(columns (car (cdr dimensions))))

		(dotimes (job rows)
			(dotimes (task columns)
				(let* ((task-estado1 (aref estado1 job task))
					   (task-estado2 (aref estado2 job task))
					   (task1 (job-task-w-constr-job-task task-estado1))
					   (task2 (job-task-w-constr-job-task task-estado2)))
					(if (and (not (null task-estado1)) (not (null task-estado2)))
						(if (not (and 	(= (job-shop-task-job.nr task1) (job-shop-task-job.nr task2))
								   		(= (job-shop-task-task.nr task1) (job-shop-task-task.nr task2))
									  	(equal (job-shop-task-start.time task1) (job-shop-task-start.time task2))))
							(return-from estados-iguais nil))))))
		(return-from estados-iguais t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Struct aux functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-job-shop-job (job)
	(let ((copia (make-job-shop-job :job.nr (job-shop-job-job.nr job)
									:tasks '()))
		  (tasks (job-shop-job-tasks job)))
		(setf (job-shop-job-tasks copia) (copy-list tasks))
		copia))

(defun copy-job-shop-task (task)
	(let ((copia (make-job-shop-task :job.nr (job-shop-task-job.nr task)
   									 :task.nr (job-shop-task-task.nr task)
   								 	 :machine.nr (job-shop-task-machine.nr task)
   								 	 :duration (job-shop-task-duration task)
   								 	 :start.time (job-shop-task-start.time task))))
		copia))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Aux functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun insert-ordered-heuristic (lst elem heuristic-function)
	(let* ((new-list nil)
		   (heuristic-value (funcall heuristic-function elem))
		   (lst-size (list-length lst)))
		(if (null lst)
			(setf new-list (append new-list elem))
			(progn
				(dolist (lst-elem lst)
					(if (< heuristic-value (funcall heuristic-function lst-elem))
						(progn
							(setf new-list (append new-list (list elem lst-elem)))
							(setf heuristic-value most-positive-fixnum))
						(setf new-list (append new-list (list lst-elem)))))
				(if (= lst-size)
					())))
		new-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Searches implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;
; Beam Search
;;;;;;;;;;;;;;;
;http://norvig.com/paip/search.lisp
;thanks to Mr. Norvig
(defun sorter (heur)
  "Return a combiner function that sorts according to heuristic."
  #'(lambda (new old)
      (let* ((all-states (append new old)))
	(stable-sort all-states #'< :key heur))))

;Also thanks to Mr.Norvig
(defun beam-sorter (beam-width heur)
       #'(lambda (old new)
	  (let ((sorted (funcall (sorter heur) old new)))
	    (if (> beam-width (length sorted))
		  sorted
		(subseq sorted 0 beam-width))))
 		)

(defun is-better-solution-job-shop (best-solution-so-far new-solution) 
    (let* ((end-best 0)
	   (end-new 0)
	   (estado-best-solution (state-job-schedule-jobs-tasks-space (car best-solution-so-far)))
	   (estado-new-solution (state-job-schedule-jobs-tasks-space (car new-solution)))
	   (dimensions (array-dimensions estado-new-solution))
           (rows (car dimensions))
           (columns (car (cdr dimensions)))
	   )
	   (loop for job from 0 to (- rows 1) do
	      (loop for task from 0 to (- columns 1) do
		  (let* ((job-task-new (job-task-w-constr-job-task (aref estado-new-solution job task)))
			(job-task-best (job-task-w-constr-job-task (aref estado-best-solution job task)))
			(end-aux-new (+ (job-shop-task-duration job-task-new) (job-shop-task-start.time job-task-new)))
			(end-aux-best (+ (job-shop-task-duration job-task-best) (job-shop-task-start.time job-task-best)))
			)
		    (when (> end-aux-new end-new)
			(setf end-new end-aux-new))
		    (when (> end-aux-best end-best)
			(setf end-best end-aux-best)))))
	   (return-from is-better-solution-job-shop (< end-new end-best))))
      
	    
(defun beam-search (problema is-better-solution-fn beam-width max-beam-width) 
  (let* ((*nos-gerados* 0)
		 (*nos-expandidos* 0)
		 (tempo-inicio (get-internal-real-time))
		 (objectivo? (problema-objectivo? problema))
		 (heur (problema-heuristica problema))
		 ;(estado= (problema-estado= problema))

		 (result nil)
		 (tempo 300))
    (labels ((beam (estados)
              (cond ((<= (- tempo (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) 0.2) (return-from beam-search result))
		    		((funcall objectivo? (first estados)) (list (first estados)))
                    ((null (first estados)) nil)
                    (t 
                     (let* ((sucessores-estado (problema-gera-sucessores problema (first estados)))
                            (sucessores-ordenados (funcall (beam-sorter beam-width heur) sucessores-estado (rest estados)));sucessores ate' beam width
                            )
			  (return-from beam (beam sucessores-ordenados)))))))
             (loop 
				(let* ((aux-result nil))
				  (setf aux-result (beam (list (problema-estado-inicial problema))))
				  (if (null result)
				      (setf result aux-result)
				     (if (funcall is-better-solution-fn result aux-result)
					 	(setf result aux-result)))
				  (if (< beam-width max-beam-width)
					(incf beam-width)
				      (return-from beam-search result))
				  (when (<= (- tempo (/ (- (get-internal-real-time) tempo-inicio) internal-time-units-per-second)) 0.2)
				      (return-from beam-search result)))))))

;;;;;;;;;;;;;
;ILDS
;;;;;;;;;;;;;
;Ref para ilds pseudo-codigo
;http://delivery.acm.org/10.1145/2020000/2019581/a1_6-prosser.pdf?ip=194.210.231.19&id=2019581&acc=ACTIVE%20SERVICE&key=2E5699D25B4FE09E%2EF7A57B2C5B227641%2E4D4702B0C3E38B35%2E4D4702B0C3E38B35&CFID=675816888&CFTOKEN=37766688&__acm__=1432052157_b9aa95a702e113482df3bdaed4e3b506
;FIXME - ordenar sucessores por f = g + h


(defun ilds-sorter (heur)
  "Return a combiner function that sorts according to heuristic."
  #'(lambda (all-states)
	(stable-sort all-states #'< :key heur)))

(defun ilds (problema maxDepth) 
  (let ((*nos-gerados* 0)
		(*nos-expandidos* 0)
		(tempo-inicio (get-internal-real-time))
		(max-runtime 300)
		(objectivo? (problema-objectivo? problema))
        ;(estado= (problema-estado= problema))
        (numMaxDiscrepancia maxDepth)
        (result nil))
    
    (labels ((ildsProbe (estado maxDiscrepancia rProfundidade)
                (let* ((sucessores-nao-ordenados (problema-gera-sucessores problema estado))
		       (sucessores (funcall (ilds-sorter (problema-heuristica problema)) sucessores-nao-ordenados))
                       (num-elem (list-length sucessores)))
                     (cond 	((funcall objectivo? estado) (list estado))
                     		((= 0 num-elem) nil)
                     		(t 
                     			(setf result nil)
                     			(if (> rProfundidade maxDiscrepancia)
                     				(setf result (ildsProbe (car sucessores) maxDiscrepancia (- rProfundidade 1))))
                     			(if (and (> maxDiscrepancia 0) (null result))
                     				(dolist (suc (cdr sucessores))
                     					(setf result (ildsProbe suc (- maxDiscrepancia 1 ) (- rProfundidade 1)))
                     					(when (not (null result))
                     					 	(return-from ildsProbe result))))
                 				(return-from ildsProbe result))))))
			(loop for maxDiscrepancia from 0 to numMaxDiscrepancia do
				(setf result (ildsProbe (problema-estado-inicial problema) maxDiscrepancia (- numMaxDiscrepancia maxDiscrepancia)))
				(when (not (null result))
					(return-from ilds result))))))

;;;;;;;;;;;;;;;;;;;;;
;Iterative-Sampling
;;;;;;;;;;;;;;;;;;;;;
(defun sondagem-iterativa (problema) 
  (let* ((*nos-gerados* 0)
		 (*nos-expandidos* 0)
		 (tempo-inicio (get-internal-run-time))
		 (objectivo? (problema-objectivo? problema))
		 ;(estado= (problema-estado= problema))
		 (result nil))

    (labels ((lanca-sonda (estado)
              (cond ((funcall objectivo? estado) (list estado))
                    ((null estado) nil)
                    (t 
                     (let* ((sucessores (problema-gera-sucessores problema estado))
                            (num-elem (length sucessores)))
                       (if(equal num-elem 0)
                           nil
                         (lanca-sonda (nth (random num-elem) sucessores))))))))
             (loop while (null result) do
               (setf result (lanca-sonda (problema-estado-inicial problema))))
             (return-from sondagem-iterativa result))))
