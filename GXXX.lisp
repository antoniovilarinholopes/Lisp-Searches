(in-package :user)
(load (compile-file "procura.lisp"))
(load (compile-file "job-shop-problemas-modelos.lisp"))
(load (compile-file "job-shop-problemas-v1.lisp"))


(defun calendarizacao (job-shop-problem procura-str) 
  (let ((internal-problema (converte-para-estado-interno job-shop-problem))
		(result nil))
    (cond ((string-equal procura-str "ILDS") (setf result (ilds internal-problema 100)))
	  	  ((string-equal procura-str "Iterative-Sampling") (setf result (sondagem-iterativa internal-problema)))
	  	  ((string-equal procura-str "Local-Beam") (setf result (beam-search internal-problema 10)))
	  	  (t
	    	(setf result (procura internal-problema procura-str))))
    (if (not (null result))
    	(car result)
    	;(converte-para-visualizacao (car result))
    	nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Job Shop aux
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct job-schedule 
  job-shop-problem
  best-schedule)

 ;Used to propagate time constraint from task precendence and machine precendence
(defstruct job-task-w-constr
  job-task
  virtual-time)
  
(defstruct state-job-schedule
	machine-times
	non-allocated-tasks
	number-tasks
	jobs-tasks-space)

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

(defun converte-para-estado-interno (job-shop-prob) 
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

	(setf state-job (make-state-job-schedule :machine-times (make-array n.jobs)
											 :non-allocated-tasks tasks-to-assign
											 :number-tasks n-total-tasks
											 :jobs-tasks-space estado-inicial))

	(cria-problema state-job operadores :objectivo? #'estado-objectivo :heuristica #'heuristica-tempo-desperdicado :custo #'maquina-gastou-mais-tempo)))
  

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
       	 (tarefas-a-atribuir (state-job-schedule-non-allocated-tasks state-job))) 

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
	    	 (tasks-to-assign-actual nil))

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
			  (setf tempo-maquina-actual (aref tempos-maquinas-actual nr.maquina))
			  (if (null tempo-maquina-actual)
			  	(setf tempo-maquina-actual task-duration)
			  	(setf tempo-maquina-actual (+ tempo-maquina-actual task-duration)))
			  (setf (aref tempos-maquinas-actual nr.maquina) tempo-maquina-actual)

			  ;actualizar lista de tasks por atribuir
			  (dolist (task tarefas-a-atribuir)
			  	(let ((task-nr (task-info-task-id task)) 
			  		  (job-nr (task-info-job-id task)))
			  		(if (not (and (= job job-nr) (= prox-task task-nr)))
			  			(setf tasks-to-assign-actual (append tasks-to-assign-actual (list task))))))

			  (setf sucessores (append sucessores (list (make-state-job-schedule :machine-times tempos-maquinas-actual
																				 :non-allocated-tasks tasks-to-assign-actual
																				 :number-tasks (state-job-schedule-number-tasks state-job)
																				 :jobs-tasks-space novo-estado))))))))
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


  
  
(defun optimize-schedule (job-problem))

(defun estado-objectivo (state-job)
	(let*  ((estado (state-job-schedule-jobs-tasks-space state-job))
			(dimensions (array-dimensions estado))
		 	(rows (car dimensions))
		 	(columns (cadr dimensions)))
  		(loop for job from 0 to (- rows 1) do
  			(if (loop for task from 0 to (- columns 1) thereis (null (job-shop-task-start.time (job-task-w-constr-job-task (aref estado job task)))))
  					(return-from estado-objectivo nil)))
  		t))

; Heuristica Tempo Desperdicado (NaoAlocadas / (NMaquinas * TotalTarefas)) *(MaximoTempoMaquinas + SumDurationNonAllocated)
(defun heuristica-tempo-desperdicado (estado)
	(let* ((tarefas-nao-alocadas (state-job-schedule-non-allocated-tasks estado))
		   (n-tarefas-nao-alocadas (list-length tarefas-nao-alocadas))
		   (tempos-maquinas (state-job-schedule-machine-times estado))
		   (n-maquinas (car (array-dimensions tempos-maquinas)))
		   (total-number-tasks (state-job-schedule-number-tasks estado))
		   (remaining-task-durations 0)
		   (max-time-machine 0)
		   (heuristica-value 0))

		;calcular duracao das tarefas que faltam realizar
		(dolist (task tarefas-nao-alocadas)
			(setf remaining-task-durations (+ remaining-task-durations (task-info-task-duration task))))

		;determinar qual o tempo de funcionamento maximo de uma maquina ate agora
		(dotimes (nr-maquina n-maquinas)
			(let ((tempo-actual (aref tempos-maquinas nr-maquina)))
				(if (and (not (null tempo-actual)) (> tempo-actual max-time-machine))
					(setf max-time-machine tempo-actual))))

		;calcular valor da heuristica no estado
		(setf heuristica-value (* (/ n-tarefas-nao-alocadas (* n-maquinas total-number-tasks)) (+ max-time-machine remaining-task-durations)))
		heuristica-value))

(defun maquina-gastou-mais-tempo (estado)
	(let* ((tempos-maquinas (state-job-schedule-machine-times estado))
		   (n-maquinas (car (array-dimensions tempos-maquinas)))
		   (max-time-machine 0))
		(dotimes (nr-maquina n-maquinas)
			(let ((tempo-actual (aref tempos-maquinas nr-maquina)))
				(if (and (not (null tempo-actual)) (> tempo-actual max-time-machine))
					(setf max-time-machine tempo-actual))))
		max-time-machine))

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

(defun is-better-solution (heur)
      #'(lambda (best-solution-so-far new-solution)
	    (> (funcall heur (first new-solution)) (funcall heur (first best-solution-so-far)))))
	    
(defun beam-search (problema beam-width max-beam-width) 
  (let* ((*nos-gerados* 0)
		 (*nos-expandidos* 0)
		 (tempo-inicio (get-internal-run-time))
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
				     (if (funcall (is-better-solution heur) result aux-result)
					 	(setf result aux-result)))
				  (if (< beam-width max-beam-width)
					(incf beam-width)
				      (return-from beam-search result))
				  (when (<= (- tempo (/ (- (get-internal-run-time) tempo-inicio) internal-time-units-per-second)) 0.2)
				      (return-from beam-search result)))))))

;;;;;;;;;;;;;
;ILDS
;;;;;;;;;;;;;
;Ref para ilds pseudo-codigo
;http://delivery.acm.org/10.1145/2020000/2019581/a1_6-prosser.pdf?ip=194.210.231.19&id=2019581&acc=ACTIVE%20SERVICE&key=2E5699D25B4FE09E%2EF7A57B2C5B227641%2E4D4702B0C3E38B35%2E4D4702B0C3E38B35&CFID=675816888&CFTOKEN=37766688&__acm__=1432052157_b9aa95a702e113482df3bdaed4e3b506
(defun ilds (problema maxDepth) 
  (let ((*nos-gerados* 0)
		(*nos-expandidos* 0)
		(tempo-inicio (get-internal-run-time))
		(objectivo? (problema-objectivo? problema))
        ;(estado= (problema-estado= problema))
        (numMaxDiscrepancia maxDepth)
        (result nil))
    
    (labels ((ildsProbe (estado maxDiscrepancia rProfundidade)
                (let* ((sucessores (problema-gera-sucessores problema estado))
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
             (return-from sondagem-iterativa solucao))))
