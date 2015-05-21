(in-package :user)
(load (compile-file "procura.lisp"))
(load (compile-file "job-shop-problemas-modelos.lisp"))


(defun calendarizacao (job-shop-problem procura-str) 
  (let ((internal-problema (converte-para-estado-interno job-shop-problem))
		(result nil))

    (cond ((equal procura-str "ILDS") (ilds internal-problema))
	  	  ((equal procura-str "Iterative-Sampling") (sondagem-iterativa internal-problema))
	  	  (t
	    	(procura internal-problema procura-str)))))


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
  
(defun converte-para-visualizacao (estado-interno)
  ) 

(defun converte-para-estado-interno (job-shop-prob) 
  (let ((operadores (list #'inicia-task))
		(n.jobs (job-shop-problem-n.jobs job-shop-prob))
		(estado-inicial nil)
		(max-num-tasks 0)
		(jobs (job-shop-problem-jobs job-shop-prob)))

	(dotimes (job n.jobs)
	  (if (> (list-length (job-shop-job-tasks (nth job jobs))) max-num-tasks)
	      	(setf max-num-tasks (list-length (job-shop-job-tasks (nth job jobs))))))

	(setf estado-inicial (make-array (list n.jobs max-num-tasks)))
	(dotimes (job n.jobs)
	  (let* ((job-tasks (job-shop-job-tasks (nth job jobs))))

	    (dolist (task job-tasks)
	      (let* ((job-task-wrapper (make-job-task-w-constr :job-task (copy-job-shop-task task) :virtual-time 0))
		      	 (task-nr (job-shop-task-task.nr task)))

		    (setf (aref estado-inicial job task-nr) job-task-wrapper)))))

	 ;inicializacao de precedencias
	 (loop for job from 0 to (- n.jobs 1) do
	 	(loop for task from 1 to (- (list-length (job-shop-job-tasks (nth job jobs))) 1) do
	  ;(dotimes (task 1 (list-length (job-shop-job-tasks (nth job jobs))))
	    (let*  ((job-w-constr (aref estado-inicial job task))
		   		(job-w-constr-before (aref estado-inicial job (- task 1)))
		   		(job-task-before (job-task-w-constr-job-task job-w-constr-before))
		   		(virtual-time-before (job-task-w-constr-virtual-time job-w-constr-before))
		   		(duration-before (job-shop-task-duration job-task-before)))
	      (setf (job-task-w-constr-virtual-time job-w-constr) (+ virtual-time-before duration-before)))))
	 (cria-problema estado-inicial operadores)))
  

(defun proxima-tarefa (estado job)
  (let* ((dimensions (array-dimensions estado))
		 (columns (cadr dimensions)))

    (loop for task from 0 to (- columns 1) do 
      (if (null (job-shop-task-start.time (job-task-w-constr-job-task (aref estado job task))))
	  	(return-from proxima-tarefa task)))

    (return-from proxima-tarefa nil)))
  
 
;operador
;operador assume que as tasks estao ordenadas por ordem crescente
;estado interno e' um array de job-task-w-constr
(defun inicia-task (estado)
  (let* ((sucessores '())
       	 (dimensions (array-dimensions estado))
       	 (rows (car dimensions))
       	 (columns (cadr dimensions))) ;numero de jobs igual ao numero de linhas

    (loop for job from 0 to (- rows 1) do
      (let* ((prox-task (proxima-tarefa estado job))
	    	 (novo-estado nil)
	   	 	 (job-task nil)
	    	 (nr.maquina nil)
	    	 (last-start-time nil)
	    	 (task-duration nil)
	    	 (copia-task nil))

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
			  (setf (aref novo-estado job prox-task) (make-job-task-w-constr :job-task copia-task :virtual-time 0))
			  ;(propaga-restr-tempo-task novo-estado prox-task job last-start-time)
			  (propaga-restr-tempo-maquina novo-estado nr.maquina job last-start-time task-duration)
			  (setf sucessores (cons novo-estado sucessores))))))
    sucessores))

(defun propaga-restr-tempo-task (estado task-nr job-nr task-duration)
	(let* ((dimensions (array-dimensions estado))
		 	(rows (car dimensions))
			(columns (cadr dimensions)))
		
			(loop for task from (+ task-nr 1) to (- columns 1) do
				(if (null (aref estado job-nr task))
					(return-from propaga-restr-tempo-task))

				(let* ((job-w-constr (aref estado job-nr task))
			    	   (job-task-actual (copy-job-shop-task (job-task-w-constr-job-task (aref estado job-nr task))))
				  	   (nr.maquina (job-shop-task-machine.nr job-task-actual))
				  	   ;(duration (job-shop-task-duration job-task-actual))
				  	   (start.time (job-shop-task-start.time job-task-actual))
				  	   (virt-time (job-task-w-constr-virtual-time job-w-constr))
				  	   (job-task-w-const-copia nil))

					(setf job-task-w-const-copia (make-job-task-w-constr :job-task job-task-actual :virtual-time (+ virt-time task-duration)))
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
				      		(setf job-task-w-const-copia (make-job-task-w-constr :job-task job-task-actual :virtual-time (+ virt-time virtual-time-inc)))
				      		(setf (aref estado job task) job-task-w-const-copia)
				      		(propaga-restr-tempo-task estado task job task-duration))))))))))


  
  
(defun optimize-schedule (job-problem)
  )


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
;FIXME last search option

;;;;;;;;;;;;;
;ILDS
;;;;;;;;;;;;;
;Ref para ilds pseudo-codigo
;http://delivery.acm.org/10.1145/2020000/2019581/a1_6-prosser.pdf?ip=194.210.231.19&id=2019581&acc=ACTIVE%20SERVICE&key=2E5699D25B4FE09E%2EF7A57B2C5B227641%2E4D4702B0C3E38B35%2E4D4702B0C3E38B35&CFID=675816888&CFTOKEN=37766688&__acm__=1432052157_b9aa95a702e113482df3bdaed4e3b506
(defun ilds (problema) 
  (let ((*nos-gerados* 0)
	(*nos-expandidos* 0)
	(tempo-inicio (get-internal-run-time))
	(objectivo? (problema-objectivo? problema))
        ;(estado= (problema-estado= problema))
        (numMaxDiscrepancia 4)
        (result nil))
    
        (labels ((ildsProbe (estado maxDiscrepancia rProfundidade)
                    (let* ((sucessores (problema-gera-sucessores problema estado))
                           (num-elem (length sucessores)))
                         (cond 	((funcall objectivo? estado) (list estado))
                         		((eq 0 num-elem) nil)
                         		(t 
                         			(setf result nil)
                         			(if (> rProfundidade maxDiscrepancia)
                         				(setf result (ildsProbe (car sucessores) maxDiscrepancia (- rProfundidade 1))))
                         			(if (and (> maxDiscrepancia 0) (null result))
                         				(dolist (suc (cdr sucessores))
                         					(setf result (ildsProbe suc (- maxDiscrepancia 1 ) (- rProfundidade 1)))
                         					(when (not (null result))
                         					 	(return-from ildsProbe (list result)))))
                     				(return-from ildsProbe (list result)))))))
				(dotimes (maxDiscrepancia numMaxDiscrepancia)
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
	 (solucao nil))
    
        (labels (#|(esta-no-caminho? (estado caminho)
                                   (member estado caminho :test estado=))|#
                 (lanca-sonda (estado)
                              (cond ((funcall objectivo? estado) (list estado))
                                    ((null estado) nil)
                                    (t 
                                     (let* ((sucessores (problema-gera-sucessores problema estado))
                                            (num-elem (length sucessores)))
                                       (if(equal num-elem 0)
                                           nil
                                         (lanca-sonda (nth (random num-elem) sucessores))))))))
                 (unless (null solucao) 
                   (setf solucao (lanca-sonda (problema-estado-inicial problema))))
                 (return-from sondagem-iterativa (list solucao)))))
