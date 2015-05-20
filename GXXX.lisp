(in-package :user)
(load (compile-file "procura.lisp"))
(load (compile-file "job-shop-problemas-modelos.lisp"))


(defun calendarizacao (job-shop-problem procura-str) 
  (let ((internal-problema (converte-para-estado-interno job-shop-problem))
	(result nil))
    (cond ((equal procura-str "ILDS") (ilds internal-problema))
	  ((equal procura-str "Iterative-Sampling") (sondagem-iterativa internal-problema))
	  (t
	    (procura internal-problema procura-str))
	    )
    )
  )



J0: T0 nil
    T1 nil
J1: T0 nil
    T1 nil
J2: T0 nil
    T1 nil

foreach job in jobs:
  task := get_next_task
  task_others := get_others_task

1ª
J0: T0 0
    T1 nil
J1: T0 nil
    T1 nil
J2: T0 nil
    T1 nil
    
J0: T0 nil
    T1 nil
J1: T0 0
    T1 nil
J2: T0 nil
    T1 nil
    
J0: T0 nil
    T1 nil
J1: T0 nil
    T1 nil
J2: T0 0
    T1 nil
    
2ª   
J0: T0 0
    T1 T0.D
J1: T0 nil
    T1 nil
J2: T0 nil
    T1 nil
    
J0: T0 0
    T1 nil
J1: T0 0
    T1 nil
J2: T0 nil
    T1 nil
    
J0: T0 0
    T1 nil
J1: T0 nil
    T1 nil
J2: T0 0
    T1 nil  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Job Shop aux
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct job-schedule 
  job-shop-problem
  best-schedule
  )

 ;Used to propagate time constraint from task precendence and machine precendence
(defstruct job-task-w-constr
  job-task
  virtual-time
  )
  
(defun converte-para-visualizacao (estado-interno)
  ) 

(defun converte-para-estado-interno (job-shop-prob) 
  (let ((operadores (list #'inicia-task))
	(n.jobs (job-shop-problem-n.jobs job-shop-prob))
	;(estado-inicial (make-array '()))
	)
	;inicializar todos os tempos virtuais a 0
    (cria-problema estado-inicial operadores)
    )
  )
  
 
;operador
;operador assume que as tasks estao ordenadas por ordem crescente
;estado interno e' um array de job-task-w-constr
(defun inicia-task (estado)
  (let ((sucessores '())
	(dimensions (array-dimensions estado))
	(columns (car dimensions))
	(rows (cdr dimensions)));numero de jobs igual ao numero de linhas
    ()
    )
  )  

(defun propaga-restr-tempo (estado maquina last-start-time task-duration)
  (let* ((virtual-time-inc (+ last-start-time task-duration))
	(dimensions (array-dimensions estado))
	(columns (car dimensions))
	(rows (cdr dimensions))
	)
     ;para cada maquina que ainda nao tenha start time, propaga o tempo virtual, actualizando o anterior
     (loop for job from 0 to rows
	(loop named inner for task from 0 to columns
	  ;nao e' garantido que todos os jobs tenham o mesmo numero de tarefas
	  (if (null (aref estado job task)) 
	      (return-from inner)
	    ;Caso exista
	    (let* ((job-task-actual (job-task-w-constr-job-task (aref estado job task)))
		  (nr.maquina (job-shop-task-machine.nr job-task-actual))
		  ;(duration (job-shop-task-duration job-task-actual))
		  (start.time (job-shop-task-start.time job-task-actual))
		  (virt-time (job-task-w-constr-virtual-time (aref estado job task)))
		  )
	      ;caso seja a maquina escolhida no operador que gera os sucessores e nao tenha sido ainda escolhida,
	      ;actualiza o valor virtual de inicio
	      (if (and (= maquina nr.maquina) (null start.time))
		  (setf (job-task-w-constr-virtual-time (aref estado job task)) (+ virt-time virtual-time-inc))
		  )
	     )
	    )
	  )
	)  
      )
  )  
  
  
(defun optimize-schedule (job-problem)
  )




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
	;(objectivo? (problema-objectivo? problema))
        (estado= (problema-estado= problema))
        (numMaxDiscrepancia 4)
        (result nil))
    
        (labels ((ildsProbe (estado maxDiscrepancia rProfundidade)
                    (let* ((sucessores (problema-gera-sucessores problema estado))
                           (num-elem (length(sucessores))))
                         (cond 	((funcall objectivo? estado) (list estado))
                         		((eq 0 num-elem) nil)
                         		(t 
                         			(setf result nil)
                         			(if (rProfundidade > maxDiscrepancia)
                         				(setf result (ildsProbe (car sucessores) maxDiscrepancia (- rProfundidade 1))))
                         			(if (and (maxDiscrepancia > 0) (null result))
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
	 ;(objectivo? (problema-objectivo? problema))
	 (estado= (problema-estado= problema))
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
                 (while(null solucao) 
                   (setf solucao (lanca-sonda (problema-estado-inicial problema))))
                 (return-from sondagem-iterativa (list solucao)))))

