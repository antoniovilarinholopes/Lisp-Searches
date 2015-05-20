(in-package :user)
(load (compile-file "procura.lisp"))
(load (compile-file "job-shop-problemas-modelos.lisp"))

(defun calendarizacao () )










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Job Shop aux
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


























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

