
(in-package :user)

;; 
;; Job-shop scheduling problem
;;
(defstruct job-shop-problem
   name
   n.jobs
   n.machines
   jobs)

;; 
;; Job
;;
(defstruct job-shop-job
   job.nr
   tasks)

;; 
;; Task
;;
(defstruct job-shop-task
   job.nr
   task.nr
   machine.nr
   duration
   start.time)
 
