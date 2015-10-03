(defun n-queens (n maxSteps &aux (queens (make-array n)))  
  
 
    (dotimes (i n)      
      (setf (aref queens i) i))
    (dotimes (i n)            
      (setf position (random n))
      (setf rowToSwap  (aref queens i ))
      (setf (aref queens i) (aref queens position))
      (setf (aref queens position)  rowToSwap))
  
     (format t "Initial:~&~d~&" queens)
     
 
    (setf steps  0)    
    (loop
     (let ((max-conflict 0)
          (candidates '())
          (worst-queen-position 0)
          (min-conflict n))            
        
	 (dotimes (i n)	      
	    (setf conf (conflict-counter  queens (aref queens i) i))
        (if (= conf  max-conflict) 
            (setf candidates (append candidates (list i)))	           
	        (if(> conf max-conflict) (progn(setf max-conflict conf) (setf candidates (list i))))))	           	      
         
     (if (eq max-conflict 0) (progn (format t "*******Moves to solve ~d-Queens:~d~&" n steps)  (setf j (+ maxSteps 1)) (return)))    
                   
     (setf worst-queen-position (random (length candidates)))
     (setf worst-queen (nth worst-queen-position candidates))                                           
     (setf candidates '())  
          
     (dotimes (i n)      
        (setf conf (conflict-counter queens  i worst-queen))       
        (cond ((= conf min-conflict) (setf candidates (append candidates (list i))) )
              ((< conf min-conflict) (progn (setf min-conflict conf) (setf candidates (list i)))))) 
                
     (if (not(null candidates)) (setf (aref queens worst-queen) (nth (random (length candidates)) candidates)))
     (incf steps)     
     (if (> steps maxsteps)  
        (progn 
          (format t "NO goods answer found!!!")
          (return)))))
     
  (format t "End:~&~d~&" queens)
  
  (dotimes (i n)
     (dotimes (k n)
        (if (= (aref queens k) i) 
            (format t "Q") 
            (format t ".")))           
     (format t "~&")))

(defun conflict-counter (queens row column &aux (conflict-count 0))  
   
   (dotimes (i (length queens))  
      (if (not(equal  i  column))        
          (if (or (= row (aref queens i)) (= (abs (- row (aref queens i))) (abs (- column i)))) 
              (incf conflict-count))))
  
  (setf conflict-count conflict-count))


;;Some Test Result
(n-queens 4 10) 
  



