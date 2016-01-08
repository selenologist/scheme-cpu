
(define *register-names* '(A B C D E F G H
                           PC SP))

(define *default-registers* (map
                              (lambda (name)
                                (cons name 0))
                              *register-names*))

(define *default-memory* (make-vector 1024 0))

(define (update-register registers which value)
    (map (lambda (register)
     (if (eq? (car register) which)
       (cons which value)
       register))
   registers))

(define (op-reg-reg operator)
  (lambda (registers A B dest)
           (update-register registers dest (operator (cdr (assq A registers))
                                                      (cdr (assq B registers))))))

(define (op-reg-imm operator)
  (lambda (registers A imm dest)
           (update-register registers dest (operator (cdr (assq A registers))
                                                      imm))))

(define add-reg-reg (op-reg-reg +))
(define sub-reg-reg (op-reg-reg -))
(define mul-reg-reg (op-reg-reg *))
(define div-reg-reg (op-reg-reg /))

(define add-reg-imm (op-reg-imm +))
(define sub-reg-imm (op-reg-imm -))
(define mul-reg-imm (op-reg-imm *))
(define div-reg-imm (op-reg-imm /))

(define (eval-cpu instruction registers memory)
  (case (car instruction)
    ((add sub mul div)
        (cons
            (cond ; there's probably a better way
                ((number? (caddr instruction))
                    (case (car instruction)
                      ((add) (add-reg-imm registers (cadr instruction) (caddr instruction) (cadddr instruction)))
                      ((sub) (sub-reg-imm registers (cadr instruction) (caddr instruction) (cadddr instruction)))
                      ((mul) (mul-reg-imm registers (cadr instruction) (caddr instruction) (cadddr instruction)))
                      ((div) (div-reg-imm registers (cadr instruction) (caddr instruction) (cadddr instruction)))))
                ((symbol? (caddr instruction))
                    (case (car instruction)
                      ((add) (add-reg-reg registers (cadr instruction) (caddr instruction) (cadddr instruction)))
                      ((sub) (sub-reg-reg registers (cadr instruction) (caddr instruction) (cadddr instruction)))
                      ((mul) (mul-reg-reg registers (cadr instruction) (caddr instruction) (cadddr instruction)))
                      ((div) (div-reg-reg registers (cadr instruction) (caddr instruction) (cadddr instruction))))))
          memory))
    ((load)
        (if (list? (cadr instruction))
            (let ((ptr
                    (cond
                      ((number? (caadr instruction))
                        (caadr instruction))
                      ((symbol? (caadr instruction))
                       ((cadr (assq (caadr instruction) registers)))))))
                (cons (update-register registers (caddr instruction) (vector-ref memory ptr))
                       memory))
            (let ((val
                    (cond
                      ((number? (caddr instruction))
                       (caddr instruction))
                      ((symbol? (caaddr instruction))
                       ((cadr (assq (caddr instruction) registers)))))))
                (cons (update-register registers (cadr instruction) val)
                      memory))))
    ((stor)
        (let ((ptr
                (cond
                  ((number? (caaddr instruction))
                    (caaddr instruction))
                  ((symbol? (caaddr instruction))
                   ((cadr (assq (caddr instruction) registers)))))))
          (cons registers
                (begin (vector-set! memory ptr (cdr (assq (cadr instruction) registers)))
                       memory))))))

(define (print-registers registers)
  (map (lambda (register)
         (print (car register) ":\t" (cdr register) "\n"))
       registers))

(define (exec-instructions state instructions)
  (if (null? instructions)
    (begin (print-registers (car state))
           state)
    (exec-instructions (eval-cpu (car instructions) (car state) (cdr state)) (cdr instructions))))

; test:
;> (define x (cons (update-register *default-registers* '() '()) (vector-copy *default-memory*)))
; > (define x (exec-instructions x '((load A 4) (add A 4 B) (mul A B C) (stor C (0)) (load (0) D))))
; A:      4
; B:      8
; C:      32
; D:      32
; E:      0
; F:      0
; G:      0
; H:      0
; PC:     0
; SP:     0
  
