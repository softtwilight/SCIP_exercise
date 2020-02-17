#lang sicp
;; expression themselves has no meaning, unless the in the environment which they
;; are evaluated. 最简单的expression(+ 1 1)也需要在理解‘+’含义的context之中被理解。

;; 3.21 the rule for evaluation;
;; 
;; procedure = λ-expression + environment pointer
;; define creates definitions by adding bindings to frames.
;; procedure的定义是在global的environment中添加了一个函数名和λ表达式的绑定。
;; procedure的执行则是在指向的环境中创建一个新的env，在其中包含了参数和参数值的绑定。

(define square
  (lambda (x) (* x x)))

;; 3.22 Applying Simple Procedures.

;; exercise 3.9
;; recursive version creates n environments, whose values are from n to 1. apply the f function.
;; iterative version creates  n + 1 environments, 1 is the factorial' s env which n = some value;
;; the n is the fact-iter's env which has three parameters.

;; 3.23 Frames as the Repository of Local State.
;; procedure 定义在global 但是他指向的env 可以不是global

;; exercise 3.10
;; let is syntactic sugar for a procedure call;
;; (let ((⟨var⟩ ⟨exp⟩)) ⟨body⟩)   ->  ((lambda (⟨var⟩) ⟨body⟩) ⟨exp⟩)
;; new version has a more env, which is initial-value = 100, then balance = initial-value = 100,
;; the balance = 100 is the inner lambda's enverionment.

;; 3.24 Internal Definitions.
;; inner-procedure is binding in the environment where the value are assaignated to outer
;; procedure's varialbe. 即 local procedure will be bound in the frame that the proceducre
;; creates when it is run, not define in the global environment.
;; local procedures can assess the arguments of the enclosing procedure.

;; exercise 3.11
;; the local state is kept in the E1, which created when make-account is called.
;; new account has a new environment E2 created separately. global env are shared by acc and acc2