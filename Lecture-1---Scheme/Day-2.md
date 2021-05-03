- Day 2
    - SICP 1.1
        - Computer Programs are essentially models.
        - You can prove the effeciency of these models with predicate calculus, but as they get bigger it gets much harder to prove.
        - This is why you make sure standard program structures for which you know the effeciency.
        - What are the three mechanisms in all powerful languages:: Primitive expressions, Means of combination and means of abstraction 
        - What is the operator and operands:: operands are the data, operator is the function you manipulate them 
        - What type of notation does scheme use:: It uses prefix notation becasue the operator is before the operand
        - What is tree accumulation?:: It is percolate values upwards from a tree to get to the final output
        - What is a special form?:: It is an form that does not adhere to the evulation rules. 
        - What are the two types of orders in evaluating a model of code?:: Applicative order and normal order; applicative order starts from the most primitive parts and then expands outwards. The normal order is starting at the operand and only doing the calculations when necessary. 
        - In scheme, what is cond:: it is basically something in the form `  (cond ((> x 0) x)            ((= x 0) 0)           ((< x 0) (- x))))   ` 
        - What is a predicate and what is it followed by?:: An operator whcih creates an expression whose value is either true or false It is followed by a consequent expression. 
        - What is the structure of an if statement in scheme?:: Basically (if (predicate) (if true expression) if false expression) AKA (if ⟨predicate⟩ ⟨consequent⟩ ⟨alternative⟩)  
        - How do you compound predicates?:: you use logical composition operations such as and, or and not
        - Why is define a special form?:: because it links the value of an expression to a new operator and therefore doesn't follow evaluation rules. 
        - Why are and and or special forms?:: because they do not all need to be evaluated, ie the moment one is true it returns true and the moment one is false in and it returns false
        - Exercise 1.1
            - 10
            - 12
            - 8
            - 3
            - 6
            - a
            - b
            - 17 WRONG (silly mistake, it's 19) simple addition error lol
            - false
            - 4
            - 16
            - 6
            - 16
        - Exercise 1.2
            - (/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))(* 3 (- 6 2) (- 2 7)))
        - Exercise 1.3
            - ```scheme
(define (square a)
	(* a a))
(define (lsos a b c)
	(+ 
		(if (or (> a b) (> a c)) (square a) 0)  
		(if (or (> b a) (> b c)) (square b) 0)
		(if (or (> c a) (> c b)) (square c) 0)
		))
```
        - Exercise 1.4
            - if b is positive then you add a and b; otherwise you subtract b from a
        - Exercise 1.5
            - WRONG! I couldn't figure this one out but if I thought about it more maybe I could have.
        - What is the difference between declarative and imperative knowledge?:: Declerative knowledge is what-is knowledge (maths); imperitive knowledge is how-to. 
        - What do predicate names end with conventionally in scheme?:: question marks (?)
        - Exercise 1.6
            - In this case there should be no difference because there is nothing to be evaluated that would break the program and also cond is a special form that would skip one of the case and not follow the evaluation rule. WRONG! - basically because the thing is recursive so it would keep trying to evaluate itself but the question I had is that isn't cond a special form.
            - So why would wrapping it in a function make it loose it's special form abilities?
            - Just know that wrapping an if else thing in a function then calling it basically makes it loose its special form ability
        - Exercise 1.7
            - Well it wouldn't work for really small numbers because good enough uses 0.001 of a difference to test whether the function is working or not working
            - It wouldn't work well for large numbers because it would take too long if the first guess is 1 and it would take foreve to reach something where good enough works. Also because in real computers the numbers aren't infinitely precise, it would mean that it may be impossible to even get a difference of 0.001 if the number is so large that at the point it doesn't store the value of the number
            - ```scheme
 (define (good-enough? guess x) 
   (< (abs (/ (- (improve guess x) guess) guess)) 0.001))
```
        - Exercise 1.8
            - ```scheme
;this is a program that can calculate the cube root of a program
(define (square x) 
   (* x x)) 

(define (cbrt-iter guess x)
	(if (good-enough? guess x) 
		guess 
		(cbrt-iter (improve guess x) x)))

(define (improve guess x)
	(/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess x) 
   (< (abs (/ (- (improve guess x) guess) guess)) 0.001))
   
(define (3root x) 
   (cbrt-iter 1.0 x)) 
   
```
        - What is a recursive function?:: A function that calls on itself
        - 
    - 
    - Week 1 lab continued
        - ```scheme
(define nsent '())
(define (duples-removed sent)
  (cond (= (count sent) 0) nsent)
  (cond (member?(first sent)(bf sent))(duples-removed (bf sent)))
  (cond (not (member?(first sent)(bf sent)))(define nsent (se (first sent) nsent)))duples-removed (bf sent))  
 )
 
 ;This solution won't work because you cant change the first variable the correct answer is shown below
 (define (dupls-removed sent)
  (cond ((empty? sent) '())
	((member? (first sent) (bf sent))
	 (dupls-removed (bf sent)))
	(else (sentence (first sent) (dupls-removed (bf sent))))))
```
    - 
    - Homeowork
        - Question was exercise 1.6 which I have done
        - 
        - Question 2
        - ```scheme
(define (square x) (* x x ))
(define (squares sent) 
	(cond ((empty? sent) '())
	(else (se (square (first sent)) (squares (bf sent))))))
	
```
        - 
        - Question 3
        - ```scheme
;this is the code I wrote I will continue it on a new page tom. 
(define (switch sent)
;function word switch 
;recursive algo

    )
```
        - I am ending it here for today :)
    - 
    - 
    - 
    - 
