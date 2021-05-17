- Day 3
    - Homework
        - Quesiton 3
        - ```scheme
(define (switch sent)
	(define (ws wd) 
		(cond 
			((or (equal? wd 'I) (equal? wd 'me)) 'you)
			((equal? wd 'you) 'me)
			(else wd)		
		)
    )
    (define (mp sent)
     	(cond  
     		((empty? sent) '())
    		(else (se (ws (first sent)) (mp (bf sent))))
    	)
     )
     (if 
     	(equal? (first (mp sent)) 'me)
     	(se 'I (bf (mp sent)))
     	(mp sent)
     )
         
)    
    

```
        - Question 4
            - ```scheme
(define (ordered? sent)
	(cond 
		((empty? (bf sent)) #t)
		((> (first sent) (first (bf sent)) ) #f)
		(else (ordered? (bf sent)))			
	)
)
```
        - Question 5
            - ```scheme
(define (ends-e sent)
	(cond 
		((empty? sent) '())
		((not (equal? (last (first sent)) 'e)) (ends-e (bf sent)))
		(else (sentence (first sent) (ends-e (bf sent))))
	)
)
```
        - Question 6
            - To test if or is a special case just have one of the things evaluate to an error
            - benefits - efficiency; having conditions that would result in error in some cases avoided
            - disadvanteges - if you don't realise that one part of your code is not evaluated and it doesn't spit out an error when you actually need that condition it may cause a problem and return false or break; behaviour is unpredictable
    - 
    - Lecture
        - Why is functional programming important?:: THis is because of parallel computing, essentially functions always give the same output based on the input and aren't dependant on something else so when we can run multiple functions at the same time and get shit to go faster now with parallelism 
        - What it applicative order?:: Evaluates sub expressions first and then goes outward
        - What is normal order?:: Evaluates everything in terms and keeps primitive expressions there untill the end. Basically keeps everything in variables
        - What's a situation in which random order and applicative order are different?:: When the procedure you are evaluating is nota function for example you use a random ![](https://remnote-user-data.s3.amazonaws.com/Mu8nHCPlos-K9CdSxqrgDbbuwReHQpVK6NmyEifjzpuGatCr6kaZktAcwVBD3Uo8kH6gAo40bVvMQfoLGml7-_s3uJO6LhUlRKJvL3Qo0vPurSiakVdqcv-CqVDJsLrG.png) 
        - 
        - What is a predicate?:: A predicate is a function in which it's range is true or false. Range is booleans.
    - 
    - 
    - 
