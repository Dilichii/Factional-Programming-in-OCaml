(*A.1*)
(*
1. int = 10
2. float = 10.
3. int = 12
4. Error: This expression has type float but an expression was expected of type int
   This error occured because to add floats you need to use +.
5. This expression has type int but an expression was expected of type float
    This error occured because to add ints you need to use + not +. which is for floats\
6. Error: This expression has type float but an expression was expected of type int
    This error occured because we tried to add together an int and a float
7. Error: This expression has type int but an expression was expected of type float
    This error occured because we tried to add together an int and a float
8. float = 7.2
9. int = 5
10. int = 7
11. val a : int = 3
12. val b : int = 4
13. bool = false
14. bool = true
15. bool = false This is different because we used a different equality operator 
      that checks if the two arrays are the same in memory, which they are not
16. (int * int * int) list = [(1, 2, 3)]
17. (int * int * int) list = [(1, 2, 3)] Both this expression and the above expression 
      result in a single member list of a three member tuple. In this case thought the input given resembled 
      as if we wanted a three member list. This happened because we didn't use semi-colons
18. int = 4
19. Syntax error    
      This is due to us using and instead of &&
20. int = 6
21. int = 4 
      This is different because the computer reads it as then (a+2) and so it was never evaluted 
      because the it was read within the if statement not as a direction to evaluate the if statement then 
      add 2 to the result
22. int = 6
23. Error: This expression has type int but an expression was expected of type unit because it 
      is in the result of a conditional with no else branch
    This error is because since they didnt add an else so the computer fills it as else () which is the incorrect 
      type as the then
*)

(*A.2*)

let sum_of_squares_of_two_largest x y z = 
if x > y && y > z then 
  x*x + y*y
else (
  if x > y && z > y then 
    x*x + z*z
  else (
    if z > x then
      y*y + z*z
    else (
      y*y + x*x
    )
  )
)

(*A.3*)
(*
  The function when run would view initially how the statement had three parts to it. Theres an if statement that resolves as an operator 
  then theres two numbers after so it knows it has to run the resulting operator on them after the if statement is evaluated 

  Thus if b is non negative a and be will be added to each other and if b is negative a and be would be subtracted from each other.
  Which PS. means -x and x would have the same result
*)

(*B.1*)
(* Using applicative order evaluation you would expect to have the test statement return an error because it would evalute the individual 
   subexpressions before the if statment. Thus applicative would evalute the p () and would create an endless loop. On the other hand 
   normal order would read the statement and evalute as it read thus it would evalute how the given 0 is equal to 0 and return 0 and never 
   get to evaluating the p ()
*)

(*B.2*)
(* When Alyssa trys to use this code it will result in a stack overflow error because the if statement's subexpression get evaluated before 
   Ocaml actually checks if the second expression needs to be run. Thus even if the user enters 3 and 9 which is correct or 4 and 5 which isn't
   correct Ocaml continues to run sqrt_iter before it every evalutes the new_if function itself resulting in an endless recursive loop.
*)
 
(*B.3*)
(*
1. Add_a is a recursive function that genereates a recurisive process because as the recursive function is called a pending equation is stored
    to the stack that only collapses and gets evaluated when the recusrion reaches its base case. The recursive funtion Add_b uses an iterative process
    each call of its recursion iterates by 1 and the return the finally value rather than any later computing.


2.

let rec add_a a b =
  if a = 0
     then b
     else inc (add_a (dec a) b)

Desugar this to:

let rec add_a =
  fun a b ->
    if a = 0
       then b
       else inc (add_a (dec a) b)

Bind the name "add_a" to the value:

  fun a b ->
    if a = 0
       then b
       else inc (add_a (dec a) b)

Evaluate (add_a 2 5)
  evaluate 2 -> 2
  evaluate 5 -> 5
  evaluate add_a -> desugar to: (fun a b -> if...)
  apply (fun a b -> if ...) to 2, 5
  substitute 2 for a, 5 for b in fun (if ...)
    -> if 2 = 0 then 5 else inc (add_a (dec 2) 5)
  evaluate (if 2 = 0 then 5 else inc (add_a (dec 2) 5))
    if is a special form, so evaluate the first operand:
      evaluate (2 = 0)
        evaluate 2 -> 2
        evaluate 0 -> 0
        evaluate = -> =
        apply = to 2, 0 -> false
    first argument of if is false, so evaluate the third operand:
      evaluate inc (add_a (dec 2) 5) 
        evaluate (dec 2)
          evaluate 2 -> 2
          evaluate dec -> dec
          apply dec to 2 -> 1
        evaluate 5 -> 5
        evaluate add_a -> desugar to: (fun a b -> if...)
        apply (fun a b -> if ...) to 1, 5
        substitute 1 for a, 5 for b in fun (if ...)
          -> if 1 = 0 then 5 else inc (add_a (dec 1) 5)
        evaluate (if 1 = 0 then 5 else inc (add_a (dec 1) 5))
          if is a special form, so evaluate the first operand:
            evaluate (1 = 0)
              evaluate 1 -> 1
              evaluate 0 -> 0
              evaluate = -> =
              apply = to 1, 0 -> false
          first argument of if is false, so evaluate the third operand:
            evaluate inc (add_a (dec 1) 5) 
              evaluate (dec 1)
                evaluate 1 -> 1
                evaluate dec -> dec
                apply dec to 1 -> 0
              evaluate 5 -> 5
              evaluate add_a -> desugar to: (fun a b -> if...)
              apply (fun a b -> if ...) to 0, 5
              substitute 0 for a, 5 for b in fun (if ...)
                -> if 0 = 0 then 5 else inc (add_a (dec 0) 5)
              evaluate (if 0 = 0 then 5 else inc (add_a (dec 0) 5))
                if is a special form, so evaluate the first operand:
                  evaluate (0 = 0)
                    evaluate 0 -> 0
                    evaluate 0 -> 0
                    evaluate = -> =
                    apply = to 0, 0 -> true
                  first argument of if is true, so evaluate the second operand:
                  evaluate b
                    evaluate 5 -> 5
              evaluate inc (5)
                evaluate 5 -> 5
                evaluate dec -> dec
                apply inc to 5 -> 6
        evaluate inc (6)
          evalute 6 -> 6
          evaluate inc -> inc
          apply inc to 6 -> 7
    result: 7
      
3.

let rec add_b a b =
  if a = 0
     then b
     else add_b (dec a) (inc b)

Desugar this to:

let rec add_b =
  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Bind the name "add_b" to the value:

  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Evaluate (add_b 2 5)
  >>> evaluate 2 -> 2
  >>> evaluate 5 -> 5
  >>> evaluate add_b -> desugar to: (fun a b -> if...)
  apply (fun a b -> if ...) to 2, 5
  substitute 2 for a, 5 for b in fun (if ...)
    -> if 2 = 0 then 5 else add_b (dec 2) (inc 5)
  evaluate (if 2 = 0 then 5 else add_b (dec 2) (inc 5))
    if is a special form, so evaluate the first operand:
      evaluate (2 = 0)
        >>> evaluate 2 -> 2
        >>> evaluate 0 -> 0
        >>> evaluate = -> =
        apply = to 2, 0 -> false
    first argument of if is false, so evaluate the third operand:
      evaluate (add_b (dec 2) (inc 5))
        evaluate (dec 2)
          >>> evaluate 2 -> 2
          >>> evaluate dec -> dec
          apply dec to 2 -> 1
        evaluate (inc 5)
          >>> evaluate 5 -> 5
          >>> evaluate inc -> inc
          apply inc to 5 -> 6
        >>> evaluate add_b -> desugar to: (fun a b -> if...)
        apply (fun a b -> if ...) to 1, 6
        substitute 1 for a, 6 for b in (if ...)
          -> if 1 = 0 then 6 else add_b (dec 1) (inc 6)
        evaluate (if 1 = 0 then 6 else add_b (dec 1) (inc 6))
          if is a special form, so evaluate the first operand:
            evaluate (1 = 0)
              >>> evaluate 1 -> 1
              >>> evaluate 0 -> 0
              >>> evaluate = -> =
              apply = to 1, 0 -> false
          first argument of if is false, so evaluate the third operand:
            evaluate (add_b (dec 1) (inc 6))
              evaluate (dec 1)
                >>> evaluate 1 -> 1
                >>> evaluate dec -> dec
                apply dec to 1 -> 0
              evaluate (inc 6)
                >>> evaluate 6 -> 6
                >>> evaluate inc -> inc
                apply inc to 6 -> 7
              >>> evaluate add_b -> desugar to: (fun a b -> if...)
              apply (fun a b -> if ...) to 0, 7
              substitute 0 for a, 7 for b in (if ...)
                -> if 0 = 0 then 7 else add_b (dec 0) (inc 7)
              evaluate (if 0 = 0 then 7 else add_b (dec 0) (inc 7))
                if is a special form, so evaluate the first operand:
                  evaluate (0 = 0)
                    >>> evaluate 0 -> 0
                    >>> evaluate 0 -> 0
                    >>> evaluate = -> =
                    apply = to 0, 0 -> true
                first argument of if is true, so evaluate the second operand:
                  >>> evaluate b
                    >>> evaluate b -> b
                  result: 7
*)
   


(*C.1*)
(* This function computes the factorial of the input number,
    which for a number n is equal to n * (n-1) * ... * 1. *)
let rec factorial n =
  if n = 0 then 1 else n * factorial (n - 1)
(*
   


*)


let e_term x = 
  1. /. float_of_int((factorial x))

let rec e_approximation x = 
  if x = 0 then 
    0.
  else e_term x +. (e_approximation (x - 1))

(*C.1.c*)
(* 
   True value of e^1 : 2.71828182846
   Value I recieved running e_approximation 20: 1.71828182845904553
*)

(*C.1.d*)
(* If you try to compute the sum to the 100th term the result given is float = infinity.
    The reason is because there occurs a floating point overflow due to how floats can only 
    be percise up to a certain space length before Ocaml sets its value to infinity.
*)

(*C.2*)
let rec is_even a = 
  if a = 0 then
    true
  else is_odd (a - 1)

and is_odd b = 
  if b = 0 then
    false
  else is_even (b - 1)

(*C.3*)
let rec f_rec n = 
  if n < 3 then
    n
  else f_rec(n - 1) + 2 * f_rec(n - 2) + 3 * f_rec(n - 3) 

let rec f_iter n = 
  if n < 3 then
    n
  else 
    let rec f_help d1 d2 d3 curr =
      if curr = n + 1 then
        d1
      else f_help (d1 + 2 * d2 + 3 * d3) d1 d2 (curr + 1)
    in f_help 2 1 0 3

let rec pascal_coefficient r c =
  match c with
    | _ when (c > r || r < 1) -> failwith "invalid arguments"
    | 1 -> 1
    | _ when (r = c || r = 2) -> 1
    | c -> pascal_coefficient (r - 1) (c - 1) + pascal_coefficient (r - 1) c