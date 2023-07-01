# Racket-Interpreter
This is an ınterpreter for an extension of Racket language.
The main functionality of this language is that you can set values to variables and use them in
expressions in a procedural fashion. Variables that are bound to values are hold in a state (a hash),
and the state changes at each iteration of the program. Below are some examples from the language:
![image](https://github.com/batuhansolmaz/Racket-Interpreter/assets/108425372/a00e1632-59fe-45f4-b992-fc90aafca696)


![image](https://github.com/batuhansolmaz/Racket-Interpreter/assets/108425372/5d4f765a-f42a-4363-8382-94bccc538764)

The state of the program
The state is a hash consisting of variable names and their values. The values can be numbers,
strings, booleans, Racket functions, or functions dened in this language. The state is updated
at each evaluation of an expression. There is a special variable named -r that holds the result of
the last expression. For example, for the rst 9 expressions in the rst example above, the state is
updated as follows:

#hash()

#hash((-r . 5) (a . 5))

#hash((-r . (2 3)) (a . 5) (b . (2 3)))

#hash((-r . 5) (a . 5) (b . (2 3)) (c . 5))

#hash((-r . 12) (a . 5) (b . (2 3)) (c . 5) (d . 12))

#hash((-r . 24) (a . 5) (b . (2 3)) (c . 5) (d . 12) (e . 24))

#hash((-r . 3) (a . 5) (b . (2 3)) (c . 5) (d . 12) (e . 24) (f . 3))

#hash((-r . 1/8) (a . 5) (b . (2 3)) (c . 5) (d . 12) (e . 24) (f . 1/8))

#hash((-r . 3.14) (a . 5) (b . (2 3)) (c . 5) (d . 12) (e . 24) (f . 1/8)
(pi-num . 3.14))

#hash((-r . #<procedure:...>) (a . 5) (area . #<procedure:...>) (b . (2 3))
(c . 5) (d . 12) (e . 24) (f . 1/8) (pi-num . 3.14))
In this way, the result of the last computation can be always accessed by the variable -r (useful for
test expressions, function results).
