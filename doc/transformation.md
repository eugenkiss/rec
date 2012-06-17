Transformation of Goto to Recursion
===================================

TODO

Goto to functinoal language see lambda papers by steele and sussman
or <http://stackoverflow.com/questions/6606240/translating-imperative-to-functional-code/6615362#6615362>

Is rather straightforward.

My way
------

~~~~~~
M1: x0 := x2 + 1;
M2: IF x1 = 0 THEN GOTO M6 END;
M3: x0 := x0 * x1;
M4: x1 := x1 - 1;
M5: GOTO M1;
M6: HALT
~~~~~~

~~~~~~
main x0 x1 x2 = M1 x0 x1 x2;

M1 x0 x1 x2 = M2 (x2 + 1) x1 x2;
M2 x0 x1 x2 = if x1 == 0 (M6 x0 x1 x2) (M3 x0 x1 x2);
M3 x0 x1 x2 = M4 (x0 * x1) x1 x2;
M4 x0 x1 x2 = M5 x0 (x1 - 1) x2;
M5 x0 x1 x2 = M1 x0 x1 x2;
M6 x0 x1 x2 = x0
~~~~~~



Transformation of Recursion to Goto
===================================

*TODO: Es gibt noch (min.) einen Fehler in der Ueberlegung. Z.B. funktioniert
`fac(x) = IF x = 0 THEN 1 ELSE fac(x-1) * x; main(x) = fac(x)`, aber nicht
`fac(x) = IF x = 0 THEN 1 ELSE x * fac(x-1); main(x) = fac(x)`. Ich muss 
irgendwie ein Activation Record benutzen um den Rueckgabewert einer Funktion
zu speichern wie hier <http://www.ee.ryerson.ca/~courses/coe808/Truman/Lecture14.pdf>. 
Vielleicht ein dritter Stack?*

Before the general algorithm for the translation of Recursion to Goto is shown
the train of thought of arriving at this algorithm shall be presented by the
following examples.


Factorial Transformation
------------------------

The ideal Goto program that computes the factorial function would look
something like this:
  
~~~~~~
    x0 := 1;
M1: IF x1 = 0 THEN GOTO M2 END;
    x0 := x0 * x1;
    x1 := x1 - 1;
    GOTO M1;
M2: HALT
~~~~~~

It would be great if a compiler that translates Recursion programs into Goto
programs could achieve such an optimal result. But this is not the aim of this
project. It shall only be shown that a Recursion program can indeed be
translated into an equivalent Goto program regardless of how slow/big the
resulting Goto program becomes. So here, a translation of a Recursion program
that computes the factorial function[^non-rec] into an equivalent but possibly
big and slow Goto program shall be presented. Note, that it is assumed that the
Recursion program is in first-order form. This is not an unreasonable
assumption since every well-formed  higher-order Recursion program can be
transformed into an equivalent first-order Recursion program using
a whole-program transformation called *Lambda Lifting* that is explained in
a later section.

  [^non-rec]: in an explicitly non-tail-recursive way so that a stack is
              mandatory.

*TODO: How does one arrive at the idea of using a stack? (somehow parameters
must be passed from one function to another...)*

*TODO: Stack based virtual machine vs. Register based virtual machine (as alternative).
Goto is some kind of counter machine?*

The generated Goto program uses two stacks. One stack is used for the arguments
of a function and the other one is used for keeping track of return
adresses[^control-stack]. It would have been possible to get by with only one
stack as is often the case on real hardware but then the management of the
stack would have become a bit more complicated[^stack-frame]. From this
examplary translation a general algorithm for the translation of any
first-order Recursion program into an equivalent Goto program can be derived.

  [^control-stack]: 
      TODO: It seems this approach is indeed sometimes taken as suggested by
      <http://www.cp.eng.chula.ac.th/~piak/teaching/ca/stack.htm>. The return
      address stack is supposedly called "control stack"
      Or even better here: <http://www.ece.cmu.edu/~koopman/stack_computers/contents.html>
  [^stack-frame]: 
      TODO: Stack-frame / activation record, frame pointer etc. see e.g.
      <http://www.cp.eng.chula.ac.th/~piak/teaching/ca/stack.htm>

The following exemplary Recursion program is going to be the subject of 
translation:
    
~~~~~~
fac n =
    IF n = 0
       THEN 1
       ELSE n * (fac (n - 1));

main n = fac n 
~~~~~~

The use of the infix `*` function is only syntactic sugar. To make the example
easier the program is desugared[^desugar]:

  [^desugar]: The `*` now represents a primitive multiplication of two numbers.

~~~~~~
mul a b = a * b;

fac n =
    IF n = 0
       THEN 1
       ELSE mul n (fac (n - 1));

main n = fac n 
~~~~~~

On the assumption that it is known how many arguments `main` receives[^args] it
is conceivable that a general algorithm exists which translates the above
Recursion program into something very similar to the following Goto program
augmented with a `PUSH`, `PUSHA`, `POP`, `GET`, `CALL` and `RETURN`
instruction: 

  [^args]: E.g. by using type-checking or by simply syntactically counting 
           `main`'s arguments

~~~~~~
      PUSH x1;
      GOTO main;

main: x0 := x0 + 0;
      PUSHA 1;
      CALL fac;
      x0 := POP;
      HALT;

fac:  x0 := x0 + 0; 
      a1 := GET fp-1;
      IF a1 = 0 
         THEN PUSH 1
         ELSE PUSH 1; 
              PUSHA 1;
              CALL sub;
              CALL fac;
              PUSHA 1;
              CALL mul
      END;
      RETURN 1;

mul:  x0 := x0 + 0;
      a1 := GET fp-1;
      a2 := GET fp-2;
      PUSH a1 * a2;
      RETURN 1;
~~~~~~

`PUSH x` pushes the value of `x` onto the argument stack whereas `x := POP`
pops a value from the argument stack and saves it into `x`. `x := GET i` gets
the value at the `i`-th position of the stack without removing it and saves the
value into `x`. `PUSHA i` pushes the `i`-th argument of the currently active
function onto the stack. `CALL f` pushes the return adress of the calling
function[^more-prec1] onto the return adress stack, sets the frame pointer `fp`
to the value of the stack pointer `sp` and jumps to the specified function
`f`[^more-prec2]. `RETURN i` saves the topmost argument temporarily, removes
the `i` current arguments by setting the stack pointer accordignly and repushes
the return value. Furthermore, it pops the topmost return adress from the
return adress stack and jumps to that return adress. 

  [^more-prec1]: More precisely: The return adress of the point in the
                 caller's code where the call is made.
  [^more-prec2]: More precisely: It is simply a jump to a label denoting the
                 function.

At the very beginning the values saved in `x1,...,xn`, where `n` corresponds to
the number of arguments of the `main` function, are pushed onto the argument
stack.

After that, each function is mapped to a segment of the Goto program which is
preceded with a label corresponding to the function's name. A more or less
direct translation of the respective function's body from the Recursion program
follows. If there are any calls to other functions[^rec-call] the arguments for
the callee are pushed onto the argument stack in reverse
order[^polish-notation] and the function is `CALL`ed. The very last instruction
of each segment, except for the `main` segment, is a `RETURN` which resumes
control to the caller. 

  [^rec-call]: including recursive calls - that's no problem.
  [^polish-notation]: 
      In other words: Imagine function calls in a Recursion program to be in
      reverse polish notation.

Special attention should be paid to the `main` segment as the last instruction
is not a `RETURN`. Instead, the topmost argument from the argument stack is
popped off, saved into the output register `x0` and the program is stopped. Put
another way: the return value of the program is the topmost argument of the
argument stack after execution stops.

Now, how are the `PUSH`, `PUSHA`, `POP`, `GET`, `CALL` and `RETURN`
instructions transformed so that the resulting program is an extended, plain
Goto program? As already mentioned several times, there have to be two stacks
and they must somehow be represented in a Goto program. One idea would be to
choose a register to represent a stack and using complicated decoding and
encoding rules since register can only contain natural numbers. Another idea
- which is used here - is to encode the stack data structure in the source 
code. 

Suppose that the following Goto program $P_2$ is attached at the end of another
Goto program $P_1$:

~~~~~~
set: x0 := x0 + 0;
IF si = 1 THEN si1 := a END;
IF si = 2 THEN si2 := a END;
...
IF si = n THEN sin := a END;
IF si > n THEN HALT END; // Stackoverflow!
GOTO return;

get: x0 := x0 + 0;
IF si = 1 THEN a := si1 END;
IF si = 2 THEN a := si2 END;
...
IF si = n THEN a := sin END;
GOTO return;

return: x0 := x0 + 0;
IF r = 1 THEN GOTO r1 END;
IF r = 2 THEN GOTO r2 END;
...
IF r = m THEN GOTO rm END;
~~~~~~

Then, in order to push something onto the stack in $P_1$, one would use
something like the following sequence of commands:

~~~~~~
    //PUSH x1;
    r := 1;
    sp := sp + 1;
    si := sp;
    a := x1;
    GOTO set;
r1: x0 := x0 + 0
~~~~~~

Likewise, to pop something from the stack:

~~~~~~
    //a1 := POP;
    r := 2;
    si := sp;
    GOTO get;
r2: x0 := x0 + 0;
    sp := sp - 1;
    a1 := a
~~~~~~

`GET`

~~~~~~
    //a1 := GET fp-1;
    r := 3;
    si := fp - 1;
    GOTO get;
r3: x0 := x0 + 0;
    a1 := a
~~~~~~

`PUSHA`

~~~~~~
    //PUSHA 1
    a := GET fp - 1;
    PUSH a
~~~~~~

A bit more information: `sp` denotes the stack pointer and `sp`$_i$, $1 \le
i \le n$, are the "memory cells" of the stack. `a` is an auxiliary register,
`r` is the register which is needed to know how to get back to the point in the
program where a `PUSH` or `POP` (or `CALL` or `RETURN`) command has been
executed. `r` could be called the "code pointer" or the return address
register. The size of the stack is a constant `n` whereas `m` is (only)
dependant on the number of `PUSH`, `POP`, `CALL` and `RETURN` instructions.

On the assumption that there is a separate stack for the return adresses `CALL`
and `RETURN` instructions can be transformed like this:

~~~~~~
    //CALL fac;
    r := 3;
    a := 4;
    GOTO pushret;
r3: x0 := x0 + 0;
    fp := sp + 1;
    GOTO fac;
r4: x0 := x0 + 0;

    //RETURN 1
    //save return value
    a := POP;
    //remove arguments (assumption: function has 1 parameter)
    sp := sp - 1;
    //repush return value
    PUSH a;
    //set frame pointer 
    fp := sp;
    //return adress stuff
    r := 5;
    GOTO popret;
r5: x0 := x0 + 0;
    r := a;
    GOTO return
~~~~~~

Of course, one has to look out for name clashes[^clash]. If e.g. the label
`return` is already used in $P_1$ one has to use another name for `return` in
$P_2$ like for example `returnreturn`.

  [^clash]: As these augmented Goto programs are not really written directly
            but only used as an intermediate language the only possible name
            clashes come from giving unfortunate names like "return" to a
            Recursion function.

*Back to the factorial example*: Here is the augmented Goto program transformed
into a plain, extended Goto program using the ideas above. There are two stacks
now and accordingly two stack pointers. `sp` is the argument stack pointer and
`rp` is the return adress stack pointer. The stack size is set to 5 to save
lines although this means that an argument bigger than 4 will cause
a stackoverflow[^stackoverflow]. There are some additional lines that deal with
error reporting that can be safely ignored. Everything else should be clear
from the explanations above:

  [^stackoverflow]: In case of a stackoverflow the program is simply stopped
                    with an arbitrary output as the "correct" output will most
                    probably not be produced anyway.

~~~~~~
      //PUSH x1;
      r := 1;
      a := x1;
      GOTO pusharg;
r1:   x0 := x0 + 0;
      GOTO main;

main: x0 := x0 + 0;

      //a1 := POP;
      r := 2;
      GOTO poparg;
r2:   x0 := x0 + 0;
      a1 := a;

      //PUSH a1;
      r := 3;
      a := a1;
      GOTO pusharg;
r3:   x0 := x0 + 0;

      //CALL fac;
      r := 4;
      a := 5;
      GOTO pushret;
r4:   x0 := x0 + 0;
      GOTO fac;
r5:   x0 := x0 + 0;

      //x0 := POP;
      r := 6;
      GOTO poparg;
r6:   x0 := x0 + 0;
      x0 := a;

      HALT;

fac:  x0 := x0 + 0;
      
      //a1 := POP;
      r := 7;
      GOTO poparg;
r7:   x0 := x0 + 0;
      a1 := a;

      IF a1 = 0
         THEN //PUSH 1;
              r := 8;
              a := 1;
              GOTO pusharg;
          r8: x0 := x0 + 0

         ELSE //PUSH a1;
              r := 9;
              a := a1;
              GOTO pusharg;
          r9: x0 := x0 + 0;

              //PUSH a1-1;
              r := 10;
              a := a1-1;
              GOTO pusharg;
          r10: x0 := x0 + 0;

              //CALL fac;
              r := 11;
              a := 12;
              GOTO pushret;
         r11: x0 := x0 + 0;
              GOTO fac;
         r12: x0 := x0 + 0;

              //CALL mul;
              r := 13;
              a := 14;
              GOTO pushret;
         r13: x0 := x0 + 0;
              GOTO mul;
         r14: x0 := x0 + 0

      END;

      //RETURN
      r := 15;
      GOTO popret;
r15:  x0 := x0 + 0;
      r := a;
      GOTO return;

mul:  x0 := x0 + 0;

      //a2 := POP;
      r := 16;
      GOTO poparg;
r16:  x0 := x0 + 0;
      a2 := a;

      //a1 := POP;
      r := 17;
      GOTO poparg;
r17:  x0 := x0 + 0;
      a1 := a;

      //PUSH a1 * a2;
      r := 18;
      a := a1 * a2;
      GOTO pusharg;
r18:  x0 := x0 + 0;

      //RETURN
      r := 19;
      GOTO popret;
r19:  x0 := x0 + 0;
      r := a;
      GOTO return;

// "P2" --------------------------------------

pusharg: x0 := x0 + 0;
sp := sp + 1; 
IF sp = 0 THEN x0 := 1234567890; HALT END; 
IF sp = 1 THEN sp1 := a END;
IF sp = 2 THEN sp2 := a END;
IF sp = 3 THEN sp3 := a END;
IF sp = 4 THEN sp4 := a END;
IF sp = 5 THEN sp5 := a END;
IF sp > 5 THEN x0 := 1234567891; HALT END; // Stackoverflow!
GOTO return;

poparg: x0 := x0 + 0;
IF sp = 0 THEN x0 := 1234567892; HALT END; 
IF sp = 1 THEN a := sp1 END;
IF sp = 2 THEN a := sp2 END;
IF sp = 3 THEN a := sp3 END;
IF sp = 4 THEN a := sp4 END;
IF sp = 5 THEN a := sp5 END;
IF sp > 5 THEN x0 := 1234567893; HALT END;
sp := sp - 1; 
GOTO return;

pushret: x0 := x0 + 0;
rp := rp + 1; 
IF rp = 0 THEN x0 := 1234567894; HALT END; 
IF rp = 1 THEN rp1 := a END;
IF rp = 2 THEN rp2 := a END;
IF rp = 3 THEN rp3 := a END;
IF rp = 4 THEN rp4 := a END;
IF rp = 5 THEN rp5 := a END;
IF rp > 5 THEN x0 := 1234567895; HALT END; // Stackoverflow!
GOTO return;

popret: x0 := x0 + 0;
IF rp = 0 THEN x0 := 1234567896; HALT END; 
IF rp = 1 THEN a := rp1 END;
IF rp = 2 THEN a := rp2 END;
IF rp = 3 THEN a := rp3 END;
IF rp = 4 THEN a := rp4 END;
IF rp = 5 THEN a := rp5 END;
IF rp > 5 THEN x0 := 1234567897; HALT END;
rp := rp - 1; 
GOTO return;

return: x0 := x0 + 0;
IF r = 0 THEN x0 := 1234567898; HALT END; 
IF r = 1 THEN GOTO r1 END;
IF r = 2 THEN GOTO r2 END;
IF r = 3 THEN GOTO r3 END;
IF r = 4 THEN GOTO r4 END;
IF r = 5 THEN GOTO r5 END;
IF r = 6 THEN GOTO r6 END;
IF r = 7 THEN GOTO r7 END;
IF r = 8 THEN GOTO r8 END;
IF r = 9 THEN GOTO r9 END;
IF r = 10 THEN GOTO r10 END;
IF r = 11 THEN GOTO r11 END;
IF r = 12 THEN GOTO r12 END;
IF r = 13 THEN GOTO r13 END;
IF r = 14 THEN GOTO r14 END;
IF r = 15 THEN GOTO r15 END;
IF r = 16 THEN GOTO r16 END;
IF r = 17 THEN GOTO r17 END;
IF r = 18 THEN GOTO r18 END;
IF r = 19 THEN GOTO r19 END;
IF r > 19 THEN x0 := 1234567899; HALT END; 

HALT
~~~~~~

*TODO: Graphical depiction of stack transformations and values (states) 
of `sp`, `rp` `spi`, `rpi` and `r` for execution with parameter e.g. 3*


Possible Optimisations
----------------------

### Tail-recursion

This program

~~~~~~
fac accu n =
    IF n = 1
       THEN accu
       ELSE fac (acc * n) (n - 1);

main n = fac 1 n;
~~~~~~

can be executed with a constant stack size for (ideally) arbitrary big
arguments as the stack does not need to grow during recursion. That is because
`fac` is tail-recursive here. An optimising compiler could detect this special
case and produce an optimized, augmented Goto program:

~~~~~~
      PUSH x1;

main: x0 := x0 + 0;
      a1 := POP;
      PUSH a1;
      CALL fac;
      x0 := POP;
      HALT;

fac:  x0 := x0 + 0; 
      a2 := POP;
      a1 := POP;
      IF a2 = 1 
         THEN PUSH a1
         ELSE PUSH a2 - 1; 
              PUSH a2 * a1;
              GOTO fac
      END;
      RETURN;
~~~~~~

As a matter of fact, if the whole program was tail-recursive it would be
possible to abandon the stack altogether. One would simply reserve the
respective number of registers for the arguments of each function. One would
not lose any expressive power regarding computability either as every recursive
function can in some way or another be transformed into a tail-recursive
function[^cit].


### Inlining

TODO

Jumps could be reduced by inlining functions. Also stack space is saved.
Even recursive functions could be inlined to a certain depth (this is in parts
done in the real world, too).


Higher-order Functions
----------------------

TODO

See: e.g. <http://factorcode.org/littledan/abstract.pdf> and there in references:
    
    Thomas Johnsson. Lambda lifting: Transforming
    programs to recursive equations. pages 190–203.
    Springer-Verlag, 1985.

    Simon L. Peyton Jones. Compiling haskell by program
    transformation: A report from the trenches. In ESOP
    ’96: Proceedings of the 6th European Symposium on
    Programming Languages and Systems, pages 18–44,
    London, UK, 1996. Springer-Verlag.

How to transform higher-order functions, i.e. functions that receive functions
as arguments or return functions, into first-order functions so that these can
be translated to an augmented Goto program like described above? Provided the
main function is in first-order form, i.e. of type $main:
(\mathbb{N},\ldots,\mathbb{N}) \to \mathbb{N}$ and the program is well
typed[^well-typed]. Then, as far as I understood, the program can be
transformed, by using *lambda lifting* as a *whole-program transformation*, so
that only first-order functions of type $(\mathbb{N},\ldots,\mathbb{N}) \to
\mathbb{N}$ remain. The translation to Goto should then be possible by virtue
of the conceivable algorithm suggested above.

  [^well-typed]: For anything else a sensible result cannot be computed anyway.


Higher-order Elimination
------------------------

### Notes

  - By augmenting GOTO with CALL and PUSH of "pointers to functions" the
    downwards funarg problem (link) could be solved without lambda lifting.
    However, closures wouldn't be first class citizens then and then Recursion
    could hardly be called a functional language anymore, so this unsatisfying
    approach should not be taken.
  
    But here is the idea anyway:
  
  
          fac n =
              IF n = 1
                 THEN 1
                 ELSE mul n (fac (n - 1));
  
          twice f n = f (f n)
  
          main n = twice fac n 
  
    becomes
    CALL &x: <http://lambda-the-ultimate.org/node/4128#comment-62784>
  
                PUSH x1;
  
          main: x0 := x0 + 0;
                main_a1 := POP;
                PUSH main_a1;
                PUSHF fac;
                CALL twice;
                x0 := POP;
                HALT;
  
          twice: x0 := x0 + 0;
                 twice_a2 := POP; //function pointer
                 twice_a1 := POP;
                 PUSH twice_a1;
                 CALL &twice_a2;
                 CALL &twice_a2;
                 RETURN;
  
          fac:  x0 := x0 + 0; 
                a1 := POP;
                IF a1 = 1 
                   THEN PUSH 1
                   ELSE PUSH a1; 
                        PUSH a1-1;
                        CALL fac;
                        CALL mul
                END;
                RETURN;
  
          mul:  x0 := x0 + 0;
                a2 := POP;
                a1 := POP;
                PUSH a1 * a2;
                RETURN;
  
- http://mlton.org/pages/References/attachments/060916-mlton.pdf 

  - Problem (see "Notes on Three Instruction Machine"):
    I encountered the <http://en.wikipedia.org/wiki/Funarg_problem>
  
    "In the implementation of imperative programming languages, the normal
    place for arguments is the stack. However, a purely stack based
    implementation does not work for higher order functional programs for the
    following reason. Consider a function $f$ which returns another function 
    $g$ in weak head normal form (i.e returned function g is already applied to 
    some but not all arguments). In this case the arguments of $g$ (which 
    should normally appear contiguously on stack) get separated by the 
    arguments of $f$. For example, consider the following program:
    
    ~~~~~~
    f x = g (2 ∗ x)
    g a b = a + b
    f 3 4
    ~~~~~~
    
    Before entering $f$ the stack contains $3$ and $4$. If we create
    environments on the stack, then before entering $g$, the stack will contain
    $(2 ∗ x)$, $3$ and $4$. Clearly, the two arguments of $g$, the $(2 ∗ x)$ on
    top of the stack and $4$ at the bottom have become separated by the middle
    $3$ which is $f$’s argument. Note that $f$’s argument cannot be discarded
    since this will be used to resolve the value of $x$ in $(2 ∗ x)$. We call
    this the split activation record problem."

  - Also from <http://en.wikipedia.org/wiki/Closure_(computer_science)#Implementation_and_theory>

    "A language implementation cannot easily support full closures if its
    run-time memory model allocates all local variables on a linear stack. In
    such languages, a function's local variables are deallocated when the
    function returns. However, a closure requires that the free variables it
    references survive the enclosing function's execution. Therefore, those
    variables must be allocated so that they persist until no longer needed.
    This explains why, typically, languages that natively support closures also
    use garbage collection. The alternative is for the language to accept that
    certain use cases will lead to undefined behaviour, as in the proposal for
    lambda expressions in C++.[7] The Funarg problem (or "functional argument"
    problem) describes the difficulty of implementing functions as first class
    objects in a stack-based programming language such as C or C++. Similarly
    in D version 1, it is assumed that the programmer knows what to do with
    delegates and local variables, as their references will be invalid after
    return from its definition scope (local variables are on the stack) - this
    still permits many useful functional patterns, but for complex cases needs
    explicit heap allocation for variables. D version 2 solved this by
    detecting which variables must be stored on the heap, and performs
    automatic allocation. Because D uses garbage collection, in both versions,
    there is no need to track usage of variables as they are passed.

    In strict functional languages with immutable data (e.g. Erlang), it is
    very easy to implement automatic memory management (garbage collection), as
    there are no possible cycles in variables references. For example in
    Erlang, all arguments and variables are allocated on the heap, but
    references to them are additionally stored on the stack. After a function
    returns, references are still valid. Heap cleaning is done by incremental
    garbage collector. In ML, local variables are allocated on a linear
    stack[citation needed]. When a closure is created, it copies the values of
    those variables that are needed by the closure into the closure's data
    structure.

    Scheme, which has an ALGOL-like lexical scope system with dynamic
    variables and garbage collection, lacks a stack programming model and does
    not suffer from the limitations of stack-based languages. Closures are
    expressed naturally in Scheme. The lambda form encloses the code and the
    free variables of its environment, persists within the program as long as
    it can possibly be accessed, and can be used as freely as any other Scheme
    expression."

------------------------------------------------------------------------------

    fac n =
        IF n = 1
           THEN 1
           ELSE mul n (fac (n - 1));

    twice f n = f (f n)

    main n = twice fac n 

------------------------------------------------------------------------------

Lambda Lifting

    g a b = a + b;
    f x = \y. g (2 ∗ x) y;
    main = f 3 4

becomes

    g a b = a + b;
    h y x = g (2 * x) y;
    f x y = h x y;
    main = f 3 4;

becomes

    g a b = a + b;
    h y x = g (2 * x) y;

    g a b = a + b;
    h y x = g (2 * x) y;
    f x y = h x y;

New

~~~~~~
makeXAdder x = \y. add x y;
sixAdder = makeXAdder 6;
twoAdder = makeXAdder 2;
main = sixAdder 0 + twoAdder 2; // result: 10
~~~~~~

becomes

~~~~~~
makeXAdder x y = add x y;
sixAdder y = makeXAdder 6 y;
twoAdder y = makeXAdder 2 y;
main = sixAdder 0 + twoAdder 2; // result: 10
~~~~~~

----------------------------------------------------------------


function getAddFiveFunc()
{
  var x = 5;
  function addX(y)
  {
    return x + y;
  }

  return addX;
}

main = getAddFiveFunc() 4;

->

function addX(y, x) {
  return x + y;
}

function getAddFiveFunc()
{
  var x = 5;
  return addX;
}

main = getAddFiveFunc() 4;

From http://en.wikipedia.org/wiki/Funarg_problem:
"For example, the Pascal programming language allows functions to be passed as
arguments but not returned as results; thus implementations of Pascal are
required to address the downwards funarg problem but not the upwards one."
-> Maybe do that the same way?

-------------------------------------------------------------------------------

    fac n =
        IF n = 1
           THEN 1
           ELSE mul n (fac (n - 1));

    twice f n = f (f n);

    main n = twice fac n 

becomes

    fac n =
        IF n = 1
           THEN 1
           ELSE mul n (fac (n - 1));

    main n = fac (fac n)

and

    plusX x = \y. y + x;
    plus1 = plusX 1;
    main n = plus1 n 

becomes

    plus1 = \y. y + 1;
    main n = plus1 n 

becomes

    plus1 y = y + 1;
    main n = plus1 n

could become

    main n = n + 1
