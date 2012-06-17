Rec Spezifikation
=================

    TODO: ggf. lokale definitionen `let` weglassen damit die Übersetzung
          weniger umfangreich wird.

**Rec** ist eine funktionale Programmiersprache deren einziger
*primitiver* Datentyp natürliche Zahlen sind und deren Programmwerte
entweder natürliche Zahlen oder Funktionen sind. **Rec** basiert stark
auf der in "Implementing a functional programming language" [SPJ 97]
präsentierten Sprache *Core* mit dem Unterschied, dass **Rec** noch
minimalistischer als *Core* ist....

Syntax
------

Die Syntax von **Rec** is wie folgt definiert:

~~~~~~
<program> -> <fn_1>;...; <fn_n>             // n >= 1

<fn>      -> <var> <var_1> ... <var_n>      // n >= 0

<expr>    -> <expr> <aexpr>                 // Applicaton
           | <expr_1> <binop> <expr_2>      // Infix binary application
           | let <defns> in <expr>          // Local definitions
           | \<var_1> ... <var_n>. <expr>   // Lambda abstractions (n >= 1)
           | aexpr                          // Atomic expression

<aexpr>   -> <var>                          // Variable
           | <num>                          // Number
           | (<expr>)                       // Parenthesised expression

<defns>   -> <defn_1>;...; <defn_n>         // n >= 1
<defn>    -> <var> = <expr>

<binop>   -> <arithop> | <relop> | <boolop>
<arithop> -> + | - | * | / | %              // Arithmetic
<relop>   -> < | <= | == | != | >= | >      // Comparison
<boolop>  -> && | ||                        // Boolean

<var>     -> <alpha><varch_1>...<varch_2>   // n >= 0
<alpha>   -> [A-Z] | [a-z]
<varch>   -> <alpha> | <digit> | _

<num>     -> <digit_1>...<digit_n>          // n >= 1
<digit>   -> [0-9]
~~~~~~

Vollständigkeitshalber finden sich im Folgenden die Assozivitäten und
Prioritäten der einzelnen Operatoren:

~~~~~~
Precedence | Associativity | Operator
--------------------------------------------
     7           Left        Application
     6           Right       ^
     5           Right       *
                 None        /
     4           Right       +
                 None        -
     3           None        == != > >= < <=
     2           Right       &&
     1           Right       ||
~~~~~~

Ein **Rec** Programm besteht also aus einer Reihe von
Funktionsdefinitionen, wobei die `main` Funktion eine besondere Funktion
ist; `main` ist der Eintrittspunkt der Programmauswertung.

Ein exemplarisches **Rec** Programm sieht wie folgt aus:

~~~~~~
double x = x + x;
main = double 21
~~~~~~


### Basisfunktionen

Jedes **Rec** Programm umfasst implizit folgende Funktionsdefinitionen:

    TODO: `if`-Funktion, `true` und `false` wie im Lambda-Kalkül,
          Listenfunktionen wie im Lambdakalkül, sowie die Standardfunktionen
          aus "Implementing a funciton programming language".


Intuitive Auswertung
--------------------

Let $P$ be a Recursion program. $P$ computes a function $f: \mathbb{N}^k \to
\mathbb{N}$ like so: At the beginning of the computation `main`, which must be
of type $\mathbb{N}^k \to \mathbb{N}$, is applied to the arguments
$n_1,\ldots,n_k \in \mathbb{N}$. $P$ is then executed as follows:

- `x o y`, where `x` (`y`) is a constant or CAF denoting a constant and `o` is 
  an arithmetic operator, evaluates like so:
    - `x + y` is the sum of `x` and `y`
    - `x - y` is the difference of `x` and `y` if the difference is not
      negative. Otherwise the result is 0.
    - `x * y` is the product of `x` and `y`.
    - `x / y` is the integer quotient of `x` and `y`
    - `x % y` is `x` to modulo `y`
- The `IF` control structure either evaluates to its `THEN` part or `ELSE` part
  depending on whether its proposition evaluated to *true* or *false*.
- The definitions for the functions used in the main function are inserted
  recursively and the resulting (big) lambda abstraction is applied to the
  given arguments using beta-reduction and the two above rules until no
  reduction is possible anymore because the result is a constant.

The result of $P$'s execution is the value that `main` evaluates to.

A function $f: \mathbb{N}^k \to \mathbb{N}$ is said to be
*Rec-computable* if there exists a Recursion programs that computes $f$
as described above.


Erweiterungen
-------------

Ein Programm kann mit Kommentaren bereichert werden; `//...` für
einzeilige und `/*...*/` für mehrzeilige Kommentare, z.B.:

~~~~~~
/* double
verdoppelt den Wert des übergebenen Parameters */
double x = x + x; // x ist das Argument
main = double 21
~~~~~~
