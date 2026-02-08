#import "style.typ": grammar, judgement, rule, side-by-side, describe

#set page(margin: (left: 1.5cm, right: 1.5cm))

#let eq = [#h(2pt) `=` #h(2pt)]
#let syn-let(x, b, e)   = $#`let` #x #eq #b #`in` #e$
#let syn-if(c, t, e)    = $#`if` #c #`then` #t #`else` #e$
#let syn-match(e, x1, e1, x2, e2) = $#`case` e " " ( #`L` x1 -> e1 #`R` x2 -> e2 )$

#let empty = $circle.filled.tiny$
#let nand = `nand`
#let nil  = `nil`
#let Nil = `Nil`
#let Bit = `Bit`
#let fn = `fn`
#let left = `L`
#let right = `R`

#let inout = $*#h(0pt)$

#let eval = $arrow.b$
#let evals(E1, e, E2, v) = $E1,e arrow.b E2,v$
#let evalArg = $arrow.r_#move(dy:-3pt)[arg]$
#let evalArgs = $arrow.r_#move(dy:-3pt)[args]$
#let evalRets = $arrow.r_#move(dy:-3pt)[rets]$
#let evalRet = $arrow.r_#move(dy:-3pt)[ret]$

#let tc = $arrow.b.double$
#let tcArg = $arrow.r.double_"arg"$
#let tcArgs = $arrow.r.double_"args"$

#let quad = h(1.5em)

== Mutable Value Semantics

#columns[
  `linearity.typ` has some strong arguments in favor of linearity. But... it's
  still pretty complicated. What happens if we surrender it in favor of a simple
  mutable value semantics?

  The "args" and "rets" judgements are a good way of describing how the
  semantics _ought_ to behave, but not a great description of how it would be
  implemented. Maybe this semantics should use references instead.

  === Syntax

  #grammar(
    ("ident",      $#h(-4pt)x, f$, [_identifier_]),
    ("value",      $v$, $nil | 0 | 1 | (v, v) | left v | right v$),
    (none, none,        $lambda[E](overline(x\:s)).e$),
    ("index",      $i$, $0 | 1$),
    ("path",       $pi$, $x overline(.i)$),
    ("arg",        $a$,  $e | inout pi$),
    ("expr",       $e$,  $nil | e; e | (e, e) | e.0 | e.1$),
    (none, none,        $0 | 1 | e nand e | #syn-if($e$, $e$, $e$)$),
    (none, none,        $left e | right e | #syn-match($e$, $x$, $e$, $x$, $e$)$),
    (none, none,        $#syn-let($x$, $e$, $e$) | pi := e$),
    (none, none,        $lambda(overline(x\:s)).e | e(overline(a))$),
  )
  
  === Dynamics

  #grammar(
    ("environment",  $E$,  $empty | E[x eq v]$)
  )

  #describe(side-by-side(
    judgement($E, overline(a) evalArgs E', overline(v)$),
    judgement($E, a evalArg E', v$)
  ))[
    Arguments $overline(a)$ evaluate to values $overline(v)$ in environment $E$,
    modifying the environment into $E'$.
  ]
  #side-by-side(
    rule($E, empty evalArgs E, empty$),
    rule(
      $E, a_0 evalArg E', v_0$,
      $E', overline(a) evalArg E'', overline(v)$,
      $E, a_0, overline(a) evalArgs E'', v_0, overline(v)$
    )
  )
  #side-by-side(
    rule(
      $E, e eval E', v$,
      $E, e evalArg E', v$
    ),
    rule(
      $E[pi] = v$,
      $E, inout pi evalArg E, v$
    )
  )

  #describe(side-by-side(
    judgement($E, overline(a := v) evalRets E'$),
    judgement($E, a := v evalRet E'$),
  ))[
    Assigning the values $overline(v)$ to the corresponding `inout` paths in
    $overline(a)$ in environment $E$ results in environment $E'$.
  ]
  #side-by-side(
    rule($E, empty evalRets E$),
    rule(
      $E, a_0 := v_0 evalRet E'$,
      $E', overline(a := v) evalRets E''$,
      $E, a_0 := v_0, overline(a := v) evalRets E''$
    )
  )
  #side-by-side(
    rule($E, e := v evalRet E$),
    rule(
      $E[pi := v] = E'$,
      $E, inout pi := v evalRet E'$
    )
  )

  #describe(judgement($E,e eval E',v$))[
    In environment $E$, expression $e$ evaluates to value $v$, while modifying
    the environment into $E'$. $E'$ always has the same shape as $E$: they have
    the same length and variables.
  ]

  // nil
  #rule($E,nil eval E,nil$)

  // seq
  #rule(
    $E_0,e_0 eval E_1,v_0 quad E_1,e_1 eval E_2,v_1$,
    $E_0,e_0; e_1 eval E_2,v_1$
  )

  // let, assign
  #rule(
    $E, e eval E', v$,
    $E'[x eq v], e eval E''[x eq v''], v'$,
    $E, #syn-let($x$, $e$, $e'$) eval E'', v'$
  )
  #rule(
    $E, e eval E', v'
      quad E'[pi := v'] = E'',v$,
    $E, pi := e eval E'[pi := v'], v$
  )

  // lambda, apply
  #rule($E, lambda(overline(x\:s)).e eval E, lambda[E](overline(x\:s)).e$)
  #rule(
    $E_0, e eval E_1, lambda(overline(x\:s)).e_lambda
      quad E_1\, overline(a) evalArgs E_2,overline(v)$,
    $E_2[overline(x eq v)], e_lambda eval E_3[overline(x eq v')], v''
      quad E_3, overline(a := v') evalRets E_4$,
    $E_0, e(overline(a)) eval E_4, v''$
  )
]
