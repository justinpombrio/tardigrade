#import "style.typ": grammar, judgement, rule

#set page(margin: (left: 1.5cm, right: 1.5cm))

#let eq = [#h(2pt) `=` #h(2pt)]
#let syn-let(x, b, e)   = { $#`let` #x #eq #b #`in` #e$ }
#let syn-clos(f, x, e)  = { $#`fn` #f (x) #eq #e$ }
#let syn-in(fs, e)      = { $#fs #`in` #e$ }
#let syn-if(c, t, e)    = { $#`if` #c #`then` #t #`else` #e$ }

#let empty = $circle.filled.tiny$
#let void = `void`
#let nand = `nand`
#let drop = `drop`
#let take = `take`
#let nil  = `nil`
#let Void = `Void`
#let Bit = `Bit`
#let Nil = `Nil`
#let fn = `fn`
#let fresh = `fresh`

#let shared = $\&$
#let mutable = $*#h(0pt)$
#let owned = $~#h(0pt)$
#let lenof(x) = $#`len` #x$

#let eval = $arrow.r$
#let evalPatt = $arrow.r_"bind"$
#let evalArg = $arrow.r_"arg"$
#let evalArgs = $arrow.r_"args"$
#let tc = $arrow.r.double$
#let tcPatt = $arrow.r.double_"bind"$
#let tcArg = $arrow.r.double_"arg"$
#let tcArgs = $arrow.r.double_"args"$

#let space = h(0.5em)
#let quad = h(1.5em)

== Linearity

#columns[
  === TODO

  - `take` is no longer needed.
  - `let` should check that memory has been cleaned up?

  === Motivation

  Linearity is hard! Yet I think it may still be worth while:
  - Large types, like a 10MB image or 10GB zipfile, should not be copyable by
    accident.
  - Not all types can be safely dropped without fanfare. Linearity allows
    checking that you've cleaned up a resource, even when that clean up requires
    other resources.
  - The entire point of value semantics is that there's no aliasing. This is
    achievable for _internal_ objects (represented purely in memory) using
    copy-on-write. But it can only be achieved for _external_ objects using
    linearity!

  === Syntax
  
  #grammar(
    ("ident",    $#h(-4pt)x, f$, [_identifier_]),
    ("mutability", $mu$, $shared | mutable$),
    ("value",      $v$, $nil | 0 | 1 | (v, v) | fn(overline(x)).e$),
    ("pattern",    $p$, $x | (p, p)$),
    ("index",      $i$, $0 | 1$),
    ("path",       $pi$, $x overline(.i)$),
    ("argument",   $a$, $e | mu pi | lambda(overline(x)).e$),
    ("expression", $e$, $nil | 0 | 1 | (e, e)$),
    (none, none,        $e nand e | #syn-if($e$, $e$, $e$)$),
    (none, none,        $drop e | take x | pi := e | e; e$),
    (none, none,        $#syn-let($p$, $e$, $e$) | fn(overline(x\:s)).e | e(overline(a))$),
  )
  
  === Dynamics
  
  #grammar(
    ("memory addr",  $m$,  $bb(N)$),
    ("memory",       $M$,  [map from $m$ to $v$]),
    ("location",     $l$,  $m overline(.i)$),
    ("environment",  $E$,  $empty | E, x eq b$),
    ("env binding",  $b$,  $m | mu l | lambda[E](overline(x)). e$),
  )

  #judgement($E tack M\/e eval M\/v$)

  #rule($E tack M\/nil eval M\/nil$)

  #rule($E tack M\/0 eval M\/0$)

  #rule($E tack M\/1 eval M\/1$)

  #rule(
    $E tack M_1\/e_1 eval M_2\/v_1$,
    $E tack M_2\/e_2 eval M_3\/v_2$,
    $E tack M_1\/(e_1, e_2) eval M_3\/(v_1, v_2)$
  )

  #rule(
    $E tack M_1\/e_1 eval M_2\/v_1$,
    $E tack M_2\/e_2 eval M_3\/v_2$,
    $v_1 nand v_2 = v$,
    $E tack M_1\/e_1 nand e_2 eval M_3\/v$
  )

  #rule(
    $E tack M\/e eval M'\/1 quad E tack M'\/e_1 eval M''\/v$,
    $E tack M\/#syn-if($e$, $e_1$, $e_0$) eval M''\/v$
  )

  #rule(
    $E tack M\/e eval M'\/0 quad E tack M'\/e_0 eval M''\/v$,
    $E tack M\/#syn-if($e$, $e_1$, $e_0$) eval M''\/v$
  )

  #rule(
    $E tack M\/e eval M'\/v$,
    $E tack M\/drop e eval M'\/nil$
  )

  /*
  #rule(
    $E[x] = m$,
    $E tack M[m := v]\/take x eval M\/v$
  )
  */

  #rule(
    $E tack M\/e eval M'\/v$,
    $E[pi] = mutable l quad M'[l] = v_0$,
    $E tack M\/pi := e eval M'[l := v]\/v_0$,
  )

  #rule(
    $E tack M_1\/e_1 eval M_2\/nil$,
    $E tack M_2\/e_2 eval M_3\/v$,
    $E tack M_1\/e_1; e_2 eval M_3\/v$,
  )

  #rule(
    $E tack M\/e eval M'\/v$,
    $E\/M\/p\/v evalPatt E'\/M'$,
    $E' tack M'\/e' eval M''\/v'$,
    $E tack M\/#syn-let($p$, $e$, $e'$) eval M''\/v'$
  )

  #rule($E tack M\/fn(overline(x\:s)).e eval M\/fn(overline(x)).e$)

  #rule(
    $E[f] = lambda[E_lambda](overline(x)).e_lambda$,
    $E tack M_0\/overline(a) evalArgs M_1\/overline(b)$,
    $E_lambda,overline(x\:b) tack M_1\/e_lambda eval M_2\/v$,
    $E tack M_0\/f(overline(a)) eval M_2\/v$
  )

  #rule(
    $E tack M_0\/e eval M_1\/fn(overline(x)).e_fn$,
    $E tack M_1\/overline(a) evalArgs M_2\/overline(b)$,
    $overline(x\:b) tack M_2\/e_fn eval M_3\/v$,
    $E tack M_0\/e(overline(a)) eval M_3\/v$
  )

  #judgement($E\/M\/p\/v evalPatt E'\/M'$)
  #rule(
    $fresh m$,
    $E\/M\/x\/v evalPatt E,x eq m\/M[m:=v]$
  )
  #rule(
    $E_1\/M_1\/p_1\/v_1 evalPatt E_2\/M_2$,
    $E_2\/M_2\/p_2\/v_2 evalPatt E_3\/M_3$,
    $E_1\/M_1\/(p_1, p_2)\/(v_1, v_2) evalPatt E_3\/M_3$
  )

  #judgement($E tack M\/overline(a) evalArgs M\/overline(b)$)
  #rule(
    $E tack M\/empty evalArgs M\/empty$
  )
  #rule(
    $E tack M\/a_0 evalArg M'\/b_0$,
    $E tack M'\/overline(a) evalArgs M''\/overline(b)$,
    $E tack M\/a_0, overline(a) evalArgs M''\/b_0, overline(b)$
  )

  #judgement($E tack M\/a evalArg M\/b$)
  #rule(
    $E tack M\/e evalArg M'\/v quad fresh m$,
    $E tack M\/e evalArg M'[m := v]\/m$
  )
  #rule(
    $E[mu pi] = l$,
    $E tack M\/mu pi evalArg M\/mu l$
  )
  #rule(
    $E tack M\/lambda(overline(x)^n).e evalArg M\/lambda[E](overline(x)^n).e$
  )
  
  === Statics
  
  #grammar(
    ("type",          $t$,     $Nil | Bit | (t, t) | fn(overline(s)).t$),
    ("arg type",      $s$, $t | mu t | lambda(overline(s)).t$),
    ("memory addr",   $m$,  $bb(N)$),
    ("type memory",   $Delta$,  [map from $m$ to $t$]),
    ("type location", $l_t$,  $x overline(.i)$),
    ("type binding",  $b_t$,  $m | mu l_t | lambda(overline(s)). t$),
    ("type env",      $Gamma$, $empty | Gamma, x:b_t$),
  )

  /*
  - $!s$ means that this part of the value _may_ be uninitialized.
  - $shared s$ and $mutable s$ means that there is a shared or mutable reference to this part
    of the value.
  */

  #judgement($Gamma tack Delta\/e tc Delta\/t$)

  #rule($Gamma tack Delta\/nil tc Delta\/Nil$)
  #rule($Gamma tack Delta\/0 tc Delta\/Bit$)
  #rule($Gamma tack Delta\/1 tc Delta\/Bit$)

  #rule(
    $Gamma tack Delta_1\/e_1 tc Delta_2\/t_1
      quad Gamma tack Delta_2\/e_2 tc Delta_3\/t_2$,
    $Gamma tack Delta_1\/(e_1, e_2) tc Delta_3\/(t_1, t_2)$
  )

  #rule(
    $Gamma tack Delta_1\/e_1 tc Delta_2\/Bit
      quad Gamma tack Delta_2\/e_2 tc Delta_3\/Bit$,
    $Gamma tack Delta_1\/e_1 nand e_2 tc Delta_3\/Bit$
  )

  #rule(
    $Gamma tack Delta\/e tc Delta'\/Bit$,
    $Gamma tack Delta'\/e_1 tc Delta''\/t quad Gamma tack Delta'\/e_0 tc Delta''\/t$,
    $Gamma tack Delta\/#syn-if($e$, $e_1$, $e_0$) tc Delta''\/t$
  )

  #rule(
    $Gamma tack Delta\/e tc Delta'\/t$,
    $Gamma tack Delta\/drop e tc Delta'\/Nil$
  )

  #rule(
    $Gamma tack Delta\/e tc Delta'\/t' quad Delta'[pi] = t$,
    $Gamma tack Delta\/pi := e tc Delta'[pi := t']\/t$
  )

  #rule(
    $Gamma tack Delta_1\/e_1 tc Delta_2\/Nil
      quad Gamma tack Delta_2\/e_2 tc Delta_3\/t$,
    $Gamma tack Delta_1\/e_1; e_2 tc Delta_3\/t$
  )

  #rule(
    $Gamma tack Delta_0\/e tc Delta_1\/t$,
    $Gamma\/Delta_1\/p\/t tcPatt Gamma'\/Delta_2 quad Gamma' tack Delta_2\/e' tc Delta_3\/t'$,
    $Gamma tack Delta_0\/#syn-let($p$, $e$, $e'$) tc Delta_3\/t'$
  )

  // TODO
  #rule(
    $$,
    $Gamma tack Delta\/fn(overline(x\:s)).e tc$
  )

  #rule(
    $Gamma[f] = lambda(overline(s)).t
      quad Gamma tack Delta\/overline(a) tcArgs Delta'\/overline(s)$,
    $Gamma tack Delta\/f(overline(a)) tc Delta'\/t$
  )

  #rule(
    $Gamma tack Delta\/e tc Delta'\/fn(overline(s)).t
      quad Gamma tack Delta'\/overline(a) tcArgs Delta''\/overline(s)$,
    $Gamma tack Delta\/e(overline(a)) tc Delta''\/t$
  )

  #judgement($Gamma\/Delta\/p\/t tcPatt Gamma\/Delta$)
  #rule(
    $fresh m$,
    $Gamma\/Delta\/x\/t tcPatt Gamma,x:m\/Delta,m:t$
  )
  #rule(
    $Gamma\/Delta\/p_1\/t_1 tcPatt Gamma'\/Delta'$,
    $Gamma'\/Delta'\/p_2\/t_2 tcPatt Gamma''\/Delta''$,
    $Gamma\/Delta\/(p_1, p_2)\/(t_1, t_2) tcPatt Gamma''\/Delta''$
  )

  #judgement($Gamma tack Delta\/overline(a) tcArgs Delta'\/overline(s)$)
  #rule($Gamma tack Delta\/empty tcArgs Delta\/empty$)
  #rule(
    $Gamma tack Delta\/a_0 tcArg Delta'\/s_0$,
    $Gamma tack Delta'\/overline(a) tcArgs Delta''\/overline(s)$,
    $Gamma tack Delta\/a_0, overline(a) tcArgs Delta''\/s_0, overline(s)$
  )

  #judgement($Gamma\/Delta\/a tcArg Gamma\/Delta\/s$)
  #rule(
    $Gamma tack Delta\/e tc Delta'\/t$,
    $fresh m$,
    $Gamma\/Delta\/e tcArg Delta'\/s$
  )
]
