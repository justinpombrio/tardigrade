#import "style.typ": grammar, judgement, rule

#let eq = [#h(2pt) `=` #h(2pt)]
#let syn-let(x, b, e)           = { $#`let` #x #eq #b #`in` #e$ }
#let syn-fn(f, x, t, t2, e, e2) = { $#`fn` #f (#x: #t): #t2 #eq #e #`in` #e2$ }
#let syn-clos(f, x, e)          = { $#`fn` #f (x) #eq #e$ }
#let syn-in(fs, e)              = { $#fs #`in` #e$ }
#let syn-if(c, t, e)            = { $#`if` #c #`then` #t #`else` #e$ }

#let empty = $circle.filled.tiny$
#let void = `void`
#let nand = `nand`
#let drop = `drop`
#let nil  = `nil`
#let Void = `Void`
#let Bit = `Bit`
#let Nil = `Nil`

#let shared = $\&$
#let mutable = $*#h(0pt)$
#let lenof(x) = $#`len` #x$

#let eval = $arrow.r.double$

#let quad = h(1.5em)

== Linearity

#columns[
  === Syntax
  
  #grammar(
    ("ident",      $x, f$, "identifier"),
    ("path",       $p$, $x overline(.i)$),
    ("index",      $i$, $0 | 1$),
    ("expression", $e$, $nil | 0 | 1 | (e, e) | e nand e$),
    (none, none,        $drop e | p | f(e) | f(beta p)$),
    (none, none,        $p := e | e; e$),
    (none, none,        syn-if($e$, $e$, $e$)),
    (none, none,        syn-let($x$, $e$, $e$)),
    (none, none,        syn-let($beta x$, $p$, $e$)),
    (none, none,        syn-fn($f$, $x$, $beta^? t$, $t$, $e$, $e$)),
    ("value",      $v$, $0 | 1 | (v, v) | void$),
  )
  
  === Dynamics
  
  #grammar(
    //("borrow kind",  $beta$, $shared | mutable$),
    ("address",      $a$,  $bb(N) overline(.i)$),
    ("environment",  $E$,  $empty | E, x = b | E, [E]$),
    ("env binding",  $b$,  $v | a | lambda x. e$),
  )

  #judgement($lenof(E) = n$)
  #rule($lenof(empty) = 0$)
  #rule($lenof((E, x eq b)) = lenof(E) + 1$)
  #rule($lenof((E, [E'])) = lenof(E) + lenof(E')$)
  
  #judgement($v[overline(.i) := v] = v$)
  #rule(
    $v_0[overline(.i) := v] = v'_0$,
    $(v_0, v_1)[.0 overline(.i) := v] = (v'_0, v_1)$
  )
  #rule(
    $v_1[overline(.i) := v] = v'_1$,
    $(v_0, v_1)[.1 overline(.i) := v] = (v_0, v'_1)$
  )
  #rule($void[empty := v] = v$)

  #judgement($E[n] = b$)
  #rule(
    $lenof(E_0) = n$,
    $(E_0,x eq b,E_1)[n] = b$
  )
  
  #judgement($E[n := b] = E$)
  #rule(
    $lenof(E_0) = n$,
    $(E_0,x eq void,E_1)[n := b] = (E_0,x eq b,E_1)$
  )

  #judgement($E[a := v] = E'$)
  #rule(
    $lenof(E_0) = n quad v[overline(.i) := v'] = v''$,
    $(E_0,x eq v,E_1)[n overline(.i) := v'] = (E_0,x eq v'',E_1)$
  )

  #judgement($E[x] = n$)
  #rule(
    $E[x] = n$,
    $(E,[E'])[x] = n$
  )
  #rule(
    $x != y quad E[x] = n$,
    $(E,y eq b)[x] = n$
  )
  #rule(
    $lenof(E) = n$,
    $(E, x eq b)[x] = n$
  )

  #judgement($E[p] = a$)
  #rule(
    $E[x] = n quad E[n] = v$,
    $E[x overline(.i)] = n overline(.i)$
  )
  #rule(
    $E[x] = n quad E[n] = a$,
    $E[x overline(.i)] = a overline(.i)$
  )

  #judgement($E[x] = E_0; b; E_1$)
  #rule(
    $E = E_0,x eq b,E_1 quad E[x] = n quad lenof(E_0) = n$,
    $E[x] = E_0; b; E_1$
  )
  
  #judgement($E\/e eval E\/v$)
  
  #rule($E\/0 eval E\/0$)
  
  #rule($E\/1 eval E\/1$)
  
  #rule(
    $E_1\/e_1 eval E_2\/v_1 quad E_2\/e_2 eval E_3\/v_2$,
    $E_1\/(e_1, e_2) eval E_3\/(v_1, v_2)$
  )

  #rule(
    $v_1 nand v_2 = v$,
    $E_1\/e_1 eval E_2\/v_1 quad E_2\/e_2 eval E_3\/v_2$,
    $E_1\/e_1 nand e_2 eval E_3\/v$
  )

  #rule(
    $E\/e eval E'\/v quad$,
    $E\/drop e eval E'\/nil$
  )

  #rule(
    $E_1\/e_1 eval E_2\/void quad E_2\/e_2 eval E_3\/v_2$,
    $E_1\/e_1; e_2 eval E_3\/v_2$
  )
  
  #rule(
    $E[p] = a$,
    $E[a := v]\/p eval E\/v$
  )

  #rule(
    $E\/e eval E'\/v quad E' tack p : a$,
    $E\/p := e eval E[a := v]\/nil$
  )

  #rule(
    $E\/e eval E'\/1 quad E'\/e_1 eval E''\/v$,
    $E\/#syn-if($e$, $e_1$, $e_2$) eval E''\/v$
  )

  #rule(
    $E\/e eval E'\/0 quad E'\/e_2 eval E''\/v$,
    $E\/#syn-if($e$, $e_1$, $e_2$) eval E''\/v$
  )

  #rule(
    $E_0\/e_0 eval E_1\/v_1$,
    $E_1,x eq v_1\/e_1 eval E_2,x eq void\/v_2$,
    $E_0\/#syn-let($x$, $e_0$, $e_1$) eval E_2\/v_2$
  )

  #rule(
    $E tack p : a$,
    $E,x eq a\/e eval E',x eq a\/v$,
    $E\/#syn-let($beta x$, $p$, $e$) eval E'\/v$
  )

  #rule(
    $E,f eq lambda x.e\/e' eval E',f eq lambda x.e\/v$,
    $E\/#syn-fn($f$, $beta^? x$, $t$, $t'$, $e$, $e'$) eval E'\/v$,
  )

  #rule(
    $E[f] = E_0; lambda x.e'; E_1 quad E\/e eval E'\/v$,
    $E_0,f eq lambda x.e',[E_1],x eq v\/e'
      eval E'_0,f eq lambda x.e',[E'_1],x eq void\/v'$,
    $E\/f(e) eval E'_0,f eq lambda x.e',E'_1\/v'$
  )

  #rule(
    $E[f] = E_0; lambda x.e'; E_1 quad E[p] = a$,
    $E_0,f eq lambda x.e',[E_1],x eq a\/e'
      eval E'_0,f eq lambda x.e',[E'_1],x eq a\/v'$,
    $E\/f(beta p) eval E'_0,f eq lambda x.e',E'_1\/v'$
  )
  
  === Statics
  
  #grammar(
    ("type",         $t$,       $Nil | Bit | (t, t)$),
    ("stack type",   $s$,       $Nil | Bit | (s, s) | !s | shared s | mutable s$),
    ("type env",     $Gamma$,   $empty | Gamma, b_Gamma$),
    ("type binding", $b_Gamma$, $x: s | beta x: t | f: beta^? t -> t$),
  )
]
