#import "style.typ": grammar, judgement, rule


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
    $E[p] = a$,
    $E[a := v]\/take p eval E\/v$
  )

  #rule(
    $E_1\/e_1 eval E_2\/void quad E_2\/e_2 eval E_3\/v_2$,
    $E_1\/e_1; e_2 eval E_3\/v_2$
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

  // #rule(
  //   $E_0\/e_0 eval E_1\/v_1$,
  //   $E_1,x eq v_1\/e_1 eval E_2,x eq void\/v_2$,
  //   $E_0\/#syn-let($x$, $e_0$, $e_1$) eval E_2\/v_2$
  // )

  // #rule(
  //   $E,f eq lambda x.e\/e' eval E',f eq lambda x.e\/v$,
  //   $E\/#syn-fn($f$, $beta^? x$, $t$, $t'$, $e$, $e'$) eval E'\/v$,
  // )

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

  #judgement($Gamma\/e tc Gamma\/t$)

  #rule($Gamma\/nil tc Gamma\/Nil$)

  #rule($Gamma\/0 tc Gamma\/Bit$)

  #rule($Gamma\/1 tc Gamma\/Bit$)

  #rule(
    $Gamma_1\/e_1 tc Gamma_2\/t_1
      quad Gamma_2\/e_2 tc Gamma_3\/t_2$,
    $Gamma_1\/(e_1, e_2) tc Gamma_3\/(t_1, t_2)$
  )

  #rule(
    $Gamma_1\/e_1 tc Gamma_2\/Bit
      quad Gamma_2\/e_2 tc Gamma_3\/Bit$,
    $Gamma_1\/e_1 nand e_2 tc Gamma_3\/Bit$
  )

  #rule(
    $Gamma\/e tc Gamma'\/t$,
    $Gamma\/drop e tc Gamma'\/Nil$
  )

  #rule($Gamma,x:t\/take x tc Gamma\/t$)

  #rule(
    $Gamma\/e tc Gamma'\/t' quad Gamma'[pi] = t$,
    $Gamma\/pi := e tc Gamma'[pi := t']\/t$
  )

  #rule(
    $Gamma_1\/e_1 tc Gamma_2\/Nil
      quad Gamma_2\/e_2 tc Gamma_3\/t$,
    $Gamma_1\/e_1; e_2 tc Gamma_3\/t$
  )

  #rule(
    $Gamma[f] = lambda(overline(s)).t quad Gamma\/overline(a) tcArgs Gamma'\/overline(s)$,
    $Gamma\/f(overline(a)) tc Gamma'\/t$
  )

  #rule(
    $Gamma\/e tc Gamma'\/fn(overline(s)).t quad Gamma'\/overline(a) tcArgs Gamma''\/overline(s)$,
    $Gamma\/e(overline(a)) tc Gamma''\/t$
  )

  #rule(
    $Gamma_0\/e tc Gamma_1\/t$,
    $Gamma_1\/p\/t tcPatt Gamma_2 quad Gamma_2\/e' tc Gamma_3\/t'$,
    $Gamma_0\/#syn-let($p$, $e$, $e'$) tc Gamma_3\/t'$
  )

  #rule(
    $Gamma\/e tc Gamma'\/Bit$,
    $Gamma'\/e_1 tc Gamma''\/t quad Gamma'\/e_0 tc Gamma''\/t$,
    $Gamma\/#syn-if($e$, $e_1$, $e_0$) tc Gamma''\/t$
  )
  
  #judgement($Gamma\/p\/t tcPatt Gamma$)
  #rule(
    $Gamma\/x\/t tcPatt Gamma,x:t$
  )
  #rule(
    $Gamma_1\/p_1\/t_1 tcPatt Gamma_2 quad Gamma_2\/p_2\/t_2 tcPatt Gamma_3$,
    $Gamma_1\/(p_1,p_2)\/(t_1,t_2) tcPatt Gamma_3$,
  )

  #judgement($Gamma\/overline(a) tcArgs Gamma'$)
