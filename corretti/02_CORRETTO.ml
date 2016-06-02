
(** Gruppo 1 **)

(*============ Esercizi 1 e 2 =============*)

(*
Per capire il comportamento di OCaml si deve tener presente che OCaml
  e' un linguaggio a scopo statico: 

Esercizio 1: quando si applica la funzione area, il valore della
variabile globale pi e` sempre quello che aveva nell'ambiente in cui
e` stata definita la funzione, quindi 3.14159

Esercizio 2: quando si applica la funzione h, il valore della
  variabile globale y e` quello che aveva nell'ambiente in cui e`
  stata definita la funzione, quindi 100. La variabile x nella
  definizione di h e` invece il parametro formale della funzione,
  quindi il valore (5) della variabile globale x e` irrilevante,
  quando h viene applicata.
*)

(*============ Esercizio 3 =============*)

(* E' facile rispondere utilizzando OCaml *)

(*============ Es4:quadruple =============*)

(* dato che in ogni definizione si utilizza un solo elemento della*)
(* quadrupla, possiamo utilizzare la variabile muta per gli altri *)

let pi1 (x,_,_,_) = x
let pi2 (_,x,_,_) = x
let pi3 (_,_,x,_) = x
let pi4 (_,_,_,x) = x

(* il loro tipo e':
    pi1: 'a * 'b * 'c * 'd -> 'a
    pi2: 'a * 'b * 'c * 'd -> 'b
    pi3: 'a * 'b * 'c * 'd -> 'c 
    pi4: 'a * 'b * 'c * 'd -> 'd
Le funzioni non si possono applicare a quintuple, ma solo a quadruple.
Se si dichiara *)

let quadrupla =
      (5,('c',"antonio",(),if 3>4 then 0 else 1),"pippo",true)

(* il valore di quadrupla e'
   (5, ('c', "antonio", (), 1), "pippo", true)
Quindi:
pi2 quadrupla = ('c', "antonio", (), 1),
pi3 (pi2 quadrupla) = pi3 ('c', "antonio", (), 1) = ()
pi4 (pi2 quadrupla) = pi4 ('c', "antonio", (), 1) = 1
*)

(*============ Es5:if-then-else =============*)

(* Quando si ha true in uno dei due rami then o else, chiedetevi: qual
 e' l'operatore logico al quale basta valutare un argomento per
 riportare true? (risposta: OR). Analogamente: se il ramo then o
 quello else hanno false: qual e' l'operatore a cui basta valutare un
 argomento per riportare false? (risposta: AND)

1) if E then true else false   -> E
2) if E then false else true   -> not E
3) if E then F else false      -> E && F
    se E e' falso, tutta l'espressione e' falsa (indipendentemente da
    F, quindi si tratta di un AND), altrimenti ha lo stesso valore di F
4) if E then F else true       -> not E || F
    se E e' falsa allora l'espressione e' vera (indipendentemente da
    F, quindi si tratta di un OR), altrimenti ha lo stesso valore di F
5) if E then true else F       ->  E || F
    se E e' vera allora l'espressione e' vera (quindi si tratta di un
    OR), altrimenti ha lo stesso valore di F
6) if E then false else F      -> not E && F
    se E e' vera allora l'espressione e' falsa (quindi si tratta di un 
    AND), altrimenti ha lo stesso valore di F

*)
