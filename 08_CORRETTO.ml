(*------------- Gruppo8 -------------*)


(*============ Es1a:subexpr =============*)
type expr = 
    Int of int
  | Var of string
  | Sum of expr * expr
  | Diff of expr * expr
  | Mult of expr * expr
  | Div of expr * expr

(* subexpr: expr -> expr -> bool *)
let rec subexpr e1 e2 =
  e1=e2 ||
  match e1 with
    Sum(x,y) | Diff(x,y) | Mult(x,y) | Div(x,y) -> 
      subexpr x e2 || subexpr y e2
  | _ -> false

(*============ Es1b:subst-in-expr =============*)
(* subst_in_expr: expr -> string -> expr -> expr *)
let rec subst_in_expr e x e' =
  match e with
    Var y -> if x=y then e' else e
  | Int _ -> e
  | Sum(a,b) -> Sum(subst_in_expr a x e',subst_in_expr b x e')
  | Diff(a,b) -> Diff(subst_in_expr a x e',subst_in_expr b x e')
  | Mult(a,b) -> Mult(subst_in_expr a x e',subst_in_expr b x e')
  | Div(a,b) -> Div(subst_in_expr a x e',subst_in_expr b x e')

(*============ Es2a:reflect =============*)
type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* reflect:   'a tree -> 'a tree *)
let rec reflect = function
    Empty -> Empty
  | Tr(x,t1,t2) -> Tr(x,reflect t2,reflect t1);;

(*============ Es2b:fulltree =============*)

(* fulltree: int -> int tree *)
  (* aux : int -> int -> int tree *)
  (* aux k n = albero completo con radice k, altezza n, e nodi
               etichettati come richiesto dall'esercizio *)
let fulltree n = 
  let rec aux k n =
    if n=0 then Empty
    else Tr(k,aux (2*k) (n-1),aux (2*k+1) (n-1))
  in aux 1 n 

(*============ Es2c:balanced =============*)
(* per verificare se un albero e' bilanciato, 
   occorre saper calcolare l'altezza di un albero.
   Nel caso degli alberi binari rappresentati con l'albero
   vuoto, possiamo definirla cosi' per evitare di avere 
   altezze negative: *)
(* height: 'a tree -> int 
    height t = altezza di t *)
let rec height = function
    Empty -> 0
  | Tr(_,t1,t2) -> 1 + max (height t1) (height t2);;

(* balanced: 'a tree -> bool *)
let rec balanced = function
    Empty -> true
  | Tr(_,t1,t2) ->
       balanced t1 && balanced t2
       && abs(height t1 - height t2) <= 1;;

(* la definizione precedente implementa un algoritmo costoso: 
   per ogni chiamata ricorsiva, i due sottoalberi vengono visitati
   due volte, una per verificare se sono bilanaciati, e un'altra
   per determinarne l'altezza. E' possibile fare le due cose
   contemporaneamente, mediante una funzione che riporta l'altezza
   di un albero se e' bilanciato, e solleva un'eccezione se non 
   e' bilanciato (la funzione locale aux nel programma qui sotto) *)
(* aux : 'a tree -> int
   aux t = altezza di t, se t e' bilanciato, altrimenti solleva
           NotBalanced *)
exception NotBalanced;;

let balanced t =
  let rec aux = function
      Empty -> 0
    | Tr(_,t1,t2) ->
             let k1 = aux t1
             and k2 = aux t2
             in if abs(k1 - k2) <= 1
                then 1 + max k1 k2
                else raise NotBalanced
  in try let _ = aux t in true
     with NotBalanced -> false;;

(*============ Es2d:preorder-postorder-inorder =============*)
(* tipo delle tre funzioni per la visita di alberi binari:
         'a tree -> 'a list *)
let rec preorder = function
    Empty -> []
  | Tr(x,t1,t2) ->  x::(preorder t1 @ preorder t2)

let rec inorder = function
    Empty -> []
  | Tr(x,t1,t2) -> (inorder t1) @ (x::(inorder t2))

let rec postorder = function
    Empty -> []
  | Tr(x,t1,t2) -> (postorder t1) @ ((postorder t2) @ [x])

(*============ Es2e:balpreorder-balinorder =============*)
(* per la definizione di queste funzioni utilizziamo le funzioni 
   take e drop, definite, rispettivamente, a lezione e nell'esercizio
   1d del Gruppo 4 *)
(* take : int -> 'a list -> 'a list *)
let rec take n = function
    [] -> []
  | x::xs -> if n<=0 then []
             else x::take (n-1) xs

(* drop : int -> 'a list -> 'a list *)
let rec drop n = function
    [] -> []
  | x::xs -> if n<=0 then x::xs
             else drop (n-1) xs;;

(* balpreorder : 'a list -> 'a tree *)
let rec balpreorder = function
    [] -> Empty
  | x::xs ->
    let k = (List.length xs)/ 2
    in Tr(x, balpreorder (take k xs),
             balpreorder (drop k xs))

(* balinorder : 'a list -> 'a tree *)
let rec balinorder = function
    [] -> Empty
  | xs -> let k = (List.length xs)/ 2
          in let y::ys = drop k xs
          in Tr(y, balinorder (take k xs),
                   balinorder ys);;

(*============ Es3:foglie-in-lista =============*)
(* foglie_in_lista: 'a list -> 'a tree -> bool *)
let rec foglie_in_lista lista = function
    Empty -> true
  | Tr(x,Empty,Empty) -> List.mem x lista
  | Tr(x,left,right) ->
      foglie_in_lista lista left &&
      foglie_in_lista lista right

(*============ Es4:num-foglie =============*)
(* num_foglie: 'a tree -> int *)
let rec num_foglie = function
    Empty -> 0
  | Tr(x,Empty,Empty) -> 1
  | Tr(x,left,right) -> (num_foglie left) + (num_foglie right)

(*============ Es5:segui-bool =============*)
(* segui_bool : bool list -> 'a tree -> 'a *)
let rec segui_bool lista tree = 
  match (tree,lista) with 
   (Empty,_) -> failwith "segui bool"
  | (Tr(x,_,_),[]) -> x
  | (Tr(_,t1,t2),y::rest) -> 
      if y then segui_bool rest t1
      else segui_bool rest t2

(*============ Es6:foglia-costo =============*)
(* foglia_costo : int tree -> int * int *)
let rec foglia_costo = function
    Empty -> failwith "foglia_costo"
  | Tr(x,Empty,Empty) -> (x,x)
  | Tr(x,left,Empty) ->
      let (y,c) = foglia_costo left
      in (y,c+x)
  | Tr(x,Empty,right) ->
      let (y,c) = foglia_costo right
      in (y,c+x)
  | Tr(x,left,right) ->
      let (yleft,cleft) = foglia_costo left in
      let (yright,cright) = foglia_costo right in
      if cleft >= cright then (yleft,cleft+x)
      else (yright,cright+x)

(*============ Es7:foglie-costi =============*)
(*  foglie_costi : int tree -> (int * int) list *)
let rec foglie_costi = function
    Empty -> []
  | Tr(x,Empty,Empty) -> [(x,x)]
  | Tr(x,left,Empty) ->
      List.map (function (y,c) -> (y,x+c)) (foglie_costi left)
  | Tr(x,Empty,right) ->
      List.map (function (y,c) -> (y,x+c)) (foglie_costi right)
   | Tr(x,left,right) ->
       List.map (function (y,c) -> (y,x+c))
   ((foglie_costi left) @ (foglie_costi right))

(*============ Es8:pattern-matching =============*)
type expr =
    Jolly
  | Int of int
  | Var of string
  | Sum of expr * expr
  | Diff of expr * expr
  | Mult of expr * expr
  | Div of expr * expr

(* pattern_matching : expr -> expr -> bool *)
let rec pattern_matching e pattern = 
  match (e,pattern) with 
  | (_, Jolly) -> true
  | (Sum(e1,e2),Sum(m1,m2))
  | (Diff(e1,e2),Diff(m1,m2))
  | (Mult(e1,e2),Mult(m1,m2)) 
  | (Div(e1,e2),Div(m1,m2))  ->
      pattern_matching e1 m1 && pattern_matching e2 m2
  | (e,m) -> e = m

(*============ Es9:max-common-subtree =============*)
(* max_common_subtree : string tree -> string tree -> string tree *)
let rec max_common_subtree tree1 tree2 =
  match (tree1,tree2) with
    (Empty,Empty) -> Empty
  | (Tr(_,_,_),Empty)
  | (Empty,Tr(_,_,_)) -> Tr("@",Empty,Empty)
  | (Tr(a,t1,t2),Tr(b,r1,r2)) -> 
      if a=b then Tr(a,max_common_subtree t1 r1,max_common_subtree t2 r2)
      else Tr("@",Empty,Empty)

(*============ Es10a:stessa-struttura =============*)
(* stessa_struttura : 'a tree -> 'b tree -> bool *)
let rec stessa_struttura t1 t2 = 
  match (t1,t2) with 
  | (Empty,Empty) -> true
  | (Tr(_,t1,t2),Tr(_,u1,u2)) -> 
      stessa_struttura t1 u1 && stessa_struttura t2 u2
  | _ -> false  

(*============ Es10b:esiste-mapping =============*)
(* is_function: ('a * 'b) list -> bool 
   is_function lst = true se la lista associativa lst rappresenta una funzione *)
let rec is_function = function
    [] -> true
  | (x,y)::rest -> 
      List.for_all (function (z,w) -> z<>x || w=y) rest
  && is_function rest

(* in realta' l'uso di List.for_all nella definizione data sopra di
   is_function rende il codice compatto, ma e' piu' pesante del
   necessario, dal punto di vista computazionale. Non e' necessario,
   per ogni (x,y) nella lista, scandire tutto il resto della lista, ma
   basta controllare per l'eventuale valore della forma (x,z)
   successivo se y=z: *)

let rec is_function = function
    [] -> true
  | (x,y)::rest -> 
          try let z = List.assoc x rest 
              in z=y && is_function rest
          with Not_found -> is_function rest

(* mapsto : 'a tree -> 'b tree -> ('a * 'b) list *)
(* mpasto t1 t2 = lista di coppie che rappresenta un mapping
   di trasformazione da t1 a t2. Fallisce se t1 e t2 non hanno 
   la stessa struttura *)
let rec mapsto t1 t2 = match (t1,t2) with
    (Empty,Empty) -> []
  | (Tr(x,left1,right1),Tr(y,left2,right2)) ->
      (x,y)::(mapsto left1 left2)@(mapsto right1 right2)
  | _ -> failwith "Errore"

(* esite_mapping : 'a tree -> 'b tree -> bool *)
let esite_mapping t1 t2 =
  try is_function (mapsto t1 t2)
  with _ -> false

(*============ Es11:path-senza-p =============*)
(* path : ('a -> bool) -> 'a tree -> 'a list *)
let rec path p =   function   
    Empty -> failwith "Errore"
  | Tr (x,Empty,Empty) -> 
      if p x then  failwith "Errore"
      else [x]
  | Tr(x,t1,t2) ->
      if p x then  failwith "Errore"
      else x::(try path p t1
               with Failure "Errore" -> path p t2)

(*============ Es12:applica-subst =============*)
type 'a sostituzione = ('a * 'a tree) list
(* applica : sostituzione -> 'a tree -> 'a tree *)
let rec applica list = function
  | Empty -> Empty
  | Tr(x,Empty,Empty) as temp -> 
                (try List.assoc x list
     with _ -> temp)
  | Tr(x,t1,t2) -> Tr(x,applica list t1,applica list t2)

(*============ Es13:path-coprente-lista =============*)

(*  funzione ausiliaria
    togli : 'a -> 'a list -> 'a list
    togli x lst = lista che si ottiene eliminando un'occorrenza di x da lst,
                    se c'e', altrimenti lst stessa *)
let rec togli x = function
    [] -> []
  | y::rest ->
      if x=y then rest 
      else y::togli x rest

(* path_coprente : 'a tree -> 'a list -> 'a list *)
let rec path_coprente tree list =
  
  match tree with
    Empty -> failwith "Errore"
  | Tr(x,Empty,Empty) ->
          if list=[] || list=[x] then [x]
          else failwith "Errore"
  | Tr(x,t1,t2) ->
          let newlist = togli x list in
          x::(try path_coprente t1 newlist
              with _ -> path_coprente t2 newlist)

(*============ Es14:path-colori-alterni =============*)

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list

(* (a) *)
(* colore : 'a -> ('b * 'a list) list -> 'b *)
(* quindi anche
   colore: 'a -> 'a col_assoc -> col *)
let colore n colori =
  let rec aux = function
      [] -> failwith "colore"
    | (c,lst)::rest ->
  if List.mem n lst then c else aux rest
  in aux colori

(* oppure *)
let colore n colori =
  try fst (List.find (fun (_,lst) -> List.mem n lst) colori)
  with _ -> failwith "colore"

(* (b) *)
(* path_to: 'a -> 'a col_assoc -> 'a tree -> 'a list *)
let path_to n colori t =
  let rec aux padre = function
      Empty -> raise Not_found
    | Tr(x,Empty,Empty) -> if x=n && colore x colori <> padre
                           then [x] else raise Not_found
    | Tr(x,t1,t2) -> let newcol = colore x colori in
                     if newcol = padre 
                     then raise Not_found
                     else x::try aux newcol t1
                             with Not_found -> aux newcol t2
  in match t with
    Empty -> raise Not_found
  | Tr(x,Empty,Empty) ->
      if x=n then [x] else raise Not_found
  | Tr(x,t1,t2) -> 
  let col = colore x colori in
  x::try aux col t1
  with Not_found -> aux col t2

(*============ Es15a:abr-check =============*)

(* tree_for_all p t = true se tutti i nodi di t soddisfano il predicato p *)
(* tree_for_all: ('a -> bool) -> 'a tree -> bool *)
let rec tree_for_all p = function
    Empty -> true
  | Tr(x,t1,t2) -> p x && tree_for_all p t1 && tree_for_all p t2

(* abr_check:  ('a * 'b) tree -> bool *)
let rec abr_check = function
   Empty -> true
 | Tr((k,_),t1,t2) ->
      tree_for_all (function (k1,_) -> k1 < k) t1 &&
      tree_for_all (function (k1,_) -> k1 > k) t2 &&
      abr_check t1 && abr_check t2

(* l'algortimo dichiarativo dato sopra e' chiaramente molto
   inefficiente.  E' possibile controllare la proprieta' ABR visitando
   ogni nodo dell'albero una sola volta. Il trucco consiste nell'usare
   una funzione ausiliaria (la funzione abr_util nel codice sotto
   riportato), con due argomenti supplementari, per rappresentare,
   rispettivamente, il minimo e il massimo valore ammesso per le
   chiavi dei nodi. Tali valori diventano via via piu' stringenti:
   quando si visita il sottoalbero sinistro di un albero con chiave k
   alla radice, i valori dovranno essere sempre inferiori a k, quindi,
   se k doveva essere inferiore a un valore massimo dato, nel
   sottoalbero sinistro le chiavi dovranno essere inferiori a k: il
   massimo viene sostituito da k. Visitando il sottoalbero destro, si
   modifichera' simmetricamente il valore minimo ammesso.  Per non
   avere problemi con i valori di partenza (non definiti) usiamo il
   tipo 'a option per rappresentare i valori minimi e massimi ammessi,
   e definiamo operazioni di confronto tra 'a option, in modo che il
   confronto con None sia sempre true. Si noti che l'uso di min_int e
   max_int vincolerebbe le chiavi ad avere tipo int *)

(* confronto tra un 'a e un 'a option: sempre vero se il secondo
   argomento e' None, altrimenti, se il secondo argomento e' Val y,
   confronta (con < o >, rispettivamente) il primo argomento con x *)
(* ( << ) : 'a -> 'a option -> bool 
   ( >> ) : 'a -> 'a option -> bool *)
let (<<) a = function
    None -> true
  | Some b -> a<b
let (>>) a = function
    None -> true
  | Some b -> a>b
 
(* abr_util: 'a option -> 'a option -> ('a * 'b) tree -> bool *)
(* abr_util minv maxv t = true se t e' un abr e tutti i suoi nodi sono
   >> minv e << maxv *)
let abr_check t =
  let rec abr_util minv maxv = function
      Empty -> true
    | Tr((x,_),left,right) ->
  x >> minv & x << maxv &&
  abr_util minv (Some x) left &&
  abr_util (Some x) maxv right
  in abr_util None None t

(* an  algoritmo alternativo che visita ciascun nodo una volta sola
   si basa su una visita in postordine dell'albero, nella quale si calcolano
   i valori dell'etichetta minima e di quella massima di ciascun sottoalbero,
   e si confrontano con la radice.
   L'implementazione sotto proposta fa uso di una funzione ausiliaria
          aux : 'a ntree -> 'a * 'a
   che, applicata a un albero t, solleva l'eccezione Failure "Empty" se t = Empty,
   solleva Failure "False" se t non e' un abr, altrimenti riporta la coppia
   (t_min,t_max) dove t_min e' l'etichetta minima in t, e t_max e' l'etichetta
   massima in t.
   Nel caso generale, per controllare se un albero Tr(x,left,right) e' un abr,
   la funzione controlla se x e' maggiore dell'etichetta minima di left e 
   maggiore dell'etichetta massima di right.
   La funzione principale richiama aux sull'albero semplicemente per controllare
   se viene sollevata l'eccezione Failure "False", nel qual caso riporta false.
   In tutti gli altri casi riporta true.  *)  
let abr_check tree = 
  let rec aux = function
      Empty -> failwith "Empty"
    | Tr(x,left,right) -> 
  match (left,right) with
    (Empty, Empty) -> (x, x)
  | (Empty, _) -> 
      let (r_min,r_max) = aux right
      in if x < r_min then (x,r_max)
         else failwith "False" 
  | (_, Empty) -> 
      let (l_min,l_max) = aux left
      in if x > l_max then (l_min, x)
         else failwith "False" 
  | _ -> 
      let ((l_min,l_max),(r_min, r_max)) = (aux left,aux right) 
      in if x < r_min && x > l_max
         then (l_min,r_max)
         else failwith "False"
  in 
  try let _ = aux tree in true
  with Failure "Empty" -> true
     | Failure "False" -> false

(*============ Es15b:abr-search =============*)
(*  abr_search : ('a * 'b) tree -> 'a -> 'b  *)
let rec abr_search abr y = 
  match abr with
    Empty -> failwith "ABR search"
  | Tr((k,v),t1,t2) -> 
      if k = y then v
      else if y < k then abr_search t1 y
           else abr_search t2 y

(*============ Es15c:abr-update =============*)
(*  abr_update : ('a * 'b) tree -> 'a * 'b -> ('a * 'b) tree *)
let rec abr_update abr pair =
  let (k,v) = pair in 
  match abr with
     Empty -> Tr((k,v),Empty,Empty)
   | Tr((k1,v1) as x,t1,t2) -> 
         if k = k1 then Tr(pair,t1,t2)
          else if k < k1
               then Tr(x,abr_update t1 pair, t2)
                else Tr(x,t1,abr_update t2 pair)

(*============ Es15d:abr-delmin =============*)
(* abr_delmin : 'a tree -> 'a * 'a tree *)
let rec abr_delmin = function
    Empty -> failwith "delmin"
  | Tr(a,Empty,t) -> (a,t)
  | Tr(a,left,right) -> 
          let (x,newleft) = abr_delmin left
          in  (x,Tr(a,newleft,right))

(*============ Es15e:abr-delete =============*)

(* per la cancellazione di un nodo, occorre definire una funzione
   che cancelli la radice di un albero, sostituendola con il "minimo"
   del suo sottoalbero destro *)
(* delroot : 'a tree -> 'a tree *)
(* delroot t = albero che si ottiene da t sostituendo la sua radice con
               il minimo del suo sottoalbero destro (che viene eliminato) *)
let rec delroot = function
    Empty -> failwith "delroot"
  | Tr(a,Empty,t2) -> t2
  | Tr(a,t1,Empty) -> t1
  | Tr(a,t1,t2) -> let (x,newright) = abr_delmin t2
                   in  Tr(x,t1,newright)

(* la funzione di cancellazione allora deve cercare il nodo da
   eliminare e poi richiamare delroot sull'albero che lo ha
   come radice *)
(* abr_delete : ('a * 'b) tree -> 'a -> ('a * 'b) tree *)
let rec abr_delete abr a = 
  match abr with
    Empty -> Empty
  | Tr((k,_) as b,t1,t2) -> 
       if a = k then delroot (Tr(b,t1,t2))
       else if a < k
            then Tr(b,abr_delete t1 a,t2)
            else Tr(b,t1,abr_delete t2 a)


(*============ Es15f:tree-sort =============*)

(* visita simmetrica *)
(* versione che evita l'append a ogni chiamata ricorsiva *)
(* inord : 'a tree -> 'a list *)
let inord t =
  let rec inordto result = function
      Empty -> result
    | Tr(x,t1,t2) -> inordto (x::(inordto result t2)) t1
  in inordto [] t

(*  abr_insert : 'a tree -> 'a -> 'a tree *)
let rec abr_insert abr y =
  match abr with
     Empty -> Tr(y,Empty,Empty)
   | Tr(x,t1,t2) -> 
        if y <= x
        then Tr(x,abr_insert t1 y, t2)
        else Tr(x,t1,abr_insert t2 y)

(* costruzione di un ABR da una lista *)
(* from_list : 'a list -> 'a tree 
   from_list lst = ABR i cui nodi sono etichettati dagli elementi di lst *)
let rec from_list = function
    [] -> Empty
  | x::rest ->
      abr_insert (from_list rest) x

(* tree_sort: 'a list -> 'a list *)
let tree_sort lst =
  inord (from_list lst)