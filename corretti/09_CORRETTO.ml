(*------------- Gruppo10 -------------*)

(* dichiarazione di tipo per gli alberi n-ari, usata negli esercizi dal
   2 in poi *)
type 'a ntree = Ntree of 'a * 'a ntree list

(*============ Es1:expr =============*)

type multi_expr = 
    MultiInt of int
  | MultiVar of string
  | MultiDiff of multi_expr * multi_expr
  | MultiDiv of multi_expr * multi_expr
  | MultiSum of multi_expr list
  | MultiMult of multi_expr list

(* a 
Scrivereunafunzionesubexpr: multi_expr -> multi_expr -> bool che, date due
espressioni aritmetiche E1 e E2, determini se E2 è una sottoespressione di E1.
*)
(* subexpr: multi_expr -> multi_expr -> bool *)
let rec subexpr expr sub =
  expr = sub ||
  match expr with
    MultiDiff(e1,e2) | MultiDiv(e1,e2)
    -> subexpr e1 sub || subexpr e2 sub
  | MultiSum elist | MultiMult elist ->
      List.exists (function e -> subexpr e sub) elist
  | _ -> false

(* b
  subst: multi_expr -> string -> multi_expr -> multi_expr  
  data un’espressione E, il nome di una variabile
  x e un’espressione E′, costruisca l’espressione che si ottiene da E sostituendo
  con E′ ogni occorrenza della variabile di nome x.
*)
(* subst: multi_expr -> string -> multi_expr -> multi_expr *)
(* la variabile e' rappresentata dal suo nome - una stringa *)
let rec subst expr x nuova =
  match expr with
    MultiVar y -> if x=y then nuova else expr
  | MultiDiff(e1,e2) -> MultiDiff(subst e1 x nuova,subst e2 x nuova)
  | MultiDiv(e1,e2) -> MultiDiv(subst e1 x nuova,subst e2 x nuova)
  | MultiSum elist ->
      MultiSum(List.map (function e -> subst e x nuova) elist)
  | MultiMult elist ->
      MultiMult(List.map (function e -> subst e x nuova) elist)
  | _ -> expr


(*============ Es2:postorder-inorder =============*)

(* albero di esempio *)
let leaf x = Ntree(x,[])

(* Esempio: *)
let t = Ntree(1,[Ntree(2,[Ntree(3,[leaf 4;
                          leaf 5]);
                    Ntree(6,[leaf 7]);
                    leaf 8]);
              leaf 9;
              Ntree(10,[Ntree(11,[leaf 12;
                            leaf 13;
                            leaf 14]);
                     leaf 15;
                     Ntree(16,[leaf 17;
                            Ntree(18,[leaf 19;
                                   leaf 20])])])])

(* visita in postordine *)
(* postorder : 'a ntree -> 'a list *)
let rec postorder = function
    Ntree(x,tlist) ->
      (List.flatten (List.map postorder tlist))@[x]

(* ma in realta' conviene usare la mutua ricorsione, per evitare
   tutti gli append delle radici alla fine delle liste. La funzione
   su liste di alberi ha come argomento anche la radice, che viene
   aggiunta alla fine (senza append): *)
(* postlist : 'a -> 'a ntree list -> 'a list 
    postlist x [t1;...;tn] =
           (postord t1) @ .... @ (postord tn) @ [x] *)
let rec postord = function
    Ntree(x,tlist) -> postlist x tlist
and postlist x = function
    [] -> [x]
  | t::rest ->
      (postord t) @ (postlist x rest)
    
(* visita simmetrica *)
let rec inorder = function
    Ntree(x,[]) -> [x]
  | Ntree(x,t::rest) -> 
      (inorder t) @ x::(List.flatten(List.map inorder rest))

(*============ Es3:foglie-in-lista =============*)

(* foglie_in_lista : 'a list -> 'a ntree -> bool *)
(* foglie_in_lista: ’a list -> ’a ntree -> bool che, data una
   lista lst e un albero n-ario t, determini se ogni foglia di t appartiene a lst*)
(* prima alternativa: uso di for_all *)
let rec foglie_in_lista lista = function
    Ntree(x,[]) -> List.mem x lista
  | Ntree(_,tlist) ->
      List.for_all (foglie_in_lista lista) tlist

(* seconda alternativa: uso della mutua ricorsione *)
(* foglie_in_lista : 'a list -> 'a ntree -> bool *)
(* foglie_bis : 'a list -> 'a ntree list -> bool 
   foglie_bis lista tlist = true se tutte le foglie di ciascun
         albero in tlist appartengono a lista *)
let rec foglie_in_lista lista = function
    Ntree(x,[]) -> List.mem x lista
  | Ntree(_,tlist) ->
      foglie_bis lista tlist
and foglie_bis lista = function
    [] -> true
  | t::rest -> foglie_in_lista lista t && foglie_bis lista rest

(*============ Es4:num-di-foglie =============*)

(*  sumof : int list -> int *)
(* sumof lista = somma degli elementi di lista *)
let rec sumof = function
    [] -> 0
  | x::rest -> x+sumof rest

(* numfoglie : 'a ntree -> int *)
let rec numfoglie = function
    Ntree(x,[]) -> 1
  | Ntree(_,tlist) ->
     sumof (List.map numfoglie tlist)

(*============ Es5:listaGuida =============*)
(*
Una lista di interi non negativi L può determinare un sottoalbero di un albero
n-ario T: quello che si ottiene, a partire dalla radice, scendendo, per ogni
elemento n di L, al sottoalbero in posizione n nella lista dei sottoalberi
(se esiste) – si ricordi che la posizione degli elementi in una lista si conta
a partire da 0. Se la lista è più lunga del ramo cui essa conduce, oppure se a
qualche livello non esiste un numero sufficiente di sottoalberi, allora L non
determina alcun sottoalbero di T.

Ad esempio, se T è l’albero:
la lista [2;0] determina il sottoalbero che ha radice 11, la lista [2;2;1]
quello che ha radice 18, [2;2;1;0] il sottoalbero costituito soltanto dal nodo 19.
Le liste [1;2] e [0;1;1] non determinano alcun sottoalbero di T.

Scrivere una funzione listaGuida: ’a list -> ’a ntree -> ’a, che, data una lista
L di interi e un albero n-ario T, riporti la radice del sot- toalbero di T
determinato da L, se L determina un sottoalbero di T, un errore altrimenti
*)

exception Error

(* listaGuida : int list -> 'a ntree -> 'a *)
let rec listaGuida lista (Ntree(x,tlist)) = 
  match lista with
    [] -> x 
  | x::rest ->
      try listaGuida rest (List.nth tlist x)
      with Failure "nth" -> raise Error

(*============ Es6:foglia-costo =============*)

(* funzione ausiliaria *)
(* maxpair : ('a * 'b) list -> 'a * 'b *)
(* maxpair lst = coppia (x,y) in lst con y massimo 
           fallisce per la lista vuota *)
let rec maxpair = function
    [] -> failwith "maxpair"
  | [p] -> p
  | ((x,c)::(y,c')::rest) ->
      if c>c' then maxpair ((x,c)::rest)
      else maxpair ((y,c')::rest)

(* foglia_costo : int ntree -> int * int *)
let rec foglia_costo = function
    Ntree(x,[]) -> (x,x)
  | Ntree(x,tlist) ->
      let (y,c) = maxpair (List.map foglia_costo tlist)
      in (y,x+c)

(*============ Es7:tutte-foglie-costi =============*)

(* tutte_foglie_costi : int ntree -> (int * int) list *)
let rec tutte_foglie_costi = function
    Ntree(x,[]) -> [(x,x)]
  | Ntree(x,tlist) ->
      List.map (function (y,c) -> (y,c+x))      
  (List.flatten 
     (List.map tutte_foglie_costi tlist))

(*============ Es8:ramo-da-lista =============*)

exception NotFound

(* remove: 'a -> 'a list -> 'a list
   remove x lst = lst senza la prima occorrenza di x,
                  fallimento se x non occorre in lst *)
let rec remove x = function
    [] -> raise NotFound
  | y::rest -> if x=y then rest else y::remove x rest

(* ramo_da_lista : 'a ntree -> 'a list -> 'a -> 'a list 
   auxlist : 'a list -> 'a -> 'a ntree list -> 'a list 
   auxlist lista x tlist = ramo in uno degli alberi in tlist
         fino a una foglia etichettata da x e che e' una 
         permutazione di lista. Fallimento se non esiste *)
let rec ramo_da_lista t lista x =
  match t with
    Ntree(y,[]) ->
      if x=y && lista=[y] then [y]
      else raise NotFound
  | Ntree(y,tlist) ->
      y::auxlist (remove y lista) x tlist
and auxlist lista x = function
    [] -> raise NotFound
  | t::rest ->
      try ramo_da_lista t lista x
      with NotFound -> auxlist lista x rest

(*============ Es9:ramo-di-primi =============*)

(* primo: int -> bool,
   determina se il numero e' primo *)
(* aux: int -> bool
   aux k = n non e' divisibile per alcun numero compreso
             tra 2 e k (inclusi) *)
let primo n =
  let rec aux = function
      1 -> true
    | k -> (n mod k)<>0 && aux (k-1)
  in n>0 && (n=1 || aux (n/2))

exception NotFound

(* ramo_di_primi: int ntree -> int *)
(* auxlist : int ntree list -> int 
   auxlist tlist = foglia di un ramo composto solo da numeri primi
        in uno degli alberi in tlist. Fallimento se non esiste *)
let rec ramo_di_primi = function 
    Ntree(x,[]) ->
      if primo x then x else raise NotFound
  | Ntree(x,tlist) ->
      if primo x then auxlist tlist
      else raise NotFound
and auxlist = function
    [] -> raise NotFound
  | t::rest ->
      try ramo_di_primi t
      with NotFound -> auxlist rest

(*============ Es10:path-non-pred =============*)

exception NotFound

(* path_non_pred : ('a -> bool) -> 'a ntree -> 'a list 
   auxlist : ('a -> bool) -> 'a ntree list -> 'a list 
     auxlist p tlist = ramo in uno degli alberi in tlist
        composto solo da nodi che non soddisfano p *)
let rec path_non_pred p = function 
    Ntree(x,[]) ->
      if p x then raise NotFound else [x]
  | Ntree(x,tlist) ->
      if p x then raise NotFound
      else x::auxlist p tlist
and auxlist p = function
    [] -> raise NotFound
  | t::rest ->
      try path_non_pred p t
      with NotFound -> auxlist p rest

(*============ Es11:same-structure =============*)

exception NotFound

(* same_structure : 'a ntree -> 'b ntree -> bool
   same_list : 'a ntree list -> 'b ntree list -> bool 
   same_list tlist tlist2 = true se tlist e tlist2 hanno lo
         stesso numero di elementi e gli alberi che stanno
         nella stessa posizione in tlist e tlist2 hanno la
         stessa struttura *)
let rec same_structure (Ntree(_,tlist)) (Ntree(_,tlist2)) =
  same_list tlist tlist2
and same_list tlist tlist2 = 
  match (tlist,tlist2) with 
    ([],[]) -> true
  | (t::rest,t1::rest1) ->
      same_structure t t1 && same_list rest rest1
  | _ -> false

(** oppure **)
let rec same_structure (Ntree(_,tlist)) (Ntree(_,tlist2)) =
  try List.for_all 
      (function (t,t') -> same_structure t t')
      (List.combine tlist tlist2)
  with Invalid_argument "List.combine" -> false

(*============ Es12:ramo-colorato =============*)

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
(* funzione ausiliaria *)
(* colore: 'a -> 'a col_assoc -> col 
   colore x assoc_colori = colore di x secondo l'associazione
         assoc_colori. Fallisce se x non ha nessun colore *)
let rec colore x = function
    [] -> failwith "colore"
  | (c,list)::rest -> 
      if List.mem x list then c
      else colore x rest

(* o anche *)
let colore x colori =
  try
    fst (List.find (fun (_,list) -> List.mem x list) colori)
  with Not_found -> failwith "colore"


exception NotFound
(*  ramo_colorato : 'a -> ('b * 'a list) list -> 'a ntree -> 'a list *)
let ramo_colorato x colassoc t =
   (* funzione ausiliaria *)
   (* colore_diverso: 'a -> 'a ntree -> bool *)
   (* colore_diverso y t = true se la radice di t ha colore diverso 
      da quello di y *)
  let colore_diverso y (Ntree(r,_)) =
     colore y colassoc <> colore r colassoc in
  (* fromnode: 'a ntree -> 'a list *)
  (* ricerca di un ramo colorato in un singolo albero *)
  let rec fromnode  = function
      Ntree(y,[]) ->
  if x=y then [x] else raise NotFound
    | Ntree(y,tlist) -> 
  y:: fromlist (List.filter (colore_diverso y) tlist)
        (* la ricerca sui sottoalberi avviene soltanto su
     quelli che hanno radice di colore diverso da y *)
  (* fromlist: 'a ntree list -> 'a list
     fromlist tlist = ramo colorato in qualche albero di tlist *)
  and fromlist  = function
      [] -> raise NotFound
    | t::rest ->
  try fromnode  t
  with NotFound -> fromlist  rest
  in fromnode t
