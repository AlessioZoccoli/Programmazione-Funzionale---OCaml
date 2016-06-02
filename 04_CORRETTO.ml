Gruppo4_CORRETTO


(*============ Es1a:length =============*)
(*length: 'a list -> int,
  length lst = numero di elementi di lst *)

(* versione dichiarativa, non tail recursive *)
let rec length = function
    [] -> 0
  | _::rest -> 1+length rest

(* versione iterativa, o tail recursive *)
(* aux: int -> 'a list -> int
     aux len lst = len + numero di elementi di lst *)
let length_it lst =
  let rec aux len = function
      [] -> len
    | _::rest -> aux (len+1) rest
  in aux 0 lst

(*============ Es1b:sumof =============*)
(*sumof: int list -> int,
  sumof lst =  somma degli elementi di lst *)

(* versione dichiarativa, non tail recursive *)
let rec sumof = function
    [] -> 0
  | x::rest -> x + sumof rest

(* versione tail recursive *)
(* aux: int -> int list -> int
   aux sum lst = sum + somma degli interi in lst *)
let sumof_it lst =
  let rec aux sum = function
      [] -> sum
    | x::rest -> aux (sum+x) rest
  in aux 0 lst

(*============ Es1c:maxlist =============*)
(* maxlist: 'a list -> 'a *)
(* maxlist lst = elemento massimo di lst *)
(* NB: la lista vuota non ha elementi, quindi nemmeno un elemento massimo *)

(* versione dichiarativa, non tail recursive *)
let rec maxlist = function
    [] -> failwith "Lista vuota"
  | [x] -> x
  | x::rest -> max x (maxlist rest)

(* versioni tail recursive *)
(* aux: 'a -> 'a list -> 'a
   aux tmp lst = massimo tra tmp e il massimo elemento di lst *)
let maxlist_it lst =
  let rec aux tmp = function
      [] -> tmp
    | x::rest -> aux (max tmp x) rest
  (* il massimo temporaneo e' inizializzato con il primo
     elemento della lista *)
  in match lst with
    [] -> failwith "Lista Vuota"
  | x::rest -> aux x rest

(* altra versione tail recursive (e dichiarativa) *)
let rec maxl = function
    [] -> failwith "Lista Vuota"
  | [x] -> x
  | x::y::rest -> maxlist ((max x y)::rest)
  (* se la lista ha almeno due elementi x e y, cerca il massimo
     della lista il cui primo elemento e' il massimo tra x
     e y, seguito dagli altri *)

(*============ Es1d:drop =============*)

(* drop: int -> â€™a list -> â€™a list
   drop n lst = lista che si ottiene da lst togliendone 
                i primi n elementi.  *)
let rec drop n = function
    [] -> []
  | _::rest as lst ->
      if n>0 then drop (n-1) rest
      else lst
(* questa funzione e' gia' tail recursive *)

(*============ Es1e:append =============*)
(* append: â€™a list -> â€™a list -> â€™a list *)
(* concatenazione di due liste *)
let rec append lst1  lst2 =
  match lst1 with
    [] -> lst2
  | x::rest -> x::append rest lst2

(* la funzione non e' tail recursive. Qualcuno potrebbe
   proporre la seguente versione tail recursive *)
let append_it lst1 lst2 =
  let rec aux visti = function
     [] ->  visti @ lst2
   | x::rest -> aux (x::visti) rest
  in aux [] lst1
(* Ma ci sono 2 problemi: il primo e' che append_it utilizza l'append.
Qual e' il secondo? *)
(*oridine inverso*)
(*============ Es1f:reverse =============*)
(* reverse: â€™a list -> â€™a list *)
(* rovescia una lista *)

(* versione dichiarativa, non tail recursive *)
let rec reverse = function
  | [] -> []
  | x::rest -> (reverse rest) @ [x]

(* versione tail recursive *)
(* aux: 'a list -> 'a list -> 'a list
   aux temp lista = (reverse lista) @ temp
        cioe', reverse di lista concatenato con temp *)
let reverse_it list =
  let rec aux temp = function
    | [] -> temp
    | x::rest -> aux (x::temp) rest
  in aux [] list

(*============ Es1g:nth =============*)
(* nth: int -> â€™a list -> â€™a
   nth n lst = elmento di lst in posizione n *)

let rec nth n = function
    [] -> failwith "nth"
  | x::rest -> 
     if n=0 then x
     else nth (n-1) rest 

(* questa definizione e' tail recursive *)

(*============ Es1h:remove =============*)
(* remove: â€™a -> â€™a list -> â€™a list
remove x lst elimina tutte le occorrenze di x da lst *)

(* versione dichiarativa, non tail recursive *)
let rec remove x = function
     [] -> []
   | y::rest ->
       if x=y then remove x rest
       else y::remove x rest

(* la versione tail recursive deve alla fine rovesciare il
   risultato *)
(* aux: 'a list -> 'a list
   aux result lst = (List.rev result) @ lista che si ottiene da lst eliminando
                                         tutte le occorrenze di x
*)
let remove_it x lst =
  let rec aux tmp = function
     [] -> List.rev tmp
   | y::rest ->
        if x=y then aux tmp rest
        else aux (y::tmp) rest
  in aux [] lst

(* l'alternativa sarebbe avere nel caso y::rest di aux:
        if x=y then ...
        else (tmp@[y]) rest
   Ma le concatenazioni sono costose: un append richiede tempo 
   lineare nella dimensione della prima lista, e facendo un 
   append per ogni elemento della lista, la funzione
   remove diventerebbe di costo quadratico. Il reverse iterativo, o 
   tail recursive,  richiede tempo lineare nella dimensione della
   lista (e List.rev e' la versione iterativa del reverse). Quindi
   il costo complessivo della remove iterativa sopra riportata resta
   lineare. *)
 
(*============ Es2a:copy =============*)
(* copy: int -> â€™a -> â€™a list *)
(* versione dichiarativa, non tail recursive *)
let rec copy n x =
  if n<=0 then []
  else x::copy (n-1) x

(* versione tail recursive *)
(* aux: 'a list -> int -> 'a list *)
(* aux tmp k = (lista composta da n occorrenze di x) @ tmp *)
let copy_it n x =
  let rec aux tmp k =
     if k <= 0 then tmp
     else aux (x::tmp) (k-1)
  in aux [] n

(*  copy 3 (copy 2 8) e' una int list list *)

(*============ Es2b:nondec =============*)
(* nondec: 'a list -> bool *)
let rec nondec = function
    [] | [_] -> true
  | x::y::rest ->
      x <= y && nondec (y::rest)
(* e' tail recursive *)

(*============ Es2c:pairwith =============*)
(* pairwith: â€™a -> â€™b list -> (â€™a * â€™b) list *)
(* versione dichiarativa, non tail recursive *)
let rec pairwith y = function
    [] -> []
  | x::rest -> (y,x)::pairwith y rest

(* la versione tail recursive deve alla fine rovesciare il
   risultato *)
(* aux: (â€™a * â€™b) list -> 'b list -> (â€™a * â€™b) list
   aux tmp [x1;...;xn] = (List.rev tmp) @ [(y,x1);...;(y,xn)]
*)
let pairwith_it y lst =
   let rec aux tmp = function
      [] -> List.rev tmp
    | x::rest -> aux ((y,x)::tmp) rest
   in aux [] lst

(*============ Es2d:duplica =============*)
(* duplica: â€™a list -> â€™a list  *)
(* versione dichiarativa, non tail recursive *)
let rec duplica = function
    [] -> []
  | x::rest -> x::x::duplica rest

(* versione tail recursive *)
(* aux: 'a list -> 'a list -> 'a list
   aux tmp [x1;...;xn] = (List.rev tmp)@[x1;x1;...;xn;xn] *)
let duplica_it lst =
  let rec aux tmp = function
      [] -> List.rev tmp
    | x::rest -> aux (x::x::tmp) rest
  in aux [] lst

(*============ Es2e:enumera =============*)
(* enumera: â€™a list -> (int * â€™a) list *)
(* versione dichiarativa, non tail recursive. Serve in ogni caso una funzione
   ausiliaria che abbia come argomento il "contatore di posizione" *)
let enumera lst =
  let rec aux count = function
     [] -> []
   | x::rest -> (count,x)::aux (count+1) rest
  in aux 0 lst

(* versione tail recursive *)
(* aux : (int * 'a) list -> int -> 'a list -> (int * 'a) list
   aux tmp n [x1;...;xk] = (List.rev tmp) @ [(n,x1);(n+1,x2);...(n+k+1,xk)]
*)
let enumera_it lst =
  let rec aux tmp count = function
     [] -> List.rev tmp
   | x::rest -> aux ((count,x)::tmp) (count+1) rest
  in aux [] 0 lst

(*============ Es2f:position =============*)
(* position: â€™a -> â€™a list -> int *)
(* versione dichiarativa, non tail recursive *)
let rec position x = function
    [] -> failwith "position"
  | y::rest ->
      if x=y then 0
      else 1 + position x rest

(* versione tail recursive *)
(* aux: int -> 'a list -> int *)
(* aux count lista = count + posizione di x in lista *)
let position_it x lst =
  let rec aux count = function
     [] -> failwith "position"
   | y::rest ->
        if x=y then count
        else aux (count+1) rest
  in aux 0 lst

(*============ Es2g:alternate =============*)
(* alternate: â€™a list -> â€™a list  *)
(* versione dichiarativa, non tail recursive *)
let rec alternate = function
    [] | [_] -> []
  | _::y::rest -> y::alternate rest

(* o anche, invertendo i casi di match *)
let rec alternate = function
    _::y::rest -> y::alternate rest
  | _ -> []

(* versione tail recursive *)
(* aux : 'a list -> 'a list -> 'a list *)
(* aux tmp lst = (List.rev tmp) @ lista con gli elementi in posizione
                                  dispari in lst *)
let alternate_it lst =
    let rec aux tmp = function
  _::y::rest -> aux (y::tmp) rest
      | _  -> List.rev tmp
    in aux [] lst

(*============ Es2h:min-dei-max =============*)
(* min_dei_max: int list list -> int *)
(* la funzione solleva un'eccezione se la lista e' vuota o se
   contiene qualche lista vuota (che non ha massimi) *)
(* la funzione maxlist utilizzata sotto e' quella definita
   nell'esercizio 1c *)

(* versione dichiarativa, non tail recursive *)
let rec min_dei_max = function
    [] -> failwith "min_dei_max"
  | [lst] -> maxlist lst
  | lst::rest ->
        min (maxlist lst) (min_dei_max rest)

(* versioni tail recursive *)
(* aux : 'a -> 'a list list -> 'a
   aux tmp lista = minimo tra tmp e il massimo elemento di lista *)
let min_dei_max_it lst =
  let rec aux tmp = function
       [] -> tmp
     | x::rest ->
           aux (min tmp (maxlist x)) rest
  in
  match lst with
     [] -> failwith "min_dei_max"
   | x::rest -> aux (maxlist x) rest

let rec min_dei_max_it2 = function
    [] -> failwith "min_dei_max"
  | [lst] -> maxlist lst
  | prima::seconda::rest ->
        min_dei_max ([min (maxlist prima) (maxlist seconda)]::rest)

(*============ Es2i:split2 =============*)
(* take: int -> 'a list -> 'a list *)
(* take n lst = primi n elementi di lst *)
let rec take n = function
    [] -> []
  | x::rest ->
      if n<=0 then []
      else x::(take (n-1) rest)

(* drop: int -> 'a list -> 'a list *)
(* drop e' la funzione dell'esercizio 1d *)

(* split2: 'a list -> 'a list * 'a list *)
let split2 lista =
  let n = (List.length lista)/2
  in (take n lista, drop n lista)