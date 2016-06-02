(*------------- Gruppo5 -------------*)


(*============ Es1a:filter-raccolti =============*)
(* funzioni definite a lezione: *)
(*  in_labirinto : int -> int * int -> bool *)
let in_labirinto dim (r,c) =
  r >= 0 && c >= 0 && r < dim && c < dim
(*  find_content : ('a * 'b list) list -> 'b -> 'a list *)
let rec find_content contenuti c =
  match contenuti with
    [] -> []
  | (x,caselle)::rest ->
      if List.mem c caselle then x::find_content rest c
      else find_content rest c

(* versioni iterative di filter_vicini e raccolti *)
(* in entrambe le funzioni l'ordine degli elementi nel risultato
   non e' importante, quindi evitiamo di rovesciare alla fine
   la lista *)
(* filter_vicini: int -> (int * int) list -> (int * int) list *)
(* aux: (int * int) list -> (int * int) list -> (int * int) list 
   aux result clist = (coppie in clist che corrispondono a caselle
                      nel labirinto) @ result
*)
let filter_vicini dim caselle =
  let rec aux result = function
    [] -> result
  | casella::rest ->
      if in_labirinto dim casella
      then aux (casella::result) rest
      else aux result rest
  in aux [] caselle

(* raccolti : ('a * 'b list) list -> 'b list -> 'a list *)
(* aux: 'a list -> 'b list -> 'a list
   aux result clist = lista con il contenuto delle caselle in
                      clist concatenato con result *)
let raccolti contenuti caselle =
   let rec aux result = function
       [] -> result
     | casella::rest ->
         aux ((find_content contenuti casella) @ result) rest
   in aux [] caselle

(* Per inciso, dopo aver visto le funzioni di ordine superiore sulle liste, 
   possiamo anche definire: *)
let raccolti_bis contenuti caselle =
  List.flatten
    (List.map (find_content contenuti) caselle)

(*============ Es1b:combine =============*)
(* combine: â€™a list -> â€™b list -> (â€™a * â€™b) list *)
let rec combine prima seconda =
  match (prima,seconda) with
    ([],[]) -> []
  | (x::restx,y::resty) -> (x,y)::combine restx resty
  | _ -> failwith "combine"

(*============ Es1c:split =============*)
(* split: (â€™a * â€™b) list -> â€™a list * â€™b list *)
let rec split = function
    [] -> ([],[])
  | (x,y)::rest ->
      let (restx,resty) = split rest
      in (x::restx,y::resty)

(*============ Es1d:cancella =============*)
(* cancella: â€™a -> (â€™a * â€™b) list -> (â€™a * â€™b) list *)
(* Si deve tenere presente che normalmente l'inserimento di un nuovo
   legame in una lista associativa e' implementato mediante un cons
   dato che il legame piu' "superficiale" nasconde gli eventuali altri.
   Quindi per cancellare un legame si devono cancellare tutte le coppie
   con la chiave data *)
let rec cancella k = function
    [] -> []
  | (x,y)::rest ->
      if x=k then cancella k rest
      else (x,y)::cancella k rest

(* Quando avrete visto le funzioni di ordine superiore sulle
   liste, tornate a esaminare questo file:
   si puo' utilizzare List.filter: *)
let cancella k assoclist =
  List.filter (function (x,_) -> x<>k) assoclist

(*============ Es2:set =============*)

(* unione *)
let rec union set = function
    [] -> set
  | x::rest ->
      if List.mem x set then union set rest
      else x::union set rest

(* oppure possiamo definire, e poi utilizzare, una funzione
   setadd: 'a -> 'a list -> 'a list
   tale che setadd x set rappresenta l'unione di {x} con
   l'insieme rappresentato da set *)
let setadd x set =
  if List.mem x set then set 
  else x::set
(* usando setadd possiamo definire union cosi': *)
let rec union set = function
  [] -> set
  | x::rest -> setadd x (union set rest)

(* intersezione *)
let rec intersect set = function
    [] -> []
  | x::rest ->
      if List.mem x set then x::intersect set rest
      else intersect set rest

(* differenza *)
(* setminus set1 set2 = elementi di set1 meno quelli di set2 *)
let rec setminus set1 set2 =
  match set1 with
    [] -> []
  | x::rest ->
      if List.mem x set2 then setminus rest set2
      else x::setminus rest set2

(* subset *)
let rec subset set1 set2 =
  match set1 with
    [] -> true
  | x::rest -> List.mem x set2 && subset rest set2

(*============ Es3:explode-implode =============*)
(* explode: string -> char list *)
(* aux : int -> char list
   aux n = list con gli ultimi n-1 caratteri di s *)
let explode s =
  let len = String.length s in
  let rec aux n =
    if n >= len then []
    else s.[n] :: aux (n+1)
  in aux 0

(* oppure, con aux tail recursive, si inizia dall'ultimo carattere
   e si va all'indietro *)
(* aux : int -> char list -> char list
   aux n result clist = (lista con i primi n+1 caratteri di s) @ result *)
let explode s =
  let rec aux n result =
    if n < 0 then result
    else aux (n-1) (s.[n] :: result)
  in aux (String.length s - 1) []

(* implode: char list -> string *)
let rec implode = function
    [] -> ""
  | c::rest -> (String.make 1 c)^(implode rest)

(* oppure, tail recursive *)
(*  aux : string -> char list -> string
    aux s lst = s^(stringa con i caratteri di lst) *)
let implode lst =
  let rec aux result = function
      [] -> result
    | c::rest -> aux (result^(String.make 1 c)) rest
  in aux "" lst

(* oppure, programmazione imperativa: *)
(* loop : int -> char list -> unit
   loop n clist modifica la string init sostituendo i caratteri
       che vanno dalla posizione n in poi con quelli di clist.
       Errore se clist e' troppo lunga *)
let implode lst =
  let init = String.create (List.length lst) in
  let rec loop n = function
    | [] -> ()
    | c::rest ->
  init.[n] <- c;
  loop (n+1) rest
  in loop 0 lst;
  init


(*============ Es4:intpairs =============*)

(* intpairs: int -> (int*int) list *)

(* Sottoproblema 1: generare una lista con tutti gli interi compresi tra
   1 e n, in qualsiasi ordine *)
(* upto : int -> int list,
   upto n = [n;n-1;...;1] *)
let rec upto n =
  if n <= 0 then []
  else n::upto(n-1)

(* sottoproblema 2: dato un elemento y e una lista [x1;x2;...;xn], 
   riportare la lista [(y,x1);(y,x2);....;(y,xn)].
   Vedi esercizio 2c del gruppo 4 *)
(* pairwith: â€™a -> â€™b list -> (â€™a * â€™b) list 
   pairwith y [x1;x2;...;xn] = [(y,x1);(y,x2);....;(y,xn)] *)
let rec pairwith y = function
    [] -> []
  | x::rest -> (y,x)::pairwith y rest

(* soluzione del problema principale *)
(* aux : int list -> (int * int) list *)
(* aux lista = lista di tutte le coppie (x,y) con
                 x in lista e y compreso tra 1 e n *)
let intpairs n =
  let range = upto n in
  let rec aux = function
     [] -> []
   | x::rest ->
        (pairwith x range)@aux rest
  in aux range

(* Quando avrete visto le funzioni di ordine superiore sulle
   liste, tornate a esaminare questo file: e' possibile
   utilizzare la funzione List.map: ciascun elemento del range va
   "accoppiato" con tutti gli elementi del range *)
(* pair: 'a -> 'b -> 'a * 'b
   e` la forma prefissa e currificata dell'operazione di costruzione
   di coppie *)
let pair x y = (x,y)

(*sol MIAAAAAAAAAA*)
let intpairs n =
    let range = upto n in
    List.map (function x -> pairwith x range) range;;

(* aux : int -> (int * int) list *)
(* aux k = lista di tutte le coppie (x,y) con
             x compreso tra 1 e k e y compreso tra 1 e n *)
let intpairs n =
  let range = upto n in
  let rec aux k =
    if k<=0 then []
    else (List.map (pair k) range) @ aux (k-1)
  in aux n

(* o, addirittura:   *)
let intpairs n =
  let range = upto n in
  List.flatten
     (List.map
        (function k -> List.map (pair k) range)
        range)

(*============ Es5:trips =============*)
(* trips: â€™a list -> (â€™a * â€™a * â€™a) list *)
let rec trips = function
    x::y::z::rest ->
      (x,y,z)::trips(y::z::rest)
  | _ -> []

(*============ Es6:choose =============*)
(* e' utile utilizzare la funzione take *)
(* take: int -> â€™a list -> â€™a list *)
let rec take n = function
    [] -> []
  | x::rest -> 
      if n=0 then []
      else x::take (n-1) rest

(* choose: int -> 'a list -> 'a list list *)
(* assumiamo n positivo *)
let rec choose n lst =
  if n > List.length lst (* sicuramente vero se lst=[] *)
  then []
  else (take n lst)::choose n (List.tl lst)


(*============ Es7:strike-ball =============*)
(* strike__ball : 'a list -> 'a list -> int * int *)
(* aux : int list * int list -> int * int *)
(* scansione contemporanea delle due liste: 
   aux l1 l2 = (s,b) con s = numero di elementi di l1
       che occorrono anche in test, ma in posizione diversa.
       b = numero di elementi di l1 che occorrono nella stessa
       posizione in l2 *)
let strike_ball test guess =
  let rec aux = function
      ([],[]) -> (0,0)
    | (x::target,y::lst) ->
  let (strike,ball) = aux(target,lst)
  in if x=y then (strike,ball+1)
  else if List.mem y test then (strike+1,ball)
       else (strike,ball)
    | _ -> failwith "strike and ball"
         (* eccezione sollevata se le due liste hanno lunghezza diversa *)
  in aux (test,guess);;

(* si noti che non e' necessario assumere che gli elementi di guess
   siano tutti distinti: se guess contiene ad esempio 3 occorrenze di
   x, una delle quali occorre nella stessa posizione in test, esse conteranno
   per un ball e 2 strike *)

(*============ Es8:insert-sort =============*)
(* inserimento di un elemento in una lista ordinata, 
   mantenendo l'ordinamento *)
(* insert: 'a -> 'a list -> 'a list *)
(* insert x lst = lista che si ottiene inserendo x in lst 
                  mantenendo l'ordinamento di lst *)
let rec insert x = function
     [] -> [x]
   | y::rest -> if x <= y then x::y::rest
                else y::insert x rest

(* ordinamento *)
(* insort: 'a list -> 'a list *)
let rec insort = function
    [] -> []
  | x::rest -> insert x (insort rest)


(*============ Es8:quick-sort =============*)

(* partizionamento *)
(* partition : 'a list -> 'a -> 'a list * 'a list *)
(* partition lst pivot = (left,right), dove left
   contiene tutti gli elementi di lst minori o uguali del pivot,
   right quelli maggiori *)
let rec partition lst pivot = match lst with
  [] -> ([],[])
| x::rest -> let (left,right) = partition rest pivot
             in if x<=pivot then (x::left,right)
                else (left,x::right)

(* ordinamento *)
(* quick: 'a list -> 'a list *)
let rec quick = function
    [] ->  []
  | x::rest -> let (left,right) = partition rest x
               in (quick left) @ (x::quick right)
