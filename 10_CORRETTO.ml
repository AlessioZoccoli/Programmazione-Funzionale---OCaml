(*------------- Gruppo11 -------------*)

(* Nelle soluzioni che seguono, si assume di aver dato le seguenti 
   definizioni *)

type 'a graph = ('a * 'a) list
(* successori : 'a -> 'a graph -> 'a list  *)
(* successori n g = lista dei successori di n nel grafo orientato 
      rappresentato da g *)
let successori nodo grafo =
    List.map snd (List.filter (function (x,y) -> x=nodo) grafo)

(*  vicini : 'a -> 'a graph -> 'a list  *)
(* vicini n g = lista dei vicini di n nel grafo non orientato 
        rappresentato da g *)
let rec vicini nodo = function 
    [] -> []
  | (x,y)::rest ->
      if x = nodo then y::vicini nodo rest
      else if y = nodo then x::vicini nodo rest
      else vicini nodo rest

(*============ Es1:test-connessi =============*)
 
(* e' sufficiente adattare un algoritmo di visita *)
(* search: 'a list -> 'a list -> bool *)
(* search visited listanodi = true se da uno dei nodi in listanodi
      si puo' raggiungere goal senza passare per nodi di visited *)
let test_connessi graph start goal =
    let rec search visited = function
        [] -> false
      | n::rest -> 
            if List.mem n visited 
            then search visited rest 
      else n=goal ||
              search (n::visited) 
                        ((successori n graph) @ rest)
    in search [] [start]

(*============ Es2:esiste-ciclo =============*)

(* la funzione non deve riportare un cammino, ma solo un bool: e'
   sufficiente adattare un algoritmo di visita.
   Pero' si deve partire dai successori del nodo di partenza, 
   e non inserire questo inizialmente tra i nodi visitati *)

(* cerca: 'a list -> 'a list -> bool
   cerca visited listanodi = true se da uno dei nodi in listanodi
     si puo' raggiungere start senza passare per nodi in visited *)
let esiste_ciclo grafo start =
  let rec cerca visited = function
      [] -> false
    | n::rest ->
  if List.mem n visited
  then cerca visited rest
  else n=start or
    cerca (n::visited)
      ((successori n grafo)@rest)
  in cerca [] (successori start grafo)

(*============ Es3:ciclo =============*)
 
(* analogo al precedente, ma adattando la ricerca di cammini.
   Si cerca un cammino fino a start a partire dai suoi successori,
   senza inserire start tra i visitati. Se la ricerca ha successo,
   si inserisce start in testa al cammino trovato *)

exception NotFound
(* ciclo : 'a graph -> 'a -> 'a list *)
(* from_node: 'a list -> 'a -> 'a list
   from_node visited n = cammino non ciclico da n a start che non passa per 
         alcun nodo in visited
   from_list: 'a list -> 'a list -> 'a list
   from_node visited listanodi = cammino non ciclico da uno dei nodi in
        listanodi fino a start che non passa per alcun nodo in
        visited *)
let ciclo graph start =
    let rec from_node visited n = 
      if List.mem n visited 
      then raise NotFound
      else if n=start then [n]
      else n::from_list (n::visited) (successori n graph)
    and from_list visited = function
        [] -> raise NotFound
      | n::rest ->
          try from_node visited n
          with NotFound -> from_list (n::visited) rest
    in start::from_list [] (successori start graph)

(*============ Es4:grafo-connesso =============*)

type 'a graph2 = 'a list * ('a * 'a) list

(* la funzione vicini utilizzata qui sotto e' quella definita
   all'inizio del file, verra' applicata soltanto alla componente
   del grafo rappresentante la lista degli archi *)

(* funzione ausiliaria per verificare se esiste un
   cammino da un nodo start a un nodo goal
   (modifica dell'es. 1 per grafi non orientati. 
   Il primo argomento e' costituito dal una lista di archi *)
(* raggiungibile : ('a * 'a)  list -> 'a -> 'a -> bool *)
(* raggiungibile archi start goal = true se goal e' raggiungibile
      da start in un grafo avente gli archi dati come primo argomento *)
(* cerca: 'a list -> 'a list -> bool *)
(* cerca visited listanodi = true se da uno dei nodi in listanodi
      si puo' raggiungere goal senza passare per nodi di visited *)
let raggiungibile archi start goal =
  let rec cerca visited = function
      [] -> false
    | n::rest ->
  if List.mem n visited
  then cerca visited rest
  else n=goal ||
    cerca (n::visited)
      ((vicini n archi)@rest)
  in cerca [] [start]

(* grafo_connesso : 'a graph2 -> bool *)
(* aux: 'a list -> bool
   aux listanodi = true se da ciascun nodo in listanodi e' raggiungibile
      il nodo che lo segue in listanodi stessa *)
let grafo_connesso (nodi,archi) =
  let rec aux = function
      [] | [_] -> true
    | n::m::rest ->
  raggiungibile archi n m && aux (m::rest)
  in aux nodi

(* oppure *)
let grafo_connesso (nodi,archi) =
  let n = List.hd nodi in
  List.for_all (raggiungibile archi n) (List.tl nodi)

(*============ Es5:miss-cann =============*)
(* missionari e cannibali *)

(* definizioni preliminari *)
type obj = Miss | Cann | Barca
type situazione = obj list * obj list 
let initial = ([Miss;Miss;Miss;Cann;Cann;Cann;Barca], [])
type azione =
    From_left of obj list
  | From_right of obj list

(* Riporto qui il codice dell'esercizio 4 del gruppo 7,
   con le definizioni di safe, applica e from_sit *)
(* conta : 'a -> 'a list -> int *)
(* conta x list = numero di occorrenze di x in lst *)
let conta x lst =
  List.length (List.filter ((=) x) lst)

(*  safe : situazione -> bool
    safe sit = true se la situazione sit e' sicura (in nessuna delle due
        rive i missionari, se presenti, sono in numero inferiore ai cannibali *)
(* aux: obj list -> bool
   aux riva = true se nella lista riva il numero di messionari, se
      diverso da 0, non e' inferiore al numero di cannibali *)
let safe (left,right) =
  let aux riva =
    let miss = conta Miss riva
    in miss=0 || miss >= conta Cann riva
  in aux left && aux right

exception Impossible
(* togli_un : 'a -> 'a list -> 'a list *)
(* togli x lst : elimina un'occorrenza di x dalla lista lst *)
let rec togli_un x = function
    [] -> raise Impossible
  | y::rest -> 
      if y=x then rest
      else y::togli_un x rest
(* togli : 'a list -> 'a list -> 'a list
   togli source lst = elimina da source un'occorrenza di ogni elemento
                      di lst *)
let rec togli source = function
    [] -> source
  | x::rest ->
      togli (togli_un x source) rest

(* applica : azione -> situazione -> situazione 
   applica act sit riporta la situazione che si ottiene applicando
   l'azione act a sit; solleva Impossible se l'azione non e'
   applicabile o se la situazione risultante non e' sicura *)
let applica act (left,right) =
  let result = 
    match act with
      From_left lst ->
  if List.length lst > 2 || lst=[]
  then raise Impossible
  else (togli_un Barca (togli left lst),
        Barca::lst @ right)
    | From_right lst ->
  if List.length lst > 2 || lst=[]
  then raise Impossible
  else (Barca::lst @ left,
        togli_un Barca (togli right lst))
  in if safe result then result
  else raise Impossible

(* 
# applica (From_left [Miss]) initial;;
Exception: Impossible.
# applica (From_left [Miss;Cann]) initial;;
- : obj list * obj list = ([Miss; Miss; Cann; Cann], [Barca; Miss; Cann])
*)

(* actions: azione list *)
(* tutte le possibili azioni *)
let actions =
  let elems =
    [[Miss];[Cann];[Miss;Cann];[Miss;Miss];[Cann;Cann]]
  in (List.map (function x -> From_left x) elems)
  @ (List.map (function x -> From_right x) elems)

(* from_sit : situazione -> situazione list
   from_sit sit : genera tutte le situazioni che si possono ottenere
   applicando un'azione possibile a sit *)
(* aux: azione list -> situazione list
   aux actlist = lista delle situazioni safe che risultano
                dall'applicazione di tutte le azioni in actlist
                applicabili alla situazione sit (parametro della 
                principale) *)
let from_sit sit =
  let rec aux = function
      [] -> []
    | a::rest ->
  try applica a sit :: aux rest
  with Impossible -> aux rest
  in aux actions

(** qui inizia la parte nuova **)
(* goal: situazione -> bool 
   goal sit = true se sit e' l'obiettivo
   (tutti gli elmenti a destra) *)
let goal (sinistra,_) =
  sinistra = []

(* miss_cann: unit -> situazione list *)
(* aux: situazione list -> situazione -> situazione list
   aux visited sit = lista senza ripetizioni di situazioni che vanno da sit alla
     situazione obiettivo, senza passare da situazioni in visited.
     Ogni situazione della lista si ottiene dalla precedente
     applicandogli un'azione possibile.
   auxlist: situazione list -> situazione list -> situazione list
   auxlist visited sitlist = lista senza ripetizioni di situazioni 
     che vanno da una situazione in sitlist alla situazione obiettivo, 
     senza passare da situazioni  in visited.
     Ogni situazione della lista si ottiene dalla precedente
     applicandogli un'azione possibile. *)
let miss_cann () =
  let rec aux visited sit =
    if List.mem sit visited
    then raise Impossible
    else if goal sit then [sit]
    else sit::auxlist (sit::visited) (from_sit sit)
  and auxlist visited = function
      [] -> raise Impossible
    | sit::rest -> 
  try aux visited sit 
  with Impossible -> auxlist visited rest
  in aux [] initial
 
(* il test List.mem per le situazioni non e` il migliore,
   si dovrebbe ignorare l'ordine degli elementi nelle liste *)
(* equal: situazione -> situazione -> bool *)
(* equal sit sit2 = sit e sit2 rappresentano la stessa situazione,
     ignorando l'ordine  degli elementi nelle liste *)
let equal (sin,dx) (sinistra,destra) =
  List.sort compare sin = List.sort compare sinistra
    && List.sort compare dx = List.sort compare destra

(* la funzione che segue differisce dalla precedente solo perche'
   controlla l'appartenenza di una situazione a visited mediante
   il test equal e non =. Specifiche e tipi di aux e auxlist sono
   come sopra, modulo la differenza del test *)
let miss_cann () =
  let rec aux visited sit =
    if List.exists (equal sit) visited
    then raise Impossible
    else if goal sit then [sit]
    else sit::auxlist  (sit::visited) (from_sit sit)
  and auxlist  visited = function
      [] -> raise Impossible
    | sit::rest -> 
  try aux visited sit 
  with Impossible -> auxlist visited rest
  in aux [] initial
 
(* Versione in cui si vuole riportare la lista delle azioni *)
(* modifichiamo innanzitutto from_sit
   from_sit2 : situazione -> (azione * situazione) list
   from_sit sit : genera tutte le coppie (act,newsit)
       dove newsit si ottiene applicando l'azione act a sit *)
(* aux: azione list -> (azione * situazione) list 
   aux azioni = riporta la lista di coppie (act,newsit) per act in
       azioni e newsit ottenuta applicando act a sit *)
let from_sit2 sit =
  let rec aux = function
      [] -> []
    | a::rest ->
  try (a,applica a sit) :: aux rest
  with Impossible -> aux rest
  in aux actions

(* miss_cann2: unit -> azione list *)
(* aux: situazione list -> azione * situazione -> azione list
   aux visited (act,sit) = lista di azioni contenente act (l'azione 
     che ha portato a sit), seguita da quelle che portano da sit alla
     situazione obiettivo, senza passare da situazioni in visited.
   auxlist: situazione list -> (azione * situazione) list -> azione list
   auxlist visited actsitlist = lista di azioni che portano da una situazione
     sit in actsitlist alla situazione obiettivo, preceduta dall'azione accoppiata
     a sit in actlist, senza passare da situazioni in visited. *)
let miss_cann2 () =
  let rec aux visited (azione,sit) =
    if List.exists (equal sit) visited
    then raise Impossible
    else if goal sit then [azione]
    else azione::auxlist (sit::visited) (from_sit2 sit)
  and auxlist visited = function
      [] -> raise Impossible
    | pair::rest -> 
  try aux visited pair
  with Impossible -> auxlist visited rest
  in let ini = initial in
  if goal ini then []
  else auxlist [ini] (from_sit2 ini)

(*============ Es6:hamiltoniano =============*)
type 'a graph2 = 'a list * ('a * 'a) list


(* la funzione successori usata sotto e' quella definita
   all'inizio del file, e viene applicata alla componente
   archi del grafo *)

(* a -- cammino che passa per ogni nodo di lista esattamente 
        una volta *)
exception NotFound

(* cammino : 'a graph2 -> 'a list -> 'a -> 'a -> 'a list *)
(* from_node:'a -> 'a list -> 'a list
   from_node  x lista = cammino da x a goal che passa
       per tutti i nodi di lista esattamente una volta 
   from_list: 'a list -> 'a list -> 'a list
   from_list  lista listanodi = cammino da uno dei nodi
       di listanodi fino a goal, che passa per tutti i nodi
       di lista esattamente una volta *)
let cammino (nodi,archi) lista start goal =
  let rec from_node  x lista = 
    if not(List.mem x lista)
    then raise NotFound
    else 
      if x=goal && lista=[x] then [x]
      else let nuova=List.filter ((<>)x) lista
         (** si assume che la lista sia senza ripetizioni **)
      in x::from_list  nuova (successori x archi)
  and from_list  lista = function
      [] -> raise NotFound
    | x::rest ->
  try from_node  x lista
  with NotFound -> from_list  lista rest
  in from_node start lista

(** evidentemente, se in un grafo esiste un ciclo hamiltoniano,
   allora per qualsiasi nodo N esiste un cammino che parte
   da uno dei successori di N, e torna a N passando esattamente una
   volta per ciascun nodo del grafo **)

(* hamiltoniano: 'a graph -> 'a list *)
(* from_list: 'a list -> 'a list -> 'a list
   from_list lista listanodi = cammino da uno dei nodi in listanodi
     fino a n che passa per ciascun elemento di lista esattamente
     una volta *)
let hamiltoniano ((nodi,archi) as g) =
  let n = List.hd nodi in
      (* n e' un nodo qualsiasi *)
  let rec from_list lista = function
      [] -> raise NotFound
    | x::rest ->
  try cammino g lista x n 
  with NotFound -> from_list lista rest in
  n::from_list nodi (successori n archi)

(*============ Es7:colori-alterni =============*)

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
(* colore: 'a -> 'a col_assoc -> col *)
(* colore x colassoc = colore di x secondo colassoc *)
let rec colore x = function
    [] -> failwith "colore"
  | (c,list)::rest -> 
      if List.mem x list then c
      else colore x rest

(* successori_diversi: 'a col_assoc -> 'a -> 'a graph -> 'a list *)
(* successori_diversi colori nodo grafo = successori di nodo in grafo
        che hanno colore diverso da quello di nodo *)
let successori_diversi colori nodo grafo =
  let col = colore nodo colori in
  List.filter 
    (function x -> colore x colori <> col)
    (successori nodo grafo)
 
exception NotFound

(* cammino_colorato: 'a graph -> 'a col_assoc -> 'a -> 'a -> 'a list 
   from_node: 'a list -> 'a -> 'a list
   from node visited x = cammino non ciclico a colori alterni da x a goal,
        che non passa da alcun nodo di visited
   from_list: 'a list -> 'a list -> 'a list
   from_list visited listanodi = cammino non ciclico a colori alterni da un nodo
         in listanodi fino a goal, che non passa da alcun nodo di visited *)
let cammino_colorato g colori start goal =
  let rec from_node visited x = 
    if List.mem x visited 
    then raise NotFound
    else 
      if x=goal then [x]
      else x::from_list (x::visited)  
    (successori_diversi colori x g)
  and from_list visited  = function
      [] -> raise NotFound
    | x::rest ->
  try from_node visited x 
  with NotFound -> from_list visited  rest
  in from_node [] start 

(*============ Es8:connessi-in-glist =============*)

(* utilizziamo la funzione test_connessi dell'esercizio 1 *)
(* test_connessi: 'a graph -> 'a -> 'a -> bool *)
let test_connessi graph start goal =
    let rec search visited = function
        [] -> false
      | n::rest -> 
            if List.mem n visited 
            then search visited rest 
      else n=goal ||
              search (n::visited) 
                        ((successori n graph) @ rest)
    in search [] [start]

(* connessi_in_glist: 'a graph list -> 'a -> 'a -> bool *)
let connessi_in_glist listagrafi b c =
  List.exists 
    (function g -> 
      test_connessi g b c || test_connessi g c b)
    listagrafi

(*============ Es9:cammino-con-nodi =============*)

(* La differenza con l'esercizio 6a sta semplicemente
   nel fatto che la lista puo' qui contenere anche altri nodi e non
   e' dato un nodo specifico da raggiungere: ci si puo' fermare appena
   sono stati incontrati tutti i nodi presenti nella lista *)
(* cammino_con_nodi : 'a graph -> 'a  -> 'a list -> 'a list *)
(* from_node: 'a list -> 'a list -> 'a -> 'a list
   from_node visited lst x = cammino senza cicli che parte da x
      e passa per tutti i nodi di lst, senza passare per nodi di
      visited.
   from_list: 'a list -> 'a list -> 'a list-> 'a list
   from_node visited lst listanodi = cammino senza cicli che parte da 
      un nodo di listanodi e passa per tutti i nodi di lst, 
      senza passare per nodi di visited. *)
exception NotFound
let cammino_con_nodi g start lista =
  let rec from_node visited lst x = 
    if List.mem x visited 
    then raise NotFound
    else 
      if lst=[x] then [x]
         (* lista esaurita => cammino trovato *)
      else x::from_list (x::visited)  
    (List.filter ((<>) x) lst)
    (successori x g)
  and from_list visited lst = function
      [] -> raise NotFound
    | x::rest ->
  try from_node visited lst x 
  with NotFound -> from_list visited lst rest
  in from_node [] lista start 

(*============ Es10:veryHard =============*)

(** qui sotto la soluzione dell'esercizio 3 del Gruppo 7 *)
(** Ridenomino la funzione successori in veryHardSucc, per non
    fare confusione con la funzione successori generale per i grafi *)
type chiave = Aperta | Chiusa
type cassaforte = chiave list
exception Fail
(* gira: chiave -> chiave, gira una chiave *)
let gira = function
    Aperta -> Chiusa
  | Chiusa -> Aperta
(* giraPrima: cassaforte -> cassaforte, riporta la configurazione
che si ottiene girando la prima chiave *)
let giraPrima = function
    x::rest -> (gira x)::rest
  | _ -> raise Fail
(*giraDopoChiusa: cassaforte -> cassaforte,  riporta la configurazione
che si ottiene girando la chiave che segue la prima chiusa. 
Fallisce se non Ã¨ possibile eseguire lâ€™operazione *)
let rec giraDopoChiusa = function
    Chiusa::x::rest ->
      Chiusa::(gira x)::rest
  | Aperta::rest ->
      Aperta::(giraDopoChiusa rest)
  | _ -> raise Fail
(* veryHardSucc: cassaforte -> cassaforte list *)
(* la funzione chiamata successori nell'es 3 del gruppo 7 *)
let veryHardSucc config =
  (giraPrima config)::
  (try [giraDopoChiusa config]
  with Fail -> [])

(** qui inizia la parte nuova **)
(* a *)
(* se cassaforte ha n chiavi, ci sono 2^n configurazione diverse *)
(* nodi : int -> cassaforte list *)
let rec nodi n =
  if n=0 then [[]]
  else let tmp = nodi (n-1) in
  (List.map (function c -> Aperta::c) tmp)
  @ (List.map (function c -> Chiusa::c) tmp)

(* b *)
(* archi: int -> (cassaforte * cassaforte list) list *)
let archi n =
  List.map (function x -> (x,veryHardSucc x)) (nodi n)

(* c *)
(* start: int -> cassaforte *)
let rec start n =
  if n = 0 then []
  else Chiusa::(start(n-1))
(* aperta: cassaforte -> bool *)
let aperta c =
  List.for_all (function x -> x=Aperta) c

(* d *)
(* apri: int -> cassaforte list *)
(* aux: cassaforte list -> cassaforte -> cassaforte list
   aux visitati c = sequenza senza ripetizioni di cassaforti, da c fino alla
      cassaforte aperta, che non passa per alcuna cassaforte
      di visitati. Ogni cassaforte si ottiene dalla precedente
      con una delle due azioni ammesse. 
   auxlist visitati clist =  sequenza senza ripetizioni di cassaforti, 
      da una di quelle in clist, fino alla cassaforte aperta, 
      che non passa per alcuna cassaforte di visitati. *)
let apri n =
 let rec aux visitati c =
   if List.mem c visitati then raise Fail
   else if aperta c then [c]
   else c::auxlist (c::visitati) (veryHardSucc c)
 and auxlist visitati = function
     [] -> raise Fail
   | c::rest ->
       try aux visitati c
       with Fail -> auxlist visitati rest
 in aux [] (start n)

(*============ Es11:cammino-di-primi =============*)

(* primo: int -> bool,
   primo n = true se n e' primo *)
let primo n =
  (* aux: int -> bool
     aux k = n non e' divisibile per alcun numero compreso
             tra 2 e k (inclusi) *)
  let rec aux = function
      1 -> true
    | k -> (n mod k)<>0 && aux (k-1)
  in n=1 or aux (n/2)

exception NotFound

(* cammino_di_primi : int graph -> int -> int -> int list *)
(* from_node: int list -> int -> int list
   from_node visited x = cammino non ciclico da x fino a goal che passa
      solo per numeri primi e non passa per alcun numero in visited.
   from_list: int list -> int list -> int list
   from_list visited lista = cammino non ciclico da uno dei nodi in lista
      fino a goal che passa solo per numeri primi e non passa per 
      alcun numero in visited. *)
let cammino_di_primi g  start goal =
  let rec from_node visited x  = 
    if List.mem x visited || not(primo x)
    then raise NotFound
    else (* x e' primo *)
      if x=goal then [x]
      else x::from_list (x::visited) (successori x g)
  and from_list visited = function
      [] -> raise NotFound
    | x::rest ->
  try from_node visited x 
  with NotFound -> from_list visited  rest
  in from_node [] start 

(* in alternativa, si puo' filtrare la lista dei successori rispetto a
   primo: 
   le specifiche e i tipi di from_node e from_list sono come sopra
*)

let cammino_di_primi_primi g  start goal =
  let rec from_node visited x  = 
    if List.mem x visited 
    then raise NotFound
    else 
      if x=goal then [x]
      else x::from_list (x::visited) 
    (List.filter primo (successori x g))
  and from_list visited = function
      [] -> raise NotFound
    | x::rest ->
  try from_node visited x 
  with NotFound -> from_list visited  rest
  (* occorre controllare start a parte *)
  in if primo start then from_node [] start 
  else raise Fail

(*============ Es12:non-contradictory-path =============*)

type form = Prop of string | Not of form 
        | And of form * form | Or of form * form 

(* funzione ausiliaria *)
(* contradictory: form list -> form -> bool *)
(* contradictory lista f = true se l'aggiunta di f a lista darebbe
   una lista contraddittoria *)
let contradictory lista = function
    Not f -> List.mem (Not(Not f)) lista or List.mem f lista
  | f -> List.mem (Not f) lista

(* nello "schema" di ricerca di cammino, la lista visited puo' memorizzare
   il cammino percorso (alla rovescia). Si puo' usare per controllare
    se il complemento o la negazione di f appartengono al cammino *)
(* non_contradictory_path : form graph -> form -> form -> form list *)
(* from_node: form list -> form -> form list
   from_node visited f = cammino non ciclico P da f a goal che non passa per alcun
        nodo in visited e tale la lista visited @ P e' non contraddittoria
   from_list: form list -> form list -> form list
   from_list visited flist = cammino non ciclico P da una delle formule in 
        flist fino a goal, che non passa per alcun nodo in visited e tale 
        che visited @ P  e' una lista non contraddittoria *) 
let non_contradictory_path g  start goal =
  let rec from_node  visited x  = 
    if List.mem x visited 
  or contradictory visited x
    then raise NotFound
    else 
      if x=goal then [x]
      else x::from_list (x::visited) (successori x g)
  and from_list visited = function
      [] -> raise NotFound
    | x::rest ->
  try from_node visited x 
  with NotFound -> from_list visited  rest
  in from_node [] start 

(*============ Es13:path-n-p =============*)

(* path_n_p : 'a graph -> ('a -> bool) -> int -> 'a -> 'a list *)
(* from_node: 'a list -> int -> 'a -> 'a list
   from_node visited k x = cammino non ciclico da x a goal, che non passa per alcun
      nodo in visited e che contiene k nodi che soddisfano p 
   from_list: 'a list -> int -> 'a list -> 'a list
   from_list visited k listanodi = cammino non ciclico da un nodo di listanodi
     fino a goal, che non passa per alcun nodo in visited e che contiene k nodi 
     che soddisfano p *)
exception NotFound
let path_n_p g p n start =
  let rec from_node visited k x  = 
    if List.mem x visited 
    then raise NotFound
    else 
      if p x && k=1 then [x]
      else 
  x::from_list (if p x then k-1 else k) (x::visited) (successori x g)
  and from_list k visited = function
      [] -> raise NotFound
    | x::rest ->
  try from_node visited k x 
  with NotFound -> from_list k visited rest
  in from_node [] n start 

(* Si noti che questo e' uno dei casi in cui non si deve aggiungere x
   a visited nel secondo caso di from_list: supponiamo di cercare un
   cammino non ciclico con 2 nodi che soddisfano p, a partire da un
   dato nodo start, in un grafo con archi (start,x) (x,start) (start,y) (y,x).
   E supponiamo che x e y soddisfano p, mentre start non lo soddisfa.
   Cerchiamo prima il cammino start -> x ->  .... 
   Ma qui si fallisce perche' torniamo a start con un solo nodo
   che soddisfa p.  Allora proviamo con start -> y -> x.  Ma se quando
   passiamo a considerare il fratello y di x abbiamo inserito x tra i
   visitati, ora si fallisce e non troviamo il cammino che invece esiste.

   Si provi con le seguenti definizioni:
   let g = [(1,2);(2,1);(1,3);(3,2)]
   let p = function  2|3 -> true
                     | _ -> false
   La valutazione di path_n_p g p 2 1 con la definizione data sopra riporta [1; 3; 2].
   Mentre fallisce se si modifica la definizione di from_list:
           and from_list k visited = function
               [] -> raise NotFound
             | x::rest ->
                        try from_node visited k x 
                        with NotFound -> from_list k (x::visited) rest
*)