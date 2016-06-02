(*------------- Gruppo7 -------------*)


(*============ Es1:esegui =============*)

(* definizioni preliminari *)
type direzione = Su | Giu | Destra | Sinistra
type azione =
    Gira
  | Avanti of int;;
let gira = function
    Su -> Destra
  | Giu -> Sinistra
  | Destra -> Giu
  | Sinistra -> Su
let avanti (x,y,dir) n =
  match dir with
    Su -> (x,y+n,dir)
  | Giu -> (x,y-n,dir)
  | Destra -> (x+n,y,dir)
  | Sinistra -> (x-n,y,dir)
let sposta (x,y,dir) act =
  match act with
    Gira -> (x,y,gira dir)
  | Avanti n -> avanti (x,y,dir) n

(* esegui: posizione -> azione list -> posizione *)
let rec esegui pos = function
    [] -> pos
  | a::rest ->
     (* esegui rest a partire dalla posizione che si
        ottiene da pos eseguendo a *)
      esegui (sposta pos a) rest


(*============ Es2:prodotto =============*)
(* definizioni preliminari *)
type nat = Zero | Succ of nat;;
let rec somma n m = 
  match n with
     Zero -> m
   | Succ k -> Succ(somma k m);;
(* prodotto: nat -> nat -> nat *)
let rec prodotto n m =
  match n with
    Zero -> Zero
  | Succ k -> somma m (prodotto k m)


(*============ Es3:cassaforti =============*)

type chiave= Aperta | Chiusa

(* a *)

exception Fail
(* gira: chiave -> chiave, 
   gira una chiave *)
let gira = function
    Aperta -> Chiusa
  | Chiusa -> Aperta

(* giraPrima: cassaforte -> cassaforte *)
let giraPrima = function
    x::rest -> (gira x)::rest
  | _ -> raise Fail

(* b *)

(* giraDopoChiusa: cassaforte -> cassaforte *)
let rec giraDopoChiusa = function
    Chiusa::x::rest ->
      Chiusa::(gira x)::rest
      (* oppure Chiusa::giraPrima(x::rest) *)
  | Aperta::rest ->
      Aperta::(giraDopoChiusa rest)
  | _ -> raise Fail

(* c *)

(* successori: cassaforte -> cassaforte list *)
let successori config =
  (giraPrima config)::
  (try [giraDopoChiusa config]
   with Fail -> [])



(*============ Es4:miss-cann =============*)

(* definizioni preliminari *)
type obj = Miss | Cann | Barca
type situazione = obj list * obj list 
let initial = ([Miss;Miss;Miss;Cann;Cann;Cann;Barca], [])
type azione =
    From_left of obj list
  | From_right of obj list

(* (a) Per definire safe dobbiamo contare i missionari e
       i cannibali sulle due rive *)
(*  conta : 'a -> 'a list -> int *)
(* conta x list = numero di occorrenze di x in lst *)
let rec conta x = function
    [] -> 0
  | y::rest ->
      if x=y then 1+conta x rest
      else conta x rest

(* oppure: *)
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

(* (b) *)
(* eccezione sollevata quando un'azione non e` applicabile *)
exception Impossible
(* quando si sposta un miss o cann, un'occorrenza di Miss (o Cann) 
   viene tolta da una lista e aggiunta  all'altra *)
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

(* applica : azione -> situazione -> situazione  *)
let applica act (left,right) =
  (* result e' la situazione che risulterebbe dall'applicazione 
     di act a (left,right), ma potrebbe non essere safe *)
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

(* (c) *)

(* actions: azione list *)
(* variabile globale: tutte le possibili azioni *)
let actions =
  let elems =
    [[Miss];[Cann];[Miss;Cann];[Miss;Miss];[Cann;Cann]]
  in (List.map (function x -> From_left x) elems)
  @ (List.map (function x -> From_right x) elems)

(* from_sit : situazione -> situazione list *)
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


(*============ Es5:most-general-match =============*)

type 'a pattern = Jolly | Val of 'a

let rec most_general_match l1 l2 =
  match (l1,l2) with
    ([],[]) -> []
  | (x::r1,y::r2) ->
      if x=y then Val x::most_general_match r1 r2
      else Jolly::most_general_match r1 r2
  | _ -> failwith "most general match"


