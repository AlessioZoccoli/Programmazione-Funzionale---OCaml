(*------------- Gruppo6 -------------*)

(* i tipi delle funzioni sono quelli dichiarati nel testo *)

(*============ Es1a:find =============*)

let rec find p = function
    [] -> failwith "find"
  | x::rest -> 
      if p x then x else find p rest

let find_applicata intlist =
  find (function x -> x*x < 30) intlist

(*============ Es1b:takewhile =============*)

let rec takewhile p = function
    [] -> []
  | x::rest ->
      if p x then x::takewhile p rest
      else []

(*============ Es1c:dropwhile =============*)

let rec dropwhile p = function
    [] -> []
  | x::rest as lst ->
      if p x then dropwhile p rest
      else lst

(*============ Es1d:partition =============*)

let rec partition p = function
    [] -> ([],[])
  | x::rest -> 
      let (yes,no) = partition p rest
      in if p x then (x::yes,no)
      else (yes,x::no)

(*============ Es1e:pairwith =============*)

let pairwith x lst =
  List.map (fun y -> (x,y)) lst

(*============ Es1f:esiste_tutti =============*)

let verifica_matrice n mat =
  List.exists  (List.for_all (function x -> x<n)) mat

(* che e' come dire: *)

let verifica_matrice n mat =
  List.exists 
    (function row -> List.for_all (function x -> x<n) row)
  mat

(*============ Es1g:setdiff =============*)

let setdiff set subset =
  List.filter (fun x -> not(List.mem x subset)) set

(*============ Es1h:subset =============*)

let subset sub set =
  List.for_all (fun x -> List.mem x set) sub

(*============ Es1i:duplica =============*)

let duplica lst =
  List.map (fun x -> x*2) lst

(*============ Es1j:mapcons =============*)

let mapcons lst x =
  List.map (function (y,l) -> (y,x::l)) lst

(*============ Es1k:tutte_liste_con =============*)

(* cons: 'a -> 'a list -> 'a list*)
let cons x rest = x::rest

(* tutte_liste_con : int -> 'a -> 'a -> 'a list list *)
let rec tutte_liste_con n x y =
  if n=0 then [[]] (* c'e' un'unica lista di lunghezza 0 *)
  else let tmp = tutte_liste_con (n-1) x y
  in (List.map (cons x) tmp) @ (List.map (cons y) tmp)

(* da ogni lista di lunghezza n-1 contenente solo x e y
   si possono ottenere due liste di lunghezza n contenenti solo
   x e y: una mettendoci x e l'altra mettendoci y *)

(*============ Es1l:interleave =============*)
(* interleave: 'a -> 'a list -> 'a list list
   interleave x lst = lista con tutte le liste che si ottengono
                      inserendo x da qualche parte in lst *)
let rec interleave x = function
    [] -> [[x]] (* c'e' un unico modo di inserire x nella lista vuota *)
  | y::rest -> 
      (x::y::rest) ::
      (List.map (cons y) (interleave x rest))

(* esempio per ragionare sul caso ricorsivo:
    x=0 e y::rest = [1;2;3] = 1::[2;3].
   La chiamata ricorsiva interleave x rest = interleave 0 [2;3]
   riporta [[0;2;3];[2;0;3];[2;3;0]].
   In ciascun elemento di questa lista si deve rimettere 1 in testa
     (List.map (cons y) (interleave x rest))
   ottenendo:
        [[1;0;2;3];[1;2;0;3];[1;2;3;0]]
   Manca solo la lista che ha 0 in testa: [0;1;2;3], che viene
   aggiunta in testa. *)

(*============ Es1m:permut =============*)

(* utilizziamo la funzione interleave dell'esercizio precedente *)
(* Ragionamento ricorsivo:
   la lista vuota ha una sola permutazione, se stessa.
   Per il caso ricorsivo ragioniamo su un esempio:
      x::rest -> [1;2;3] = 1::[2;3]
    La chiamata ricorsiva costruisce tutte le permutazioni di [2;3]:
        [[2;3];[3;2]]
    Da ogni elemento di questa lista si possono ottenere diverse permutazioni
    di [1;2;3], inserendo 1 in ogni posizione possibile nella lista:
        interleave 1 [2;3] = [[1;2;3];[2;1;3];[2;3;1]]
        interleave 1 [3;2] = [[1;3;2];[3;1;2];[3;2;1]]
    Quindi List.map (interleave 1)  [[2;3];[3;2]] =
        [[[1;2;3];[2;1;3];[2;3;1]]; [[1;3;2];[3;1;2];[3;2;1]]]
    Dobbiamo togliere un po' di parentesi:
      List.flatten [[[1;2;3];[2;1;3];[2;3;1]]; [[1;3;2];[3;1;2];[3;2;1]]]
         = [[1;2;3];[2;1;3];[2;3;1];[1;3;2];[3;1;2];[3;2;1]]
     Ed e' il risultato voluto *)

(* permut : 'a list -> 'a list list *)
let rec permut = function
    [] -> [[]]
  | x::rest ->
      List.flatten
  (List.map (interleave x) (permut rest))

(*============ Es2a:in_riga =============*)

(* esempio di matrice con contenuti di tipo 'a  *)
type 'a mat = int * ((int * int) * 'a ) list
let labirinto = (5,
     [((1,0),"oro");
      ((3,1),"oro");
      ((4,3),"oro");
      ((0,1),"argento");
      ((2,4),"argento");
      ((0,2),"mostro");
      ((1,1),"mostro");
      ((1,3),"mostro");
      ((2,3),"mostro");
      ((3,0),"mostro");
      ((4,2),"mostro")])

(* in_riga: 'a mat -> int -> 'a -> bool *)
let in_riga (_,assoc) riga valore =
  List.exists
    (function ((r,_),v) -> r=riga && v=valore)
    assoc

(*============ Es2b:trova_colonna =============*)

(* trova_colonna: 'a mat -> int -> 'a -> 'a *)
let trova_colonna (_,assoc) riga valore =
  snd (fst (List.find 
        (function ((r,_),v) -> r=riga && v=valore)
        assoc))

(*============ Es2c:in_tutte =============*)

(* upto : int -> int -> int list *)
(* upto inizio fine = lista dei numeri compresi tra inizio e fine *)
let rec upto inizio fine =
  if inizio > fine then []
  else inizio:: upto (inizio+1) fine

(* utilizziamo la funzione in_riga dell'esercizio 2a: *)
(* in_riga: 'a mat -> int -> 'a -> bool *)
let in_riga (_,assoc) riga valore =
  List.exists
    (function ((r,_),v) -> r=riga && v=valore)
    assoc

(* in_tutte : 'a mat -> 'a -> bool *)
let in_tutte matrice valore =
  let (dim,_) = matrice in
  List.for_all
    (function riga -> in_riga matrice riga valore)
    (upto 0 (dim-1))

(*============ Es3a:find =============*)

(* find: 'a -> 'a list -> 'a list * 'a list *)
let rec find x = function
    [] -> failwith "Errore"
  | y::rest -> 
      if x=y then ([],rest)
      else let (prima,dopo) = find x rest
      in (y::prima,dopo)


(*============ Es3b:spezza =============*)

(* la funzione
   find: 'a -> 'a list -> 'a list * 'a list 
   e` quella definita al punto precedente *)

(* spezza:  'a -> 'a list -> 'a list * 'a list *)
let spezza x lst =
  find x (snd (find x lst))

(*============ Es4:prendi =============*)

let rec prendi p = function
    [] -> failwith "prendi"
  | x::rest ->
      if p x then (x,rest)
      else let (y,result) = prendi p rest
      in (y,x::result)

(* oppure, tail recursive (l'ordine degli elementi nel risultato 
   non e' importante) *)
(* aux: 'a list -> 'a list -> 'a * 'a list
   aux result lista = (x,resto) dove x e' il primo elemento di lista
                      che soddisfa p e resto contiene tutti gli elementi
                      di result e tutti quelli di lista, eccetto un'occorrenza
                      di x *)
let prendi p lst =
  let rec aux result = function
      [] -> failwith "prendi"
    | x::rest ->
  if p x then (x,rest@result)
  else  aux (x::result) rest
   in aux [] lst

