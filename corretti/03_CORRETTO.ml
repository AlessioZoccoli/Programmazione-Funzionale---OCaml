(*============ Es1:somma-di-ore =============*)

(* verifica: int * int -> bool *)
(* verifica coppia = true se coppia rappresenta correttamente
   un'ora *)
let verifica (h,m) =
  h>=0 && h<24 && m>=0 && m<60

(* sottoproblema: dati due interi m1 e m2 compresi tra 0 e 59, riporta una*)
(* coppia (h,m) che rappresenta la somma di m1 e m2 in ore, minuti *)
(* somma_minuti : int -> int -> int * int *)
let somma_minuti m1 m2 =
  let m = m1+m2               (* sia m=m1+m2 *)
  in (m/60, m mod 60)         (* nell'espressione (m/60, m mod 60) *)

(* somma_ore : (int * int) -> (int * int) -> int * int *)
let somma_ore (h1,m1) (h2,m2) =
  if verifica (h1,m1) && verifica (h2,m2)
  then
     let (h3,m3) = somma_minuti m1 m2
     in ((h1+h2+h3) mod 24, m3)
  else failwith "Formato scorretto"

(* oppure identifichiamo come sottoproblema utile quello di 
   "normalizzare" un orario *)
(*  normalizza : int * int -> int * int *)
let normalizza (h,m) =
  ((h+m/60) mod 24, m mod 60)

let somma_ore (h1,m1) (h2,m2) =
  if verifica (h1,m1) && verifica (h2,m2)
  then normalizza (h1+h2,m1+m2)
  else failwith "Formato scorretto"

(*============ Es2a:read-max =============*)

(* read_max: unit -> int *)
(* aux implementa un ciclo, il suo parametro e' il massimo provvisorio *)
(* aux :  int -> int *)

let read_max () =
  let rec aux n =
    try (* se non si immette un numero, viene sollevata un'eccezione
     e il ciclo termina, riportando n *)
      let k = read_int()
      in aux (max n k)
    with _ -> n
  in try aux (read_int())
     with _ -> failwith "Sequenza vuota"

(* la funzione precedente implementa un algoritmo iterativo.
   Un algoritmo ricorsivo ragionerebbe cosi':
   leggo un numero n,
   provo a calcolare il massimo dei successivi,
   se esiste, riporto il massimo tra n e tale numero,
   altrimenti riporto n stesso.
   Implementazione: *)

let rec read_max () =
  try
     let n = read_int()
     in try max n (read_max())
        with _ -> n
  with _ -> failwith "Sequenza vuota"
(*============ Es2b:read-max-min =============*)

(* read_max_min: unit -> int * int *)
(* algoritmo analogo al precedente *)
(* aux implementa un ciclo, i suoi due parametri sono il minimo e
    il massimo provvisorio *)
(* aux :  int -> int -> int*)

let read_max_min () =
  let rec aux nmax nmin =
    try (* se non si immette un numero, viene sollevata un'eccezione
     e il ciclo termina, riportando (nmin,nmax) *)
      let k = read_int()
      in aux (max nmax k) (min nmin k)
    with _ -> (nmax,nmin)
  in 
  try let n = read_int() in
          (* attenzione: si deve leggere un solo numero, che e'
             inizialmente sia il minimo che il massimo provvisorio *)
       aux n n 
  with _ -> failwith "Sequenza vuota"
(*============ Es2c:tutti-minori =============*)
(* In questo esercizio si deve stare attenti a una cosa: appena
   viene immesso un numero che non e' minore di quello dato, si potrebbe
   interrompere l'input della sequenza di numeri perche' il risultato
   e' stabilito. Ma non si vuole questo. Quindi il test che controlla
   il numero appena letto va eseguito dopo aver effettuato la chiamata
   ricorsiva  *)
(* tutti_minori: int -> bool *)

let rec tutti_minori_di n =
  try let k = read_int()
      in tutti_minori_di n && k < n
  with _ -> true

(* notate che se la sequenza e' vuota (non viene immesso alcun numero),
   il risultato e' true, per due motivi:
   1) non esiste nessun numero immesso maggiore o uguale a n
   2) se alla fine della ricorsione si riportasse false, 
      la funzione riporterebbe sempre false
*)

(* Per forzare l'esecuzione della chiamata ricorsiva prima di terminare, 
   si puo' anche usare una let: *)
let rec tutti_minori_di n =
  try let k = read_int()
      in let result = tutti_minori_di n 
         in k < n && result
  with _ -> true

(*============ Es2d:occorre =============*)

(* questo esercizio e' molto simile al precedente, e anche la sua soluzione *)
(* occorre: int -> bool *)
let rec occorre n =
  try let k = read_int()
      in occorre n || k=n
  with _ -> false

(* differenza: nel caso "base" - nessun numero letto - si riporta false *)
(* Notate che true e' l'elemento neutro rispetto all'AND, mentre false
   e' l'elemento neutro rispetto all'OR *)

(*============ Es2e:numero-di-stringhe =============*)

(* num_di_stringhe: unit -> int *)

(* algoritmo ricorsivo, funzione non tail recursive *)
let rec num_di_stringhe () =
   if read_line()="" then 0
   else 1 + num_di_stringhe()

(* algoritmo iterativo, funzione tail recursive *)
(* aux: int -> int, il suo parametro e' il numero di stringhe gia' lette *)
let num_di_stringhe () =
    let rec aux n = 
      if read_line() = "" then n  (** n e non 0 **)
      else aux (n+1)
  in aux 0               (* inizializzazione del ciclo *)

(*============ Es2f:stringa-max =============*)
(* stringa_max: unit -> string *)
(* aux implementa un ciclo: ha due parametri, la stringa di lunghezza
     massima tra quelle lette fino a quel momento, e la sua lunghezza (per
     evitare di calcolarla piu' volte *)
(* aux :  string -> int -> string *)

let stringa_max () =
  let rec aux smax len =
      let nuova = read_line()
      in if nuova = "" then smax
         else let nuovalen = String.length nuova
              in if len < nuovalen
                 then aux nuova nuovalen
                 else aux smax len
   in aux "" 0

(* a differenza che per la ricerca del minimo in una sequenza di interi,
   esiste una stringa di lunghezza minima (quella vuota), che si puo' 
   prendere come massimo provvisorio iniziale *)

(*============ Es3a:sumbetween =============*)

(* sumbetween : int -> int -> int *)
(* sumbetween n m = n + (n+1) + ... + (m-1) + m *)
let rec sumbetween n m =
  if n > m then 0
  else n + sumbetween (n+1) m

(*============ Es3b:sumto =============*)

(* ovvio che potremmo definire *)
(* sumto: int -> int *)
let sumto n =
  (n * (n + 1)) / 2

(* o si puo' utilizzare sumbetween *)
let sumto n = sumbetween 0 n

(* ma dato che stiamo facendo esercizi sulla ricorsione: *)
let rec sumto n =
  if n = 0 then 0
  else n + sumto (n-1)
(* oppure *)
let rec sumto = function
    0 -> 0 
  | n -> n + sumto (n-1)

(* versione iterativa: *)
(* aux: int -> int -> int *)
(* aux somma k = somma + (0+1+2+...+k) *)
let sumto n =
  let rec aux somma k =
    if k=0 then somma
    else aux (somma+k) (k-1)
  in aux 0 n

(*============ Es3c:power =============*)

(* power : int  ->  int -> int *)
(* power n k = potenza k-esima di n, assumendo k e n non negativi *)
let rec power n k =
  if k=0 then 1
  else n * power n (k-1)

(* se vogliamo considerare il caso di input con n o k negativo: *)
let power n k =
  let rec aux = function
     0 -> 1
   | k -> n * aux (k-1)
  in if n < 0 || k < 0 then failwith "Potenza negativa"
     else aux k

(*============ Es3d:fib =============*)

(* fib : int -> int *)
let rec fib  = function
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n-1) + fib (n-2)

(* anche qui si potrebbe considerare il caso in cui l'argomento e'
   negativo... *)

(*============ Es3e:maxstring =============*)
(* maxstring: string -> char *)
(*  aux : char -> int -> char *)
(* aux implementa il ciclo di scansione della stringa: 
   maxchar e' il massimo carattere
   incontrato fino a questo punto, i e' la posizione del carattere 
   da considerare *)

let maxstring s =
  let rec aux maxchar i =
    try
      aux (max maxchar s.[i]) (i+1)
    with _ -> (* stringa terminata *)
  maxchar
  in (* consideriamo anche il caso della stringa vuota *)
  try aux s.[0] 1
   (* inizializzazione del ciclo con il carattere in posizione 0,
      iniziando dal successivo *)
  with _ -> failwith "Stringa vuota"

let max string =
   let rec aux (string, i) =
     if i=(String.length string) then string.[i-1]
      else aux(string, (i+1))
   in aux (string,0) 
