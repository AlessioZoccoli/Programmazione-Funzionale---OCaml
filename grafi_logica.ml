(* SETTEMBRE 2015 
http://cialdea.dia.uniroma3.it/teaching/pf/materiale/testi-di-esami/2014-15/settembre.pdf
*)

(* 2 
G grafo orientato e N nodo di. I cicli devono essere liste di nodi senza ripetizioni.
*)

type 'a graph = ('a * 'a) list

let successori g x =
  List.map snd (List.filter (fun (n,_) -> n=x) g)

(* a 
  Funzione ciclo: 'a graph ->'a -> 'a list
  ciclo g n = se esiste un ciclo senza ripetizioni su n in g. Se non esiste
  fallisce
*)

exception NotFound

let ciclo g start =
  let rec from_node visited a =
    if List.mem a visited
    then raise NotFound
    else if a = start then [a]
    else a::from_list (a::visited) (successori g a)
  and from_list visited = function
      [] -> raise NotFound
    | x::rest -> try from_node visited x
                 with NotFound -> from_list visited rest
  in start::from_list [] (successori g start)


(* b
  ciclo_valido: 'a graph -> 'a list list -> 'a -> 'a list
  applicando, a un grafo g, una lista di liste, riporta, se esiste, un ciclo
  senza ripetizioni su n in g tale che ogni lista-i in lista_di_liste contenga
  contenga almeno un nodo che occorre nel ciclo
*)

(* IDEALE : vanno tolti gli elementi che non mi servono più, come cammino che passa per
tutti i nodi di una lista. Toglierò tutte le sottoliste che contengono un
elemento che ho inserito nel cammino.
con Purge*)

let rec purge x = function
    [] -> []
  | y::rest -> if List.mem x y then purge x rest
               else y::purge x rest

let rec purge x lst =
  List.filter (fun l -> not (List.mem x l)) lst

let ciclo_valido g lista start =
  let rec from_node lista visited a =
    if List.mem a visited
    then raise NotFound
    else
      let nuovalista = purge a lista in
      if a = start
      then if lista = [] then [a]  (* o purgo qui o come ho fatto ora con nuovalista*)
           else raise NotFound          (* se ritorno su start ma la lista non è vuota fallisco*)
    else a::from_list (nuovalista) (a::visited) (successori g a)
  and from_list lista visited = function
      [] -> raise NotFound
    | x::rest -> try from_node lista visited x
                 with NotFound -> from_list lista visited rest
  in start::from_list (purge start lista) [] (successori g start)


(******************************************************)

(* almeno_uno: 'a list -> 'a list list -> bool
  true se ogni listarella è coperta dal path*)
let rec almeno_uno path = function
    [] -> true
  | x::rest -> List.exists (fun y -> List.mem y x) path && almeno_uno path rest

let ciclo_valido g lista start =
  let rec from_node visited a =
    if List.mem a visited then raise NotFound
    else if a = start
    then
      if almeno_uno (a::visited) lista then [a]  (* visited viene usato per fare il test*)
      else raise NotFound
    else a::from_list (a::visited) (successori g a)
  and from_list visited = function
      [] -> raise NotFound
    | x::rest -> try from_node visited x
                 with NotFound -> from_list visited rest
  in start::from_list [] (successori g start)

(***************************************************)
(*                    GRAFO PESATO                 *)
(***************************************************)

(*
http://cialdea.dia.uniroma3.it/teaching/pf/materiale/testi-di-esami/2014-15/febbraio.pdf
a ogni arco è associato un valore, il peso. Posso rappresentare tutto ciò con
una tripla. Il peso di un cammino è la somma dei pesi degli archi che lo
compongono 

wpath: 'a graph -> 'a -> 'a -> int  -> 'a list * int
se g è un grafo pesato orientato, con pesi positivi,
wpath g start goal pesomax = (path,w)
dove path è un cammino di peso non superiore a pesomax. Eccezione se goal non
raggiungibile da start mediante cammini di peso minore di pesomax
*)

type 'a pgraph = ('a * 'a * int) list

let path g start goal pesomax =
  let rec from_node visited peso a =
    if List.mem a visited || (peso > pesomax)
    then raise NotFound
    else if a = goal then ([a],peso)                        (* qui peso avrà già il valore finale *)
    else
      let (nodi,costo) = from_list (a::visited) (peso) (successori g a)
      in (a::nodi,costo)
  and from_list visited peso = function
      [] -> raise NotFound
    | (x,n)::rest -> try from_node visited (n+peso) x
                 with NotFound -> from_list visited peso rest
  in from_node [] 0 start