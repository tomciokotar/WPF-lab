type 'a queue = Node of 'a * int * 'a queue * 'a queue | Leaf;;
exception Empty;;

(* Drzewo puste to po prostu lisc. *)
let empty = Leaf;;

(* Funkcje, ktore wyluskuja kolejno kolejne parametry drzewa. *)
(* Prawe poddrzewo *)
let prawy q = 
	match q with
		Leaf -> Leaf |
		Node (_,_,_,p) -> p;;

(* Lewe poddrzewo *)
let lewy q = 
	match q with
		Leaf -> Leaf |
		Node (_,_,l,_) -> l;;

(* Prawa wysokosc *)
let wys q = 
	match q with
		Leaf -> 0 |
		Node (_,h,_,_) -> h;;

(* Priorytet wierzcholka - pomijamy Leaf, ktory jest tu niepotrzebny,
   poniewaz kazde mozliwe wykonanie "pr q" w kodzie bedzie wykonywane po
   uprzednim upewnieniu sie, ze q jest typu Node, a nie Leaf. Warning 
   mozna olac. *)
let pr q = 
	match q with
		Node (p,_,_,_) -> p;;

(* Funkcja wywolywana przez join - mamy gwarancje, ze d1 ma mniejszy
   priorytet, niz d2. *)
let rec polacz d1 d2 =
	let d3 = join (prawy d1) d2 in
	if wys (lewy d1) < wys d3 then
		Node (pr d1, wys (lewy d1) + 1, d3, lewy d1)
	else
		Node (pr d1, wys d3 + 1, lewy d1, d3)

(* Join - tak wlasciwie wywoluje funkcje polacz ustawiajac q1 i q2 tak,
   zeby mniejszy priorytet byl w pierwszym argumencie. *)
and join q1 q2 = 
	if q1 = Leaf then q2
	else if q2 = Leaf then q1
	else if pr q1 > pr q2 then polacz q2 q1
	else polacz q1 q2;;

(* Dolaczanie elementu do drzewa. *)	
let add e q = join (Node (e, 1, Leaf, Leaf)) q;;

(* Sprawdzamy, czy drzewo jest puste. *)
let is_empty q = if q = Leaf then true else false;;

(* Usuwamy najmniejszy element z drzewa, a jesli drzewo jest puste, 
   podnosimy wyjatek. *)
let delete_min q = 
	if is_empty q then raise Empty
	else (pr q, join (lewy q) (prawy q));;










