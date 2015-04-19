type point = float * float;;
type kartka = point -> int;;

(* Zaczyna sie masa funkcji wszelakich - wszystko poza det'em sluzy
   do wyznaczenia symetrii punktu wzgledem prostej - mozliwe, ze dalo
   sie prosciej, ale tak, jak jest, tez powinno dzialac :) *)

let det ((ax,ay):point) ((bx,by):point) ((cx,cy):point) = 
	ax*.by +. bx*.cy +. cx*.ay -. by*.cx -. cy*.ax -. ay*.bx;;

(* Dla dwoch punktow tworzy wektor. *)
let wektor ((ax,ay):point) ((bx,by):point) = (bx-.ax, by-.ay);;
(* Wczytuje wektor i robi prostopadly. *)
let prostopadly (ax,ay) = (-.ay, ax);;
(* Wczytuje dwa punkty i na ich podstawie robi prosta o postaci
   y = ax + b i trzyma ja w parze postaci (a,b). *)
let prosta ((ax,ay):point) ((bx,by):point) = 
	let a = (by-.ay)/.(bx-.ax) in (a, ay -. a*.ax);;

(* Wyznacza punkt bedacy przecieciem dwoch prostych. *)
let przeciecie (a1,b1) (a2,b2) = 
	let x = (b2 -. b1)/.(a1 -. a2) in ((x, a1*.x +. b1):point);;

(* Wyznacza punkt, ktory jest symetryczny do punktu (x,y) wzgledem punktu (a,b). *)
let wzgledempunktu ((x,y):point) ((a,b):point) = ((2.*.a -. x, 2.*.b -. y):point);;

(* A ta wspaniala funkcja sklada wszystko wyzej do kupy - moze nie bede
   opisywal w szczegolach, co dokladnie robi, ale wyznacza punkt
   symetryczny do punktu (x,y) wzgledem prostej przedstawionej za
   pomoca dwoch punktow, ktore sa kolejnymi argumentami tej
   funkcji. Jako, ze dzialamy na prostej postaci y = ax + b, musimy 
   oddzielic sytuacje, gdy jest to prosta rownolegla do osi y.*)
let symetria ((x,y):point) ((ax,ay):point) ((bx,by):point) = 
	if ax = bx then ((2.*.ax -. x, y):point)
	else
		let (przecx, przecy) = 
			let (xp,yp) = 
				let (vx,vy) = prostopadly (wektor (ax,ay) (bx,by))
				in (x +. vx, y +. vy)
			in przeciecie (prosta (x,y) (xp,yp)) (prosta (ax,ay) (bx,by))
		in wzgledempunktu (x,y) (przecx, przecy);;

(* Teraz mamy prostokat i kolko - wyznaczamy funkcje, ktora dla danego
   punktu sprawdza, czy nalezy on do prostokata/kolka. W tym drugim
   korzystamy z rownania okregu. Te funkcje beda pozniej wywolywane 
   przez funkcje zloz i to na nich bedzie sie konczyc cala rekurencja,
   za pomoca ktorej bedziemy wyznaczac ilosc przebic dla danego punktu
   i kartki *)
let prostokat ((ax,ay):point) ((bx,by):point) = 
	let czynalezy ((x,y):point) = 
		if x < ax || x > bx || y < ay || y > by then 0
		else 1
	in (czynalezy:kartka);;

let kolko ((px,py):point) r = 
	let czynalezy ((x,y):point) = 
		let rownanie = (x-.px)*.(x-.px) +. (y-.py)*.(y-.py) in
			if rownanie > r*.r then 0
			else 1
	in (czynalezy:kartka);;

(* Funkcja zloz - dla prostej i innej funkcji wyznaczajacej ilosc przebic
   wyznaczamy kolejna funkcje, ktora oblicza ilosc przebic dla kartki,
   ktorej reprezentacje w postaci funkcji mamy w argumencie - z tym, ze
   jest ona zlozona wzdluz prostej. Teraz tak - jesli punkt, w ktorym
   bedziemy przebijac kartke, jest na lewo od prostej, to wynikiem jest
   wartosc funkcji dla tego punktu dla kartki bez ostatniego zlozenia + wartosc
   tej samej funkcji dla punktu symetrycznego wzgledem podanej prostej.
   Jesli punkt jest na prawo, to wynikiem jest 0, bo nic nie przebijemy.*)
let zloz ((ax,ay):point) ((bx,by):point) (nalezy:kartka) = 
	let czynalezy (x,y) = 
		let d = det (ax,ay) (bx,by) (x,y) in
			if d = 0. then nalezy (x,y)
			else if d < 0. then 0
			else nalezy (x,y) + nalezy (symetria (x,y) (ax,ay) (bx,by))
	in (czynalezy:kartka);;

(* Tutaj lecimy wprost z definicji tego, co ta funkcja ma robic, raczej
   nie ma tu za wiele do opisywania. *)	
let rec skladaj l k = 
	match l with
		[] -> k |
		((p1:point), (p2:point))::t -> skladaj t (zloz p1 p2 k);;
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
