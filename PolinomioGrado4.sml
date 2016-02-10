(* Polinomios de Grado 4 *)

(* Ingreso de datos -------------------------------------- *)
print("\n\t\t\t\t Polinomio de la forma ax^4 + bx^3 + cx^2 + dx + e \n\t\t\t\t -> Ingrese valor de a: \n");
val a = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );

if Real.compare(a,0.0) = EQUAL then use "NoEsGrado4.sml"
else print("\n\t\t\t\t Polinomio si es de grado 4!\n");

print("\n\t\t\t\t -> Ingrese valor de b: \n");
val b = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );

print("\n\t\t\t\t -> Ingrese valor de c: \n");
val c = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );

print("\n\t\t\t\t -> Ingrese valor de d: \n");
val d = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );

print("\n\t\t\t\t -> Ingrese valor de e: \n");
val e = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );

if Real.compare(e,0.0) = EQUAL then use "PolinomioGrado4E0.sml"
else print("\n e distinto de 0\n");
(* Fin Ingreso de datos -------------------------------------- *)


val (B,C,D,E) = ( b/a , c/a , d/a , e/a );

val cb = ~C;
val cc = ~4.0*E + D*B;
val cd = ~( B*B*E + D*D ) + 4.0*C*E;

val (discrim, q, r, RRe, RIm, DRe, DIm, dum1, ERe, EIm, s, t, term1, r13, sqR, y1, z1Re, z1Im, z2Re) = (0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0);

val q = (3.0*cc - (cb*cb))/9.0;
val	r = ~(27.0*cd) + cb*(9.0*cc - 2.0*(cb*cb));
val	r = r/54.0;
val	discrim = q*q*q + r*r;
val	term1 = (cb/3.0);

(* Funciones *)
(* -------------------------------------------- *)
fun DiscMayorZero(s,t,y1,discrim,r,term1) = let
	val s = r + Math.sqrt(discrim)
	val s = if s < 0.0 then ( ~1.0*Math.pow(~s, (1.0/3.0)) ) else ( Math.pow( s, (1.0/3.0) ) )
	val t = r - Math.sqrt(discrim)
	val t = if t < 0.0 then ( ~1.0*Math.pow(~t, (1.0/3.0)) ) else ( Math.pow( t, (1.0/3.0) ) )
	val y1 = ~term1 + s + t
in
	y1
end;
(* -------------------------------------------- *)
fun DiscEqualZero (r13,y1,r,term1) = let
	val r13 = if r < 0.0 then ( ~1.0*Math.pow(~r, (1.0/3.0)) ) else ( Math.pow( r, (1.0/3.0) ) )
	val y1 = ~term1 + 2.0*r13
in 
	y1
end;
(* -------------------------------------------- *)
fun DiscMinorZero (dum1,r13,y1,q,r,term1) = let
	val q = ~q
	val dum1 = q*q*q
	val dum1 = Math.acos(r/Math.sqrt(dum1))
	val r13 = 2.0*Math.sqrt(q)
	val y1 = ~term1 + r13*Math.cos(dum1/3.0)
in 
	y1
end;
(* -------------------------------------------- *)


val y1 = if Real.compare(discrim,0.0) = GREATER then (
	DiscMayorZero(s,t,y1,discrim,r,term1)
) else if Real.compare(discrim,0.0) = EQUAL then (
	DiscEqualZero(r13,y1,r,term1)
) else (
	DiscMinorZero(dum1,r13,y1,q,r,term1)
);

(* Determined y1, a real root of the resolvent cubic. *)

val term1 = B/4.0;
val sqR = ~C + term1*B + y1;  
val (RRe,RIm,DRe,DIm,ERe,EIm,z1Re,z1Im,z2Re) = (0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0);


(* ------z1Im------------------------------------ *)
fun dum1MinorZero(z1Im:real,dum1:real) = 
let 
	val z1Im = 2.0*Math.sqrt(~dum1)
in
	0.0::0.0::0.0::0.0::0.0::0.0::0.0::z1Im::0.0::nil
end;
(* -------z1Re,z2Re----------------------------------- *)
fun dum1MayorZero(z1Re:real,z2Re:real,dum1:real) = 
let
  	val z1Re = 2.0*Math.sqrt(dum1)
	val z2Re = ~z1Re
in 
	0.0::0.0::0.0::0.0::0.0::0.0::z1Re::0.0::z2Re::nil
end;
(* ------------------------------------------ *)
fun auxDum (dum1:real) = if dum1 < 0.0 then dum1MinorZero(z1Im,dum1)
	else dum1MayorZero(z1Re,z2Re,dum1)
(* ------------------------------------------ *)
fun sqrEqualZero(dum1,y1,E) = 
let
	val dum1 = ~4.0*E + y1*y1
in
	auxDum(dum1)
end;
(* ----RRe,z1Re,z2Re-------------------------------------- *)
fun sqrMayorZero (RRe,z1Re,z2Re,sqR,B,C,D) =
let
	val RRe = Math.sqrt(sqR)
	val z1Re = ~(8.0*D + B*B*B)/4.0 + B*C
	val z1Re = z1Re/RRe
	val z2Re = ~z1Re
in
	RRe::0.0::0.0::0.0::0.0::0.0::z1Re::0.0::z2Re::nil
end;
(* ----RIm------------------------------------ *)
fun sqrMinorZero(z1Im,sqR,B,C,D) =
let
	val RIm = Math.sqrt(~sqR);
	val z1Im = ~(8.0*D + B*B*B)/4.0 + B*C;
	val z1Im = z1Im/RIm;
	val z1Im = ~z1Im;
in
	0.0::RIm::0.0::0.0::0.0::0.0::0.0::z1Im::0.0::nil
end;
(* ------------------------------------------ *)



val lista = if Real.compare(sqR,0.0) = EQUAL then sqrEqualZero(dum1,y1,E)
			else if Real.compare(sqR,0.0) = GREATER then sqrMayorZero(RRe,z1Re,z2Re,sqR,B,C,D)
			else sqrMinorZero(z1Im,sqR,B,C,D) ;



(*RRe,RIm,DRe,DIm,ERe,EIm,z1Re,z1Im,z2Re*)

(* Funcion que retorna una posicion de una lista, empieza desde el 0 *)
fun select ( n:int , []:real list):real = 0.0 | select (n, (x::xs)) = if Int.compare(n,0)=EQUAL then x else select ((n-1), xs);
(* ------------------------------------------ *)

val RRe = select(0,lista);
val RIm = select(1,lista);
val DRe = select(2,lista);
val DIm = select(3,lista);
val ERe = select(4,lista);
val EIm = select(5,lista);
val z1Im = select(7,lista);

val z1Re = z1Re + ~(2.0*C + sqR) + 3.0*B*term1;
val z2Re = z2Re + ~(2.0*C + sqR) + 3.0*B*term1;



(* ------------------------------------------ *)
fun z1ImEqualZero ( z1Re, z2Re, DIm, EIm, DRe, ERe ) = let
	val (DRe,DIm) = if z1Re >= 0.0 then ( Math.sqrt(z1Re) , DIm ) else ( DRe , Math.sqrt(~z1Re) )
	val (ERe,EIm) = if z2Re >= 0.0 then ( Math.sqrt(z2Re) , EIm ) else ( ERe , Math.sqrt(~z2Re) )
in 
	DRe::DIm::ERe::EIm::nil
end;

(* ------------------------------------------ *)
fun z1ImNotZero (r:real, dum1:real, ERe:real, z1Re:real, z1Im:real, DRe:real, DIm:real, EIm:real ) = let
	val r = Math.sqrt( z1Re*z1Re + z1Im*z1Im ) 
	val r = Math.sqrt(r)
	val dum1 = Math.atan2(z1Im, z1Re)
	val dum1 = dum1/2.0
	val DRe = r*Math.cos(dum1)
	val ERe = DRe
	val DIm = r*Math.sin(dum1)
	val EIm = ~DIm
in
	DRe::DIm::ERe::EIm::nil
end;
(* ------------------------------------------ *)


val lista2 = if Real.compare( z1Im , 0.0 ) = EQUAL then z1ImEqualZero( z1Re, z2Re, DIm, EIm, DRe, ERe )
			 else z1ImNotZero( r , dum1 , ERe , z1Re , z1Im , DRe , DIm , EIm ) ;


val DRe = select(0,lista2);
val DIm = select(1,lista2);
val ERe = select(2,lista2);
val EIm = select(3,lista2);

print("\n\t\t\t\t -> x1 = " ^ Real.toString( ~term1 + (RRe + DRe)/2.0 ) ^ " + (" ^ Real.toString( (RIm + DIm)/2.0 ) ^ ") i \n"); 

print("\n\t\t\t\t -> x2 = " ^ Real.toString( ~(term1 + DRe/2.0) + RRe/2.0 ) ^ " + (" ^ Real.toString( ( ~DIm + RIm )/2.0 ) ^ ") i \n");

print("\n\t\t\t\t -> x3 = " ^ Real.toString( ~(term1 + RRe/2.0) + ERe/2.0 ) ^ " + (" ^ Real.toString( ( ~RIm + EIm )/2.0) ^ ") i \n");

print("\n\t\t\t\t -> x4 = " ^ Real.toString( ~(term1 + (RRe + ERe)/2.0) ) ^ " + (" ^ Real.toString( ~(RIm + EIm)/2.0 ) ^ ") i \n\n\n");


use "MenuPrincipal.sml";

