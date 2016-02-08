(* Polinomios de Grado 3 *)
print("\n\t\t\t\t Polinomio de la forma ax^3 + bx^2 + cx + d \n\t\t\t\t -> Ingrese valor de a: \n");
val a = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );

if a < 1.0 then use "NoEsGrado2.sml"
else print("\n\t\t\t\t Polinomio si es de grado 2!\n");

print("\n\t\t\t\t -> Ingrese valor de b: \n");
val b = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );

print("\n\t\t\t\t -> Ingrese valor de c: \n");
val c = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );

print("\n\t\t\t\t -> Ingrese valor de d: \n");
val d = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );

val detertinante = 18.0*a*b*c*d - 4.0*b*b*b*d + b*b*c*c - 4.0*a*c*c*c - 27.0*a*a*d*d;

val X = b/a;
val Y = c/a;
val Z = d/a;
(* ----------------------------------------------------------------- *)
fun indexList ( n:int , []:real list):real = 0.0 
									       | indexList (n, (x::xs)) = if Int.compare(n,0)=EQUAL then x
											     					  else indexList ((n-1), xs);
(* ----------------------------------------------------------------- *)
fun cmpMenMas (t:real) = if Real.compare( t, ~1.0 ) = LESS then ( ~1.0 
						) else ( 
							if Real.compare( t, 1.0 ) = GREATER then (
								1.0 
							) else (
								t
							)
						);
(* ----------------------------------------------------------------- *)
fun rMenorCero ( r: real, r2: real, q3: real ) = if Real.compare( r , 0.0 ) = LESS then (
													Math.pow( Real.abs(r) + Math.sqrt(r2-q3) , 1.0/3.0 ) 
												) else (
													~1.0 * Math.pow( Real.abs(r) + Math.sqrt(r2-q3) , 1.0/3.0 )
												);
(* ----------------------------------------------------------------- *)
fun aIgualCero ( a: real, q: real, A:real ) = if Real.compare( a , 0.0 ) = EQUAL then (
										0.0 
									  ) else (
										q/A
									  );
(* ----------------------------------------------------------------- *)
fun r2Menorq3 ( r:real, a: real , q:real , r2:real , q3: real ) = 
	let 
		val t0 = r/Math.sqrt(q3)
		val t = Math.acos( cmpMenMas(t0) )
		val atercios = a/3.0
		val qraiz = ~2.0*Math.sqrt(q)
		val TwoPi = 2.0*Math.pi
	in 
		( qraiz * Math.cos(t/3.0)-atercios )::(qraiz * Math.cos( (t+TwoPi)/3.0 ) - atercios)::(qraiz * Math.cos( (t-TwoPi)/3.0 ) - atercios)::nil
	end ;
(* ----------------------------------------------------------------- *)
fun menorEps ( eps: real , A:real , B:real , atercios: real ) =	if Real.compare( Real.abs( 0.5*Math.sqrt(3.0)*(A-B) ) , eps ) = LESS  then ( 
																	((A+B)-atercios)::(~0.5*(A+B)-atercios)::(~0.5*(A+B)-atercios)::nil
																) else (
																	((A+B)-atercios)::(~0.5*(A+B)-atercios)::(0.5*Math.sqrt(3.0)*(A-B))::nil
																);
(* ----------------------------------------------------------------- *)
fun r2mayorq3 ( r:real, a: real , q:real , r2:real , q3: real )  = 
	let 
		val A = rMenorCero( r , r2 , q3 ) 
		val B = aIgualCero( a , q , A )
		val atercios = a/3.0
		val eps = Math.pow( 10.0 , ~14.0 )
	in
		menorEps( eps , A , B , atercios )
	end ;
(* ----------------------------------------------------------------- *)
val a2 = X*X;
val q  = (a2 - 3.0*Y)/9.0 ;
val r  = (X*(2.0*a2-9.0*Y) + 27.0*Z)/54.0 ;
val r2 = r*r;
val q3 = q*q*q;

fun SolveP3( a: real , a2:real , q:real , r:real , r2:real , q3:real ): real list =
    if Real.compare(r2,q3) = LESS then (
		r2Menorq3( r , a , q , r2 , q3 )
    ) else (
		r2mayorq3( r , a , q , r2 , q3 )
    );

val solutions = SolveP3( X, a2 , q , r , r2 , q3 );

print("\n\n\n\n\n\n\n\n");

if Real.compare( detertinante , 0.0 ) = LESS then (
	print("\t\t\t\t -> La primera solucion es:  x1 = " ^ Real.toString( indexList(0,solutions) ) ^ "\n\t\t\t\t -> la segunda solucion es:  x2 = " ^ Real.toString( indexList(1,solutions) ) ^ " + (" ^ Real.toString( indexList(2,solutions) ) ^ ") i " ^ "\n\t\t\t\t -> la tercera solucion es:  x2 = " ^ Real.toString( indexList(1,solutions) ) ^ " - (" ^ Real.toString( indexList(2,solutions) ) ^ ") i \n\n")
) else (
	print("\t\t\t\t -> La primera solucion es:  x1 = " ^ Real.toString( indexList(0,solutions) ) ^ "\n\t\t\t\t -> La segunda solucion es:  x1 = " ^ Real.toString( indexList(1,solutions) ) ^ "\n\t\t\t\t -> La tercera solucion es:  x1 = " ^ Real.toString( indexList(2,solutions) ) ^ "\n\n")
);
 
use "MenuPrincipal.sml";





