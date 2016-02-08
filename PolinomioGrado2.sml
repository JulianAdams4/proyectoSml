(* Polinomios de Grado 2 *)
print("\n\t\t\t\t Polinomio de la forma ax^2 + bx + c \n\t\t\t\t -> Ingrese valor de a: \n");
val a = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );

if a < 1.0 then use "NoEsGrado2.sml"
else print("\n\t\t\t\t Polinomio si es de grado 2!\n");

print("\n\t\t\t\t -> Ingrese valor de b: \n");
val b = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );

print("\n\t\t\t\t -> Ingrese valor de c: \n");
val c = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );

val disc1 = (b*b) - 4.0*a*c;

fun Second (discri1:real):real = if discri1 < 0.0 then ~discri1
									  else discri1;
val disc2 = Second(disc1);
									  
val part1 = ~b/(2.0*a);
val part2 = Math.sqrt(disc2)/(2.0*a);

fun printRoots(disc:real) = if disc<0.0 then print("\n\t\t\t\t    Las raices del polinomio son:\n\t\t\t\t   -> x1 = " ^ Real.toString(part1) ^ " + " ^ Real.toString(part2) ^ " i" ^ "\n\t\t\t\t   -> x2 = " ^ Real.toString(part1) ^ " - " ^ Real.toString(part2) ^ " i\n") 
							else print("\n\t\t\t\t    Las raices del polinomio son:\n\t\t\t\t   -> x1 = " ^ Real.toString(part1+part2) ^ "\n\t\t\t\t   -> x2 = " ^ Real.toString(part1-part2) ^ "\n");
							
printRoots(disc1);

use "MenuPrincipal.sml";