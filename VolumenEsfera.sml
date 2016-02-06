print("\n\t\t\t\t -> Ingrese valor del Radio: \n");
val Radio = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );
print("\n\n\t\t\t\t R. El volumen de la Esfera es: " ^ Real.toString( (4.0/3.0)*3.14159268*Radio*Radio*Radio )  ^ " [u^3]\n\n");

use "MenuPrincipal.sml";