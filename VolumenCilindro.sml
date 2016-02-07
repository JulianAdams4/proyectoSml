print("\n\t\t\t\t -> Ingrese valor del Radio: ");
val Radio = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );
print("\n\t\t\t\t -> Ingrese valor de la Altura: ");
val Altura = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );
print("\n\n\t\t\t\t R. El volumen del Cilindro es: " ^ Real.toString( Math.pi*Radio*Radio*Altura ) ^ " [u3]\n\n");

use "MenuPrincipal.sml";