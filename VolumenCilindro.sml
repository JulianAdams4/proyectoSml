print("\nIngrese valor del Radio: ");
val Radio = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );
print("\nIngrese valor de la Altura: ");
val Altura = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );
print("\n\nEl volumen del Cilindro es: " ^ Real.toString(3.14159268*Radio*Radio*Altura) ^ " [u3]\n\n");

use "Volumenes.sml";