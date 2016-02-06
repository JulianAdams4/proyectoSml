print("\nIngrese valor del Radio: ");
val Radio = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );
print("\n\nEl volumen de la Esfera es: " ^ Real.toString( (4.0/3.0)*3.14159268*Radio*Radio*Radio )  ^ " [u3]\n\n");

use "Volumenes.sml";