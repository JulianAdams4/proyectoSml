print("\nIngrese valor del Radio: ");
val x = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );
print("\nIngrese valor de la Altura: ");
val y = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );
print("\nIngrese valor de la Altura: ");
val z = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );
print("\n\nEl Volumen del cuerpo es: " ^ Real.toString(x*y*z) ^ " [u3]\n\n");

use "Volumenes.sml";