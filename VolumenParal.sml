print("\n\t\t\t\t -> Ingrese valor del Ancho: \n");
val x = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );
print("\n\t\t\t\t -> Ingrese valor de la Altura: \n");
val y = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );
print("\n\t\t\t\t -> Ingrese valor de la Profundidad: ");
val z = Real.fromInt( Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn) );
print("\n\n\t\t\t\t R. El Volumen del cuerpo es: " ^ Real.toString(x*y*z) ^ " [u3]\n\n");

use "MenuPrincipal.sml";