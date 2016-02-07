(*  val h = valOf(Int.fromString(valOf(TextIO.inputLine TextIO.stdIn)))  *)

(* -------- Programa --------- *)
print("\n\t\t\t\t Modulo de Volumenes --------------- \n\n\t\t\t\t Digite [1] para Volumen de una Esfera\n\t\t\t\t Digite [2] para Volumen de un Cilindro\n\t\t\t\t Digite [3] para Volumen de un Paralelepipedo\n\n\t\t\t\t -> Ingrese opcion: \n");
val opMenuInicial = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);
	
if opMenuInicial = 1 then use "VolumenEsfera.sml"
else if opMenuInicial = 2 then use "VolumenCilindro.sml"
else if opMenuInicial = 3 then use "VolumenParal.sml"
else use "OpNoValidaMenuVolumenes.sml";





