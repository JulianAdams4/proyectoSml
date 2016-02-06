(*  val h = valOf(Int.fromString(valOf(TextIO.inputLine TextIO.stdIn)))  *)

(* -------- Programa --------- *)
print("\n\tDigite [1] para Volumen de una Esfera\n\tDigite [2] para Volumen de un Cilindro\n\tDigite [3] para Volumen de un Paralelepipedo\n\tDigite [4] para SALIR\n\n\tIngrese opcion: ");
val opMenuInicial = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);
	
if opMenuInicial = 1 then use "VolumenEsfera.sml"
else if opMenuInicial = 2 then use "VolumenCilindro.sml"
else if opMenuInicial = 3 then use "VolumenParal.sml"
else OS.Process.exit(OS.Process.success);





