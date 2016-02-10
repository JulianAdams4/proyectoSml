(* Archivo que contiene el Menu Principal del programa *)

print("\n\n\t\t\t\t Menu principal\n\n\n\t\t\t\t 1. Presione [1] para el Modulo de Volumenes\n\t\t\t\t 2. Presione [2] para el Modulo de Matrices\n\t\t\t\t 3. Presione [3] para el Modulo de Polinomios\n\t\t\t\t 4. Presione [4] para Salir...\n\n\t\t\t\t -> Ingrese opcion: \n");

val opMenPrin = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);

if opMenPrin = 1 then use "MenuVolumenes.sml"
else if opMenPrin = 2 then use "MultiplicacionMatrices.sml"
else if opMenPrin = 3 then use "MenuPolinomios.sml"
else if opMenPrin = 4 then OS.Process.exit(OS.Process.success)
else use "OpNoValidaMenuPrincipal.sml";