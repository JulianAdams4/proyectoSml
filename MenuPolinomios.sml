

(* -------- Programa --------- *)
print("\n\t\t\t\t Modulo de Polinomios --------------- \n\n\t\t\t\t Digite [1] para Polinomios de grado 2\n\t\t\t\t Digite [2] para Polinomios de grado 3\n\t\t\t\t Digite [3] para Polinomios de grado 4\n\n\t\t\t\t -> Ingrese opcion: \n");
val opMenuPol = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);
	
if opMenuPol = 1 then use "PolinomioGrado2.sml"
else if opMenuPol = 2 then use "PolinomioGrado3.sml"
else if opMenuPol = 3 then use "PolinomioGrado4.sml"
else use "OpNoValidaMenuPolinomios.sml";