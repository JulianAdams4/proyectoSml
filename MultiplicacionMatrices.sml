print("\t\t\t\t Ingrese numero de filas de la primera matriz\n");
val f1 = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);
print("\t\t\t\t Ingrese numero de columnas de la primera matriz\n");
val c1 = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);
print("\t\t\t\t Ingrese numero de filas de la segunda matriz\n");
val f2 = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);
print("\t\t\t\t Ingrese numero de columnas de la segunda matriz\n");
val c2 = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);

if c1 <> f2 then use "NoSePuedeMult.sml"
	else print("\t\t\t\t Si se puede multiplicar!\n");


print("\n\t\t\t\t Ingrese la primera matriz (En forma de lista de listas) \n");
val h = (valOf(TextIO.inputLine TextIO.stdIn))::nil

fun InMat () = 
let 
	val a = []
	val a = (Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn))::nil
in
	a
end;


fun LeerFilas ( rows:int ) =  let
	val Z = InMat()
	val b = ref rows
in
	while !b > 0 do (
		
		b:= !b - 1
	)
end ;


print("\n\t\t\t\t Ingrese la primera matriz (En forma de lista de listas) \n");
val c2 = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) TextIO.stdIn);


fun headcol [] = []
| headcol ((x::_) :: rows) = x :: headcol rows;


fun tailcols [] = []
| tailcols ((_::xs) :: rows) = xs :: tailcols rows;


fun transp ([]::rows) = []
| transp rows = headcol rows :: transp (tailcols rows);


fun dotprod([], []) = 0.0
| dotprod(x::x1,y::y1) = x*y + dotprod(x1,y1);


fun rowprod(row, []) = []
| rowprod(row, col::cols) =
       dotprod(row,col) :: rowprod(row,cols);


fun rowlistprod([], cols) = []
| rowlistprod(row::rows, cols) =
       rowprod(row,cols) :: rowlistprod(rows,cols);

fun matprod(rowsA,rowsB) = rowlistprod(rowsA, transp rowsB);









print("\n\t\t\t\t      :v    (Y) \n\n\n\n");
use "MenuPrincipal.sml";



fun M([],[]) = 0 | M((x::xs),(y::ys)) = x*y + M(xs,ys);

fun ingFila ( num:int , Lista ) = Lista@(num::nil);

fun  A ( x::xs , y::ys ) = 
let
	val L = M( x , y )
	val lt = ingFila( L , [] )
in
	lt
end;