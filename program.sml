


datatype markering = H | U | B
datatype sikkerhet = Sikker of markering | Utgangspunkt of markering
datatype gardering = EnkelUtg | HalvUtenUtg | Heil
type kampTips = int * sikkerhet
type kupongKamp = int * gardering
type kampkryss = int * markering list


structure ExtList =
   struct

      fun interleave l i =
         let
            fun recur [] acc = rev acc
              | recur (x :: []) acc = recur [] (x :: acc)
              | recur (x :: xs) acc = recur xs (i :: x :: acc)
         in
            recur l []
         end
     end


structure Show =
   struct
      
      type 'a t = 'a -> string

      val int: int t = Int.toString

      val char: char t = Char.toString

      val list: 'a t -> 'a list t =
       fn show => fn xs => concat (ExtList.interleave (map show xs) ",")

      val pair: 'a t * 'b t -> ('a * 'b) t =
       fn (showa,showb) => fn (a,b) => showa a ^ showb b

       val markeringToString: markering -> string = 
       fn H => "H" | U => "U" | B => "B"      
   end


local
   open Show
in
   val show : (int * markering list) list list -> string = list (list (pair (int, (list markeringToString))))
end




val toTreTreSetup = [
	[EnkelUtg, EnkelUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg], (*1*)
	[EnkelUtg, HalvUtenUtg, EnkelUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg], (*2*)
	[EnkelUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg], (*3*)
	[EnkelUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg, EnkelUtg], (*4*)
	[HalvUtenUtg, EnkelUtg, EnkelUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg], (*5*)
	[HalvUtenUtg, EnkelUtg, EnkelUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg], (*6*)
	[HalvUtenUtg, EnkelUtg, EnkelUtg, HalvUtenUtg, HalvUtenUtg, EnkelUtg], (*7*)
	[HalvUtenUtg, HalvUtenUtg, EnkelUtg, EnkelUtg, EnkelUtg, HalvUtenUtg], (*8*)
	[HalvUtenUtg, HalvUtenUtg, EnkelUtg, EnkelUtg, HalvUtenUtg, EnkelUtg], (*9*)
	[HalvUtenUtg, HalvUtenUtg, EnkelUtg, HalvUtenUtg, EnkelUtg, EnkelUtg], (*10*)
	[EnkelUtg, EnkelUtg, EnkelUtg, Heil, HalvUtenUtg, HalvUtenUtg], (*11*)
	[EnkelUtg, EnkelUtg, Heil, HalvUtenUtg, EnkelUtg, HalvUtenUtg], (*12*)
	[EnkelUtg, EnkelUtg, Heil, HalvUtenUtg, HalvUtenUtg, EnkelUtg], (*13*)
	[HalvUtenUtg, EnkelUtg, Heil, EnkelUtg, EnkelUtg, HalvUtenUtg], (*14*)
	[HalvUtenUtg, EnkelUtg, Heil, EnkelUtg, HalvUtenUtg, EnkelUtg], (*15*)
	[HalvUtenUtg, EnkelUtg, Heil, HalvUtenUtg, EnkelUtg, EnkelUtg], (*16*)
	[EnkelUtg, Heil, Heil, EnkelUtg, EnkelUtg, HalvUtenUtg], (*17*)
	[EnkelUtg, Heil, Heil, EnkelUtg, HalvUtenUtg, EnkelUtg], (*18*)
	[EnkelUtg, Heil, Heil, HalvUtenUtg, EnkelUtg, EnkelUtg], (*19*)
	[Heil, Heil, Heil, EnkelUtg, EnkelUtg, EnkelUtg] (*20*)
]

fun markeringForKamp(utgangspunkt: markering, systemvalg: gardering) =
	case systemvalg of
		EnkelUtg => [utgangspunkt]
		| HalvUtenUtg => List.filter (fn x => x <> utgangspunkt) [H,U,B]
		| Heil => [H,U,B]

fun produserKupong(tips: kampTips list, kupongSetup: kupongKamp list, kupong: kampkryss list): kampkryss list = 
	case tips of 
		[] => kupong
		| (i, Sikker(x)) :: rest => produserKupong(rest, kupongSetup, kupong @ [(i, markeringForKamp(x, EnkelUtg))])
		| (i, Utgangspunkt(x)) :: rest => 
			case kupongSetup of
				[] => kupong (* TODO throw Exception*)
				| (j, markValg) :: tail => produserKupong(rest, tail, kupong @ [(j, markeringForKamp(x, markValg))])

fun toTreTre(tips: kampTips list): kampkryss list list =
	let 
		val usikre = List.filter (fn (i, sik) => sik = Utgangspunkt(H) orelse sik = Utgangspunkt(U) orelse sik = Utgangspunkt(B)) tips
		val usikreKampNr = List.map #1 usikre
		fun tail_rec_kupong_setups(kupongUtgSetups: gardering list list, kupongNr: int, acc: kupongKamp list list) =
			case kupongUtgSetups of
				[] => acc
				| head :: tail => tail_rec_kupong_setups(tail, kupongNr + 1, ListPair.zip(usikreKampNr, head) :: acc)
		fun tail_helper(tips: kampTips list, kupongSetups: kupongKamp list list, kuponger: kampkryss list list) =
			case kupongSetups of
				[] => kuponger
				| head :: tail => tail_helper(tips, tail, produserKupong(tips, head, []) :: kuponger)

	in 
		tail_helper(tips, tail_rec_kupong_setups(toTreTreSetup, 1, []) , [])
	end

val test = toTreTre(
	[(1, Sikker(H)),
	(2, Sikker(B)),
	(3, Sikker(H)),
	(4, Utgangspunkt(U)),
	(5, Sikker(H)),
	(6, Utgangspunkt(U)),
	(7, Utgangspunkt(U)),
	(8, Utgangspunkt(U)),
	(9, Sikker(B)),
	(10, Utgangspunkt(U)),
	(11, Utgangspunkt(U)),
	(12, Sikker(H))])


fun printToOutStream outstream str = let val os = outstream
                                     in
                                       TextIO.output(os,str);
                                       TextIO.closeOut os
                                     end;

val ost = TextIO.openOut "/Users/haga/dev/scala/result.txt"

val blabla = show test;

printToOutStream ost blabla


(*val stringTest = writefile "/Users/haga/dev/scala/result.txt" (show test)*)


(* ,1(\l)  \n\n1$1 *)
(* ,(\d)  \n$1 *)




