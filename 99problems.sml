(* WORKING WITH LISTS 
=====================*)

(* P01 Find the last element of a list *)

fun last lista = 
  case lista of 
      [] => raise Empty
   | x::[] => x
   | _::xs => last xs

(* Tests *)
val last1 = last [1]
val last2 = last [1,2,3]


(* P02 Find the last but one element of a list *)

fun penultimate lista =
  case lista of
      [] => raise Empty
   | x::[] => raise Empty
   | x::y::[] => x
   | x::xs => penultimate xs

(* Tests *)
val penultimate1 = penultimate [1,2,3,4,5,6]
val penultimate2 = penultimate [1,2]


(* P03 Find the Kth element of a list *)

fun nth (k: int, lista)=
  case (k, lista) of
      (_, []) => raise Empty
   | (0, x::_) => x
   | (k, _::xs) => nth(k-1, xs)

(* Tests *)
val nth1 = nth(2, [1,2,3,4,5,6])
val nth2 = nth(0, [1,2,3])


(* P04 Find the number of elements of a list *)
fun length lista = 
let 
    fun aux(ls, acc)=
      case ls of
	  [] => acc
       | x::xs => aux(xs, acc+1)
in
    aux(lista, 0)
end

(* Tests *)
val length1 = length([1,2,3,4])
val length2 = length([])


(* P05 Reverse a list *)
fun reverse lista =
let
    fun aux(ls, acc)=
      case ls of
	  [] => acc
       | x::xs => aux(xs, x::acc)
in
    aux(lista, [])
end

(* Tests *)
val reverse1 = reverse([1,2,3,4])


(* P06 Find out whether a list is a palindrome *)
fun isPalindrome lista =
  reverse lista = lista

(* Tests *)
val isPalindrome1 = isPalindrome([1,2,3,2,1])
val isPalindrome2 = isPalindrome([1,2,3])

(* P07 Flatten a nested list structure *)
fun flatten lista = 
  case lista of 
      [] => []
   | x::xs => x @ flatten xs

(* Tests *)
val flatten1 = flatten ([[1,1], [5, 8], [1,2,3]])
val flatten2 = flatten ([[1], [], [12,32], []])


(* P08 Eliminate consecutive duplicates of list elements *)
fun compress lista =
let 
    fun aux (ls, prev, acc) = 
      case ls of
	  [] => acc
       | x::xs => case prev of
		      NONE => aux(xs, SOME x, x::acc)
		   | SOME i => if i = x then aux(xs, prev, acc) else aux(xs, SOME x, x::acc)
in
   reverse(aux(lista, NONE, []))
end

(* Tests *)
val compress1 = compress [1,1,1,2,2,3,1,1,3,4]
val compress2 = compress [1,2,3,4,5,5]


(* P09 Pack consecutive duplicates of list elements ito sublists *)
fun pack lista = 
let
   fun aux(ls, prev, packed, acc)=
     case ls of
	 [] => acc @ [packed]
      | x::xs => case prev of
		     NONE => aux(xs, SOME x, x::packed, acc)
		  | SOME i => if i = x then aux(xs, prev, x::packed, acc) else aux(xs, SOME x, [x], acc @ [packed])
in 
    aux(lista, NONE, [], [])
end

(* Tests *)
val pack1 = pack [1,1,2,2,2,3,4,5,5,6]


(* P10 Run-length encoding of a list *)
fun encode lista =
  map (fn x => (length x, hd x)) (pack lista)

(* Tests *)
val encode1 = encode [1,1,2,2,2,3,4,5,5,6]
val encode2 = encode ["a", "a", "a", "b", "c", "d", "d"]


(* P11 Modified run-length encoding --> It can't be done in SML *)


(* P12 Decode a run-length encoded list *)

(* helper function
Make a list with the same element n times *)
fun aux1 (n, element)=
  if n = 0
  then []
  else element::aux1(n-1, element)

fun decode lista =
  case lista of
      [] => []
   | x::xs => aux1 x @ decode xs

(* Tests *)
val decode1 = decode [(3,"a"), (1,"b"), (2,"c")] 


(* P13 .... *)


(* P14 Duplicate the elements of a list *)
fun duplicate lista = 
let
    val encoded_list = encode lista

    fun double_first ls = 
      case ls of
	  [] => []
       | (i,j)::xs => (2*i, j) :: double_first xs
    
    val decoded_list = decode(double_first(encoded_list))
in 
    decoded_list
end
  
(* Tests *)
val duplicate1 = duplicate ["a", "a", "b", "c", "c", "c"]


(* P15 Duplicate the elements of a list a given number of times *)
fun duplicateN (n, lista) = 
let
    val encoded_list = encode lista

    fun ntimes_first(ls, n) = 
      case ls of
	  [] => []
       | (i,j)::xs => (n*i, j) :: ntimes_first(xs, n)
    
    val decoded_list = decode(ntimes_first(encoded_list, n))
in 
    decoded_list
end

(* Tests *)
val duplicateN1 = duplicateN(3, ["a", "b", "b", "c", "d", "d", "d"])


(* P16 Drop every Nth element from a list *)
fun drop(n, lista)=
let
    fun aux(ls, acc, k)=
      case ls of
	  [] => acc
       | x::xs => if k mod n = 0 
		  then aux(xs, acc, k+1)
		  else aux(xs, acc @ [x], k+1)
in
    aux(lista, [], 1)
end

(* Tests *)
val drop1 = drop(3, ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"])


(* P17 Split a list into two pairs *)
fun split(n, lista)=
let 
    fun aux(ls, k, acc)=
      if k > n
      then (acc, ls)
      else case ls of 
	       [] => (acc, ls)
	    | x::xs => if k=n
		       then (acc @ [x], xs)
		       else aux(xs, k+1, acc @ [x])
in
    aux(lista, 1, [])
end

(* Tests *)
val split1 = split(2, [1,2,3,4,5,6,6,7])
      

(* P18 Extract a slice from a list *)
fun slice (lista, i, k)=
let
    fun aux(ls, index, acc)=
      if index >= k 
      then acc
      else case ls of
	       [] => acc
	    | x::xs => if index >= i
		       then aux(xs, index+1, acc @ [x])
		       else aux(xs, index+1, acc)
in 
    aux(lista,0,[])
end

(* Tests *)
val slice1 = slice ([1,2,3,4,5,6,6,7], 1, 5)
      

(* P19 Rotate a list N places to the left. *)
fun rotate (n , lista)=
  if n >= 0
  then let val (i, j) = split (n, lista) in j @ i end
  else let val (i, j) = split (length lista - (~n), lista) in j @ i end

(* Tests *)
val rotate1 = rotate (3, ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"])
val rotate2 = rotate (~2, ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"])


(* P20 Remove the Kth element from a list *)
fun removeAt (n, lista)=
let
    fun aux(ls, acc, index, elem)=
      case ls of 
       [] => (acc, valOf elem)
    | x::xs => if index = n
	       then (acc @ xs, valOf(SOME x))
	       else aux(xs, acc @ [x], index+1, elem)  
in
    aux(lista, [], 0, NONE)
end


(* Tests *)
val removeAt1 = removeAt (3, ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"])


(* P21 Insert an element at a given position into a list *)
fun insertAt(e, pos, lista)=
let
    fun aux(ls, acc, posit)=
      case ls of
	  [] => acc
       | x::xs => if posit = 0
		  then acc @ [e] @ [x] @ xs
		  else aux(xs, acc @ [x], posit -1)
in
    aux(lista, [], pos)
end


(* Tests *)
val insertAt1 = insertAt ("w", 3, ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"])


(* P22 Create a list containing all integers within a given range *)
fun range(s, e)=
let
    fun aux(st, en, acc)=
      if st > en
      then acc
      else aux(st+1, en, acc @ [st])
in
    aux(s,e,[])
end


(* Tests *)
val range1 = range(1,4)
val range2 = range(3,3)


(* P23 Extract a given number of randomly selected elements from a list *)
fun randomSelect(n, lista)=
let 
    fun remove(k, acc, ls)=
      if k = 0
      then acc
      else
	  let
	      val seed = Random.rand(0, (length ls) -1)
	      val index = Random.randRange(0, (length ls) - 1) seed
	      val item_removed = #2 (removeAt(index, ls))
              val remaining_list = #1 (removeAt(index, ls))
	  in
	      remove(k-1, acc @ [item_removed], remaining_list)
	  end
in
    remove(n, [], lista)
end


(* Tests *)
val randomSelect1 = randomSelect(3, ["a", "b", "c", "d", "e", "f", "g"])


(* P24 Lotto: Draw N different random numbers from the set 1...M *)
fun lotto(n, set)=
let
    val range_set = range(1,set)
in
    randomSelect(n, range_set)
end


(* Tests *)
val lotto1 = lotto(6, 49)


(* P25 Generate a random permutation of the elements of a list *)
fun randomPermute lista=
  randomSelect (length lista, lista)


(* Test *)
val randomPermute1 = randomPermute ["a", "b", "c", "d", "e" , "f"]


(* P26 Generate the combinations of K distinct objects chosen from the N elements of a list *)
fun combinations(n, lista)=
  case (n, lista) of
      (0, _) => [[]]
   | (_, []) => []
   | (k, x::xs) => map (fn y => x::y) (combinations (k-1, xs)) @ combinations (k, xs)


(* Test *)
val combinations1 = combinations(2, [1,2,3,4])   


(* P27 Group the elements of a set into disjoin subsets *)


(* P28 Sorting a list of lists according to the length of sublists *)
fun lsort lista =
let
    fun min ls = 
      case ls of 
	  [] => NONE
	     | x::[] => SOME x
	     | x::xs => let
		           val tl_min = min xs
	                in
			    if length x < length (valOf tl_min)
			    then SOME x
			    else tl_min
	                end

    fun remove_element(e, ls)=
      case ls of 
	  [] => []
	     | x::xs => if e=x then xs else x::remove_element(e,xs) 

    fun sort(ls, acc)=
      case ls of
	  [] => []
        | x::[] => acc @ [x]
	| x::xs::ys =>  let
	                    val min_tl = valOf (min (xs::ys))
                        in
			    if length x < length min_tl
			    then sort(xs::ys, acc @ [x])
			    else sort(remove_element(min_tl,x::xs::ys), acc @ [min_tl])
	                end
in
    sort(lista, [])   
end

		   
(* Test *)
val lsort1 = lsort ([[1,2,3],[1,1,1,1], [1], [1,2], [10,11]])


(* ARITHMETIC
============ *)


(* P31 Determine whether a given integer number is prime *)
fun isPrime n = 
let
    fun aux i = 
      if i >= n 
      then true
      else
	  if n mod i = 0
	  then false
	  else aux (i+1)
in
    aux 2
end

(* Test *)
val isPrime1 = isPrime 3539


(* P32 Determine the greatest common divisor of two psitive integer numbers *)
fun gcd(i, j) =
  if j = 0 
  then i
  else gcd(j, i mod j)


(* Test *)
val gcd1 = gcd(36, 63)


(* P33 Determine whether two positive integer numbers are coprime *)
fun coprime(i, j) =
  gcd(i,j) = 1


(* Test *)
val coprime1 = coprime(35, 64)


(* P34 Calculate Euler's totient function phi(m) *)
fun totient n =
let 
    fun aux(index, acc) =
      if index > n
      then acc
      else if coprime(index, n) then aux(index+1, acc+1) else aux(index+1, acc)
in
    aux(1, 0)
end


(* Test *)
val totient1 = totient 10
