
(*Gary Tsai*)
(*SML Project*)

(* Question 2 *)

fun fact(x,0) = x
|   fact(0,y) = 0
|   fact(x,y) = if y <= x then x else y * fact (x,y-1);


fun c(n,0) = 0
|   c(0,k) = 0
|   c(n,k) = if k > n then 0 else fact(n-k+1, n) div fact (1,k);





(*Question 3*)

fun cut(m,[]) = []
     | cut(m,n::l) = if m=n then cut(m,l) else n::cut(m,l);

fun rem_duplicate [] = []
     | rem_duplicate (m::l) = m::rem_duplicate(cut(m,l)); 




(*Question 1*)

fun divide (pivot, []) = ([], [])

  | divide (pivot, a::b) =
      let val (small, big) = divide (pivot, b) 
      in
	if a < pivot then (a::small, big) 
	       	     else (small, a::big)
      end;


fun quicksort ([]) = []
  | quicksort (p::[]) =[p]
  | quicksort (p::rest) = 
      let val (small, big) = divide(p,rest)
      in
	quicksort(small) @ [p] @ quicksort(big)
      end;


(*Testing examples*)


fact(4,1);
fact (5,5);

c(5,2);
c(3,2);

rem_duplicate [9];
rem_duplicate [1,2,1] ;
rem_duplicate ["a","a","a"] ;
rem_duplicate [[1],[1,2],[1,2,3],[1,2],[4,5]] ;
rem_duplicate [1,3,1,3,1,4,6] ;



 