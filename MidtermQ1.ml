open Week_01
open Week_02
open Week_03
open Week_04 
open Week_05

let smallest_missing_non_negative_integer arr = 
	let j = ref 0 in
	while (arr.(!j)!= !j && arr.(!j)< (Array.length arr) && arr.(!j) != arr.(arr.(!j))) do
		swap arr !j (arr.(!j));
		j := !j + 1
  done

  for i = 0 to (Array.length arr)  do
    if (i != arr.(i)) then i
    else i+1  
  done
	



let test_smallest_missing_non_negative_integer candidate =
  (candidate (Array.copy [|0; 1|]) = 2) &&
  (candidate (Array.copy [|1; 0|]) = 2) &&
  (candidate (Array.copy [|0; 1; 3; ~-4|]) = 2) &&
  (candidate (Array.copy [|1; ~-2; 3; ~-1; 0|]) = 2) &&
  (candidate (Array.copy [|3; 1; 0|]) = 2) &&
  (candidate (Array.copy [|0; 10; 3; 33; 2; 5; 100|]) = 1) &&
  (candidate (Array.copy [|0; 10; 3; 33; 2; 5; 1|]) = 4) &&
  (candidate (Array.copy [|0; 10; 11; 2; 3; 7; 6; 6; 3; 33; 2; 5; 1|]) = 4) &&
  (candidate (Array.copy [|0; 10; 11; 2; 3; ~-33; 7; 6; 4; 4; 4; 6; 3; 33; 2; 5; 1|]) = 8) &&
  (candidate (Array.copy [|1; 2; 3; 4; 5; 6; ~-33; ~-33; ~-33; 7; 8; 9; ~-33; ~-34; 12; 12; 0|]) = 10) &&
  (candidate (Array.copy [|1; 2; 3; 4; 5; 6; 7; 8; 9|]) = 0) &&
  (candidate (Array.copy [|10; 9; 8; 7; 0; 6; 5; 4; 3; 2; 1|]) = 11);;

  smallest_missing_non_negative_integer test_smallest_missing_non_negative_integer;;

  