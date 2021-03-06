let swap arr i j =
  let tmp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- tmp;;


let fact n =
  let rec helper n acc =
    if n = 0 then acc
    else helper (n-1) (acc*n) in
  helper n 1;;

let print_int_sub_array l u arr =
  assert (l <= u);
  assert (u <= Array.length arr);
  Printf.printf "[| ";
  for i = l to u - 1 do
    Printf.printf "%d" arr.(i);
    if i < u - 1
    then Printf.printf "; "
    else ()
  done;
  Printf.printf " |] "

let print_int_array arr =
  let len = Array.length arr in
  print_int_sub_array 0 (len) arr

let equivalent ls1 ls2 =
  let contains_all xs1 xs2 =
    List.for_all (fun e -> List.exists (fun x -> x = e) xs2) xs1
  in
  (List.length ls1 == List.length ls2) &&
    (contains_all ls1 ls2) &&
      (contains_all ls2 ls1)
  ;;


(* Exercise 7.2 1: *)

let perm_unit_test (cand: 'a array -> 'a array list) =
  cand [||] = [[||]]  &&
    equivalent (cand [|1; 2; 3|])
      [[|1; 2; 3|]; [|1; 3; 2|]; [|2; 1; 3|]; [|2; 3; 1|]; [|3; 1; 2|]; [|3; 2; 1|]]  &&
      equivalent (cand [|1;2;3;4|]) (cand [|3;4;2;1|])
;;


let permutations a =
  let len = Array.length(a) in
  let rec perms arr i =
    if i = len then [Array.copy a]
    else
      let ls = ref [] in
      for j = i to len - 1 do
        swap arr i j;
        let sub_perms = perms arr (i + 1) in
        List.iter (fun sp -> sp.(i) <- arr.(i)) sub_perms;
        swap arr i j;
        ls := !ls @ sub_perms
      done;
      !ls
  in perms (Array.copy a) 0;;

(* 
Passes the unit test; also passes the polymorphic unit test, done separately bc ocaml
can be a bit too typesafe at times.
*)
perm_unit_test permutations;;
equivalent (permutations [|(1,'a');(2,'b');(3,'c');(4,'d')|])
  (permutations [|(1,'a');(4,'d');(2,'b');(3,'c')|]);;



(* Exercise 7.2 2: *)

let perm a m =
  let n = Array.length(a) in
  if m >= fact(n) then a
  else begin
      let len = ref 0 in
      let b = ref 0 in
      let continue = ref true in
      let i = ref 0 in
      let rec helper list pos num =
        len := Array.length(list) - pos;
        b := 0;
        if !len = 0 then [||]
        else if !len = 1 then list
        else
          begin
            continue := true;
            i := 0;
            while !continue do
              if num>=(!i*fact(!len-1)) && num < ((!i+1)*fact(!len-1))
              then begin
                  for k = (!i + pos) downto pos + 1 do
                    swap list k (k-1)
                  done;
                  b := !i*fact(!len-1);
                  continue := false
                end
              else ();
              i := !i + 1;
            done;
            helper list (pos + 1) (num - !b)
          end
      in
      helper a 0 m
    end;;


(* Test: *)
perm [|1|] 0;;
perm [|1|] 1;;
perm [|1|] 2;;
perm [|1;2;3|] 0;;
perm [|1;2;3|] 1;;
perm [|1;2;3|] 2;;
perm [|1;2;3|] 3;;
perm [|1;2;3|] 4;;
perm [|1;2;3|] 5;;
perm [|1;2;3|] 6;;
perm [|1;2;3|] 7;;

let copy l1 =
  let len = Array.length l1 in
  let arr = Array.make len l1.(0) in
  for i=0 to len-1 do
    arr.(i) <- l1.(i)
  done;
  arr;;

let perm_test func list =
  let len = Array.length(list) in
  let result = ref true in
  for i=0 to fact(len)-1 do
    for j=i+1 to fact(len)-1 do
      let copy1 = copy list in
      let copy2 = copy list in
      let noproblem = (func copy1 i != func copy2 j) in
      if noproblem then () else Printf.printf "Problem found! %d %d\n" i j;
      result := !result && noproblem;
    done;
  done;
  if !result then Printf.printf "No problem found!!\n" else Printf.printf "Problem found!\n"              
;;

perm_test perm [|1;2|];;
perm_test perm [|1;2;3|];;
perm_test perm [|1;2;3;4|];;
perm_test perm [|1;2;3;4;5|];;
perm_test perm [|1;2;3;4;5;6|];;






(* Exercise 7.2 3&4: *)

module type Comparable = sig
  type t
  val comp : t -> t -> int
end

                       
module StupidSorting (Comp: Comparable) = struct
  include Comp

 let rec sorted ls =
    match ls with
    | [] -> true
    | h :: t ->
       List.for_all (fun e -> comp e h >= 0) t && sorted t

 let is_sorted arr =
   sorted (Array.to_list arr)

 let sort_unit_test cand =
   (is_sorted (cand [|2;1;0|])) &&
   (is_sorted (cand [|0;1;2|])) &&
   (is_sorted (cand [|5;3;7|])) &&
   (is_sorted (cand [|42;42|]))
 
 let remove arr index =
   let len= Array.length arr in
   let res = Array.make (len-1) arr.(0) in
   for i=0 to index-1 do
     res.(i) <- arr.(i)
   done;
   for i=index to (len-2) do
     res.(i) <- arr.(i+1)
   done;
   res;;
 

 let create_list arr =
   let len= Array.length arr in
   let final_arr = copy arr in
   let help = ref (copy arr) in
   let le = ref len in
   let num = ref 0 in
   for i=0 to len-1 do
     (if !le = 1
      then num := 0
      else num := Random.int (fact(!le)-1));
     let per = perm !help !num in
     final_arr.(i) <- per.(0);
     help := remove per 0;
     le := !le - 1;
   done;
   final_arr;;

 let silly_sort arr =
   let all_perms = permutations arr in
   let rec find_sorted perms =
     match perms with
     | [] -> raise (Failure "Something went wrong - maybe check your comparison function?")
     | h :: t -> ( match is_sorted(h) with
                   | true -> h
                   | false -> find_sorted t
                 )
   in find_sorted all_perms
    
 let super_silly_sort arr =
   let final_arr = ref (copy arr) in
   while (not (is_sorted(!final_arr))) do
     final_arr := create_list arr
   done;
   !final_arr;;
    
end;;          
  
module Desc = struct
  type t = int
  let comp = 
    fun x y -> if x < y then -1
            else if x = y then 0
            else 1
end;;

module StupidIntDescSort = StupidSorting(Desc);;
open StupidIntDescSort;;

sort_unit_test silly_sort;;
sort_unit_test super_silly_sort;;



