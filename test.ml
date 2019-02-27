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



(* Exercise 7.2 1: *)
(* Very space intensive - copies arrays way too much - should be a way to not do that*)
  
let permutations a =
  let len = Array.length(a) in
  let rec perms arr i =
    if i == len then [Array.make len 0]
    else
      let ls = ref [] in
      for j = i to len - 1 do
        Printf.printf "\ni = %d j = %d\n" i j;
        swap arr i j;
        let sub_perms = perms (Array.copy arr) (i + 1) in
        List.iter (fun sp -> print_int_array sp) sub_perms;
        List.iter (fun sp -> sp.(i) <- arr.(i)) sub_perms;
        List.iter (fun sp -> print_int_array sp) sub_perms;
        ls := !ls @ sub_perms
      done;
      !ls
  in perms (Array.copy a) 0;;




(* Exercise 7.2 2: *)

let perm a m =
  let n = Array.length(a) in
  if m >= fact(n) then a
  else begin
      let rec helper list pos num =
        let len = Array.length(list) - pos in
        let b = ref 0 in
        if len = 0 then [||]
        else if len = 1 then list
        else
          begin
            let continue = ref true in
            let i = ref 0 in
            while !continue do
              if num>=(!i*fact(len-1)) && num < ((!i+1)*fact(len-1))
              then begin
                  swap list pos (!i+pos);
                  b := !i*fact(len-1);
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
