let swap arr i j =
  let tmp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- tmp;;


let fact n =
  let rec helper n acc =
    if n = 0 then acc
    else helper (n-1) (acc*n) in
  helper n 1;;

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
