(*Library Functions *)
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

let copy l1 =
  let len = Array.length l1 in
  let arr = Array.make len l1.(0) in
  for i=0 to len-1 do
    arr.(i) <- l1.(i)
  done;
  arr;;

(* Exercise 7.2 4: (Bonus) *)

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
  for i=0 to len-1 do
    let num = Random.int (fact(!le)-1) in
    let per = perm !help num in
    final_arr.(i) <- per.(0);
    help := remove per 0;
    le := !le - 1;
  done;
  final_arr;;

let super_silly_sort arr =
  let len = Array.length arr in
  let final_arr = ref (copy arr) in
  while is_sorted(!final_arr) do
    final_arr := create_list arr
  done;
  !final_arr;;

