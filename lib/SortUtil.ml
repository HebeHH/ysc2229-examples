(*
This file is part of teaching material of Yale-NUS College module
"YSC2229: Introductory Data Structures and Algorithms"

Copyright (c) 2019 Ilya Sergey

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

open Week_03

let better_merge aux lo mid hi dest =
  let i = ref lo in
  let j = ref mid in
  for k = lo to hi - 1 do
    aux.(k) <- dest.(k)
  done;
  for k = lo to hi - 1 do
    if !i >= mid
    then (dest.(k) <- aux.(!j); j := !j + 1)
    else if !j >= hi
    then (dest.(k) <- aux.(!i); i := !i + 1)
    else if fst aux.(!i) <= fst aux.(!j)
    then (dest.(k) <- aux.(!i); i := !i + 1)
    else (dest.(k) <- aux.(!j); j := !j + 1)
  done

let fast_merge_sort arr = 
  let len = Array.length arr in
  let aux = copy_array arr 0 len in

  let rec sort lo hi = 
    if hi - lo <= 1 then ()
    else
      let mid = lo + (hi - lo) / 2 in
      sort lo mid; sort mid hi;
      better_merge aux lo mid hi arr

  in
  sort 0 len

let time_string msg f x =
  let t = Sys.time () in
  let _fx = f x in
  Printf.sprintf "Execution elapsed time of %s: %f sec\n"
    msg (Sys.time () -. t)

