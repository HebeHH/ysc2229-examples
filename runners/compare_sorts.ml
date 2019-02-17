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
open Week_05
open SortUtil

module AP = Week_05.ArrayPrinter(KV) 

(* TODO: 

1. parse command-line arguments 
2. Read from file
3. Write to file
4. Run utop from the project
5. Run utop from emacs

*)

let time_two_sorts len = 
  let arr = generate_key_value_array len in
  let arr' = copy_array arr 0 len in
  let t = time_string "Simple Merge Sort  " merge_sort arr in
  let t' = time_string "Enhanced Merge Sort" fast_merge_sort arr' in
  Printf.printf "%s%s" t t'

(* Simple executable *)
let () =   
  let len = 100000 in
  time_two_sorts len
     
