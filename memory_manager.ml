(* Introduction to Data Structures and Algorithms (YSC2229), Sem2, 2018-2019 *)
(* Ilya Sergey <ilya.sergey@yale-nus.edu.sg> *)
(* Version of Wed 20 Feb 2019 *)


(*

The signature of the allocator module.

Implement a concrete module that instantiates it and use this
   signature to provide it an interface for the clients to use.

* N.B.: The "heap" structure should internally keep track of "free"
   memory segments; you can use an OCaml list (or lists) of for
   tracking free slots in "memory" arrays.

* N.B.: All interaction with the heap goes through the signature of
   the Allocator module.

* N.B.: Ordinary pointers can be implemented as integers (athough the
   clients of the module may not know it).

* N.B.: You will have to figure out how to "dispatch" pointers when
   they are dereferenced (via deref_as_* functions). That is you will
   have to determine whether a given pointer p points to another
   pointer, integer, or a string in order to chose the correct array.
   Devise a discipline to discriminate the pointers into those three
   catergories based on their value.

**)

module type Allocator = sig
  (* An abstract type for dynamic storage                                          *)
  type heap
  (* An abstract type for the pointer (address in the dynamic storage)             *)
  type ptr

  (* Create a new heap.                                                            *)
  val make_heap : unit -> heap

  (* Returns the "null" pointer. Noting can be assigned to it.                     *)
  val null : heap -> ptr
  val is_null : heap -> ptr -> bool

  (***                       Operations with pointers                            ***)
  (* All should throw exceptions for if the pointer is_null                        *)  

  (* Allocating a contiguous segment of dynamically-typed pointers in a heap.      *)
  (* Throws an specific "Out-Of-Memory" error if no allocation is possible.        *)
  val alloc : heap -> int -> ptr
  (* Frees the space in heap taken by the pointer.                                 *)
  val free : heap -> ptr -> int -> unit

  (* Dereferencing a pointer with an offset [0 .. n] obtainin a value it points to *)

  (* Dereference as an pointer, throw an exception if the target is not an pointer *)  
  val deref_as_ptr : heap -> ptr -> int -> ptr
  (* Dereference as an integer, throw an exception if the target is not an integer *)  
  val deref_as_int : heap -> ptr -> int -> int
  (* Dereference as an integer, throw an exception if the target is not an string  *)  
  val deref_as_string : heap -> ptr -> int -> string

  (* Assigning values to a pointer with an offset.                                 *)
  (* Should throw an "Out-Of-Memory" error if not possible to create a new value   *)
  (* The last argument is a value being assigned (of the corresponding type)       *)
  val assign_ptr : heap -> ptr -> int -> ptr -> unit
  val assign_int : heap -> ptr -> int -> int -> unit
  val assign_string : heap -> ptr -> int -> string -> unit
    (**)

end




module OurAllocator = struct
  (* An abstract type for the pointer (address in the dynamic storage)             *)
  let mem_size = 50;;
               
  type kind = Master | Allocated | Null | Pointer | Integer | Str
                                                
  type ptr = {
      of_kind : kind;
      index : int
    }
  (* An abstract type for dynamic storage                                          *)
  type heap = {
      master : ptr array;
      pointers : ptr array;
      integers: int array;
      strings: string array;
      pointers_free : bool array;
      integers_free: bool array;
      strings_free: bool array
    };;

  (* Returns the "null" pointer. Noting can be assigned to it.                     *)
  let null heap =
    {of_kind = Null; index = 0};;
  
  let is_null heap pointer =
    pointer.of_kind = Null;;
  

  (* Create a new heap.                                                            *)
  let  make_heap () =
    let new_heap = {
        master = Array.make mem_size (null());
        pointers = Array.make mem_size (null());
        integers = Array.make mem_size 0;
        strings = Array.make mem_size "";
        pointers_free = Array.make mem_size true;
        integers_free =  Array.make mem_size true;
        strings_free =  Array.make mem_size true
      }
    in new_heap;;


  (***                       Operations with pointers                            ***)
  (* All should throw exceptions for if the pointer is_null                        *)  

  (* Allocating a contiguous segment of dynamically-typed pointers in a heap.      *)
  (* Throws an specific "Out-Of-Memory" error if no allocation is possible.        *)

  let alloc hp len =
    let rec enough_free pos left =
      if pos >= mem_size then raise Out_of_memory
      else
        match left with
        | 0 -> true
        |_ -> (hp.master.(pos).of_kind = Null) && enough_free (pos+1) (left-1)
    in
    let change_status pos =
      for i = pos to pos + len do
        hp.master.(i) <- {of_kind = Allocated; index = 0}
      done
    in
    let rec find_free pos =
      if pos >= mem_size then raise Out_of_memory
      else
        match enough_free pos len with
        | true -> change_status pos;
                  {of_kind = Master; index = pos}
        | false -> find_free (pos + 1)
    in
    find_free 0
  ;;
          
      
  (* Frees the space in heap taken by the pointer.                                 *)
  let free hp pointer offset =
    if pointer.index + offset >= mem_size then raise Out_of_memory
    else
      let sub_ptr = hp.master.(pointer.index + offset) in
      (match sub_ptr.of_kind with
      | Pointer -> hp.pointers_free.(sub_ptr.index) <- true
      | Integer -> hp.integers_free.(sub_ptr.index) <- true
      | Str ->  hp.strings_free.(sub_ptr.index) <- true
      | _ -> ()
      );
      hp.master.(pointer.index+offset) <- null hp
  ;;
  

    
  (* Dereferencing a pointer with an offset [0 .. n] obtainin a value it points to *)
  
 

  (* Dereference as an pointer, throw an exception if the target is not an pointer *)  
  let deref_as_ptr hp pointer offset =
    let rec deref pointer offset =
      match pointer.of_kind with
      | Master -> deref (hp.master.(pointer.index + offset)) 0
      | Pointer -> hp.pointers.(pointer.index + offset)
      | _ -> raise (Failure "Target is not a pointer")
    in
    deref pointer offset

  (* Dereference as an integer, throw an exception if the target is not an integer *)  
  let deref_as_int hp pointer offset =
    let rec deref pointer offset =
      match pointer.of_kind with
      | Master -> deref (hp.master.(pointer.index + offset)) 0
      | Integer -> hp.integers.(pointer.index + offset)
      | _ -> raise (Failure "Target is not a pointer")
    in deref pointer offset
     
  (* Dereference as an integer, throw an exception if the target is not an string  *)   
  let deref_as_string hp pointer offset =
    let rec deref pointer offset =
      match pointer.of_kind with
      | Master -> deref (hp.master.(pointer.index + offset)) 0
      | Str -> hp.strings.(pointer.index + offset)
      | _ -> raise (Failure "Target is not a pointer")
    in deref pointer offset

  (* Assigning values to a pointer with an offset.                                 *)
  (* Should throw an "Out-Of-Memory" error if not possible to create a new value   *)
  (* The last argument is a value being assigned (of the corresponding type)       *)

  let assign_val list free_list value =
    let rec add index =
      if index >= mem_size then raise Out_of_memory
      else
        match free_list.(index) with
        | false -> add (index + 1)
        | true -> list.(index) <- value;
                  free_list.(index) <- false;
                  index
    in
    add 0;;
     
  let assign_ptr hp pointer offset value =
    if pointer.index + offset >= mem_size then raise Out_of_memory
    else
      match hp.master.(pointer.index + offset).of_kind with
      | Allocated|Null -> let target_index = assign_val hp.pointers hp.pointers_free value in
                          hp.master.(pointer.index + offset) <- {of_kind = Pointer;
                                                                 index = target_index}
      | _ -> raise Out_of_memory;;
  
  let assign_int hp pointer offset value =
    if pointer.index + offset >= mem_size then raise Out_of_memory
    else
      match hp.master.(pointer.index + offset).of_kind with
      | Allocated|Null -> let target_index = assign_val hp.integers hp.integers_free value in
                          hp.master.(pointer.index + offset) <- {of_kind = Integer;
                                                                 index = target_index}
      | _ -> raise Out_of_memory;;
  
  let assign_string hp pointer offset value =
    if pointer.index + offset >= mem_size then raise Out_of_memory
    else
      match hp.master.(pointer.index + offset).of_kind with
      | Allocated|Null -> let target_index = assign_val hp.strings hp.strings_free value in
                          hp.master.(pointer.index + offset) <- {of_kind = Str;
                                                                 index = target_index}
      | _ -> raise Out_of_memory;;
         
end



(*

An incomplete double-linked list implemented via bare pointers,
   parameterised over the memory allocator interface A.

* N.B.: The dll_node is no longer type-safe, it is just a pointer, and
   you will have to implement all the functions in a memory-safe way.
   The only allowed crashes are "Out-Of-Memory" exceptions.

*)
module DoubleLinkedList(A: Allocator) = 
  struct
    open A
    type dll_node = ptr

    (* Example: creating a node with an integer and a string *)
    let mk_node heap i s = 
      let segment = alloc heap 4 in
      assign_int heap segment 0 i;
      assign_string heap segment 1 s;
      let z = null heap in
      assign_ptr heap segment 2 z;
      assign_ptr heap segment 3 z;
      segment
       
    let prev heap (n : dll_node) = () (* Implement me! *)
    let next heap (n : dll_node) = () (* Implement me! *)
    let int_value heap (n : dll_node) = () (* Implement me! *)
    let string_value heap (n : dll_node) = () (* Implement me! *)
          
    let insert_after heap (n1 : dll_node) (n2 : dll_node) = 
      () (* Implement me! *)
      
    (* Prints the double-linked list starting from the node *)
    let print_from_node heap n = () (* Implement me! *)

    let remove heap n = () (* Implement me! *)
  end 

(*

A familiar Queue interface:

*)

module type Queue = 
  sig
    type 'e t
    val mk_queue : int -> 'e t
    val is_empty : 'e t -> bool
    val is_full : 'e t -> bool
    val enqueue : 'e t -> 'e -> unit
    val dequeue : 'e t -> 'e option
    val queue_to_list : 'e t -> 'e list
  end



(* A queue based on a double-linked list *)
module HeapDLLQueue(A: Allocator) = struct
  module DLL = DoubleLinkedList(A)
  open A
  open DLL

  type 'e t = {
    store : heap;
    head : dll_node;
    tail : dll_node;
  }

  let mk_queue _ = () (* Implement me! *)
  let is_empty q = () (* Implement me! *) 
  let enqueue q e = () (* Implement me! *) 
  let dequeue q = () (* Implement me! *) 
  let queue_to_list q = () (* Implement me! *) 
        
end
