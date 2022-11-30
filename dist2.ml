open Stdlib

let cins =2
let cdel =2

let sub (couple : char*char) : int = 
  match couple with
    'A','A' |'T','T'|'G','G'|'C','C'  -> 0
  |'A','T'|'T','A'|'G','C'|'C','G' -> 3
  |_-> 4
;;
   
let print_array( arr : int array ) =
  let i = ref 0 in

    while (!i<Array.length arr) do 
      print_int arr.(!i);
      i := !i +1;
   
   
  
  done;
  print_endline "";
;;
   



let dist2 (s1r : string ref)(s2r :string ref) : int  =
  let lenX = String.length !s1r in
  let lenY = String.length !s2r in
  (* let tab : int array array =Array.make_matrix (lenX+1) (lenY+1) 0 in*)
  let init c i = i*c in
  let t1 : (int array) ref  = ref (Array.init (lenX+1) (init cdel))  in
  let t2 : int array ref = ref (Array.make (lenX+1) 0) in

  
  let min a b c = 
   
    if a>b then 
      if b>c then c else b 
    else if a>c then c else a
  in
  (**
  for i = 0 to lenX do 
    tab.(i).(0) <- i*cdel ;
  done; *)
  
  (** for j = 0 to lenY do 
    tab.(0).(j) <- j*cins ;
  done; *)

  let j = ref 1 in

  
 
  while !j < (lenY+1) do

    !t2.(0) <- !j*cins; 
    for i = 1 to lenX do
        
        !t2.(i) <-  ( min (!t2.(i-1)+cins)  (!t1.(i)+cdel) (!t1.(i-1)+(sub (!s1r.[i-1],!s2r.[!j-1]))) );
        
    done;
    
    incr j ;
    t1 := !t2;
 
    t2 := Array.make (lenX+1) 0;
    
    
  done;
  !t1.(lenX)
;; 




let prog_dyn (s1 : string)(s2 : string)=
  let s1r = ref s1 in
  let s2r = ref s2 in 
  let tab : int   = (dist2  s1r s2r) in 
  (* let ali :(string*string) = sol1  tab s1r s2r in *)
  tab
;;


let () = 
  let fileIN = open_in Sys.argv.(1) in
  let i = int_of_string (input_line fileIN) in
  let j = int_of_string (input_line fileIN) in
  let s1 = input_line fileIN in
  let s2 = input_line fileIN in
  print_int i;
  print_endline "";
  print_int j; 
  print_endline "";

  let s_to_sdef (s : string)(i : int ) : char =
    s.[i*2]
  in

  let s1def = String.init (i) (s_to_sdef s1) in
  let s2def = String.init (j) (s_to_sdef s2) in
  
  let data = prog_dyn s1def s2def in

  match data with
  i ->
    print_int i;
    print_endline "";
    
;;
