open Stdlib

let cins =2
let cdel =2
   
   
let sub (couple : char*char) : int = 
  match couple with
    'A','A' |'T','T'|'G','G'|'C','C'  -> 0
  |'A','T'|'T','A'|'G','C'|'C','G' -> 3
  |_-> 4
;;

let dist : ((int array array) ref) = ref (Array.make_matrix 0 0 0);;


let dist1 (s1r : string ref)(s2r :string ref) : int  =
  let lenX = String.length !s1r in
  let lenY = String.length !s2r in
  dist :=Array.make_matrix (lenX+1) (lenY+1) 0 ;
  
  let min a b c = 
    if a>b then 
      if b>c then c else b 
    else if a>c then c else a
  in

  for i = 0 to lenX do 
    !dist.(i).(0) <- i*cdel ;
  done;
  
  for j = 0 to lenY do 
    !dist.(0).(j) <- j*cins ;
  done;
  
  for i = 1 to lenX do
    for j = 1 to lenY do
      begin
        !dist.(i).(j) <-  (min (!dist.(i).(j-1)+cins) (!dist.(i-1).(j)+cdel) (!dist.(i-1).(j-1)+(sub (!s1r.[i-1],!s2r.[j-1])) ));
      end
    done
  done;

  !dist.(lenX).(lenY)
;; 




let prog_dyn (s1 : string)(s2 : string)=
  let s1r = ref s1 in
  let s2r = ref s2 in 
  let d : int   = (dist1  s1r s2r) in 
  (* let ali :(string*string) = sol1  !dist s1r s2r in *)
  d
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
