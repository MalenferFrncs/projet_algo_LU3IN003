open Stdlib

let cins =2
let cdel =2
   
   
let sub (couple : char*char) : int = 
  match couple with
    'A','A' |'T','T'|'G','G'|'C','C'  -> 0
  |'A','T'|'T','A'|'G','C'|'C','G' -> 3
  |_-> 4
;;


    
    
(* x et y indexé de 0 a n-1 et tab de 0 a n *)

let sol1 (tab : ((int array  array)ref)) (x : string ref)(y :string ref):(string*string) =
  let s : (string list ref)  = ref [] in
  let t : string list ref = ref [] in
  let i  = ref (String.length(!x)) in
  let j  = ref (String.length(!y)) in
  
  
  while (!i>0 || !j>0) do 
    
    if (!j>0) && !tab.(!i).(!j) = (!tab.(!i).(!j-1)+cins) then 
      begin 
        s := "-"::!s ;
        t := (String.make 1 (!y.[!j-1]))::!t ;
        j := !j -1;
      end
    else begin 
      if (!i>0) && !tab.(!i).(!j) = (!tab.(!i-1).(!j)+cdel) then 
        begin
          
          s := String.make 1 !x.[!i-1]::!s;
          t := "-"::!t;
          i := !i-1; 
        end
      else (*if !tab.(!i).(!j) = (!tab.(!i-1).(!j-1)+(sub (!x.[!i],!y.[!j]) )) then *)
        begin 
          s := String.make 1 !x.[!i-1]::!s;
          t := String.make 1 !y.[!j-1]::!t;
          i := !i-1;
          j := !j-1;
        end 
    end;
    
  done;
  
  
  (String.concat "" !s),(String.concat "" !t);;



let dist1 (s1r : string ref)(s2r :string ref) : int array array =
  let lenX = String.length !s1r in
  let lenY = String.length !s2r in
  let tab : int array array =Array.make_matrix (lenX+1) (lenY+1) 0 in
  
  let min a b c = 
    if a>b then 
      if b>c then c else b 
    else if a>c then c else a
  in

  for i = 0 to lenX do 
    tab.(i).(0) <- i*cdel ;
  done;
  
  for j = 0 to lenY do 
    tab.(0).(j) <- j*cins ;
  done;
  
  for i = 1 to lenX do
    for j = 1 to lenY do
      begin
        tab.(i).(j) <-  (min (tab.(i).(j-1)+cins) (tab.(i-1).(j)+cdel) (tab.(i-1).(j-1)+(sub (!s1r.[i-1],!s2r.[j-1])) ));
      end
    done
  done;
  tab 
;; 




let prog_dyn (s1 : string)(s2 : string)=
  let s1r = ref s1 in
  let s2r = ref s2 in 
  let tab : int array array ref  = ref  (dist1  s1r s2r) in 
  let ali :(string*string) = sol1  tab s1r s2r in 
  (!tab.(String.length s1).(String.length s2),ali)
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
  i,(s1,s2) ->
    print_int i;
    print_endline "";
    print_string s1;
    print_endline "" ;
    print_string s2;
    print_endline "";
;;



      



