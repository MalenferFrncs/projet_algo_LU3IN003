(**

pour completer les fins de sol1 si besoin
(avant d'avoir integré la conditions !i>0 et !j>0 dans les if internes)

if !i>0 then (
  while !i>0 do
    s := String.make 1 !x.[!i]::!s;
    t := "-"::!t;
    i := !i-1; 
  done;)
else ();
if !j>0 then (
  while !j>0 do 
    s := "-"::!s;
    t := String.make 1 !y.[!j]::!t;
    j := !j-1;
  done;)
else ();


(*fct pour affivher les matrices *)
let print_array_array ( arr : int array  array) =
  let i = ref 0 in
  let j = ref 0 in
  while (!j<Array.length arr.(0)) do
    while (!i<Array.length arr) do 
      print_int arr.(!i).(!j);
      i := !i +1;
    done;
    print_string "\n";
    j := !j +1;
    i:= 0;
  done;
;;

(* x et y indexé de 0 a n-1 et tab de 0 a n *)

(* let sol1 (tab : ((int array  array)ref)) (x : string ref)(y :string ref):(string*string) =
  let s : (string list ref)  = ref [] in
  let t : string list ref = ref [] in
  let i  = ref (String.length(!x)) in
  let j  = ref (String.length(!y)) in
  
  
  while (!i>0 && !j>0) do 
    
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
  
  
  (String.concat "" !s),(String.concat "" !t);; *)


*)