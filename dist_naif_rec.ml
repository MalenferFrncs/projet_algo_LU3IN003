type data = (string ref)*(string ref)*int*int*int*int (*s1,s2,0,0,0,255 *)
let indel = 2 

let () = 
  let fileIN = open_in Sys.argv.(1) in
  let i = int_of_string (input_line fileIN) in
  let j = int_of_string (input_line fileIN) in
  let s1 = input_line fileIN in
  let s2 = input_line fileIN in

  let s_to_sdef (s : string)(i : int ) : char =
    s.[i*2]
  in

  let s1def = String.init (i) (s_to_sdef s1) in
  let s2def = String.init (j) (s_to_sdef s2) in
  



   
   
   
   let sub (couple : char*char) : int = 
     match couple with
       'A','A' |'T','T'|'G','G'|'C','C'  -> 0
     |'A','T'|'T','A'|'G','C'|'C','G' -> 3
     |_-> 4
   in
   
   
   let rec dist_naif_rec(x:data) : int = 
     let distr = ref 0 in 
   
     match x with
       s1,s2,i,j,c,dist -> 
         distr := dist ;
         if (i = (String.length !s1 ) && j = (String.length !s2)) then
           if c < !distr then distr := c 
           else distr := !distr
         else 
             
         if i < ((String.length !s1) ) && j< ((String.length !s2)) then 
           (** i+1-1 car caractère suivant mais indices des carac de 0 a n-1 au lieu de 1 a n *)
           let ndata : data =((s1,s2,(i+1),(j+1),c+(sub ((String.get !s1 (i)),(String.get !s2 (j)))),!distr)) in  
           
           distr := (dist_naif_rec ndata); 
         else ();
           
         if i < ((String.length !s1) ) then 
           let ndata : data =((s1,s2,(i+1),j,c+indel,!distr)) in 
           
           distr := (dist_naif_rec ndata);
         else ();
           
         if j < ((String.length !s2)) then 
           let ndata : data =((s1,s2,(i),(j+1),c+indel,!distr)) in 
           
           distr := dist_naif_rec ndata;
         else ();
         
         
         !distr
        in
        


     
   (*on choisit 255 comme valeur max positive, sur de sequences plus longues
      on peu envisager de prendres une valeur plus elevé*)
  let dist_naif (s1 : string)(s2 : string) :int =
    let s1ref = ref s1 in
    let s2ref = ref s2 in
    let data = (s1ref,s2ref,0,0,0,255) in 
    dist_naif_rec data
  in

  print_int (dist_naif s1def s2def);
  print_endline "";

  
   

    
             