(* MODULE CHARLIST *)

type charList = 
  |Cell of char*(charList ref)*(charList ref)
  | Null;;




let ncharList (s1:charList)  :charList =
  match s1 with
    Cell(c1,rnext1,rprec1) -> 
      rnext1 := s1;
      rprec1 := s1; 
      s1;

  |_-> Null



let fn (s1:charList)(s2:charList):charList=
  match s1,s2 with
    Cell(c1,rnext1,rprec1),Cell(c2,rnext2,rprec2) -> 
      begin
        match !rprec1,!rprec2 with
          Cell(ci,rnexti,rpreci),Cell(cj,rnextj,rprecj)->

            let mem1 = !rprec1 in
            rprec1 := !rprec2 ;
            rprec2 := mem1;

            let mem2 = !rnexti in
            rnexti := !rnextj;
            rnextj:= mem2;
            s1 
        |Cell(ci,nexti,rpreci),Null -> s1
        |_-> s2
      end
  |Cell(c1,rnext1,rprec1),Null  -> s1
  |Null,Cell(c2,rnext2,rprec2) -> s2
  |_-> Null

let string_of_charList (cl: charList) :string = 

  match cl with
    Null -> ""
  |Cell(c,rnext,rprec) ->

      let rec loop (c : charList) (cin : charList ref) :string list= 

        if  c == !cin then[]
        else 
          match c with
            Cell(c,rnext,rprec)-> (String.make 1 c)::(loop !rnext cin)
          |_ -> failwith "pb dans string_of_charList"
      in
      String.concat "" (String.make 1 c::(loop !rnext (ref cl )))

(* MODULE STRING_INDICE *)

(*** definie un tuple stringI et les methodes associés 
  stringI permet de representer une sous-chaine d'une chaine de caractère
  elle possède 3 element

  s : une reference vers la chaine de caractère
  d : l'indice de la chaine referencé par s a partir de laquel la sous chaine si commence
  t : la taille (en nombre d'element) de la sous chaine si

  si t = 0 alors la sous chaine est vide

  les sous chaines sont initialisé de tel sorte a etre equivalent a la chaine d'origine cad
  s = ref chaine ; d = 0 ; t = String.length !(ref chaine)

   ***)


type stringI = {s :string ref; d : int ; t : int } 

   (* crée une nouvelle stringI a partir d'une ref sur une chaine*)
let create (str:string ref) : stringI =
  {s=str;d=0;t=String.length(!str)}
;;
   
   (* coupe un  StringI en une stringI de i caractère et une stringI de t-i caractère*)
   (* la premiere demare a l'indice de depart et a i caractère, la seconde demare a d+i et a t-i caractère*)
let divI (si : stringI) (i : int): stringI*stringI= 
  if i > si.t then failwith "index oob" 
  else
    ({s=si.s;d=si.d;t=i},{s=si.s;d=si.d+i;t=si.t-i})
;; 
   
   (*retourne l'element a ieme case de la sous chaine si*)
let getSI (si : stringI) (i:int) : char =
  if( 0 >= si.t )then failwith "SI vide"
  else if(si.t>i) then !(si.s).[si.d + i]
  else failwith "index oob"
;;
   (* retourne la sous chaine composé des i premiers elements de si*)
let substrLow (si:stringI) (i:int) =
  {s=si.s;d=si.d;t=i}
;;
   
   (*retourne la sous chaine composé des t-i derniers elements de si*)
let substrHigh (si:stringI) (i:int) =
  {s=si.s;d=si.d+i;t=si.t-i}
;;
   
   (*affiche la sous chaine si*)
let printSI (si : stringI): unit = 
  let i = ref 0 in
  while !i < si.t do
    print_char (getSI si !i);
    incr i;
  done;
  print_endline "";
;;
   


(* VALEURS DES PARAMETRE POUR ALIGNEMENTS *)
let cins =2
let cdel =2
let csubconc = 3
let csubautre = 4

let sub (couple : char*char) : int = 
  match couple with
    'A','A' |'T','T'|'G','G'|'C','C'  -> 0
  |'A','T'|'T','A'|'G','C'|'C','G' -> csubconc
  |_-> csubautre
;;

let mot_gaps (i : int) : charList =
  if i = 0 then failwith "on ne peu pas faire un mot de 0 gap"
  else 
    let ckgap = ncharList (Cell('-',ref Null,ref Null)) in
    
    for k =2 to i do
      fn ckgap (ncharList (Cell('-',ref Null,ref Null)))   
    done;
    ckgap
;;
  
let charList_of_stringI (si:stringI) : charList =
  if 0 >= si.t then failwith "on ne peu pas faire de mot de taille nul"
  else
    begin
      let cl = ncharList (Cell(getSI si 0,ref Null, ref Null)) in
      for i = 1 to si.t-1 do
        fn cl (ncharList (Cell(getSI si i,ref Null, ref Null)));
      done;  
      cl
    end

let allign_lettre_mot (si1,si2 : stringI*stringI) : charList*charList =
  let all_char(c :char) (si : stringI)= 
    let min_c = ref 10 in
    let min_i = ref 0 in
    for i = 0 to si.t-1 do
      if !min_c > sub(c,getSI si i) then
        begin
          min_i := i;
          min_c := sub(c,getSI si i)
        end
      else ();
    done;
    let mot_all = fn (mot_gaps !min_i) (ncharList (Cell(c,ref Null,ref Null))) in
    if !min_i = si.t-1 then mot_all 
    else  fn mot_all (mot_gaps (si.t-1- !min_i))
  in


  (all_char (getSI si1 0) si2),(charList_of_stringI si2)

;;
   
(* CORPS DE DIV PR REGNER *)

(* retour le cout de l'alignement minimal *)
let disti2 (si1 : stringI)(si2 :stringI) : int  =
  
  let init c i = i*c in
  let t1 : (int array) ref  = ref (Array.init (si1.t+1) (init cdel))  in
  let t2 : int array ref = ref (Array.make (si1.t+1) 0) in
 
  let min a b c =    
    if a>b then 
      if b>c then c else b 
    else if a>c then c else a
  in

  let j = ref 1 in   
  while !j < (si2.t+1) do

    !t2.(0) <- !j*cins; 
    for i = 1 to si1.t do    
      !t2.(i) <-  ( min (!t2.(i-1)+cins)  (!t1.(i)+cdel) (!t1.(i-1)+(sub (getSI si1 (i-1),getSI si2 (!j-1)))) );        
    done;
    
    incr j ;
    t1 := !t2; 
    t2 := Array.make (si1.t+1) 0;    
  done;
  !t1.(si1.t)
;; 

(*renvoit la coupure de j associé de i*)
let coupure (si1,si2 : stringI*stringI) (i : int): int =
  let si1d,si1f = divI si1 i in
  let j = ref 0 in
  let res = ref 1 in
  while !j < si2.t do 
    let si2d,si2f = divI si2 !j in
    if ((disti2 si1d si2d)+(disti2 si1f si2f)) = (disti2 si1 si2) then (
      res := !j;
      j:=si2.t; )
    else 
      incr j;
  done;
  !res

;;

let sol2 (s1,s2 : string*string) : string*string*int =  
  let si1 = create (ref s1) in
  let si2 = create (ref s2) in

  let rec loop (si1,si2 : stringI*stringI) : charList*charList = 
    (*let len1 = si1.t - si1.d + 1 in
let len2 = si2.t - si2.d + 1 in*)
    if  si1.t > 1 && si2.t >= 1 then
      begin 
        let c1 = si1.t/2 in
        let c2 = coupure (si1,si2) c1 in
      (*appel recusif*)
        let (cl1d,cl2d) = loop ((substrLow si1 c1),(substrLow si2 c2)) in
        let (cl1f,cl2f) = loop ((substrHigh si1 c1),(substrHigh si2 c2)) in
      (* fusion sous pb*)
        
        let clx =fn cl1d cl1f in
        let cly =fn cl2d cl2f in
        (clx,cly)
      end
    else (* cas de bases*)
      begin
        match si1.t,si2.t with
          (2,0)->  (fn (ncharList (Cell((getSI si1 0),ref Null,ref Null)) ) (ncharList (Cell((getSI si1 1),ref Null,ref Null))) ),(mot_gaps 2)  
        |(1,1)->(ncharList (Cell((getSI si1 0),ref Null,ref Null)) ),(ncharList (Cell ((getSI si2 0),ref Null , ref Null)))
        |(1,0)->(ncharList (Cell((getSI si1 0),ref Null,ref Null)) ),(ncharList (Cell (('-'),ref Null , ref Null)))
        |_-> allign_lettre_mot (si1,si2)
      end
  in

  let (c1,c2) = loop (si1,si2) in
  (string_of_charList c1),(string_of_charList c2),(disti2 si1 si2)
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
  
  (* print_string s1def;
  print_string s2def; *)


  let al1,al2,cout = sol2 (s1def,s2def) in
  print_endline al1;
  print_endline al2;
  print_int cout;
  print_endline "";
;;