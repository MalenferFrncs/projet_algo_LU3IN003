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

(*retourne l'element a la case d'indice i de la sous chaine si*)
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
    print_char !(si.s).[si.d+ !i];
  done;
  print_endline "";
;;








