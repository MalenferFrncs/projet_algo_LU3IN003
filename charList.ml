

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

