unit = ()

zero = 0
one = 1

nil   = []
list1 = one : nil
list2 = one : list1
list3 = zero : list2
list4 = append list3 list3
list5 = append list4 list4

append l1 l2 =
    case l1 of
      [] -> l2
      hd : tl -> hd : append tl l2

myseq x y = case x of z -> y

forcelist list = 
   case list of
      [] -> unit
      h : t -> seq h $ forcelist t

doit = seq (forcelist list5) list5
