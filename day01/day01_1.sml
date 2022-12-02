fun add_list [] = 0
  | add_list (x::xs) = x + add_list(xs);

fun add_list_list [] = nil
  | add_list_list (x::xs) = add_list(x) :: add_list_list(xs);

fun read_input (filename: string) = 
  let val fd = TextIO.openIn filename
  fun create_list inp = 
    case TextIO.inputLine inp of
         SOME line => line :: create_list inp
       | NONE => []
  in
    create_list fd before TextIO.closeIn fd
  end;

fun convert_to_int_list_list ([], curr: int list, res: int list list) = res @
  [curr]
   | convert_to_int_list_list (s::ss: string list, curr: int list, res: int list list) = 
   case Int.fromString s of
        SOME num => convert_to_int_list_list(ss, curr @ [num], res)
      | NONE => convert_to_int_list_list(ss, [], res @ [curr]);

fun upto ([], _) = []
  | upto (x, 0) = []
  | upto (x::xs, n) = x::upto(xs, n - 1);

fun from ([], _) = []
  | from (x, 0) = x
  | from (x::xs, n) = from(xs, n - 1);

fun merge ([], x) = x
  | merge (x, []) = x
  | merge (x::xs, y::ys) = 
    if x >= y then x::merge(xs, y::ys)
    else y::merge(x::xs, ys);

fun len [] = 0
  | len (x::xs) = 1 + len(xs);

fun merge_sort [] = []
  | merge_sort [x] = [x]
  | merge_sort (x::xs) = 
  let val half_len = len(x::xs) div 2
  in
    merge(merge_sort(upto(x::xs, half_len)), merge_sort(from(x::xs, half_len)))
  end;

val input = convert_to_int_list_list (read_input "input.txt", [], []);
print(Int.toString(add_list(upto(merge_sort(add_list_list(input)), 1))));
