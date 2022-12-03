val PROJECT_ROOT = case OS.Process.getEnv("PROJECT_ROOT") of
                        SOME root => root
                      | NONE => raise Fail "PROJECT_ROOT environment variable must be set"; 
val input_file = String.concat (PROJECT_ROOT::["/day01/input.txt"]);
use (String.concat (PROJECT_ROOT::["/common/input.sml"]));

fun sum (a, b) = a + b;
fun add_list x = List.foldr sum 0 x;
fun add_list_list x = List.map add_list x;

fun convert_to_int_list_list ([], curr: int list, res: int list list) = res @
  [curr]
   | convert_to_int_list_list (s::ss: string list, curr: int list, res: int list list) = 
   case Int.fromString s of
        SOME num => convert_to_int_list_list(ss, curr @ [num], res)
      | NONE => convert_to_int_list_list(ss, [], res @ [curr]);

fun merge ([], x) = x
  | merge (x, []) = x
  | merge (x::xs, y::ys) = 
    if x >= y then x::merge(xs, y::ys)
    else y::merge(x::xs, ys);

fun merge_sort [] = []
  | merge_sort [x] = [x]
  | merge_sort (x::xs) = 
  let val half_len = List.length (x::xs) div 2
  in
    merge(merge_sort(List.take(x::xs, half_len)), merge_sort(List.drop(x::xs, half_len)))
  end;

val input = convert_to_int_list_list (read_input input_file, [], []);
print(Int.toString(add_list(List.take(merge_sort(add_list_list(input)), 1))));
print(Int.toString(add_list(List.take(merge_sort(add_list_list(input)), 3))));
