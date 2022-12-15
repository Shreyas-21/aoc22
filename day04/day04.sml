val PROJECT_ROOT = case OS.Process.getEnv("PROJECT_ROOT") of
                        SOME root => root
                      | NONE => raise Fail "PROJECT_ROOT environment variable must be set";
val input_file = String.concat (PROJECT_ROOT::["/day04/input.txt"]);
use (String.concat (PROJECT_ROOT::["/common/input.sml"]));

fun split_range str = String.tokens (fn x => x = #"-") str;

fun split_pair str = String.tokens (fn x => x = #",") str;

fun includes (a::b::c::d::xs, ans) =
  if (a <= c andalso b >= d) orelse (c <= a andalso d >=
  b) then
    includes(xs, ans+1)
  else
    includes(xs, ans)
  | includes (nil, ans) = ans;

fun overlaps (a::b::c::d::xs, ans) =
  if (b >= c andalso d >= a) then
    overlaps(xs, ans+1)
  else
    overlaps(xs, ans)
  | overlaps (nil, ans) = ans;

val input = List.map strip_newline (read_input(input_file));
val numbers = List.map Option.valOf (List.map Int.fromString (List.concat (List.map split_range (List.concat (List.map
  split_pair input)))));

fun solve numbers = includes(numbers, 0);
fun solve2 numbers = overlaps(numbers, 0);

print(Int.toString(solve numbers));
print(Int.toString(solve2 numbers));
