val PROJECT_ROOT = case OS.Process.getEnv("PROJECT_ROOT") of
                        SOME root => root
                      | NONE => raise Fail "PROJECT_ROOT environment variable must be set"; 
val input_file = String.concat (PROJECT_ROOT::["/day03/input.txt"]);
use (String.concat (PROJECT_ROOT::["/common/input.sml"]));

fun get_priority a =
  if Char.isLower a then
    (Char.ord a) - (Char.ord #"a") + 1
  else
    (Char.ord a) - (Char.ord #"A") + 27;

fun sum (a, b) = a + b;

fun containsa (a, x) = List.exists (fn y => a = y) x;

fun find_common (l1, l2) = List.filter (fn x => containsa(x, l2)) l1;

fun solve ([], score) = score
  | solve (x::xs, score) = 
  let
    val chars = String.explode x
    val half_len = (List.length chars) div 2
    val first_half = List.take(chars, half_len)
    val second_half = List.drop(chars, half_len)
    val ans = List.hd(find_common(first_half, second_half))
    val pr = get_priority ans
  in
    solve(xs, score+pr)
  end;

fun solve2 ([], score) = score
  | solve2 (x::y::z::xs, score) =
  let
    val xchars = String.explode x
    val ychars = String.explode y
    val zchars = String.explode z
    val ans = List.hd(find_common(find_common(xchars, ychars), zchars))
    val pr = get_priority ans
  in
    solve2(xs, score+pr)
  end;

val input = List.map strip_newline (read_input(input_file));
print(Int.toString(solve(input, 0)));
print(Int.toString(solve2(input, 0)));
