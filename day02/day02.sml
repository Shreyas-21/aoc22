val PROJECT_ROOT = case OS.Process.getEnv("PROJECT_ROOT") of
                        SOME root => root
                      | NONE => raise Fail "PROJECT_ROOT environment variable must be set"; 
val input_file = String.concat (PROJECT_ROOT::["/day02/input.txt"]);
use (String.concat (PROJECT_ROOT::["/common/input.sml"]));

fun solve1 ([], score) = score
  | solve1(x::strats, score) =
  if x = "A X" then solve1(strats, score + 4)
  else if x = "A Y" then solve1(strats, score + 8)
  else if x = "A Z" then solve1(strats, score + 3)
  else if x = "B X" then solve1(strats, score + 1)
  else if x = "B Y" then solve1(strats, score + 5)
  else if x = "B Z" then solve1(strats, score + 9)
  else if x = "C X" then solve1(strats, score + 7)
  else if x = "C Y" then solve1(strats, score + 2)
  else if x = "C Z" then solve1(strats, score + 6)
  else raise Domain;

fun solve2 ([], score) = score
  | solve2(x::strats, score) =
  if x = "A X" then solve2(strats, score + 3)
  else if x = "A Y" then solve2(strats, score + 4)
  else if x = "A Z" then solve2(strats, score + 8)
  else if x = "B X" then solve2(strats, score + 1)
  else if x = "B Y" then solve2(strats, score + 5)
  else if x = "B Z" then solve2(strats, score + 9)
  else if x = "C X" then solve2(strats, score + 2)
  else if x = "C Y" then solve2(strats, score + 6)
  else if x = "C Z" then solve2(strats, score + 7)
  else raise Domain;

val input = List.map strip_newline (read_input(input_file));
print(Int.toString(solve1(input, 0)));
print(Int.toString(solve2(input, 0)));
