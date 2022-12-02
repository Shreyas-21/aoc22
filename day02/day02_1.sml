fun strip_newline s: string = Substring.string(Substring.trimr 1
  (Substring.full(s)))

fun read_input (filename: string) = 
  let val fd = TextIO.openIn filename
  fun create_list inp = 
    case TextIO.inputLine inp of
         SOME line => strip_newline line :: create_list inp
       | NONE => []
  in
    create_list fd before TextIO.closeIn fd
  end;

fun solve ([], score) = score
  | solve(x::strats, score) =
  if x = "A X" then solve(strats, score + 4)
  else if x = "A Y" then solve(strats, score + 8)
  else if x = "A Z" then solve(strats, score + 3)
  else if x = "B X" then solve(strats, score + 1)
  else if x = "B Y" then solve(strats, score + 5)
  else if x = "B Z" then solve(strats, score + 9)
  else if x = "C X" then solve(strats, score + 7)
  else if x = "C Y" then solve(strats, score + 2)
  else if x = "C Z" then solve(strats, score + 6)
  else raise Domain;

print(Int.toString(solve(read_input("input.txt"), 0)))
