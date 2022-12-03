fun read_input (filename: string) = 
  let val fd = TextIO.openIn filename
  fun create_list inp = 
    case TextIO.inputLine inp of
         SOME line => line :: create_list inp
       | NONE => []
  in
    create_list fd before TextIO.closeIn fd
  end;

fun strip_newline s: string = Substring.string(Substring.trimr 1
  (Substring.full(s)))
