let exitOK     = 0
let exitBadArg = 1
let exitErr    = 2

let usage_msg = "asm2c [-d] <file1> [<file2>]"

let speclist =
  [("-d", Arg.Set Flags.debug, "Output debug information")]

let process_file filename =
  if !Flags.debug then
    Printf.eprintf "Parsing %s\n" filename ;
  let inBuffer = open_in filename in
  let lineBuffer = Lexing.from_channel inBuffer in
  lineBuffer.Lexing.lex_curr_p <- { lineBuffer.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  try
    let _ = Parser.prog Lexer.prog lineBuffer in
    exitOK
  with
  | Parser.Error ->
     Format.eprintf "%a Syntax error (parsing error)\n" Utils.pr_pos (Lexing.lexeme_start_p lineBuffer);
     exitErr

let _ =
  Arg.parse speclist
    (fun f -> let rc = process_file f in
              if rc = exitOK then ()
              else exit 0)
    usage_msg;
