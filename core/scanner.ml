open Utility
open SourceCode
open Lexing

type position_context = SourceCode.source_code

module type LexerSig = sig
  type token
  type lexer_context
  type 'a grammar = (Lexing.lexbuf -> token) -> Lexing.lexbuf -> 'a

  val lexer : lexer_context
           -> newline_hook:(unit -> unit)
           -> (Lexing.lexbuf -> token)
  val fresh_context : unit -> lexer_context
end

module Scanner (Lex : LexerSig) = struct
  let read : context:Lex.lexer_context
          -> ?nlhook:(unit -> unit)
          -> parse:('result Lex.grammar)
          -> infun:(bytes -> int -> int)
          -> name:string
          -> unit
          -> 'result * source_code =
  fun ~context ?nlhook ~parse ~infun ~name () ->
    let code = new source_code in
    let lexbuf = {(from_function (code#parse_into infun))
                 with lex_curr_p={pos_fname=name; pos_lnum=1; pos_bol=0; pos_cnum=0}} in
    try
      Printf.printf("(before) lexbuf.lex_curr_p.pos_cnum: %d\n") lexbuf.lex_curr_p.pos_cnum;
      Printf.printf("(before) lexbuf.lex_curr_p.pos_lnum: %d\n") lexbuf.lex_curr_p.pos_lnum;
      Printf.printf("(before) lexbuf.lex_curr_p.pos_bol: %d\n") lexbuf.lex_curr_p.pos_bol;
      let p = parse (Lex.lexer context ~newline_hook:(from_option identity nlhook)) lexbuf in
      Printf.printf("\n");
      Printf.printf("(after) lexbuf.lex_curr_p.pos_cnum: %d\n") lexbuf.lex_curr_p.pos_cnum;
      Printf.printf("(after) lexbuf.lex_curr_p.pos_lnum: %d\n") lexbuf.lex_curr_p.pos_lnum;
      Printf.printf("(after) lexbuf.lex_curr_p.pos_bol: %d\n") lexbuf.lex_curr_p.pos_bol;

      (p, code)
    with
    | Parser.Error ->
          let line, column = code#find_line lexbuf.lex_curr_p in
            raise
              (Errors.RichSyntaxError
                 {Errors.filename = name;
                  Errors.linespec = string_of_int lexbuf.lex_curr_p.pos_lnum;
                  Errors.message = "";
                  Errors.linetext = line;
                  Errors.marker = String.make column ' ' ^ "^" })
      | Sugartypes.ConcreteSyntaxError (pos, msg) ->
          let start, finish = Position.start pos, Position.finish pos in
          let linespec =
            if start.pos_lnum = finish.pos_lnum
            then string_of_int start.pos_lnum
            else (string_of_int start.pos_lnum  ^ "..."
                  ^ string_of_int finish.pos_lnum) in
          let line = code#extract_line_range (start.pos_lnum-1) finish.pos_lnum in
          let _, column = code#find_line finish in
            raise
              (Errors.RichSyntaxError
                 {Errors.filename = name;
                  Errors.linespec = linespec;
                  Errors.message = msg;
                  Errors.linetext = line;
                  Errors.marker = String.make column ' ' ^ "^"})
      | Lexer.LexicalError (lexeme, position) ->
          let line, column = code#find_line position in
            raise
              (Errors.RichSyntaxError
                 {Errors.filename = name;
                  Errors.linespec = string_of_int position.pos_lnum;
                  Errors.message = "Unexpected character : " ^ lexeme;
                  Errors.linetext = line;
                  Errors.marker = String.make column ' ' ^ "^"})

                  (* let read : context:Lex.lexer_context
    -> ?nlhook:(unit -> unit)
    -> parse:('result Lex.grammar)
    -> infun:(bytes -> int -> int)
    -> name:string
    -> unit
    -> 'result * source_code =
    Printf.printf("Inside Lexer.read\n");
    flush stdout;
  fun ~context ?nlhook ~parse ~infun ~name () ->
    let code = new source_code in
    let absolute_pos = ref 0 in
    let line_start = ref 0 in
    let current_line = ref 1 in

    let lexbuf = Lexing.from_function (fun buffer nchars ->
      let nchars = infun buffer nchars in
      for i = 0 to nchars - 1 do
        if Bytes.get buffer i = '\n' then (
          (* Update line start and line number *)

          line_start := !absolute_pos + i + 1;
          incr current_line
        )
      done;
      absolute_pos := !absolute_pos + nchars;
      nchars
    ) in

    (* Initialize lexbuf positions *)
    lexbuf.lex_start_p <- {
      pos_fname = name;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    };
    lexbuf.lex_curr_p <- {
      pos_fname = name;
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    };

    try
      (* Update the lexbuf position at each newline *)
      (* let update_lexbuf_position lexbuf =
        lexbuf.lex_curr_p <- {
          lexbuf.lex_curr_p with
          pos_lnum = !current_line;
          pos_bol = !line_start;
        }
      in *)

      
      Printf.printf("(before) Lexbuf charnum: %d\n") lexbuf.lex_curr_p.pos_cnum;
      Printf.printf("(before) Lexbuf linenum: %d\n") lexbuf.lex_curr_p.pos_lnum;
      Printf.printf("(before) Lexbuf bol: %d\n") lexbuf.lex_curr_p.pos_bol;

      (* Parse with the provided grammar *)
      let p = parse (Lex.lexer context ~newline_hook:(from_option identity nlhook)) lexbuf in
      (* Ensure final position update *)
      (* let p = parse (Lex.lexer context ~newline_hook:(from_option identity nlhook)) lexbuf in *)
      (* update_lexbuf_position lexbuf; *)

      Printf.printf("Lexbuf charnum: %d\n") lexbuf.lex_curr_p.pos_cnum;
      Printf.printf("Lexbuf linenum: %d\n") lexbuf.lex_curr_p.pos_lnum;
      Printf.printf("Lexbuf bol: %d\n") lexbuf.lex_curr_p.pos_bol;
      (p, code)

    with
      | Parser.Error ->
            let line, column = code#find_line lexbuf.lex_curr_p in
              raise
                (Errors.RichSyntaxError
                    {Errors.filename = name;
                    Errors.linespec = string_of_int lexbuf.lex_curr_p.pos_lnum;
                    Errors.message = "";
                    Errors.linetext = line;
                    Errors.marker = String.make column ' ' ^ "^" })
        | Sugartypes.ConcreteSyntaxError (pos, msg) ->
            let start, finish = Position.start pos, Position.finish pos in
            let linespec =
              if start.pos_lnum = finish.pos_lnum
              then string_of_int start.pos_lnum
              else (string_of_int start.pos_lnum  ^ "..."
                    ^ string_of_int finish.pos_lnum) in
            let line = code#extract_line_range (start.pos_lnum-1) finish.pos_lnum in
            let _, column = code#find_line finish in
              raise
                (Errors.RichSyntaxError
                    {Errors.filename = name;
                    Errors.linespec = linespec;
                    Errors.message = msg;
                    Errors.linetext = line;
                    Errors.marker = String.make column ' ' ^ "^"})
        | Lexer.LexicalError (lexeme, position) ->
            let line, column = code#find_line position in
              raise
                (Errors.RichSyntaxError
                    {Errors.filename = name;
                    Errors.linespec = string_of_int position.pos_lnum;
                    Errors.message = "Unexpected character : " ^ lexeme;
                    Errors.linetext = line;
                    Errors.marker = String.make column ' ' ^ "^"}) *)

  let normalize_context = function
    | None   -> Lex.fresh_context ()
    | Some c -> c
end
