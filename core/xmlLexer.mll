(** This is a cutdown version of lexer.mll that just implements the
    XML lexer functionality.
*)

{

  open Lexing
  open Utility
  open XmlParser

  class lexer_context =
  object
    val lexers = Stack.create ()

    method push_lexer (lexer : Lexing.lexbuf -> XmlParser.token) =
      Stack.push lexer lexers

    method pop_lexer =
      ignore (Stack.pop lexers) [@warning "-5"]

    method next_lexer =
      Stack.top lexers
  end

  let fresh_context () = new lexer_context

  let bump_lines lexbuf n =
    Printf.printf "bump_lines: n = %d\n" n;
    flush stdout;
    lexbuf.lex_curr_p <- {
      lexbuf.lex_curr_p with 
      pos_lnum = lexbuf.lex_curr_p.pos_lnum + n;
      pos_bol = lexbuf.lex_curr_p.pos_cnum+1;
      }

  let count_newlines = StringUtils.count '\n'

  (* let bump_lines lexbuf lexeme =
  (* Count newlines in the lexeme *)
  let nl_count = StringUtils.count '\n' lexeme in
  (* Find the position of the last newline in the lexeme, if any *)
  let last_nl_pos =
    try Some (String.rindex lexeme '\n')
    with Not_found -> None
  in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
    pos_lnum = lexbuf.lex_curr_p.pos_lnum + nl_count;
    pos_bol = match last_nl_pos with
      | Some idx -> lexbuf.lex_curr_p.pos_cnum - (String.length lexeme - idx - 1)
      | None -> lexbuf.lex_curr_p.pos_bol
  } *)

  (* let bump_lines lexbuf lexeme =
  let nl_count = StringUtils.count '\n' lexeme in
  match String.rindex_opt lexeme '\n' with
  | Some last_nl_pos ->
      let after_newline = String.sub lexeme (last_nl_pos + 1) (String.length lexeme - last_nl_pos - 1) in
      let indent = String.length after_newline in
      let new_pos = lexbuf.lex_curr_p.pos_cnum + String.length lexeme in
      lexbuf.lex_curr_p <- {
        lexbuf.lex_curr_p with
        pos_lnum = lexbuf.lex_curr_p.pos_lnum + nl_count;
        pos_bol = new_pos - indent;  (* Calculate from the new position *)
        pos_cnum = new_pos;  (* Update current position *)
      }
  | None -> 
      lexbuf.lex_curr_p <- {
        lexbuf.lex_curr_p with
        pos_cnum = lexbuf.lex_curr_p.pos_cnum + String.length lexeme
      } *)


  exception LexicalError of (string * Lexing.position)
}

let def_id = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*)
let def_attr_id = (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '_' '-' '0'-'9']*)
let def_kind = ['A'-'Z'] def_id*
let octal_code = (['0'-'3']['0'-'7']['0'-'7'])
let hex_code   = (['0'-'9''a'-'f''A'-'F']['0'-'9''a'-'f''A'-'F'])
let def_qname = ('#' | def_attr_id (':' def_attr_id)*)
let def_tagname = ('#' | def_id (':' def_attr_id)*)
let def_integer = (['1'-'9'] ['0'-'9']* | '0')
let def_float = (def_integer '.' ['0'-'9']+ ('e' ('-')? def_integer)?)
let def_blank = [' ' '\t' '\n' '\r']
let char_contents = ([^ '\"' '\\']|"\\\"" |"\\\\" | "\\n" | "\\r" | "\\t" | ('\\' octal_code) | ('\\' ['x' 'X'] hex_code))
let string_contents = char_contents*

let xml_opening = ('<' def_id)
let xml_closing_tag = ('<' '/' def_id '>')

(* Each lexer when called must return exactly one token and possibly
   modify the stack of remaining lexers.  The lexer on top of the stack
   will be called next;  when each action starts it's the current lexer.

   Each rule takes two arguments: the currently operative precedence
   table and the stack.
*)

rule lex ctxt nl = parse
  | eof                                 { END }
  | "<!"                                { (* come back here after ignoring <!...> *)
                                          ctxt#push_lexer (ignore ctxt nl); IGNORE }
  | "<?"                                { (* come back here after ignoring <?...> *)
                                          ctxt#push_lexer (ignore ctxt nl); IGNORE }
  | def_blank                           { IGNORE }
  | '<' (def_tagname as id)             { (* come back here after scanning the start tag *)
                                          ctxt#push_lexer (starttag ctxt nl); LXML id }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
and starttag ctxt nl = parse
  | def_qname as var                    { VARIABLE var }
  | '='                                 { EQ }
  | '>'                                 { (* Switch to `xmllex' *)
                                          ctxt#pop_lexer; ctxt#push_lexer (xmllex ctxt nl); RXML }
  | '"'                                 { (* Come back here after scanning the attr value *)
                                          ctxt#push_lexer (attrlex ctxt nl); LQUOTE }
  | "/>"                                { ctxt#pop_lexer (* fall back *); SLASHRXML }
  | '\n'                                { nl () ; bump_lines lexbuf 1; starttag ctxt nl lexbuf }
  | def_blank                           { starttag ctxt nl lexbuf }
  | eof                                 { END }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
and xmllex ctxt nl = parse
  | [^ '<' ]* as cdata                  { bump_lines lexbuf (count_newlines cdata); CDATA cdata }
  | "</" (def_tagname as var) '>'       { (* fall back *)
                                          ctxt#pop_lexer; ENDTAG var }
  | "<![CDATA["                         { (* switch to cdata, then back here *)
                                          ctxt#push_lexer (cdata ctxt nl); LCDATA }
  | "<!"                                { (* come back here after ignoring <!...> *)
                                          ctxt#push_lexer (ignore ctxt nl); IGNORE }
  | "<?"                                { (* come back here after ignoring <?...> *)
                                          ctxt#push_lexer (ignore ctxt nl); IGNORE }
  | '<' (def_tagname as var)            { (* switch to `starttag' to handle the nested xml, then back here *)
                                          ctxt#push_lexer (starttag ctxt nl); LXML var }
  | eof                                 { END }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
and attrlex ctxt nl = parse
  | '"'                                 { (* fall back *)
                                          ctxt#pop_lexer; RQUOTE }
  | [^ '"']* as string                  { bump_lines lexbuf (count_newlines string); STRING string }
  | _                                   { raise (LexicalError (lexeme lexbuf, lexeme_end_p lexbuf)) }
and ignore ctxt nl = parse
  | '>'                                 { (* fall back *)
                                          ctxt#pop_lexer; IGNORE }
  | [^ '>']+                            { IGNORE }
  | eof                                 { END }
and cdata ctxt nl = parse
  | "]]>"                               { (* fall back *)
                                          ctxt#pop_lexer; RCDATA }
  | _ as c                              { CHAR c }

{
 let lexer ctxt ~newline_hook =
   ctxt#push_lexer (lex ctxt newline_hook);
   fun lexbuf -> ctxt#next_lexer lexbuf
}
