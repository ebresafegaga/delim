{
    open Lexing 
    open Parser

    exception SyntaxError of Lexing.position * string

    let next_line lexbuf =
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
            { pos with 
                pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1 }
}


let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = '-'? digit+

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read_token = parse 
    | "true" { TRUE }   
    | "false" { FALSE }
    | "("  { LPAREN }
    | ")" { RPAREN }
    | "[" { LBRACK }
    | "]" { RBRACK }
    | "{" { LBRACE }
    | "}" { RBRACE }
    | "," { COMMA }
    | ";" { SEMICOLON }
    | "==" { EQUALEQUAL }
    | "=" { EQUALS }
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { TIMES }
    | "/" { DIV }
    | ">" { GT }
    | "<" { LT }
    | '"' { read_string (Buffer.create 17) lexbuf }
    | whitespace { read_token lexbuf }
    | newline  { next_line lexbuf; read_token lexbuf }
    | "#"  { read_single_line_comment lexbuf }
    | id { ID (Lexing.lexeme lexbuf) }
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    (* | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) } *)
    | eof { EOF }

and read_string buf = parse
    | '"'       { STRING (Buffer.contents buf) }
    | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
    | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string buf lexbuf
    }
    | _ { raise (SyntaxError (lexbuf.lex_curr_p, "Illegal string character: " ^ Lexing.lexeme lexbuf)) }
    | eof { raise (SyntaxError (lexbuf.lex_curr_p, "String is not terminated")) }

and read_single_line_comment = parse
  | newline { next_line lexbuf; read_token lexbuf }
  | eof { EOF }
  | _ { read_single_line_comment lexbuf }