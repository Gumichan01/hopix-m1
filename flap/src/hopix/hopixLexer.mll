{
  open Lexing
  open Error
  open Position
  open HopixParser

  exception SyntaxError of string

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)


  (* [clean_string s] cleans the string removing the first '\'' (single quote)
     if it is here. Otherwise, the string itself is returned *)
  let clean_string s : string =
    match String.get s 0 with
    | '\'' -> (String.sub s 1 ((String.length s) - 1))
    | _ -> s

  (* [convert_char s] takes a string that is one of :

     - "\000" - "\255"
     - "\0x00" - "0xFF"
     - "0b00000000" - "0b11111111"

     and returns a character that is matching with one of the ASCII value
     if the character is printable *)
  let convert_char (s : string) : char =
    Char.chr ( int_of_string (String.sub s 1 ((String.length s) - 1)) )

   (* [convert_string s] takes a string that is one of :

      - "\000" - "\255"
      - "\0x00" - "0xFF"
      - "0b00000000" - "0b11111111"

      and returns a transformed string using convert_char *)
  let convert_string (s:string) : string =
    (s |> convert_char |> Char.escaped)

  let read_char_as_string s =
  match (String.length s) with
  | 1 -> s.[0]
  | 2 -> s.[1]
  | _ ->
    (match s with
	 | "\'\\n" -> '\n'
     | "\'\\t" -> '\t'
     | "\'\\b" -> '\b'
     | "\'\\r" -> '\r'
     | "\'\\\'" -> '\''
     | "\'\\\\" -> '\\'
     | _ as ss -> (convert_char (clean_string ss))
    )

  let convert_num cn =
    Char.chr(int_of_string(String.sub cn 2 ((String.length cn)-3)))

}

(* Simple tokens *)
let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let symbol  = [ '+' '-' '*' '/' '<' '=' '>' ]*

let digit   = ['0'-'9']

let lowercase_alpha = ['a'-'z']

let uppercase_alpha = ['A'-'Z']

let hexavalue = '0'['x''X']['0'-'9' 'a'-'f' 'A'-'F']+

let binaryvalue = '0'['B''b']['0'-'1']+

let char_num = '\\'['0'-'2']['0'-'9']['0'-'9']

let char_hexa = '\\''0'['x''X']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']

let char_bin = '\\''0'['b''B']['0'-'1']+

let printable = [' ' - '\255']

let alpha = lowercase_alpha | uppercase_alpha

(* Complex token *)
let infix_alien_identifier = "`" (alpha | symbol | digit)+ "`"

let alien_prefix_id = '`'['A'-'Z' 'a'-'z' '0'-'9' '+' '-' '*' '/' '<' '=' '>' '_']+

let alien_infix_id = alien_prefix_id '`'

let type_con = ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let var_id = alien_prefix_id

let label_id = type_con

let constr_id = ['A'-'Z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let type_variable = '\'' ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let int = digit+ | hexavalue | binaryvalue

let atom = char_num | char_hexa | char_bin | printable | "\\'" | "\\n" | "\\t"
           | "\\b" | "\\r" | "\\\\"

let char = atom

let string = "\"" (atom)* "\""


rule token = parse
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }

  (** Keywords *)
  | "val"           { VAL                        }
  | "type"          { TYPE                       }
  | "rec"           { REC                        }
  | "and"           { AND                        }
  | "extern"        { EXTERN                     }
  | "do"            { DO                         }
  | "done"          { DONE                       }
  | "if"            { IF                         }
  | "then"          { THEN                       }
  | "else"          { ELSE                       }
  | "fi"            { FI                         }
  | "true" as b
  | "false" as b    { BOOL (bool_of_string b)    }

  (** Literals *)
  | int as d                { INT (Int32.of_string d)                 }
  | "'"char as c"'"	        { CHAR (read_char_as_string c)            }
  | '"'                     { read_string (Buffer.create 1024) lexbuf }
  | '\''char_num '\''  as c
  | '\''char_hexa '\'' as c
  | '\''char_bin '\''  as c { CHAR (convert_num c)                    }

  (** Infix operators *)
  | "-"             { MINUS    }
  | "+"             { PLUS     }
  | "*"             { MULT     }
  | "/"             { DIVIDE   }
  | "&&"            { BOOLAND  }
  | "||"            { BOOLOR   }
  | "="             { EQUAL    }
  | "<="            { LTE      }
  | ">="            { GTE      }
  | "<"             { LT       }
  | ">"             { GT       }


  (** Identifiers *)
  | var_id as id     	             { ID id            }
  | label_id as tcon		         { MASTER_TKN tcon  }
  | type_variable as tvar		     { TYPE_VAR tvar    }
  | constr_id as cons		         { CONSTR cons      }
  | infix_alien_identifier as alienp { INFIXID alienp   }

  (** Punctuation *)
  | ":="            { DEQUAL        }
  | ":"             { DDOT          }
  | ";"             { SEMICOLON     }
  | "."             { DOT           }
  | ","             { COMMA         }
  | "("             { LPAREN        }
  | ")"             { RPAREN        }
  | "->"            { RARROW        }
  | "<-"            { LARROW        }
  | "=>"            { EQRARROW      }
  | "{"             { LCBRACK       }
  | "}"             { RCBRACK       }
  | "["             { LSBRACK       }
  | "]"             { RSBRACK       }
  | "|"             { VBAR          }
  | "&"             { AMP           }
  | "#"             { SHARP         }
  | "\\"            { BACKSLASH     }
  | "?"             { QMARK         }
  | "_"             { UNDERSCORE    }
  | eof             { EOF           }

  (** Comment block *)
  | "{*"            { comment 0 lexbuf                     }
  | "**"            { inlinecomment 0 lexbuf               }
  | _               { error lexbuf "unexpected character." }

and comment count_level = parse
  | "{*"            { comment (succ count_level) lexbuf    }
  | "*}"            { if count_level = 0
                      then token lexbuf
                      else comment (count_level -1) lexbuf }

  | _               { comment count_level lexbuf           }
  | eof             { error lexbuf "unclosed comment"      }

and read_string buffer = parse
  | '"'             { STRING (Buffer.contents buffer)                        }
  | '\\' '\''       { Buffer.add_char buffer '\''; read_string buffer lexbuf }

  | char_num  as s
  | char_bin  as s
  | char_hexa as s  { Buffer.add_string buffer (convert_string s);
                      read_string buffer lexbuf                              }

  | '\\' 'n'        { Buffer.add_char buffer '\n'; read_string buffer lexbuf }
  | '\\' 't'        { Buffer.add_char buffer '\t'; read_string buffer lexbuf }
  | '\\' 'b'        { Buffer.add_char buffer '\b'; read_string buffer lexbuf }
  | '\\' 'r'        { Buffer.add_char buffer '\r'; read_string buffer lexbuf }

  | '\\' '"'        { assert false (* by backslash before the last '"' *)    }
  | printable as p  { Buffer.add_char buffer p; read_string buffer lexbuf    }

  | eof             { raise End_of_file                                      }

(* TODO at the end of line, recall token *)
and inlinecomment count_level = parse
  | eof             { token lexbuf                                           }
  | _               { inlinecomment count_level lexbuf                       }
