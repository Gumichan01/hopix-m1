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

  let convert_char s =
  match (String.length s) with
  | 1 -> s.[0]
  | 2 -> s.[1]
  | 3 -> match s with
		 | "\'\\n" -> '\n'
         | "\'\\t" -> '\t'
         | "\'\\b" -> '\b'
         | "\'\\r" -> '\r'
         | "\'\\\'" -> '\''
         | "\'\\\\" -> '\\'
         | _ -> failwith "convert_char parse error"

  let convert_num cn =
    Char.chr(int_of_string(String.sub cn 2 ((String.length cn)-3)))
}

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let symbol = [ '+' '-' '*' '/' '<' '=' '>' ]

let digit = ['0'-'9']

let lowercase_alpha = ['a'-'z']

let uppercase_alpha = ['A'-'Z']

let alpha = lowercase_alpha | uppercase_alpha

let alphanum = alpha | digit | '_'

let basic_identifier = lowercase_alpha alphanum*

let prefix_alien_identifier = "`" (alpha | symbol | digit)+

let infix_alien_identifier = "`" (alpha | symbol | digit)+ "`"

let identifier = basic_identifier | prefix_alien_identifier

(* Symboles terminaux *)

let blankvalue = '\n' | '\t' | '\b' | '\r'

let hexavalue = '0'['x''X']['0'-'9' 'a'-'f' 'A'-'F']+

let binaryvalue = '0'['B''b']['0'-'1']+

let alien_prefix_id = '`'['A'-'Z' 'a'-'z' '0'-'9' '+' '-' '*' '/' '<' '=' '>' '_']+

let alien_infix_id = alien_prefix_id '`'

let type_con = ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let constr_id = ['A'-'Z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let type_variable = '\'' ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let int = ['0'-'9']+ | (hexavalue) | (binaryvalue)

let char_num = '\\'['0'-'2']['0'-'9']['0'-'9']

let char_hexa = '\\''0'['x''X']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']

let char_bin = '\\''0'['b''B']['0'-'1']+

let printable = [' ' - '\255']

let atom = char_num | char_hexa | char_bin | printable | "\\'" | "\\n" | "\\t"
           | "\\b" | "\\r" | "\\\\"

let char = atom

let string = "\"" (atom | "\"")* "\""


rule token = parse
  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }

  (** Keywords *)
  | "val"           { VAL        }
  | "type"          { TYPE       }
  | "rec"           { REC        }
  | "and"           { AND        }
  | "extern"        { EXTERN     }
  | "do"            { DO         }
  | "done"          { DONE       }
  | "if"            { IF         }
  | "then"          { THEN       }
  | "else"          { ELSE       }
  | "fi"            { FI         }

  (** Literals *)
  | int as d                { INT (Int32.of_string d)                 }
  | "'"char as c"'"	        { CHAR (convert_char c)                   }
  | '"'                     { read_string (Buffer.create 1024) lexbuf }
  | '\''char_num '\''  as c
  | '\''char_hexa '\'' as c
  | '\''char_bin '\''  as c { CHAR (convert_num c)                    }

  (** Infix operators *)
  | "-"             { MINUS "-"      	}
  | "+"             { PLUS "+"        	}
  | "*"             { STAR "*"        	}
  | "/"             { SLASH "/"		    }
  | "&&"            { DBLAND "&&"       }
  | "||"            { DBLOR "||"        }
  | "="             { EQUAL "="         }
  | "<="            { INFEQU "<="       }
  | ">="            { SUPEQU "<="       }
  | "<"             { INF "<"           }
  | ">"             { SUP ">"           }


  (** Identifiers *)
  | type_variable as tvar		     { TYPE_VAR tvar   }
  | type_con as tcon		         { MASTER_TKN tcon }
  | constr_id as cons		         { CONSTR cons     }
  | alien_prefix_id as alieni  	     { ID alieni       }
  | infix_alien_identifier as alienp { INFIXID alienp  }

  (** Punctuation *)
  | ":="	        { DEQUAL     }
  | ":"             { DDOT       }
  | ";"             { SEMICOLON  }
  | "."             { DOT        }
  | ","             { COMMA      }
  | "("             { LPAREN     }
  | ")"             { RPAREN     }
  | "->"            { RARROW     }
  | "<-"            { LARROW     }
  | "=>"            { EQRARROW   }
  | "{"             { LCBRACK    }
  | "}"             { RCBRACK    }
  | "["             { LSBRACK    }
  | "]"             { RSBRACK    }
  | "|"             { VBAR       }
  | "&"             { AMP        }
  | "#"             { HASHTAG    }
  | "\\"            { BACKSLASH  }
  | "?"             { QMARK      }
  | "_"             { UNDERSCORE }
  | eof             { EOF        }

  (** Comment block *)
  | "{*"            { comment 0 lexbuf                     }
  | "**"            { inlinecomment 0 lexbuf               }
  (** Lexing error. **)
  | _               { error lexbuf "unexpected character." }

and comment count_level = parse
			| "{*" { comment (succ count_level) lexbuf   }
			| "*}" {if count_level = 0
                    then token lexbuf
                    else comment (count_level -1) lexbuf }
			| _    { comment count_level lexbuf          }
			| eof  { error lexbuf "unclosed comment"     }

and read_string buffer = parse
    | '"'           { STRING (Buffer.contents buffer)                        }

    | '\\' '\''     { Buffer.add_char buffer '\''; read_string buffer lexbuf }

    | [^ '"' '\\']+ { Buffer.add_string buffer (Lexing.lexeme lexbuf);
                      read_string buffer lexbuf                              }

    | _             { raise (SyntaxError ("Illegal string : "
                                          ^ Lexing.lexeme lexbuf))           }
    | eof           { raise End_of_file                                      }

and convert_char_num s = parse
    | _ { print_char(s); }

and inlinecomment count_level = parse
| _       { inlinecomment count_level lexbuf }
| "**"    { inlinecomment count_level lexbuf }

| newline {
  print_string("newline");
  if count_level = 0
  then next_line_and token lexbuf
  else inlinecomment count_level lexbuf      }

| eof  {token lexbuf}
