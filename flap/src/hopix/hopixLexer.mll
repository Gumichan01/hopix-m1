{
  open Lexing
  open Error
  open Position
  open HopixParser

  let next_line_and f lexbuf  =
    Lexing.new_line lexbuf;
    f lexbuf

  let error lexbuf =
    error "during lexing" (lex_join lexbuf.lex_start_p lexbuf.lex_curr_p)

  let convert_char s = match (String.length s) with
		| 1 -> s.[0]
		| 2 -> s.[1]
		| 3 -> match s with
			| "\'\\n" -> '\n'
			| "\'\\t" -> '\t'
			| "\'\\b" -> '\b'
			| "\'\\r" -> '\r'
			| "\'\\\'" -> '\''
			| "\'\\\\" -> '\\'
			| _ -> failwith "convert char parse error"

  (* let convert_char_num s = *)
  (*   let rec exp i l = *)
  (*     if i < 0 then l else exp (i - 1) (s.[i] :: l) in *)
  (*   exp (String.length s - 1) *)

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

let hexavalue = '0'['x''X'] ['0'-'9' 'a'-'f' 'A'-'F']

let binaryvalue = '0'['B''b']['0'-'1']

let alien_prefix_id = '`'['A'-'Z' 'a'-'z' '0'-'9' '+' '-' '*' '/' '<' '=' '>' '_']+

let alien_infix_id = alien_prefix_id '`'

let type_con = ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let constr_id = ['A'-'Z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let type_variable = '\'' ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let int = ['0'-'9']+ | (hexavalue)+ | (binaryvalue)+

let char_num = "\\"['0'-'2']['0'-'9']['0'-'9']

let char_hexa = "\\0"['x''X']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F']

let char_bin = "\\0"['b''B']['0'-'1']+

let atom = ['a'-'z'] | ['A'-'Z'] | char_num (* ['\000'-'\255'] *) (* "\\"['0'-'2']['0'-'9']['0'-'9'] *) | char_hexa  (* "\\0"['x''X']['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' 'a'-'f' 'A'-'F'] *) 
			| char_bin (* "\\0"['b''B']['0'-'1']+ *) | "\\'" | "\\n" | "\\t" | "\\b" | "\\r" | "\\\\"

let char = atom

let string =  '"' atom* '"'


rule token = parse
  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }

  (** Keywords *)
  | "VAL"           { VAL        }
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


  (** Literals *)
  | int as d     { (* print_string("Integer "); *)INT (Int32.of_string d)	}
  (* | "'"char_num as c"'" { (\* print_string("YO ");print_string("char (");print_string(c);print_string("|");print_int(String.length c);print_string(")"); *\) CHAR (convert_char_num c)} *)
  | "'"char as c"'"	 { (* print_string("char (");print_string(c);print_string("|");print_int(String.length c);print_string(")"); *) CHAR (convert_char c) }
  | string as c	 { (* print_string("string "); print_string(c); *) STRING c		  	}
  

  (** Infix operators *)
  | "-"             { MINUS "-"      	}
  | "+"             { PLUS "+"        	}
  | "*"             { STAR "*"        	}
  | "/"             { SLASH "/"		}
  | "&&"            { DBLAND "&&"       }
  | "||"            { DBLOR "||"        }
  | "="             { EQUAL "="         }
  | "<="            { INFEQU "<="       }
  | ">="            { SUPEQU "<="       }
  | "<"             { INF "<"           }
  | ">"             { SUP ">"           }


  (** Identifiers *)
  | type_variable as t		{ (* print_string("type "); *)TYPE_VAR t	  }
  | type_con as i		{ (* print_string("type_con "); *)MASTER_TKN i  }
  | constr_id as i		{ (* print_string("type_con "); *)CONSTR i      }
  | alien_prefix_id as i  	{ (* print_string("alien_id "); *)ID i      	  }
      
  (** Punctuation *)
  | ":="	    { (* print_string("DEQUAL "); *)DEQUAL       }
  | ":"             { (* print_string("DDOT "); *)DDOT           }
  | ";"             { (* print_string("SEMICOLON "); *)SEMICOLON }
  | "."             { (* print_string("DOT "); *)DOT             }
  | ","             { (* print_string("COMMA "); *)COMMA         }
  | "("             { LPAREN    			   }
  | ")"             { RPAREN    			   }
  | "->"            { RARROW    			   }
  | "<-"            { LARROW                               }
  | "=>"            { EQRARROW          }
  | "{"             { (* print_string("LCBRACK "); *)LCBRACK     }
  | "}"             { (* print_string("RCBRACK "); *)RCBRACK     }
  | "["             { (* print_string("LSBRACK "); *)LSBRACK     }
  | "]"             { (* print_string("RSBRACK "); *)RSBRACK     }
  | "|"             { (* print_string("VBAR "); *)VBAR           }
  | "&"             { AMP  }
  | "#"             { HASHTAG }
  | "\\"            { BACKSLASH }
  | "?"             { QMARK     }
  | "_"             { UNDERSCORE }
  | eof             { EOF       }


  (** Comment block *)
  | "{*"            { comment 0 lexbuf      }
  (* | "**"            { inlinecomment 0 lexbuf} *)
  (** Lexing error. *)
  | _               { print_string("NO LEXER ");error lexbuf "unexpected character." }
and comment count_level = parse
			| "{*" { comment (succ count_level) lexbuf         }
			| "*}" {
			  if count_level = 0
			  then
			    token lexbuf
			  else
			    comment (count_level -1) lexbuf
			}
			| _    { comment count_level lexbuf                }
			| eof  { error lexbuf "CLOSE YOUR FUCKING COMMENT" }

(* and inlinecomment count_level = parse *)
(*     | _ {inlinecomment count_level lexbuf } *)
(*     | "**" {inlinecomment count_level lexbuf} *)
(*     | "k" { token lexbuf } *)







