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

let alien_infix_id = ['A'-'Z' 'a'-'z' '0'-'9' '+' '-' '*' '/' '<' '=' '>' '_']+

let alien_prefix_id = alien_infix_id

let var_id = ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9']* | alien_prefix_id

let label_id = ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let constr_id = ['A'-'Z' '_'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let type_con = ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let type_variable = '\'' ['a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

let int = ['0'-'9']+ | (hexavalue)+ | (binaryvalue)+

let atom = '\\' ['0'-'1'] (digit) (digit) | '\\' ['0'-'2'] ['0'-'5'] (digit)
	   | '\\' (hexavalue)(hexavalue) | (binaryvalue)+ | '\\' | '\'' | '\n'

let char = '\'' atom '\''

let string =  '\"' atom* '\"'



rule token = parse
  (** Layout *)
  | newline         { next_line_and token lexbuf }
  | blank+          { token lexbuf               }

  (** Keywords *)
  | "val"           { print_string("Val ");VAL  }
  | "type"          { print_string("TYPE");TYPE }
  | "rec"           { print_string("REC ");REC        }
  | "and"           { print_string("AND ");AND        }
  | "extern"        { print_string("EXTERN ");EXTERN  }


  (** Literals *)
  | int as d     { print_string("Integer ");INT (int_of_string d) }
  | digit+ as d     { INT (Int32.of_string d) }

  (** Infix operators *)
  | "-"             { MINUS "-"      	}
  | "+"             { PLUS "+"        	}
  | "*"             { STAR "*"        	}
  | "/"             { SLASH "/"		}

  (** Identifiers *)
  | type_variable as t		{ print_string("type ");TYPE_VAR t	}
  | var_id as i  		{ print_string("var_id ");MASTER_TKN i	} 
  | alien_prefix_id as i  	{ print_string("alien_id ");ID i      	} 
  | type_con as i		{ print_string("type_con ");MASTER_TKN i	}

  (** Punctuation *)
  | ":="	    { print_string("DEQUAL ");DEQUAL    }
  | ":"             { DDOT      }
  | ";"             { SEMICOLON }
  | "."             { DOT       }
  | ","             { COMMA     }
  | "("             { LPAREN    }
  | ")"             { RPAREN    }
  | "->"            { RARROW    }
  | "{"             { LCBRACK   }
  | "}"             { RCBRACK   }
  | "["             { RSBRACK   }
  | "]"             { LSBRACK   }
  | eof             { EOF       }


  (** Comment block *)
  | "{*"            { comment 0 lexbuf      }
  (** Lexing error. *)
  | _               { error lexbuf "unexpected character." }
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
