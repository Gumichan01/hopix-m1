exception Error

type token = 
  | VAL
  | TYPE_VAR of (string)
  | TYPE
  | STAR of (string)
  | SLASH of (string)
  | SEMICOLON
  | RSBRACK
  | RPAREN
  | REC
  | RCBRACK
  | RARROW
  | PLUS of (string)
  | MINUS of (string)
  | MASTER_TKN of (string)
  | LSBRACK
  | LPAREN
  | LCBRACK
  | INT of (int)
  | ID of (string)
  | EXTERN
  | EOF
  | DOT
  | DEQUAL
  | DDOT
  | COMMA
  | AND

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState54
  | MenhirState48
  | MenhirState43
  | MenhirState35
  | MenhirState33
  | MenhirState31
  | MenhirState29
  | MenhirState27
  | MenhirState24
  | MenhirState19
  | MenhirState17
  | MenhirState14
  | MenhirState12
  | MenhirState9
  | MenhirState5
  | MenhirState3
  | MenhirState0

  

  open HopixAST


let _eRR =
  Error

let rec _menhir_goto_separated_nonempty_list_AND_separated_pair_located_identifier__DEQUAL_located_expression___ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((HopixAST.identifier Position.located *
   HopixAST.expression Position.located)
  list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let ((((_menhir_stack, _menhir_s, x000, _startpos_x000_, _endpos_x000_), _startpos__menhir_p00_, _endpos__menhir_p00_), _, x01, _startpos_x01_, _endpos_x01_), _startpos__menhir_p0_) = _menhir_stack in
        let _menhir_p0 = () in
        let _menhir_p00 = () in
        let _startpos = _startpos_x000_ in
        let _v : ((HopixAST.identifier Position.located *
   HopixAST.expression Position.located)
  list) = let x =
          let _startpos__menhir_p0_ = _startpos__menhir_p00_ in
          let _endpos = _startpos__menhir_p0_ in
          let _endpos__menhir_p0_ = _endpos__menhir_p00_ in
          let x0 = x01 in
          let _menhir_p0 = _menhir_p00 in
          let x00 = x000 in
          let y =
            let _startpos = _endpos__menhir_p0_ in
            let x = x0 in
            (
  Position.with_poss _startpos _endpos x
)
          in
          let x =
            let _endpos = _startpos__menhir_p0_ in
            let x0 = x00 in
            let x =
              let x = x0 in
              (
  Id x
)
            in
            (
  Position.with_poss _startpos _endpos x
)
          in
              ( (x, y) )
        in
            ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_AND_separated_pair_located_identifier__DEQUAL_located_expression___ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let xs = _v in
        let ((((_menhir_stack, _menhir_s, x000, _startpos_x000_, _endpos_x000_), _startpos__menhir_p00_, _endpos__menhir_p00_), _, x01, _startpos_x01_, _endpos_x01_), _startpos__menhir_p0_) = _menhir_stack in
        let _menhir_p0 = () in
        let _menhir_p00 = () in
        let _startpos = _startpos_x000_ in
        let _v : ((HopixAST.identifier Position.located *
   HopixAST.expression Position.located)
  list) = let x =
          let _startpos__menhir_p0_ = _startpos__menhir_p00_ in
          let _endpos = _startpos__menhir_p0_ in
          let _endpos__menhir_p0_ = _endpos__menhir_p00_ in
          let x0 = x01 in
          let _menhir_p0 = _menhir_p00 in
          let x00 = x000 in
          let y =
            let _startpos = _endpos__menhir_p0_ in
            let x = x0 in
            (
  Position.with_poss _startpos _endpos x
)
          in
          let x =
            let _endpos = _startpos__menhir_p0_ in
            let x0 = x00 in
            let x =
              let x = x0 in
              (
  Id x
)
            in
            (
  Position.with_poss _startpos _endpos x
)
          in
              ( (x, y) )
        in
            ( x :: xs ) in
        _menhir_goto_separated_nonempty_list_AND_separated_pair_located_identifier__DEQUAL_located_expression___ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x = _v in
        let _v : ((HopixAST.identifier Position.located *
   HopixAST.expression Position.located)
  list) =     ( x ) in
        _menhir_goto_loption_separated_nonempty_list_AND_separated_pair_located_identifier__DEQUAL_located_expression____ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_run12 : _menhir_env -> 'ttv_tail * _menhir_state * (HopixAST.expression) * Lexing.position * Lexing.position -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _v _endpos ->
    let _menhir_stack = (_menhir_stack, _v, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _menhir_env._menhir_startp
    | MASTER_TKN _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState12 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState12

and _menhir_run14 : _menhir_env -> 'ttv_tail * _menhir_state * (HopixAST.expression) * Lexing.position * Lexing.position -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _v _endpos ->
    let _menhir_stack = (_menhir_stack, _v, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_startp
    | MASTER_TKN _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run17 : _menhir_env -> 'ttv_tail * _menhir_state * (HopixAST.expression) * Lexing.position * Lexing.position -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _v _endpos ->
    let _menhir_stack = (_menhir_stack, _v, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _menhir_env._menhir_startp
    | MASTER_TKN _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState17 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState17

and _menhir_run19 : _menhir_env -> 'ttv_tail * _menhir_state * (HopixAST.expression) * Lexing.position * Lexing.position -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _v _endpos ->
    let _menhir_stack = (_menhir_stack, _v, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _menhir_env._menhir_startp
    | MASTER_TKN _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (HopixAST.expression) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    match _menhir_s with
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | PLUS _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos = _menhir_env._menhir_endp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _endpos__3_ = _endpos in
            let ((_menhir_stack, _menhir_s, _startpos__1_), _, e, _startpos_e_, _endpos_e_) = _menhir_stack in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : (HopixAST.expression) = (
  e
) in
            _menhir_goto_very_simple_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | SLASH _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | STAR _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SLASH _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | AND | DOT | MINUS _ | PLUS _ | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, x0, _startpos_x0_, _endpos_x0_), _menhir_p0, _endpos__menhir_p0_), _, x1, _startpos_x1_, _endpos_x1_) = _menhir_stack in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : (HopixAST.expression) = let rhs =
              let _startpos = _endpos__menhir_p0_ in
              let x = x1 in
              (
  Position.with_poss _startpos _endpos x
)
            in
            let _startpos_rhs_ = _endpos__menhir_p0_ in
            let b =
              let _endpos = _startpos_rhs_ in
              let _startpos = _endpos_x0_ in
              let x =
                        ( "`*"  )
              in
              (
  Position.with_poss _startpos _endpos x
)
            in
            let _endpos_b_ = _startpos_rhs_ in
            let _startpos_b_ = _endpos_x0_ in
            let lhs =
              let _endpos = _startpos_b_ in
              let x = x0 in
              (
  Position.with_poss _startpos _endpos x
)
            in
            let _startpos_lhs_ = _startpos in
            (
  let op = Position.(map (fun x -> Variable (map (fun _ -> Id x) b))) b in
  let app1 = Position.with_poss _startpos_lhs_ _endpos_b_ (Apply (op, lhs)) in
  Apply (app1, rhs)
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND | DOT | MINUS _ | PLUS _ | RPAREN | STAR _ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, x0, _startpos_x0_, _endpos_x0_), _menhir_p0, _endpos__menhir_p0_), _, x1, _startpos_x1_, _endpos_x1_) = _menhir_stack in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : (HopixAST.expression) = let rhs =
              let _startpos = _endpos__menhir_p0_ in
              let x = x1 in
              (
  Position.with_poss _startpos _endpos x
)
            in
            let _startpos_rhs_ = _endpos__menhir_p0_ in
            let b =
              let _endpos = _startpos_rhs_ in
              let _startpos = _endpos_x0_ in
              let x =
                        ( "`/"  )
              in
              (
  Position.with_poss _startpos _endpos x
)
            in
            let _endpos_b_ = _startpos_rhs_ in
            let _startpos_b_ = _endpos_x0_ in
            let lhs =
              let _endpos = _startpos_b_ in
              let x = x0 in
              (
  Position.with_poss _startpos _endpos x
)
            in
            let _startpos_lhs_ = _startpos in
            (
  let op = Position.(map (fun x -> Variable (map (fun _ -> Id x) b))) b in
  let app1 = Position.with_poss _startpos_lhs_ _endpos_b_ (Apply (op, lhs)) in
  Apply (app1, rhs)
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | MINUS _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | SLASH _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | STAR _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | AND | DOT | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, x0, _startpos_x0_, _endpos_x0_), _menhir_p0, _endpos__menhir_p0_), _, x1, _startpos_x1_, _endpos_x1_) = _menhir_stack in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : (HopixAST.expression) = let rhs =
              let _startpos = _endpos__menhir_p0_ in
              let x = x1 in
              (
  Position.with_poss _startpos _endpos x
)
            in
            let _startpos_rhs_ = _endpos__menhir_p0_ in
            let b =
              let _endpos = _startpos_rhs_ in
              let _startpos = _endpos_x0_ in
              let x =
                       ( "`+"  )
              in
              (
  Position.with_poss _startpos _endpos x
)
            in
            let _endpos_b_ = _startpos_rhs_ in
            let _startpos_b_ = _endpos_x0_ in
            let lhs =
              let _endpos = _startpos_b_ in
              let x = x0 in
              (
  Position.with_poss _startpos _endpos x
)
            in
            let _startpos_lhs_ = _startpos in
            (
  let op = Position.(map (fun x -> Variable (map (fun _ -> Id x) b))) b in
  let app1 = Position.with_poss _startpos_lhs_ _endpos_b_ (Apply (op, lhs)) in
  Apply (app1, rhs)
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SLASH _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | STAR _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | AND | DOT | MINUS _ | PLUS _ | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, x0, _startpos_x0_, _endpos_x0_), _menhir_p0, _endpos__menhir_p0_), _, x1, _startpos_x1_, _endpos_x1_) = _menhir_stack in
            let _startpos = _startpos_x0_ in
            let _endpos = _endpos_x1_ in
            let _v : (HopixAST.expression) = let rhs =
              let _startpos = _endpos__menhir_p0_ in
              let x = x1 in
              (
  Position.with_poss _startpos _endpos x
)
            in
            let _startpos_rhs_ = _endpos__menhir_p0_ in
            let b =
              let _endpos = _startpos_rhs_ in
              let _startpos = _endpos_x0_ in
              let x =
                        ( "`-"  )
              in
              (
  Position.with_poss _startpos _endpos x
)
            in
            let _endpos_b_ = _startpos_rhs_ in
            let _startpos_b_ = _endpos_x0_ in
            let lhs =
              let _endpos = _startpos_b_ in
              let x = x0 in
              (
  Position.with_poss _startpos _endpos x
)
            in
            let _startpos_lhs_ = _startpos in
            (
  let op = Position.(map (fun x -> Variable (map (fun _ -> Id x) b))) b in
  let app1 = Position.with_poss _startpos_lhs_ _endpos_b_ (Apply (op, lhs)) in
  Apply (app1, rhs)
) in
            _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos__menhir_p2_ = _startpos in
            let ((((_menhir_stack, _menhir_s, _startpos__menhir_p0_, _endpos__menhir_p0_), x00, _startpos_x00_, _endpos_x00_), _startpos__menhir_p1_, _endpos__menhir_p1_), _, x0, _startpos_x0_, _endpos_x0_) = _menhir_stack in
            let _menhir_p2 = () in
            let _menhir_p1 = () in
            let _menhir_p0 = () in
            let _startpos = _startpos__menhir_p0_ in
            let _v : (HopixAST.definition) = let e =
              let _startpos = _endpos__menhir_p1_ in
              let _endpos = _startpos__menhir_p2_ in
              let x = x0 in
              (
  Position.with_poss _startpos _endpos x
)
            in
            let x =
              let _endpos = _startpos__menhir_p1_ in
              let _startpos = _endpos__menhir_p0_ in
              let x0 = x00 in
              let x =
                let x = x0 in
                (
  Id x
)
              in
              (
  Position.with_poss _startpos _endpos x
)
            in
            (
  DefineValue (x, e)
) in
            _menhir_goto_vdefinition _menhir_env _menhir_stack _menhir_s _v _startpos
        | MINUS _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | PLUS _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | SLASH _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | STAR _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _ = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos__menhir_p2_ = _startpos in
            let ((((_menhir_stack, _menhir_s, _startpos__menhir_p0_, _endpos__menhir_p0_), x00, _startpos_x00_, _endpos_x00_), _startpos__menhir_p1_, _endpos__menhir_p1_), _, x0, _startpos_x0_, _endpos_x0_) = _menhir_stack in
            let _menhir_p2 = () in
            let _menhir_p1 = () in
            let _menhir_p0 = () in
            let _startpos = _startpos__menhir_p0_ in
            let _v : (HopixAST.definition) = let e =
              let _startpos = _endpos__menhir_p1_ in
              let _endpos = _startpos__menhir_p2_ in
              let x = x0 in
              (
  Position.with_poss _startpos _endpos x
)
            in
            let x =
              let _endpos = _startpos__menhir_p1_ in
              let _startpos = _endpos__menhir_p0_ in
              let x0 = x00 in
              let x =
                let x = x0 in
                (
  Id x
)
              in
              (
  Position.with_poss _startpos _endpos x
)
            in
            (
  DefineValue (x, e)
) in
            _menhir_goto_vdefinition _menhir_env _menhir_stack _menhir_s _v _startpos
        | MINUS _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | PLUS _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | SLASH _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | STAR _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | MASTER_TKN _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31)
        | MINUS _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | PLUS _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | SLASH _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | STAR _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, x000, _startpos_x000_, _endpos_x000_), _startpos__menhir_p00_, _endpos__menhir_p00_), _, x01, _startpos_x01_, _endpos_x01_) = _menhir_stack in
            let _menhir_p00 = () in
            let _startpos = _startpos_x000_ in
            let _endpos = _endpos_x01_ in
            let _v : ((HopixAST.identifier Position.located *
   HopixAST.expression Position.located)
  list) = let x =
              let _startpos__menhir_p0_ = _startpos__menhir_p00_ in
              let _endpos__menhir_p0_ = _endpos__menhir_p00_ in
              let x0 = x01 in
              let _menhir_p0 = _menhir_p00 in
              let x00 = x000 in
              let y =
                let _startpos = _endpos__menhir_p0_ in
                let x = x0 in
                (
  Position.with_poss _startpos _endpos x
)
              in
              let x =
                let _endpos = _startpos__menhir_p0_ in
                let x0 = x00 in
                let x =
                  let x = x0 in
                  (
  Id x
)
                in
                (
  Position.with_poss _startpos _endpos x
)
              in
                  ( (x, y) )
            in
                ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_AND_separated_pair_located_identifier__DEQUAL_located_expression___ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | AND ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _menhir_stack = (_menhir_stack, _startpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | MASTER_TKN _v ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35)
        | MINUS _v ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | PLUS _v ->
            _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | SLASH _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | STAR _v ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) _v _menhir_env._menhir_endp
        | DOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s, x000, _startpos_x000_, _endpos_x000_), _startpos__menhir_p00_, _endpos__menhir_p00_), _, x01, _startpos_x01_, _endpos_x01_) = _menhir_stack in
            let _menhir_p00 = () in
            let _startpos = _startpos_x000_ in
            let _endpos = _endpos_x01_ in
            let _v : ((HopixAST.identifier Position.located *
   HopixAST.expression Position.located)
  list) = let x =
              let _startpos__menhir_p0_ = _startpos__menhir_p00_ in
              let _endpos__menhir_p0_ = _endpos__menhir_p00_ in
              let x0 = x01 in
              let _menhir_p0 = _menhir_p00 in
              let x00 = x000 in
              let y =
                let _startpos = _endpos__menhir_p0_ in
                let x = x0 in
                (
  Position.with_poss _startpos _endpos x
)
              in
              let x =
                let _endpos = _startpos__menhir_p0_ in
                let x0 = x00 in
                let x =
                  let x = x0 in
                  (
  Id x
)
                in
                (
  Position.with_poss _startpos _endpos x
)
              in
                  ( (x, y) )
            in
                ( [ x ] ) in
            _menhir_goto_separated_nonempty_list_AND_separated_pair_located_identifier__DEQUAL_located_expression___ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_simple_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (HopixAST.expression) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _menhir_env._menhir_startp
    | MASTER_TKN _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | AND | DOT | MINUS _ | PLUS _ | RPAREN | SLASH _ | STAR _ ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, s, _startpos_s_, _endpos_s_) = _menhir_stack in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : (HopixAST.expression) = (
      s
) in
        _menhir_goto_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_very_simple_expression : _menhir_env -> 'ttv_tail -> _menhir_state -> (HopixAST.expression) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    match _menhir_s with
    | MenhirState33 | MenhirState29 | MenhirState24 | MenhirState3 | MenhirState19 | MenhirState17 | MenhirState14 | MenhirState12 | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let e = _v in
        let _startpos_e_ = _startpos in
        let _endpos_e_ = _endpos in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : (HopixAST.expression) = (
  e
) in
        _menhir_goto_simple_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x1 = _v in
        let _startpos_x1_ = _startpos in
        let _endpos_x1_ = _endpos in
        let (_menhir_stack, _menhir_s, x0, _startpos_x0_, _endpos_x0_) = _menhir_stack in
        let _startpos = _startpos_x0_ in
        let _endpos = _endpos_x1_ in
        let _v : (HopixAST.expression) = let b =
          let _startpos = _endpos_x0_ in
          let x = x1 in
          (
  Position.with_poss _startpos _endpos x
)
        in
        let _startpos_b_ = _endpos_x0_ in
        let a =
          let _endpos = _startpos_b_ in
          let x = x0 in
          (
  Position.with_poss _startpos _endpos x
)
        in
        (
  Apply (a, b)
) in
        _menhir_goto_simple_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos
    | _ ->
        _menhir_fail ()

and _menhir_goto_vdefinition : _menhir_env -> 'ttv_tail -> _menhir_state -> (HopixAST.definition) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let vd = _v in
    let _startpos_vd_ = _startpos in
    let _startpos = _startpos_vd_ in
    let _v : (HopixAST.definition) = (
  vd
) in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXTERN ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | REC ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp
    | VAL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | EOF ->
        _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_goto_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> (HopixAST.ty) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _endpos ->
    match _menhir_s with
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x0 = _v in
        let _endpos_x0_ = _endpos in
        let (((_menhir_stack, _menhir_s, _startpos__menhir_p0_, _endpos__menhir_p0_), x00, _startpos_x00_, _endpos_x00_), _startpos__menhir_p1_, _endpos__menhir_p1_) = _menhir_stack in
        let _menhir_p1 = () in
        let _menhir_p0 = () in
        let _startpos = _startpos__menhir_p0_ in
        let _endpos = _endpos_x0_ in
        let _v : (HopixAST.definition) = let y =
          let _startpos = _endpos__menhir_p1_ in
          let x = x0 in
          (
  Position.with_poss _startpos _endpos x
)
        in
        let x =
          let _endpos = _startpos__menhir_p1_ in
          let _startpos = _endpos__menhir_p0_ in
          let x0 = x00 in
          let x =
            let x = x0 in
            (
  Id x
)
          in
          (
  Position.with_poss _startpos _endpos x
)
        in
        (
  DeclareExtern(x,y)
) in
        _menhir_goto_vdefinition _menhir_env _menhir_stack _menhir_s _v _startpos
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let x0 = _v in
        let _endpos_x0_ = _endpos in
        let (((_menhir_stack, _menhir_s, _startpos__menhir_p0_, _endpos__menhir_p0_), x00, _startpos_x00_, _endpos_x00_), _startpos__menhir_p1_, _endpos__menhir_p1_) = _menhir_stack in
        let _menhir_p1 = () in
        let _menhir_p0 = () in
        let _startpos = _startpos__menhir_p0_ in
        let _endpos = _endpos_x0_ in
        let _v : (HopixAST.definition) = let y =
          let _startpos = _endpos__menhir_p1_ in
          let x = x0 in
          (
  Position.with_poss _startpos _endpos x
)
        in
        let x =
          let _endpos = _startpos__menhir_p1_ in
          let _startpos = _endpos__menhir_p0_ in
          let x0 = x00 in
          let x =
            let x = x0 in
            (
  Id x
)
          in
          (
  Position.with_poss _startpos _endpos x
)
        in
        (
  DeclareExtern(x,y)
) in
        _menhir_goto_vdefinition _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_located_definition__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (HopixAST.t) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, ds, _startpos_ds_) = _menhir_stack in
            let _v : (HopixAST.t) = (
  ds
) in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _1 = _v in
            Obj.magic _1
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, x0, _startpos_x0_), _, xs, _startpos_xs_) = _menhir_stack in
        let _startpos = _startpos_x0_ in
        let _v : (HopixAST.t) = let x =
          let _endpos = _startpos_xs_ in
          let x = x0 in
          (
  Position.with_poss _startpos _endpos x
)
        in
            ( x :: xs ) in
        _menhir_goto_list_located_definition__ _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        _menhir_fail ()

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x00 = _v in
    let _startpos_x00_ = _startpos in
    let _endpos_x00_ = _endpos in
    let _startpos = _startpos_x00_ in
    let _endpos = _endpos_x00_ in
    let _v : (HopixAST.expression) = let x =
      let x0 = x00 in
      let x =
        let x = x0 in
        (
  Id x
)
      in
      (
  Position.with_poss _startpos _endpos x
)
    in
    (
  Variable x
) in
    _menhir_goto_very_simple_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _menhir_env._menhir_startp
    | MASTER_TKN _v ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (int) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x00 = _v in
    let _startpos_x00_ = _startpos in
    let _endpos_x00_ = _endpos in
    let _startpos = _startpos_x00_ in
    let _endpos = _endpos_x00_ in
    let _v : (HopixAST.expression) = let l =
      let x0 = x00 in
      let x =
        let x = x0 in
        (
  LInt x
)
      in
      (
  Position.with_poss _startpos _endpos x
)
    in
    (
  Literal l
) in
    _menhir_goto_very_simple_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let x00 = _v in
    let _startpos_x00_ = _startpos in
    let _endpos_x00_ = _endpos in
    let _startpos = _startpos_x00_ in
    let _endpos = _endpos_x00_ in
    let _v : (HopixAST.expression) = let x =
      let x0 = x00 in
      let x =
        let x = x0 in
        (
  Id x
)
      in
      (
  Position.with_poss _startpos _endpos x
)
    in
    (
  Variable x
) in
    _menhir_goto_very_simple_expression _menhir_env _menhir_stack _menhir_s _v _startpos _endpos

and _menhir_goto_loption_separated_nonempty_list_AND_separated_pair_located_identifier__DEQUAL_located_expression____ : _menhir_env -> 'ttv_tail -> _menhir_state -> ((HopixAST.identifier Position.located *
   HopixAST.expression Position.located)
  list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _ = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos__3_ = _startpos in
        let ((_menhir_stack, _menhir_s, _startpos__1_), _, xs0) = _menhir_stack in
        let _startpos = _startpos__1_ in
        let _v : (HopixAST.definition) = let x =
          let xs = xs0 in
              ( xs )
        in
        (
  DefineRecValue(x)
) in
        _menhir_goto_vdefinition _menhir_env _menhir_stack _menhir_s _v _startpos
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DEQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_startp
        | MASTER_TKN _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | DEQUAL ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | ID _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | INT _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | LPAREN ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_startp
        | MASTER_TKN _v ->
            _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run44 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let str0 = _v in
    let _endpos_str0_ = _endpos in
    let _endpos = _endpos_str0_ in
    let _v : (HopixAST.ty) = let vs =
      let str = str0 in
      (
  TId str
)
    in
    (
  print_string("TyVar parsed\n");
  TyVar(vs)
) in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _endpos

and _menhir_run45 : _menhir_env -> 'ttv_tail -> _menhir_state -> (string) -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v _startpos _endpos ->
    let _ = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let str0 = _v in
    let _startpos_str0_ = _startpos in
    let _endpos_str0_ = _endpos in
    let _endpos = _endpos_str0_ in
    let _v : (HopixAST.ty) = let vs =
      let str = str0 in
      (
  TCon str
)
    in
    (
  print_string("TyCon parsed\n");
  TyCon (vs,[])
) in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v _endpos

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState54 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState48 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _, _), _, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState43 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _, _), _, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState35 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState31 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState29 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState27 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState24 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _, _), _, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState19 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState17 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState14 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState12 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, _, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (((_menhir_stack, _menhir_s, _, _), _, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _startpos = _menhir_env._menhir_startp in
    let _v : (HopixAST.t) =     ( [] ) in
    _menhir_goto_list_located_definition__ _menhir_env _menhir_stack _menhir_s _v _startpos

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | DEQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _startpos, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_startp
            | MASTER_TKN _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MASTER_TKN _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | DEQUAL ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _startpos, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | ID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | INT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _menhir_env._menhir_startp
            | MASTER_TKN _v ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_run27 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | MASTER_TKN _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
    | DOT ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState27 in
        let _v : ((HopixAST.identifier Position.located *
   HopixAST.expression Position.located)
  list) =     ( [] ) in
        _menhir_goto_loption_separated_nonempty_list_AND_separated_pair_located_identifier__DEQUAL_located_expression____ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27

and _menhir_run41 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos _endpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos, _endpos) in
    let _tok = _menhir_discard _menhir_env in
    match _tok with
    | ID _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | DDOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _startpos, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | MASTER_TKN _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TYPE_VAR _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MASTER_TKN _v ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _startpos = _menhir_env._menhir_startp in
        let _endpos = _menhir_env._menhir_endp in
        let _menhir_stack = (_menhir_stack, _v, _startpos, _endpos) in
        let _tok = _menhir_discard _menhir_env in
        (match _tok with
        | DDOT ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _startpos = _menhir_env._menhir_startp in
            let _endpos = _menhir_env._menhir_endp in
            let _menhir_stack = (_menhir_stack, _startpos, _endpos) in
            let _tok = _menhir_discard _menhir_env in
            (match _tok with
            | MASTER_TKN _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_startp _menhir_env._menhir_endp
            | TYPE_VAR _v ->
                _menhir_run44 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v _menhir_env._menhir_endp
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s, _, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (HopixAST.t) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = max_int;
      } in
    Obj.magic (let _menhir_stack = () in
    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EXTERN ->
        _menhir_run41 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | REC ->
        _menhir_run27 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp
    | VAL ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _menhir_env._menhir_startp _menhir_env._menhir_endp
    | EOF ->
        _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)



