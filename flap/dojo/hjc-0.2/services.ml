open Config
open Utils
open Unix
open Printf

(* FIXME: A lot of the following code should be generated from the
   FIXME: API description. *)

let general_options = [
  "-v",
  Arg.Set verbose_mode,
  " Set verbose mode."
]

let print_raw_json doc =
  Yojson.Safe.pretty_to_channel Pervasives.stdout doc;
  print_newline ();
  flush Pervasives.stdout

let print_json = function
  | (`Assoc l) as doc when List.mem_assoc "status" l ->
    begin try match List.assoc "status" l with
      | `String s -> printf "%s\n%!" s
      | _ -> raise Not_found
      with _ -> print_raw_json doc
    end
  | doc -> print_raw_json doc

let call_api service ?postprocess ?forms ?posts gets =
  print_json (Utils.call_api service ?postprocess ?forms ?posts gets)

let options o = o @ general_options

let login ?(register=false) ?username ?password () =
  let if_none x f = match x with None -> f () | Some x -> x in
  let username = if_none username (fun () ->
    Printf.printf "login: %!";
    input_line Pervasives.stdin
  )
  in
  let password = if_none password (fun () ->
    Printf.printf "password: %!";
    let s = noecho_input_line () in
    Printf.printf "\n%!";
    s
  )
  in
  call_api (if register then "register" else "login") ~posts:[
    ("login",    username);
    ("password", password)
  ] []

let sign_command register =
  let username = ref None in
  let password = ref None in
  let command_name = if register then "register" else "login" in
  process command_name
    (options [
      "--dojo", Arg.String (Config.set_url), " Specify a Dojo URL.";
      "--username", Arg.String (set_opt username), " Specify a username.";
      "--password", Arg.String (set_opt password), " Specify a password.";
    ])
    ("hjc " ^ command_name)
    (no_extra_arguments (fun () ->
      login ~register ?username:!username ?password:!password ())
    )

let login_command = sign_command false

let register_command = sign_command true

let logout () =
  call_api "logout" ~posts:[] []

let logout_command =
  process "logout"
    (options [])
    "hjc logout"
    (no_extra_arguments logout)

let whoami () =
  call_api "whoami" ~posts:[] []

let whoami_command =
  process "whoami"
    (options [])
    "hjc whoami"
    (no_extra_arguments whoami)

let chroot = function
  | [ path ] ->
    call_api "chroot" ~posts:["path", path] []
  | _ ->
    Printf.eprintf "Invalid usage of chroot command.\n";
    exit 1

let chroot_command =
  process "chroot"
    (options [])
    ("hjc chroot [path]")
    chroot

let shutdown = function
  | [] ->
    call_api "shutdown" ~posts:[] []
  | _ ->
    Printf.eprintf "Invalid usage of shutdown command.\n";
    exit 1

let shutdown_command =
  process "shutdown"
    (options [])
    ("hjc shutdown")
    shutdown

let get_user_info = function
  | [ cmd ] ->
    call_api "get_user_info" ~posts:["cmd", cmd] []
  | _ ->
    Printf.eprintf "Invalid usage of get_user_info command.\n";
    exit 1

let get_user_info_command =
  process "get_user_info"
    (options [])
    ("hjc get_user_info [cmd]")
    get_user_info

let set_admin_email = function
  | [ cmd ] ->
    call_api "set_admin_email" ~posts:["email", cmd] []
  | _ ->
    Printf.eprintf "Invalid usage of set_admin_email command.\n";
    exit 1

let set_admin_email_command =
  process "set_admin_email"
    (options [])
    ("hjc set_admin_email [email]")
    set_admin_email

let set_mailer = function
  | [ cmd ] ->
    call_api "set_mailer" ~posts:["cmd", cmd] []
  | _ ->
    Printf.eprintf "Invalid usage of set_mailer command.\n";
    exit 1

let set_mailer_command =
  process "set_mailer"
    (options [])
    ("hjc set_mailer [cmd]")
    set_mailer

let update_password = function
  | [ login ] ->
    call_api "update_password" ~posts:["login", login] []
  | _ ->
    Printf.eprintf "Invalid usage of update_password command.\n";
    exit 1

let update_password_command =
  process "update_password"
    (options [])
    ("hjc update_password [login]")
    update_password

let digest password =
  Digest.(to_hex (string ("hj:" ^ password ^ ":")))

let set_password = function
  | [ login; password ] ->
    call_api "set_password" ~posts:["login", login; "password_digest", digest password] []
  | _ ->
    Printf.eprintf "Invalid usage of set_password command.\n";
    exit 1

let set_password_command =
  process "set_password"
    (options [])
    ("hjc set_password [login] [password]")
    set_password

let exercise_create = function
  | [ name ] ->
    call_api "exercise_create" ~posts:["name", name] []
  | _ ->
    Printf.eprintf "Invalid usage of exercise_create command.\n";
    exit 1

let exercise_create_command =
  process "exercise_create"
    (options [])
    ("hjc exercise_create [name]")
    exercise_create

let exercise_url exercise =
  "/aka/" ^ exercise

let exercise_focus = function
  | [ exercise ] ->
    Config.set_focus (exercise_url exercise);
    Printf.printf "Focus on `%s'.\n" exercise
  | _ ->
    Printf.eprintf "Invalid usage of focus command.\n";
    exit 1

let exercise_focus_command =
  process "exercise_focus"
    (options [])
    "hjc exercise_focus [identifier]"
    exercise_focus

let submit = function
  | [ question; file ] ->
    failwith "To be implemented"
  | _ ->
    Printf.eprintf "Invalid usage of submit command.\n";
    exit 1

let submit_command =
  process "submit"
    (options [])
    "hjc submit [question] [file]"
    submit

let read file =
  try
    let cin = open_in file in
    let b = Buffer.create 13 in
    let rec read () =
      try
        Buffer.add_string b (input_line cin ^ "\n");
        read ()
      with _ -> ()
    in
    read ();
    close_in cin;
    Buffer.contents b
  with _ ->
    Printf.eprintf "Error while reading input file.\n";
    exit 1

let on_exercise f =
  match Config.get_focus () with
    | None ->
      Printf.eprintf "First, focus on some exercise using `hjc focus'.\n";
      exit 1
    | Some exo ->
      f exo

let update = function
  | [ file ] ->
    on_exercise (fun exo ->
      let postprocess = Printf.sprintf "sed s,this,'%s',g" file in
      call_api ~postprocess "update" ~forms:[
        "id", exo;
        "content", "@" ^ file
      ] []
    )
  | _ ->
    Printf.eprintf "Invalid usage of update command.\n";
    exit 1

let update_command =
  process "update"
    (options [])
    "hjc update [description_file]"
    update

let exercise_upload = function
  | [ resource_name; file ] -> begin
    on_exercise (fun exo ->
      call_api "exercise_upload" ~forms:[
        "identifier", exo;
        "resource_name", resource_name;
        "file", "@" ^ file
      ] [])
  end
  | _ ->
    Printf.eprintf "Invalid usage of exercise_upload command.\n";
    exit 1

let exercise_upload_command =
  process "exercise_upload"
    (options [])
    "hjc exercise_upload [ressource_name] [file]"
    exercise_upload

let exercise_upload_tar = function
  | [ file ] -> begin
    on_exercise (fun exo ->
      call_api "exercise_upload_tar" ~forms:[
        "identifier", exo;
        "file", "@" ^ file
      ] [])
  end
  | _ ->
    Printf.eprintf "Invalid usage of exercise_upload_tar command.\n";
    exit 1

let exercise_upload_tar_command =
  process "exercise_upload_tar"
    (options [])
    "hjc exercise_upload_tar [tarball]"
    exercise_upload_tar

let exercise_download version = function
  | [ name ] ->
    on_exercise (fun exo ->
      call_api "exercise_download" ~posts:[
        "identifier", exo;
        "resource_name", name;
        "version", (version ())
      ] [])
  | _ ->
    Printf.eprintf "Invalid usage of exercise_download command.\n";
    exit 1

let exercise_download_command =
  let version = ref "" in
  process "exercise_download"
    (options [
      "--version", Arg.Set_string version, " Download a specific version."
    ])
    ("hjc exercise_download [resource_name]")
    (exercise_download (fun () -> !version))

let exercise_publish status = function
  | [ name ] ->
    on_exercise (fun exo ->
      call_api "exercise_publish" ~posts:[
        "identifier", exo;
        "resource_name", name;
        "status", status
      ] [])
  | _ ->
    Printf.eprintf "Invalid usage of exercise_publish command.\n";
    exit 1

let exercise_publish_command =
  process "exercise_publish"
    (options [])
    ("hjc exercise_publish [resource_name]")
    (exercise_publish "1")

let exercise_unpublish_command =
  process "exercise_unpublish"
    (options [])
    ("hjc exercise_unpublish [resource_name]")
    (exercise_publish "0")

let exercise_ls show_all = function
  | [ filter ] ->
    let options = if show_all () then "--all" else "" in
    on_exercise (fun exo ->
      call_api "exercise_ls" ~posts:[
        "identifier", exo;
        "options", options;
        "filter", filter;
      ] [])
  | _ ->
    Printf.eprintf "Invalid usage of exercise_ls command.\n";
    exit 1

let exercise_ls_command =
  let show_all = ref false in
  process "exercise_ls"
    (options [
      "--all", Arg.Set show_all, " Show all versions.";
    ])
    ("hjc exercise_ls filter")
    (exercise_ls (fun () -> !show_all))

let exercise_update ?postprocess = function
  | [] ->
    on_exercise (fun exo ->
      call_api ?postprocess "exercise_update" ~posts:[
        "identifier", exo
      ] [])
  | _ ->
    Printf.eprintf "Invalid usage of exercise_update command.\n";
    exit 1

let exercise_update_command =
  process "exercise_update"
    (options [])
    ("hjc exercise_update")
    exercise_update

let optional_focus e f =
  match e with
  | None -> f ()
  | Some exercise ->
    let old_url = Config.get_focus () in
    Config.set_focus (exercise_url exercise);
    f ();
    match old_url with
      | None -> ()
      | Some exercise -> Config.set_focus exercise

let exercise_push exercise = function
  | [ file ] -> optional_focus exercise (fun () ->
    let postprocess = Printf.sprintf "sed s,this,'%s',g" file in
    exercise_upload [ "source.aka"; file ];
    exercise_update ~postprocess []
  )
  | _ ->
    Printf.eprintf "Invalid usage of exercise_push command.\n";
    exit 1

let exercise_push_command =
  let exercise = ref None in
  process "exercise_push"
    (options [
      "--on", Arg.String (set_opt exercise),
      " Specify an exercise to focus on.";
    ])
    "hjc exercise_push [file]"
    (fun o -> exercise_push !exercise o)

let exercise_refresh exercise = function
  | [] -> optional_focus exercise (fun () ->
    on_exercise (fun exo ->
      call_api "exercise_refresh_evaluations" ~posts:[
        "identifier", exo
      ] [])
  )
  | _ ->
    Printf.eprintf "Invalid usage of exercise_refresh command.\n";
    exit 1

let exercise_refresh_command =
  let exercise = ref None in
  process "exercise_refresh"
    (options [
      "--on", Arg.String (set_opt exercise),
      " Specify an exercise to focus on.";
    ])
    "hjc exercise_refresh"
    (fun o -> exercise_refresh !exercise o)

let exercise_subscribe exercise = function
  | [] ->
    optional_focus exercise (fun () ->
      on_exercise (fun exo ->
      call_api "exercise_subscribe" ~posts:[
        "identifier", exo;
      ] [])
    )
  | _ ->
    Printf.eprintf "Invalid usage of exercise_subscribe command.\n";
    exit 1

let exercise_subscribe_command =
  let exercise = ref None in
  process "exercise_subscribe"
    (options [
      "--on", Arg.String (set_opt exercise),
      " Specify an exercise to focus on.";
    ])
    ("hjc exercise_subscribe")
    (fun x -> exercise_subscribe !exercise x)

let exercise_questions exercise = function
  | [] ->
    optional_focus exercise (fun () ->
      on_exercise (fun exo ->
      call_api "exercise_questions" ~posts:[
        "identifier", exo
      ] [])
    )
  | _ ->
    Printf.eprintf "Invalid usage of exercise_questions command.\n";
    exit 1

let exercise_questions_command =
  let exercise = ref None in
  process "exercise_questions"
    (options [
      "--on", Arg.String (set_opt exercise),
      " Specify an exercise to focus on.";
    ])
    ("hjc exercise_questions")
    (fun x -> exercise_questions !exercise x)

let exercise_questions_latex exercise = function
  | [] ->
    optional_focus exercise (fun () ->
      on_exercise (fun exo ->
      call_api "exercise_questions_latex" ~posts:[
        "identifier", exo
      ] [])
    )
  | _ ->
    Printf.eprintf "Invalid usage of exercise_questions_latex command.\n";
    exit 1

let exercise_questions_latex_command =
  let exercise = ref None in
  process "exercise_questions_latex"
    (options [
      "--on", Arg.String (set_opt exercise),
      " Specify an exercise to focus on.";
    ])
    ("hjc exercise_questions_latex")
    (fun x -> exercise_questions_latex !exercise x)

let exercise_answer exercise = function
  | [ qid; answer ] ->
    optional_focus exercise (fun () ->
      on_exercise (fun exo ->
      call_api "exercise_push_new_answer" ~posts:[
        "identifier", exo;
        "question_identifier", qid;
        "answer", answer
      ] [])
    )
  | _ ->
    Printf.eprintf "Invalid usage of exercise_answer command.\n";
    exit 1

let exercise_answer_command =
  let exercise = ref None in
  process "exercise_answer"
    (options [
      "--on", Arg.String (set_opt exercise),
      " Specify an exercise to focus on.";
    ])
    ("hjc exercise_answer")
    (fun x -> exercise_answer !exercise x)

let exercise_evaluation_state exercise = function
  | [ qid ] ->
    optional_focus exercise (fun () ->
      on_exercise (fun exo ->
      call_api "exercise_evaluation_state" ~posts:[
        "identifier", exo;
        "question_identifier", qid;
      ] [])
    )
  | _ ->
    Printf.eprintf "Invalid usage of exercise_evaluation_state command.\n";
    exit 1

let exercise_evaluation_state_command =
  let exercise = ref None in
  process "exercise_evaluation_state"
    (options [
      "--on", Arg.String (set_opt exercise),
      " Specify an exercise to focus on.";
    ])
    ("hjc exercise_evaluation_state")
    (fun x -> exercise_evaluation_state !exercise x)

let exercise_user_answers exercise = function
  | [ uid ] ->
    optional_focus exercise (fun () ->
      on_exercise (fun exo ->
      call_api "exercise_answers_of_user" ~posts:[
        "identifier", exo;
        "uid", uid;
      ] [])
    )
  | _ ->
    Printf.eprintf "Invalid usage of exercise_user_answers command.\n";
    exit 1

let exercise_user_answers_command =
  let exercise = ref None in
  process "exercise_user_answers"
    (options [
      "--on", Arg.String (set_opt exercise),
      " Specify an exercise to focus on.";
    ])
    ("hjc exercise_user_answers")
    (fun x -> exercise_user_answers !exercise x)


let machinist_create = function
  | [ name ] ->
      call_api "machinist_create" ~posts:[
        "name", name;
      ] []
  | _ ->
    Printf.eprintf "Invalid usage of machinist_create command.\n";
    exit 1

let machinist_create_command =
  process "machinist_create"
    (options [])
    ("hjc machinist_create")
    machinist_create

let machinist_url m = "/machinists/" ^ m

let machinist_upload = function
  | [ machinist; resource_name; file ] -> begin
    call_api "machinist_upload" ~forms:[
      "identifier", (machinist_url machinist);
      "resource_name", resource_name;
      "file", "@" ^ file
    ] []
  end
  | _ ->
    Printf.eprintf "Invalid usage of machinist_upload command.\n";
    exit 1

let machinist_upload_command =
  process "machinist_upload"
    (options [])
    "hjc machinist_upload [ressource_name] [file]"
    machinist_upload

let machinist_download version = function
  | [ machinist; name ] ->
    call_api "machinist_download" ~posts:[
      "identifier", (machinist_url machinist);
      "resource_name", name;
      "version", (version ())
    ] []
  | _ ->
    Printf.eprintf "Invalid usage of machinist_download command.\n";
    exit 1

let machinist_download_command =
  let version = ref "" in
  process "machinist_download"
    (options [
      "--version", Arg.Set_string version, " Download a specific version."
    ])
    ("hjc machinist_download [resource_name]")
    (machinist_download (fun () -> !version))

let machinist_ls show_all = function
  | [ machinist; filter ] ->
    let options = if show_all () then "--all" else "" in
    call_api "machinist_ls" ~posts:[
      "identifier", (machinist_url machinist);
      "options", options;
      "filter", filter;
    ] []
  | _ ->
    Printf.eprintf "Invalid usage of machinist_ls command.\n";
    exit 1

let machinist_ls_command =
  let show_all = ref false in
  process "machinist_ls"
    (options [
      "--all", Arg.Set show_all, " Show all versions.";
    ])
    ("hjc machinist_ls filter")
    (machinist_ls (fun () -> !show_all))

let rec join2 = function
  | [] -> []
  | [x] -> []
  | x :: y :: xs -> (x, y) :: join2 xs

let machinist_set_logins = function
  | machinist :: logins ->
    let logins =
      String.concat "," (
        List.map (fun (login, key) -> login ^ ":" ^ key) (join2 logins)
      )
    in
    call_api "machinist_set_logins" ~posts:[
      "identifier", (machinist_url machinist);
      "logins", logins;
    ] []
  | _ ->
    Printf.eprintf "Invalid usage of machinist_set_logins command.\n";
    exit 1

let machinist_set_logins_command =
  process "machinist_set_logins"
    (options [])
    ("hjc machinist_set_logins")
    machinist_set_logins

let machinist_set_addresses = function
  | machinist :: addresses ->
    let addresses =
      String.concat "," (
        List.map (fun (ip, port) -> ip ^ ":" ^ port) (join2 addresses)
      )
    in
    call_api "machinist_set_addresses" ~posts:[
      "identifier", (machinist_url machinist);
      "addresses", addresses;
    ] []
  | _ ->
    Printf.eprintf "Invalid usage of machinist_set_addresses command.\n";
    exit 1

let machinist_set_addresses_command =
  process "machinist_set_addresses"
    (options [])
    ("hjc machinist_set_addresses")
    machinist_set_addresses

let machinist_exec = function
  | machinist :: command  ->
    let command = String.concat " " command in
    call_api "machinist_execute" ~posts:[
      "identifier", (machinist_url machinist);
      "command", command;
    ] []
  | _ ->
    Printf.eprintf "Invalid usage of machinist_execute command.\n";
    exit 1

let machinist_exec_command =
  process "machinist_execute"
    (options [])
    ("hjc machinist_execute")
    machinist_exec

let answers_upload exercise = function
  | [ resource_name; file ] -> begin
    on_exercise (fun exo ->
      call_api "answers_upload" ~forms:[
        "identifier", exo;
        "resource_name", resource_name;
        "file", "@" ^ file
      ] [])
  end
  | _ ->
    Printf.eprintf "Invalid usage of answers_upload command.\n";
    exit 1

let answers_upload_command =
  let exercise = ref None in
  process "answers_upload"
    (options [
      "--on", Arg.String (set_opt exercise),
      " Specify an exercise to focus on.";
    ])
    "hjc answers_upload [ressource_name] [file]"
    (fun x -> answers_upload !exercise x)

let answers_download version = function
  | [ name ] ->
    on_exercise (fun exo ->
      call_api "answers_download" ~posts:[
        "identifier", exo;
        "resource_name", name;
        "version", (version ())
      ] [])
  | _ ->
    Printf.eprintf "Invalid usage of answers_download command.\n";
    exit 1

let answers_download_command =
  let version = ref "" in
  process "answers_download"
    (options [
      "--version", Arg.Set_string version, " Download a specific version."
    ])
    ("hjc answers_download [resource_name]")
    (answers_download (fun () -> !version))

let answers_ls show_all = function
  | [ filter ] ->
    let options = if show_all () then "--all" else "" in
    on_exercise (fun exo ->
      call_api "answers_ls" ~posts:[
        "identifier", exo;
        "options", options;
        "filter", filter;
      ] [])
  | _ ->
    Printf.eprintf "Invalid usage of answers_ls command.\n";
    exit 1

let answers_ls_command =
  let show_all = ref false in
  process "answers_ls"
    (options [
      "--all", Arg.Set show_all, " Show all versions.";
    ])
    ("hjc answers_ls filter")
    (answers_ls (fun () -> !show_all))

let teamer_create = function
  | [ name ] ->
      call_api "teamer_create" ~posts:[
        "name", name;
      ] []
  | _ ->
    Printf.eprintf "Invalid usage of teamer_create command.\n";
    exit 1

let teamer_create_command =
  process "teamer_create"
    (options [])
    ("hjc teamer_create")
    teamer_create

let teamer_url m = "/teamer/" ^ m

let teamer_update ?postprocess = function
  | [teamer] ->
    call_api ?postprocess "teamer_update" ~posts:[
        "identifier", (teamer_url teamer)
      ] []
  | _ ->
    Printf.eprintf "Invalid usage of teamer_update command.\n";
    exit 1

let teamer_update_command =
  process "teamer_update"
    (options [])
    ("hjc teamer_update [teamer]")
    teamer_update

let teamer_versions ?postprocess = function
  | [teamer] ->
    call_api ?postprocess "teamer_versions" ~posts:[
        "identifier", (teamer_url teamer)
      ] []
  | _ ->
    Printf.eprintf "Invalid usage of teamer_versions command.\n";
    exit 1

let teamer_versions_command =
  process "teamer_versions"
    (options [])
    ("hjc teamer_versions [teamer]")
    teamer_update

let teamer_upload = function
  | [ teamer; resource_name; file ] -> begin
    call_api "teamer_upload" ~forms:[
      "identifier", (teamer_url teamer);
      "resource_name", resource_name;
      "file", "@" ^ file
    ] []
  end
  | _ ->
    Printf.eprintf "Invalid usage of teamer_upload command.\n";
    exit 1

let teamer_upload_command =
  process "teamer_upload"
    (options [])
    "hjc teamer_upload [teamer] [ressource_name] [file]"
    teamer_upload

let teamer_reserve_for_user = function
  | [ teamer; sid; uid; slot ] -> begin
    call_api "teamer_reserve_for_user" ~forms:[
      "identifier", (teamer_url teamer);
      "sid", sid;
      "uid", uid;
      "slot", slot
    ] []
  end
  | _ ->
    Printf.eprintf "Invalid usage of teamer_reserve_for_user command.\n";
    exit 1

let teamer_reserve_for_user_command =
  process "teamer_reserve_for_user"
    (options [])
    "hjc teamer_reserve_for_user [teamer_id] [sid] [uid] [slot]"
    teamer_reserve_for_user

let teamer_confirm_for_user = function
  | [ teamer; sid; uid; slot ] -> begin
    call_api "teamer_confirm_for_user" ~forms:[
      "identifier", (teamer_url teamer);
      "sid", sid;
      "uid", uid;
      "slot", slot
    ] []
  end
  | _ ->
    Printf.eprintf "Invalid usage of teamer_confirm_for_user command.\n";
    exit 1

let teamer_confirm_for_user_command =
  process "teamer_confirm_for_user"
    (options [])
    "hjc teamer_confirm_for_user [teamer_id] [sid] [uid] [slot]"
    teamer_confirm_for_user

let teamer_withdraw_for_user = function
  | [ teamer; sid; uid; slot ] -> begin
    call_api "teamer_withdraw_for_user" ~forms:[
      "identifier", (teamer_url teamer);
      "sid", sid;
      "uid", uid;
      "slot", slot
    ] []
  end
  | _ ->
    Printf.eprintf "Invalid usage of teamer_withdraw_for_user command.\n";
    exit 1

let teamer_withdraw_for_user_command =
  process "teamer_withdraw_for_user"
    (options [])
    "hjc teamer_withdraw_for_user [teamer_id] [sid] [uid] [slot]"
    teamer_withdraw_for_user
