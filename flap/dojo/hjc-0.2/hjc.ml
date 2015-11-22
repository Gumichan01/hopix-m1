open Unix
open Config
open Utils
open Services

(* Command line processing. *)

let _ = run_command [
  register_command;
  update_password_command;
  set_password_command;
  login_command;
  logout_command;
  whoami_command;
  chroot_command;
  shutdown_command;
  get_user_info_command;
  set_mailer_command;
  set_admin_email_command;
  exercise_create_command;
  exercise_focus_command;
  exercise_upload_command;
  exercise_upload_tar_command;
  exercise_download_command;
  exercise_publish_command;
  exercise_unpublish_command;
  exercise_ls_command;
  exercise_update_command;
  exercise_refresh_command;
  exercise_push_command;
  exercise_questions_command;
  exercise_questions_latex_command;
  exercise_subscribe_command;
  exercise_answer_command;
  exercise_user_answers_command;
  exercise_evaluation_state_command;
  answers_upload_command;
  answers_download_command;
  answers_ls_command;
  machinist_create_command;
  machinist_upload_command;
  machinist_download_command;
  machinist_ls_command;
  machinist_set_logins_command;
  machinist_set_addresses_command;
  machinist_exec_command;

  teamer_create_command;
  teamer_update_command;
  teamer_versions_command;
  teamer_upload_command;
  teamer_reserve_for_user_command;
  teamer_confirm_for_user_command;
  teamer_withdraw_for_user_command;

]
  "usage: hjc <command> [<args>]\n\
    \n\
    The available commands are:\n\
    \  register                  Register \n\
    \  update_password           Send password update link\n\
    \  set_password	         Set password\n\
    \  login                     Authenticate the user on the hacking dojo\n\
    \  logout                    Exit the hacking dojo\n\
    \  whoami                    Returns the authenticated user login\n\
    \  chroot                    Change the root of the system\n\
    \  shutdown                  Shut the system down\n\
    \  get_user_info             Set get_user_info command\n\
    \  exercise_create           Create a fresh exercise\n\
    \  exercise_focus            Focus on an exercise\n\
    \  exercise_upload           Upload a resource for an exercise\n\
    \  exercise_upload_tar       Upload exercise resources in a tarball\n\
    \  exercise_download         Download a resource of an exercise\n\
    \  exercise_ls               List the resources of an exercise\n\
    \  exercise_update           Trigger the update of an exercise code\n\
    \  exercise_push             Upload and update an exercise code\n\
    \  exercise_refresh          Refresh the evaluations of an exercise\n\
    \  exercise_questions        Retrieve the questions of an exercise\n\
    \  exercise_questions_latex  Retrieve the LaTeX of an exercise\n\
    \  exercise_subscribe        Subscribe to an exercise\n\
    \  exercise_answer           Submit an answer to a question\n\
    \  exercise_user_answers     Get the answers of a user\n\
    \  exercise_evaluation_state Get the state of an evaluation\n\
    \  answers_upload            Upload a resource for an exercise answers\n\
    \  answers_download          Download a resource of an exercise answers\n\
    \  answers_ls                List the resources of an exercise answers\n\
    \  machinist_create          Create a new machinist\n\
    \  machinist_upload          Upload a resource for a machinist\n\
    \  machinist_download        Download a resource of a machinist\n\
    \  machinist_ls              List the resources of a machinist\n\
    \  machinist_set_logins      Set the logins of a machinist\n\
    \  machinist_set_addresses   Set the addresses of a machinist\n\
    \  machinist_execute         Execute a command through a machinist\n\
    \  teamer_create             Create a new teamer\n\
    \  teamer_update             Update a teamer\n\
    \  teamer_versions           Get all the versions of a teamer\n\
    \  teamer_upload             Upload a teamer resource\n\
    \  teamer_reserve_for_user   Reserve a team slot for a user\n\
    \n\
    See 'hj <command> -help' for more information on a specific command.\n\
    "
