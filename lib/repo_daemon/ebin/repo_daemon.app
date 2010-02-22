%%% -*- mode:erlang -*-
{application, repo_daemon,
 [
  % A quick description of the application.
  {description, "Automate the movement of packages from writable to public repositories."},

  % The version of the applicaton
  {vsn, "0.2.0.0"},

  % All modules used by the application.
  {modules,
   [
    rd_app,
    rd_sup,
    rd_store,
    rd_file_tree,
    rd_trans_server
   ]},

  % All of the registered names the application uses.
  {registered, []},

  % Applications that are to be started prior to this one.
  {applications, [eunit, kernel, stdlib, sasl, gas, fslib, epkg, ewrepo, ewlib, sgte, edoc, cryptographic]},

  % OTP application loader will load, but not start, included apps
  {included_applications, []},

  % configuration parameters
  {env, []},

  % The Module and Args to start this application.
  {mod, {rd_app, []}}
 ]
}.
