%%% -*- mode:erlang -*-
{application, portius,
 [
  % A quick description of the application.
  {description, "Manage the erlware repos"},

  % The version of the applicaton
  {vsn, "0.6.0.1"},

  % All modules used by the application.
  {modules,
   [
	por_app,
	por_sup,
	por_file_tree,
	por_manage,
	por_auth,
	por_doc_builder,
	por_app_template,
	por_release_template,
	por_trans_server
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
  {mod, {por_app, []}}
 ]
}.
