%%% -*- mode:erlang -*-
{application, portius,
 [
  % A quick description of the application.
  {description, "Manage the erlware repos"},

  % The version of the applicaton
  {vsn, "0.3.0.2"},

  % All modules used by the application.
  {modules,
   [
	por_app,
	por_sup,
	por_file_tree,
	por_template,
	por_trans_server
   ]},

  % All of the registered names the application uses.
  {registered, []},

  % Applications that are to be started prior to this one.
  {applications, [eunit, kernel, stdlib, sasl, gas, fslib, epkg, ewrepo, ewlib, sgte]},

  % OTP application loader will load, but not start, included apps
  {included_applications, []},

  % configuration parameters
  {env, []},

  % The Module and Args to start this application.
  {mod, {por_app, []}}
 ]
}.
