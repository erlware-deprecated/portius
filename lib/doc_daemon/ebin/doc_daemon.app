%%% -*- mode:erlang -*-
{application, doc_daemon,
 [
  % A quick description of the application.
  {description, "Manage the creation of online docmentation for portius"},

  % The version of the applicaton
  {vsn, "0.2.0.4"},

  % All modules used by the application.
  {modules,
   [
    dd_app,
    dd_sup,
    dd_store,
    dd_doc_server,
    dd_doc_builder,
    dd_release_template,
    dd_app_template
   ]},

  % All of the registered names the application uses.
  {registered, []},

  % Applications that are to be started prior to this one.
  {applications, [eunit, kernel, stdlib, repo_daemon, sasl, gas, fslib, epkg, ewrepo, ewlib,
		  sgte, edoc, cryptographic, faxien]},

  % OTP application loader will load, but not start, included apps
  {included_applications, []},

  % configuration parameters
  {env, []},

  % The Module and Args to start this application.
  {mod, {dd_app, []}}
 ]
}.
