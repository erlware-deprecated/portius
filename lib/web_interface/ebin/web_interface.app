%% This is the application resource file (.app file) for the gen_web_server,
%% application.
{application, web_interface, 
  [{description, "An application that uses the gen_web_server to serve up repo contents to faxien"},
   {vsn, "0.2.1.0"},
   {modules, [wi_app,
              wi_sup,
              wi_server]},
   {registered,[]},
   {applications, [kernel, stdlib, gen_web_server]},
   {mod, {wi_app, []}},
   {start_phases, []}]}.

