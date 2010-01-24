{application, streamer,
 [{description, "streamer"},
  {vsn, "0.01"},
  {modules, [
    streamer,
    streamer_app,
    streamer_sup,
    streamer_web,
    streamer_deps
  ]},
  {registered, []},
  {mod, {streamer_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
