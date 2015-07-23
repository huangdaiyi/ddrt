-module(ddrt_runder).

-export([run/1, run_compile/1, run_render/1]).
-include ("include/ddrt.hrl").

run(T) ->
    case run_compile(T) of
      {ok, _} -> run_render(T);
      {ok, _, _} -> run_render(T);
      {ok, _, _, _} -> run_render(T);
      _ -> error
    end.

compile_opts(#template_opts{compile_vars = undefined,
                            compile_opts = Opts}) ->
    Opts;
compile_opts(#template_opts{compile_vars = Vars,
                            compile_opts = Opts}) ->
    [{default_vars, Vars} | Opts].

run_compile(T) ->
    erlydtl:compile(T#template_opts.source,
                    T#template_opts.module, compile_opts(T)).

run_render(#template_opts{renderer = Renderer} = T) ->
    Output = if is_atom(Renderer) ->
                    (T#template_opts.module):Renderer(T#template_opts.render_vars,
                                                      T#template_opts.render_opts);
                is_function(Renderer) -> Renderer(T)
             end,
    case Output of
      {ok, O} -> iolist_to_binary(O);
      _RenderOutput -> error_ok
    end.
