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

compile_opts(#template_opts{compile_vars = undefined, compile_opts = Opts}) -> Opts;
compile_opts(#template_opts{compile_vars = Vars, compile_opts = Opts}) -> [{default_vars, Vars} | Opts].

run_compile(T) ->
    erlydtl:compile(T#template_opts.source,
                    T#template_opts.module, compile_opts(T)).

run_render(#template_opts{render = Render} = T) ->
    Output = if is_atom(Render) ->
                    (T#template_opts.module):Render(T#template_opts.render_vars,
                                                      T#template_opts.render_opts);
                is_function(Render) -> Render(T)
             end,
    case Output of
      {ok, O} -> iolist_to_binary(O);
      _RenderOutput -> error_ok
    end.

%%% ===================================================================
%%% Tests  
%%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

run_test() ->
    ok =  meck:new(erlydtl, [non_strict]),
    ok =  meck:expect(erlydtl, compile, fun
            (file,_,_) -> {ok, 1};
            (dir,_,_) -> {ok, 1, 2};
            (template,_,_) -> {ok, 1, 2, 3};
            (_,_,_) -> error
          end),

    ok = meck:new(template_parse, [non_strict]),
    ok =  meck:expect(template_parse, render, fun
              (error, _)-> error;
              (_, _) -> {ok, "test"}
            end),

    Opts = #template_opts{module = template_parse, render = render},
    ?assertEqual(<<"test">>, run(Opts#template_opts{source = file})),
    ?assertEqual(<<"test">>, run(Opts#template_opts{source = dir})),
    ?assertEqual(<<"test">>, run(Opts#template_opts{source = template})),
    ?assertEqual(error, run(Opts#template_opts{source = other})),

    ?assertEqual(error_ok, run(Opts#template_opts{source = template, render_vars = error})),
    ?assert(is_binary(run(Opts#template_opts{source = template}))),
    ?assertEqual(<<"test">>, run(Opts#template_opts{source = template, render = fun(_) -> {ok, "test"} end})),

    true = meck:validate(erlydtl),
    true = meck:validate(template_parse),
    
    ok = meck:unload(erlydtl),
    ok = meck:unload(template_parse).

compile_opts_test() ->
    Opts = #template_opts{module = template_parse, render = render, compile_vars = undefined},
    ?assertEqual([report, return, {auto_escape, false}, force_recompile, {out_dir, false},{debug_info , true}], compile_opts(Opts)),
    ?assertEqual([{default_vars, test}, report, return, {auto_escape, false}, force_recompile, {out_dir, false},{debug_info , true}], 
      compile_opts(Opts#template_opts{compile_vars = test})).

-endif.