%% -----------------------------------------------------------------------------
%% Copyright (c) 2002-2011 Tim Watson (watson.timothy@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -----------------------------------------------------------------------------
-module(rebar_plugin_manager).
-export([preprocess/2,
         command_info/1,
         plugins/1,
         once/2,
         do_in_deps_dir/2]).

-define(INFO(Msg), ?INFO(Msg, [])).
-define(INFO(Msg, Args), ?LOG(info, Msg, Args)).
-define(LOG(Level, Msg, Args), rebar_log:log(Level, Msg, Args)).
-define(ABORT(Msg, Args), rebar_utils:abort(Msg, Args)).

preprocess(Config, _) ->
    Plugins = plugins(Config),
    ?INFO("Managing Plugins: ~p~n", [Plugins]),
    update_code_path(Config),
    {ok, []}.

command_info(What) ->
    case erlang:function_exported(rebar_utils, command_info, 1) of
        true ->
            rebar_utils:command_info(What);
        false ->
            {'EXIT', {Error, CallStack}} = (catch 1 = 2),
            {M, _, _} = erlang:hd(erlang:tl(CallStack)),
            ?ABORT("Plugin ~p requires a version of rebar that "
                   "supports `rebar_utils:command_info/1'~n"
                   "Please upgrade to a supported version.~n", [M])
    end.

plugins(Config) ->
    lists:flatten(rebar_config:get_all(Config, plugins)).

once(Tag, Command) ->
    case rebar_config:get_global(Tag, undefined) of
        undefined ->
            try (Command())
            after rebar_config:set_global(Tag, done)
            end;
        done ->
            ok
    end.

do_in_deps_dir(DepsDir, Handler) ->
    case file:list_dir(DepsDir) of
        {ok, Dirs} ->
            [ Handler(Dir) || Dir <- Dirs ];
        {error, Err} ->
            rebar_log:log(warn, "Unable to process deps: ~p~n", [Err])
    end.

deps_dir(Config) ->
    case rebar_config:get_global(alt_deps_dir, undefined) of
        undefined ->
            Dir = rebar_config:get_local(Config, alt_deps_dir,
                        rebar_config:get_local(Config, deps_dir, "deps")),
            DepsDir = filename:absname(Dir),
            rebar_utils:ensure_dir(filename:join(DepsDir, "foo")),
            rebar_config:set_global(alt_deps_dir, DepsDir),
            Dir;
        AltDepsDir ->
            AltDepsDir
    end.

%%
%% Internal API
%%

update_code_path(Config) ->
    once(setup_path,
        fun() ->
            do_in_deps_dir(deps_dir(Config),
                        (fun(Dir) -> code:add_pathz(Dir) end))
        end).
