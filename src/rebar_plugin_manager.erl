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
         'check-config'/2,
         command_info/1,
         plugins/1,
         once/3,
         is_base_dir/1,
         is_base_dir/2,
         is_app_dir/1,
         is_app_dir/2,
         debug/2,
         info/2,
         warn/2,
         error/2,
         log/3,
         sh/2,
         read_config/4,
         read_all_config/2,
         write_config/4,
         delete_config/3,
         skip_dir/2,
         is_skip_dir/2,
         reset_skip_dirs/1,
         abort/2,
         cp_r/2,
         rm_rf/1,
         ensure_dir/1,
         do_in_deps_dir/2,
         generate_handler/4]).

-define(DEBUG(Msg, Args), ?LOG(debug, Msg, Args)).
-define(INFO(Msg), ?INFO(Msg, [])).
-define(INFO(Msg, Args), ?LOG(info, Msg, Args)).
-define(WARN(Msg, Args), ?LOG(warn, Msg, Args)).
-define(ERROR(Msg, Args), ?LOG(error, Msg, Args)).
-define(LOG(Level, Msg, Args), rebar_log:log(Level, Msg, Args)).
-define(ABORT(Msg, Args), rebar_utils:abort(Msg, Args)).

-type command_name() :: atom().
-type command_data() :: any().
-type instruction_set() :: list(command_data()).
-type command() :: {'command',
                    command_name(),
                    command_data(),
                    instruction_set()}.

-export_type([command_name/0,
              command_data/0,
              instruction_set/0,
              command/0]).

%%
%% Rebar Facing API
%%

-spec preprocess(rebar_config:config(), string()) -> {ok, list()}.
preprocess(Config, _) ->
    Plugins = plugins(Config),
    ?INFO("Managing Plugins: ~p~n", [Plugins]),
    [ init(P, Config) || P <- Plugins, requires_init(P, Config) ],
    update_code_path(Config),
    {ok, []}.

-spec 'check-config'(rebar_config:config(), any()) -> ok.
'check-config'(Config, _) ->
    case rebar_config:get_global(Config, key, undefined) of
        undefined ->
            ?ABORT("check-config requires you to pass "
                   "key=<name> on the command line...~n", []);
        Key ->
            [ check_config(list_to_atom(Key), Mode, Config) || Mode <-
                                                [get_local, get, get_all] ],
            ok
    end.

%%
%% Plugin Facing API
%%

check_config(Key, ReadFunc, Config) ->
    Args = case ReadFunc of
        get_all -> [Config, Key];
        _ -> [Config, Key, undefined]
    end,
    case apply(rebar_config, ReadFunc, Args) of
        undefined ->
            io:format("config key '~p' not found (~p)~n",
                      [Key, ReadFunc]);
        Value ->
            io:format("config key '~p' found (~p): ~p~n",
                      [Key, ReadFunc, Value])
    end.

-spec command_info(atom()) -> atom() | list(atom()) | {'error', 'unsupported'}.
command_info(What) ->
    case erlang:function_exported(rebar_utils, command_info, 1) of
        true ->
            M = rebar_utils,
            F = command_info,
            apply(M, F, [What]);
        false ->
            V = 1,
            case (catch V = undefined) of
                {'EXIT', {_, CallStack}} ->
                    {M, _, _} = erlang:hd(erlang:tl(CallStack)),
                    ?ABORT("Plugin ~p requires a version of rebar that "
                           "supports `rebar_utils:command_info/1'~n"
                           "Please upgrade to a supported version.~n", [M])
            end
    end.

-spec plugins(rebar_config:config()) -> list(atom()).
plugins(Config) ->
    lists:flatten(rebar_config:get_all(Config, plugins)).

-spec once(atom(), fun(() -> any()), rebar_config:config()) -> 'ok' | term().
once(Tag, Command, Config) ->
    case rebar_config:get_global(Config, Tag, undefined) of
        undefined ->
            try (Command())
            after rebar_config:set_global(Config, Tag, done)
            end;
        done ->
            ok
    end.

-spec is_base_dir(rebar_config:config()) -> boolean().
is_base_dir(Config) ->
    is_base_dir(rebar_utils:get_cwd(), Config).

-spec is_base_dir(string(), rebar_config:config()) -> boolean().
is_base_dir(Dir, Config) ->
    Dir == rebar_config:get_xconf(Config, base_dir).

-spec is_app_dir(rebar_config:config()) -> boolean().
is_app_dir(Config) ->
    is_app_dir(Config, rebar_utils:get_cwd()).

-spec is_app_dir(rebar_config:config(), file:filename()) -> boolean().
is_app_dir(_Config, Dir) ->
    rebar_app_utils:is_app_dir(Dir).

-spec do_in_deps_dir(string(), fun((string()) -> any())) -> 'ok'.
do_in_deps_dir(DepsDir, Handler) ->
    case file:list_dir(DepsDir) of
        {ok, Dirs} ->
            [ Handler(Dir) || Dir <- Dirs ];
        {error, Err} ->
            rebar_log:log(warn, "Unable to process deps: ~p~n", [Err])
    end.

-spec deps_dir(rebar_config:config()) -> string().
deps_dir(Config) ->
    case rebar_config:get_global(Config, alt_deps_dir, undefined) of
        undefined ->
            Dir = rebar_config:get_local(Config, alt_deps_dir,
                        rebar_config:get_local(Config, deps_dir, "deps")),
            DepsDir = filename:absname(Dir),
            rebar_utils:ensure_dir(filename:join(DepsDir, "foo")),
            rebar_config:set_global(Config, alt_deps_dir, DepsDir),
            Dir;
        AltDepsDir ->
            AltDepsDir
    end.

-spec debug(string(), [term()]) -> 'ok'.
debug(Msg, Args) ->
    log(debug, Msg, Args).

-spec info(string(), [term()]) -> 'ok'.
info(Msg, Args) ->
    log(info, Msg, Args).

-spec warn(string(), [term()]) -> 'ok'.
warn(Msg, Args) ->
    log(warn, Msg, Args).

-spec error(string(), [term()]) -> 'ok'.
error(Msg, Args) ->
    log(error, Msg, Args).

-spec log(atom(), string(), [term()]) -> 'ok'.
log(Level, Msg, Args) ->
    rebar_log:log(Level, Msg, Args).

-spec read_config(atom(), rebar_config:config(),
                  atom(), term()) -> term().
read_config(xconf, Config, Key, Default) ->
    rebar_config:get_xconf(Config, Key, Default);
read_config(global, Config, Key, Default) ->
    rebar_config:get_global(Config, Key, Default);
read_config(local, Config, Key, Default) ->
    rebar_config:get_local(Config, Key, Default);
read_config(env, Config, Key, Default) ->
    case catch(rebar_config:get_env(Config, Key)) of
        {'EXIT', _} -> Default;
        Other       -> Other
    end.

-spec read_all_config(rebar_config:config(), atom()) -> [term()].
read_all_config(Config, Key) ->
    rebar_config:get_all(Config, Key).

-spec write_config(atom(), rebar_config:config(),
                   atom(), term()) -> rebar_config:config().
write_config(xconf, Config, Key, Value) ->
    rebar_config:set_xconf(Config, Key, Value);
write_config(global, Config, Key, Value) ->
    rebar_config:set_global(Config, Key, Value);
write_config(local, Config, Key, Value) ->
    rebar_config:set(Config, Key, Value);
write_config(env, Config, Key, Values) ->
    rebar_config:save_env(Config, Key, Values).

-spec delete_config(atom(),
                    rebar_config:config(),
                    atom()) -> rebar_config:config().
delete_config(xconf, Config, Key) ->
    rebar_config:erase_xconf(Config, Key);
delete_config(Kind, _, Key) ->
    abort("cannot erase ~p config key ~p: unsupported operation~n",
          [Kind, Key]).

-spec skip_dir(file:filename(), rebar_config:config()) -> rebar_config:config().
skip_dir(Path, Config) ->
    rebar_config:set_skip_dir(Config, Path).

-spec is_skip_dir(rebar_config:config(), file:filename()) -> boolean().
is_skip_dir(Config, Path) ->
    rebar_config:is_skip_dir(Config, Path).

-spec reset_skip_dirs(rebar_config:config()) -> rebar_config:config().
reset_skip_dirs(Config) ->
    rebar_config:reset_skip_dirs(Config).

-spec abort(string(), [term()]) -> 'ok'.
abort(Msg, Args) ->
    rebar_utils:abort(Msg, Args).

%% TODO: this is under-defined
-spec sh(string(), [term()]) -> term().
sh(Cmd, Opts) ->
    rebar_utils:sh(Cmd, Opts).

-spec cp_r([file:filename()], file:filename()) -> 'ok'.
cp_r(Src, Dest) ->
    rebar_file_utils:cp_r(Src, Dest).

-spec rm_rf(file:filename()) -> 'ok'.
rm_rf(Path) ->
    rebar_file_utils:rm_rf(Path).

-spec ensure_dir(file:filename()) -> 'ok'.
ensure_dir(Target) ->
    rebar_utils:ensure_dir(Target).

%%
%% @doc Generate 'Command'(Config, _AppFile) handler functions for each
%% command in `Cmds' and append them to either (a) a new module or (b) the
%% origin (module) if (a) fails.
%%
-spec generate_handler(Base::string(),
                       Cmds::list(rebar_plugin_manager:command()),
                       Origin::module(),
                       Config::rebar_config:config()) -> module().
generate_handler(Base, Cmds, Origin, Config) ->
    case rebar_config:get_global(Config, {Base, Origin}, undefined) of
        undefined ->
            ?DEBUG("Generating handler(s) for ~p: ~p~n", [Base, Cmds]),
            Exports = [ {C, 2} || {command, C, _, _} <- Cmds ],
            Functions = [ gen_function(Base, C, Origin) ||
                                            {command, C, _, _} <- Cmds ],

            %% Using get_object_code can lead to all kinds of situations when
            %% we're in an escript, when we're called by other code that isn't
            %% and so on. This *mixed* approach is intended to cover the edge
            %% cases I've seen thus far.
            {Forms, Loader} = case code:get_object_code(Origin) of
                {_,Bin,_} ->
                    ?DEBUG("Compiling from existing binary~n", []),
                    GeneratedForms = to_forms(atom_to_list(Origin),
                                                Exports, Functions, Bin),
                    {GeneratedForms, fun load_binary/2};
                error ->
                    Path = filename:split(code:which(Origin)),
                    {P, [_,BeamName]} = lists:split(length(Path) - 2, Path),
                    %% NB: this is very specific to .erl sources!!!!
                    SrcName = 
                        filename:basename(BeamName, 
                                          code:objfile_extension()) ++ ".erl",
                    File = filename:join(P, SrcName),
                    case compile:file(File, [debug_info,
                                             binary, return_errors]) of
                        {ok, _, Bin} ->
                            ?DEBUG("Compiling from binary~n", []),
                            {to_forms(atom_to_list(Origin), Exports,
                                    Functions, Bin), fun load_binary/2};
                        Error ->
                            ?WARN("Unable to recompile ~p: ~p~n",
                                  [Origin, Error]),
                            {mod_from_scratch(Base, Exports, Functions),
                                    fun evil_load_binary/2}
                    end;
                _ ->
                    ?WARN("Cannot modify ~p - generating new module~n",
                          [Origin]),
                    {mod_from_scratch(Base, Exports, Functions),
                                                        fun evil_load_binary/2}
            end,
            Loaded = case compile:forms(Forms, [report, return]) of
                {ok, ModName, Binary} ->
                    Loader(ModName, Binary);
                {ok, ModName, Binary, _Warnings} ->
                    Loader(ModName, Binary);
                CompileError ->
                    ?ABORT("Unable to compile: ~p~n", [CompileError])
            end,
            rebar_config:set_global(Config, {Base, Origin}, Loaded),
            Loaded;
        Handler ->
            Handler
    end.

%%
%% Internal API
%%

init(Plugin, Config) ->
    Plugin:init(Config).

requires_init(Plugin, Config) ->
    case rebar_config:get_global(Config, 
                                {?MODULE, initialised, Plugin}, undefined) of
        undefined ->
            erlang:function_exported(Plugin, init, 1);
        _ ->
            false
    end.

mod_from_scratch(Base, Exports, Functions) ->
    [{attribute, ?LINE, module, list_to_atom(Base ++
                                             "_custom_build_plugin")},
     {attribute, ?LINE, export, Exports}] ++ Functions.

to_forms(Base, Exports, Functions, Bin) ->
    case beam_lib:chunks(Bin, [abstract_code]) of
        {ok, {_,[{abstract_code,{_,[FileDef, ModDef|Code]}}|_]}} ->
            Code2 = lists:keydelete(eof, 1, Code),
            [FileDef, ModDef] ++
            [{attribute,31,export,Exports}] ++
            Code2 ++ Functions; %%  ++ [EOF],
        _ ->
            [{attribute, ?LINE, module, list_to_atom(Base)},
             {attribute, ?LINE, export, Exports}] ++ Functions
    end.

gen_function(Base, Cmd, Origin) ->
    {function, ?LINE, Cmd, 2, [
        {clause, ?LINE,
            [{var,?LINE,'Config'},
             {var,?LINE,'File'}],
            [],
            [{call,?LINE,
                {remote,?LINE,
                    {atom,?LINE,Origin},
                    {atom,?LINE,execute_command}},
                    [erl_parse:abstract(Cmd),
                     erl_parse:abstract(Base),
                     {var,?LINE,'Config'},
                     {var,?LINE,'File'}]}]}]}.

evil_load_binary(Name, Binary) ->
    %% this is a nasty hack - perhaps adding the function to *this*
    %% module would be a better approach, but for now....
    ?DEBUG("Evil Loading: ~p~n", [Name]),
    Name = load_binary(Name, Binary),
    {ok, Existing} = application:get_env(rebar, any_dir_modules),
    application:set_env(rebar, any_dir_modules, [Name|Existing]),
    Name.

load_binary(Name, Binary) ->
    case code:load_binary(Name, "", Binary) of
        {module, Name}  ->
            ?DEBUG("Module ~p loaded~n", [Name]),
            Name;
        {error, Reason} -> ?ABORT("Unable to load binary: ~p~n", [Reason])
    end.

update_code_path(Config) ->
    %% TODO: this is far too simplistic, especially once we move to a
    %% central repository based approach. We need to differentiate between
    %% things that we *know* should be on the code path (e.g., plugins, deps)
    %% and other "uninteresting" directories

    %% TODO: also we should put some explicit support in for seivy, where it isn't
    %%  already on the code path...
    once(setup_path,
         fun() ->
             do_in_deps_dir(deps_dir(Config),
                            (fun(Dir) -> code:add_pathz(Dir) end))
         end,
         Config).
