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
         is_base_dir/0,
         is_base_dir/1,
         do_in_deps_dir/2,
         generate_handler/3]).

-define(DEBUG(Msg, Args), ?LOG(debug, Msg, Args)).
-define(INFO(Msg), ?INFO(Msg, [])).
-define(INFO(Msg, Args), ?LOG(info, Msg, Args)).
-define(WARN(Msg, Args), ?LOG(warn, Msg, Args)).
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

-spec preprocess(rebar_config:config(), string()) -> {ok, list()}.
preprocess(Config, _) ->
    Plugins = plugins(Config),
    ?INFO("Managing Plugins: ~p~n", [Plugins]),
    [ init(P, Config) || P <- Plugins, requires_init(P) ],
    update_code_path(Config),
    {ok, []}.

-spec command_info(atom()) -> atom() | list(atom()).
command_info(What) ->
    case erlang:function_exported(rebar_utils, command_info, 1) of
        true ->
            rebar_utils:command_info(What);
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

-spec once(atom(), fun(() -> any())) -> 'ok' | term().
once(Tag, Command) ->
    case rebar_config:get_global(Tag, undefined) of
        undefined ->
            try (Command())
            after rebar_config:set_global(Tag, done)
            end;
        done ->
            ok
    end.

-spec is_base_dir() -> boolean().
is_base_dir() ->
    is_base_dir(rebar_utils:get_cwd()).

-spec is_base_dir(string()) -> boolean().
is_base_dir(Dir) ->
    Dir == rebar_config:get_global(base_dir, undefined).

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
%% @doc Generate '<Command>'(Config, _AppFile) handler functions for each
%% command in `Cmds' and append them to either (a) a new module or (b) the
%% origin (module) if (a) fails.
%%
-spec generate_handler(Base::string(), Cmds::list(rebar_cmd_builder:command()),
                        Origin::module()) -> module().
generate_handler(Base, Cmds, Origin) ->
    case rebar_config:get_global({Base, Origin}, undefined) of
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
                    File = code:which(Origin),
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
            rebar_config:set_global({Base, Origin}, Loaded),
            Loaded;
        Handler ->
            Handler
    end.

%%
%% Internal API
%%

init(Plugin, Config) ->
    Plugin:init(Config).

requires_init(Plugin) ->
    case rebar_config:get_global({?MODULE, initialised, Plugin}, undefined) of
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
        end).
