# Rebar Plugin Manager

This plugin provides an API for various common plugin tasks (such as processing
deps and alt_deps, generate command callback functions at runtime, etc). Using
this plugin, you can guarantee that all your other plugins declared as deps,
will be available on the code path for all commands, including transient deps.

In order to achieve this, you need to do one of two things in your project.

1. Expect users to install `rebar_plugin_manager` to `$ERL_LIBS` or `code:lib_dir`
2. Include `rebar_plugin_manager` as a dependency, and point `plugin_dir` at it

In the former case, you should at least enforce the presence of the plugin:

```erlang
{deps, [{rebar_plugin_manager, "0.0.1"}]}.
{plugins, [rebar_plugin_manager]}.
```

In the latter case, you will need to specify a source (or use
[alt_deps](https://github.com/hyperthunk/rebar_alt_deps)) to ensure rebar pulls
the plugin during `get-deps` or `install-deps`, then point your `plugin_dir`
config to the source directory like so:

```erlang
{deps, [{rebar_plugin_manager, "0.0.1"}]}.
{alt_repositories, [
    {rebar_plugin_manager,
        {github, "hyperthunk", "v0.0.1"}},
]}.

{plugins, [rebar_plugin_manager]}.
{plugin_dir, "deps/rebar_plugin_manager/src"}.
```

Naturally you can't put your own build plugins somewhere custom using this
mechanism, which is annoying. To compensate for this, `rebar_plugin_manager`
provides an API by which you can hook in additional directories for loading
plugins from source.

### Plugin Loading API

```erlang
{plugins, [rebar_plugin_manager]}.
{plugin_dir, "deps/rebar_plugin_manager/src"}.

{plugin_sources, ["build/plugins", "test/plugins"]}.
```

When using this API, all exported commands (from the plugins) are exposed via
the `rebar_plugin_manager` module itself (i.e., it exports a handler which in
turn calls the requisite plugin module).

There is also limited support for loading plugins from remote sources, although
this __only__ works for single module plugins (i.e., where there is only one
.erl file to download) and is far less flexible than using something like
[alt_deps](https://github.com/hyperthunk/rebar_alt_deps).

## Installation

The `rebar_plugin_manager` requires a recent version of rebar with support for
plugins hooking into the `Module:preprocess/2` mechanism.

Include the following tuple in your rebar deps:

```erlang
{deps, [{rebar_plugin_manager, ".*", {git,
    "git://github.com/hyperthunk/rebar_plugin_manager.git"}}]}.
```

Then you will be able to fetch and install the plugin (locally) with rebar.
Alternatively, you may put the plugin into your `ERL_LIBS` path somewhere and
use it in many projects. This can be done manually, or using a package manager:

    user@host$ epm install hyperthunk/rebar_plugin_manager    # or
    user@host$ sutro install hyperthunk/rebar_plugin_manager  # or
    user@host$ agner install rebar_plugin_manager
