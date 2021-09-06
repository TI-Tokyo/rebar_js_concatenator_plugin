rebar_js_concatenator_plugin
=====

A rebar3 plugin that concatenates and optionally uglifies sets of JS
files into bundles.

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar_js_concatenator_plugin, {git, "https://host/user/rebar_js_concatenator_plugin.git", {tag, "0.1.0"}}}
    ]}.

and enable it like so:

    {provider_hooks, [{pre, [{compile, {default, rebar_js_concatenator_plugin}}]}]}.
