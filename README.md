js_fumble
=====

A rebar plugin

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {js_fumble, {git, "https://host/user/js_fumble.git", {tag, "0.1.0"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 js_fumble
    ===> Fetching js_fumble
    ===> Compiling js_fumble
    <Plugin Output>
