rebar3\_todo
=====

A plugin to detect TODOs left in comments

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_todo, {git, "https://github.com/ferd/rebar3_todo.git", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 todo

