-module(rebar3_todo).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    %% initialize all commands here
    {ok, State1} = rebar3_todo_prv:init(State),
    {ok, State1}.
