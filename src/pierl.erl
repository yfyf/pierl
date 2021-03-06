%%% This module models asynchronous pi-calculus with
%%% channels that are:
%%% * buffered
%%% * preserve FIFO order (this, combined with the previous
%%%   point implies that they are _queues_)
%%% * owned, there's a single process owning a channel and they
%%%   need to be explicitly delegated to transfer ownership
%%%
%%% Moreover, it is extended with a `spawn` primitive that
%%% starts a parallel process parametrised over it's own
%%% channel.
%%%
%%% The model is far from complete:
%%% * it leaks channels
%%% * it can't be distributed
%%% * delegation can suffer from race conditions
%%% but it's definitely a start!
-module(pierl).

-include("pierl.hrl").

-define(PI_MSG_TAG, $m).
-define(TAG_MSG(Chan, Msg), {?PI_MSG_TAG, {Chan, Msg}}).

-define(REC_CASE(Chan, ContF),
    ?TAG_MSG(Chan, NS__Msg) ->
        io:format(user,
            "[debug] [chan: ~200p] (recv) msg: ~200p~n", [Chan, NS__Msg]),
        ContF(NS__Msg)
).

-define(BLOCKED_CHAN, '$blocked').
-define(CHAN_REG_TABLE, chan_reg_ets).

-define(ASSERT(Val),
    case Val of
        true ->
            ok;
        Other ->
            throw({assertion_failed, Other})
    end).

% stub
-define(ATOMICALLY(X), X).

-export([
    start/0
]).

%% Pi-calculus primitives
-export([
    spawn/1,
    new_chan/0,
    send/2,
    recv/1,
    recv/2,
    delegate/2
]).

%% ======================================================================
%% Setup helpers
%% ======================================================================

start() ->
    setup_universe().

setup_universe() ->
    ets:new(?CHAN_REG_TABLE, [named_table, public, ordered_set,
        {read_concurrency, true},
        %% Hack to make this work in the shell even when it crashes
        {heir, erlang:group_leader(), undefined}]).

%% ======================================================================
%% Pi-calculus primitives
%% ======================================================================

spawn(F) when is_function(F) ->
    Pid = erlang:spawn(fun () -> receive (OwnChan) -> F(OwnChan) end end),
    Chan = new_chan(Pid),
    Pid ! Chan,
    Chan;
spawn(Instructions) when is_list(Instructions) ->
    F = compile(Instructions),
    spawn(F).

new_chan() ->
    new_chan(self()).

send(Chan, Msg) ->
    timer:sleep(5),
    %% lets pretend this is atomic somehow
    ?ATOMICALLY(begin
        Owner = get_owner(Chan),
        io:format(user,
            "[debug] [chan: ~200p] <send> msg: ~200p, from: ~200p~n",
            [Chan, Msg, self()]),
        Owner ! ?TAG_MSG(Chan, Msg)
    end),
    ok.

recv(Chan, ContF) ->
    recv([{Chan, ContF}]).

recv(Binders) ->
    ?ASSERT(
        lists:all(fun ({Chan, _}) -> i_own(Chan) end, Binders)
    ),
    ManyBinders = force_5_binders(Binders),
    recv_inner(ManyBinders).

%% @doc Hack: if there are less than 5 binders, we extend
%% the list with blocking ones to have exactly 5, because `recv_inner' expects
%% that much. If there are more, we fail.
force_5_binders(Binders) when length(Binders) > 5 ->
    throw(more_than_5_receive_cases_not_supported);
force_5_binders(Binders) ->
    Len = length(Binders),
    Remain = 5 - Len,
    Binders ++ lists:duplicate(Remain, null_binder()).

null_binder() ->
    {
        '$non_existing_channel',
        fun (M) -> throw({received_on_nonexisting_channel, M}) end
    }.


recv_inner([{C1, F1}, {C2, F2}, {C3, F3}, {C4, F4}, {C5, F5}]) ->
    receive
        ?REC_CASE(C1, F1);
        ?REC_CASE(C2, F2);
        ?REC_CASE(C3, F3);
        ?REC_CASE(C4, F4);
        ?REC_CASE(C5, F5)
    end.

delegate(Chan, ReceiverChan) ->
    %% 1. Two delegations of the same channel can't be triggered simultaneously
    %% 2. The channel message queue must be empty or needs to be forwarded
    %% etc.
    %% TODO: maybe delegations should happen as a two-phase protocol?
    %% First, just pass a magic message, it can then be handled
    %% by repeaters and so on transparently.
    %% When it's received, send an acknowledgement.
    %% But perhaps it has unintuitive semantics, where you
    %% intended a specific receiver, but it just passes the channel
    %% along... Questionable.
    ?ASSERT(i_own(Chan)),
    ?ATOMICALLY(begin
        ReceiverPid = get_owner(ReceiverChan),
        ok = block_channel(Chan),
        ok = forward_messages(Chan, ReceiverPid),
        ok = send(ReceiverChan, ?DELEGATION(Chan)),
        ok = reg_owner(Chan, ReceiverPid)
    end).

%% ======================================================================
%% Internal helpers
%% ======================================================================

forward_messages(Chan, ReceiverPid) ->
    receive
        Msg = ?TAG_MSG(Chan, _) ->
            io:format(user,
                "[debug] [chan: ~200p] |flush| message [~200p] into [~200p]~n",
                [Chan, Msg, ReceiverPid]),
            ReceiverPid ! Msg,
            forward_messages(Chan, ReceiverPid)
    after
    %% Now, there might still be messages in transit, but we
    %% can't wait for them because we don't know how long they can take.
    %% However, we need to preserve the order within a channel.
    %% In practice, we could try to investigate local messages in transit
    %% and try to maintain consistent state among remote message routers.
    %% However, this seems both very hard and very taxing in throughput.
        50 -> ok
    end.

compile(_) ->
    throw(not_implemented).

reg_owner(Chan, Pid) ->
    true = ets:insert(?CHAN_REG_TABLE, {Chan, Pid}),
    ok.

block_channel(Chan) ->
    reg_owner(Chan, ?BLOCKED_CHAN).

get_owner(Chan) ->
    case ets:lookup(?CHAN_REG_TABLE, Chan) of
        [{_, ?BLOCKED_CHAN}] ->
            timer:sleep(10),
            get_owner(Chan);
        [{Chan, Pid}] ->
            Pid;
        [] ->
            throw({channel_not_found, Chan})
    end.

new_chan(Owner) ->
    NextId = case ets:last(?CHAN_REG_TABLE) of
        '$end_of_table' -> 1;
        Id -> Id + 1
    end,
    ok = reg_owner(NextId, Owner),
    NextId.

%% should be checked statically
i_own(Chan) ->
    Me = self(),
    case get_owner(Chan) == Me of
        true ->
            true;
        false ->
            throw({channel_not_owned_by, Chan, Me})
    end.
