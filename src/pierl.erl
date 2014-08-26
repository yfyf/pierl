%%% This module models asynchronous pi-calculus with
%%% channels that are:
%%% * buffered
%%% * preserve FIFO order (this, combined with the previous
%%%   point implies that they are _queues_)
%%% * owned, there's a single process owning a channel and they
%%%   need to be explicitly delegated to transfer owernship
%%%
%%% The model is far from complete:
%%% * it leaks channels
%%% * it can't be distributed
%%% * delegation can suffer from race conditions
-module(pierl).

-compile({no_auto_import, [spawn/1]}).
-compile({no_auto_import, [send/2]}).
-include("pierl.hrl").

-define(PI_MSG_TAG, $m).
-define(TAG_MSG(Chan, Msg), {?PI_MSG_TAG, {Chan, Msg}}).

-define(BLOCKED_CHAN, '$blocked').
-define(CHAN_REG_TABLE, chan_reg_ets).

% stub
-define(ATOMICALLY(X), X).
-define(ASSERT(Val),
    case Val of
        true ->
            ok;
        Other ->
            throw({assertion_failed, Other})
    end).

-export([
    setup_universe/0
]).

%% Pi-calculus primitives
-export([
    spawn/1,
    new_chan/0,
    send/2,
    recv/2,
    delegate/2
]).

%% ======================================================================
%% Setup helpers
%% ======================================================================

setup_universe() ->
    ets:new(?CHAN_REG_TABLE, [named_table, public, set]).

%% ======================================================================
%% Pi-calculus primitives
%% ======================================================================

-spec spawn(fun()) -> pid().
spawn(F) when is_function(F) ->
    erlang:spawn(F);
spawn(Instructions) when is_list(Instructions) ->
    F = compile(Instructions),
    spawn(F).

new_chan() ->
    Chan = make_ref(),
    reg_owner(self(), Chan),
    Chan.

send(Chan, Msg) ->
    %% lets pretend this is atomic somehow
    ?ATOMICALLY(begin
        Owner = get_owner(Chan),
        Owner ! ?TAG_MSG(Chan, Msg)
    end).

recv(Chan, ContF) ->
    ?ASSERT(i_own(Chan)),
    receive
        ?TAG_MSG(Chan, M) -> ContF(M)
    end.

delegate(Chan, ReceiverChan) ->
    %% 1. Two delegations of the same channel can't be triggered simultaneously
    %% 2. The channel message queue must be empty or needs to be forwarded
    %% etc.
    ?ASSERT(i_own(Chan)),
    ?ATOMICALLY(begin
        ok = block_channel(Chan),
        ok = forward_messages(Chan, ReceiverChan),
        ok = send(ReceiverChan, ?DELEGATION(Chan)),
        ok = reg_owner(Chan, get_owner(ReceiverChan))
    end).

%% ======================================================================
%% Internal helpers
%% ======================================================================

forward_messages(Chan, ReceiverChan) ->
    %% First of all, lets forward everything we have right now
    ReceiverPid = get_owner(ReceiverChan),
    ok = flush_into(Chan, ReceiverPid),
    %% Now, there might still be messages in transit, but we
    %% can't wait for them because we don't know how long they can take.
    %% However, we need to preserve the order within a channel.
    %% In practice, we could try to investigate local messages in transit
    %% and try to maintain consistent state among remote message routers.
    %% However, this seems both very hard and very taxing in throughput.
    ok.


flush_into(Chan, ReceiverPid) ->
    receive
        Msg = ?TAG_MSG(Chan, _) ->
            ReceiverPid ! Msg,
            flush_into(Chan, ReceiverPid)
    after
        0 -> ok
    end.

compile(_) ->
    throw(not_implemented).

reg_owner(Chan, Pid) ->
    ets:insert(?CHAN_REG_TABLE, {Chan, Pid}).

block_channel(Chan) ->
    reg_owner(Chan, ?BLOCKED_CHAN).

get_owner(Chan) ->
    case ets:lookup(?CHAN_REG_TABLE, Chan) of
        ?BLOCKED_CHAN ->
            timer:sleep(10),
            get_owner(Chan);
        Pid ->
            Pid
    end.

%% should be checked statically
i_own(Chan) ->
    Me = self(),
    case get_owner(Chan) == Me of
        true ->
            ok;
        false ->
            throw({channel_not_owned_by, Chan, Me})
    end.
