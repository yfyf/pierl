-module(pierl_demo).
-include("pierl.hrl").
-compile(export_all).

-define(PI, pierl).

forwarder(OwnChan) ->
    ?PI:recv(OwnChan, fun ({AckChan, OutputChan}) ->
        ?PI:send(AckChan, ack),
        forwarder(OwnChan, OutputChan)
    end).

forwarder(OwnChan, OutputChan) ->
    ?PI:recv(OwnChan, fun
            %% also forward delegations
            (?DELEGATION(Chan)) ->
                ?PI:delegate(Chan, OutputChan);
            (M) ->
                ?PI:send(OutputChan, M)
    end),
    forwarder(OwnChan, OutputChan).

sender(OwnChan, PrinterChan) ->
    random:seed(now()),
    ?PI:recv(OwnChan, fun (?DELEGATION(AckChan)) ->
        ?PI:recv(AckChan, fun (ack) ->
            %% got ack, we can go!
            ErrorChan = ?PI:new_chan(),
            ?PI:delegate(ErrorChan, PrinterChan),
            sender_body(PrinterChan, ErrorChan, 2)
            end
        )
    end).

sender_body(TargetChan, _, 0) ->
    ?PI:send(TargetChan,
       "Ok, I am done with you."
    );
sender_body(TargetChan, ErrorChan, Count) ->
    case random:uniform(3) of
        1 ->
            ?PI:send(TargetChan,
               "You will get this message " ++
                    integer_to_list(Count - 1) ++
                    " more times."
            ),
            sender_body(TargetChan, ErrorChan, Count - 1);
        _ ->
            ?PI:send(ErrorChan,
                "I failed to send you new messages, Printer."
            ),
            sender_body(TargetChan, ErrorChan, Count)
    end.

printer(OwnChan) ->
    ?PI:recv(OwnChan,
        fun (?DELEGATION(ErrorChan)) ->
            printer(OwnChan, ErrorChan)
        end
    ).

printer(PrintChan, ErrorChan) ->
    ?PI:recv([
        {PrintChan, fun (M) ->
            io:format(user,
                "Printer: oh hey, I got some message for printing: ~p~n",
                [M]
             )
        end},
        {ErrorChan, fun (Err) ->
            io:format(user,
                "Printer: aahh! Something bad happened, err: ~p~n",
                [Err]
             )
        end}
    ]),
    printer(PrintChan, ErrorChan).

start() ->
    ?PI:spawn(fun main/1).

main(_) ->
    random:seed(now()),
    PrinterChan = ?PI:spawn(fun printer/1),
    ForwarderChan = ?PI:spawn(fun forwarder/1),
    AckChan = ?PI:new_chan(),
    ?PI:send(ForwarderChan, {AckChan, PrinterChan}),
    SenderChan = ?PI:spawn(
        fun (SelfChan) -> sender(SelfChan, ForwarderChan) end),
    %% This so that sometimes the ack would end up in this process
    %% and sometimes it would get delayed and already arrive after the
    %% delegation.
    timer:sleep(random:uniform(12)),
    ?PI:delegate(AckChan, SenderChan).
