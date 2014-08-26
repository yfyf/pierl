-module(pierl_demo).
-include("pierl.hrl").
-compile(export_all).

-define(PI, pierl).

repeater(OwnChan) ->
    ?PI:recv(OwnChan, fun ({AckChan, OutputChan}) ->
        ?PI:send(AckChan, ack),
        repeater(OwnChan, OutputChan)
    end).

repeater(OwnChan, OutputChan) ->
    ?PI:recv(OwnChan, fun (M) ->
        ?PI:send(OutputChan, M),
        repeater(OwnChan, OutputChan)
    end).

sender(OwnChan, TargetChan) ->
    ?PI:recv(OwnChan, fun (?DELEGATION(AckChan)) ->
        ?PI:recv(AckChan, fun (ack) ->
            %% got ack, we can go!
            sender_body(TargetChan, 2)
            end
        )
    end).

sender_body(TargetChan, 0) ->
    ?PI:send(TargetChan,
       "Ok, I am done with you."
    );
sender_body(TargetChan, Count) ->
    ?PI:send(TargetChan,
       "You will get this message " ++
            integer_to_list(Count - 1) ++
            " more times."
    ),
    sender_body(TargetChan, Count - 1).

printer(OwnChan) ->
    ?PI:recv(OwnChan,
        fun (M) ->
            io:format(user,
                "Printer: oh hey, I got some message for printing: ~p~n",
                [M]),
            printer(OwnChan)
        end
    ).

start() ->
    ?PI:spawn(fun main/1).

main(_) ->
    random:seed(now()),
    PrinterChan = ?PI:spawn(fun printer/1),
    RepeaterChan = ?PI:spawn(fun repeater/1),
    AckChan = ?PI:new_chan(),
    ?PI:send(RepeaterChan, {AckChan, PrinterChan}),
    SenderChan = ?PI:spawn(fun (SelfChan) -> sender(SelfChan, RepeaterChan) end),
    %% This so that sometimes the ack would end up in this process
    %% and sometimes it would get delayed and already arrive after the
    %% delegation.
    timer:sleep(random:uniform(12)),
    ?PI:delegate(AckChan, SenderChan).
