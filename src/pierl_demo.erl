-module(pierl_demo).
-include("pierl.hrl").
-compile(export_all).

-define(PI, pierl).

repeater(OwnChan) ->
    ?PI:recv(OwnChan, fun ({From, OutputChan}) ->
        ?PI:send(From, ack),
        repeater(OwnChan, OutputChan)
    end).

repeater(OwnChan, OutputChan) ->
    ?PI:recv(OwnChan, fun (M) ->
        ?PI:send(OutputChan, M),
        repeater(OwnChan, OutputChan)
    end).

sender(TargetChan) ->
    sender(TargetChan, 0).

sender(TargetChan, Count) ->
    ?PI:send(TargetChan,
       "You got the same message " ++ integer_to_list(Count) ++ " times."
    ),
    sender(TargetChan, Count + 1).

printer(OwnChan) ->
    ?PI:recv(OwnChan,
        fun (M) ->
            io:format("Printer: oh hey, I got some message for printing: ~p~n", [M]),
            printer(OwnChan)
        end
    ).

start() ->
    ?PI:spawn(fun main/1).

main(OwnChan) ->
    PrinterChan = ?PI:spawn(fun printer/1),
    RepeaterChan = ?PI:spawn(fun repeater/1),
    ?PI:send(RepeaterChan, {OwnChan, PrinterChan}),
    ?PI:recv(OwnChan, fun (ack) ->
        ?PI:spawn(fun (_) -> sender(RepeaterChan) end)
    end).
