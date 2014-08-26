-module(pierl_demo).

repeater(OwnChan) ->
    recv(OwnChan, fun (OutputChan) ->
        repeater(OwnChan, OutputChan)
    end).

repeater(OwnChan, OutputChan) ->
    recv(OwnChan, fun (M) ->
        send(OutputChan, M),
        repeater(OwnChan, OutputChan)
    ).
