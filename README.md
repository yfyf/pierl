π-erl: asynchronous pi-calculus modelled in Erlang
===================================================

# Welcome!

Possibly you googled / duckduckgo'ed / searched (hopefully not bing'ed) for
something like "pi-calculus erlang" and perhaps you were not even sure what you
were looking for. Well, let me explain what you will find here.

There are many attempts to model Erlang *in π-calculus*. Haha! It's funny
because it doesn't really work. Or at least, it can't work unless you chose the
right flavour of π-calculus. And even then, it will probably still not work.

So here I did the opposite. I modelled a (slightly perverse) version of
asynchronous π-calculus *in Erlang*. If you are interested in the details of
the perverseness, you can look at the header of [src/pierl.erl](src/pierl.erl)
for details.

For some (actually, just 1, sorry) examples take a look at
[src/pierl_demo.erl](src/pierl_demo.erl). I hope I to add some comments and
more examples later on.

Here's how you can run it:

        λ make
        erlc -o ebin/ src/*.erl
        λ make run
        erl -pa ebin/ -s pierl
        <..>
        1> l(pierl_demo).
        {module,pierl_demo}
        2> pierl_demo:start().
        {1409,66878,194273}
        % you should now be spammed with messages like:
        Sending message [{{1409,66878,194273},{1409,66878,194304}}] on chan
            [{1409, 66878, 194312}] from [<0.35.0>]
        Received message [{{1409,66878,194273},{1409,66878,194304}}] on chan
            [{1409, 66878, 194312}]
        Sending message [ack] on chan [{1409,66878,194273}] from [<0.37.0>]
        Received message [ack] on chan [{1409,66878,194273}]
        Sending message ["You got the same message 0 times."] on chan
            [{1409,66878, 194312}] from [<0.39.0>]
        Received message ["You got the same message 0 times."] on chan
            [{1409,66878, 194312}]
        Sending message ["You got the same message 0 times."] on chan
            [{1409,66878, 194304}] from [<0.37.0>]
        Sending message ["You got the same message 1 times."] on chan
            [{1409,66878, 194312}] from [<0.39.0>]
        Received message ["You got the same message 0 times."] on chan
            [{1409,66878, 194304}]
        Received message ["You got the same message 1 times."] on chan
            [{1409,66878, 194312}]
        Printer: oh hey, I got some message for printing: "You got the same
            message 0 times."
        <..>


# Why?

Mostly, because it is so fun and easy to do (unlike the `Erlang -> π-calculus`
direction...). Here, look at this:

    λ wc -l src/pierl.erl src/pierl.hrl
    169 src/pierl.erl
    3 src/pierl.hrl
    172 total

:sparkling_heart: Erlang :sparkling_heart:

Also, eventually I hope I can make this into a full-fledged eDSL on top of
Erlang (or maybe Core Erlang). And then, even more eventually and if it turns
out any good, it might even become a small statically typed language. Or so the
dream goes.
