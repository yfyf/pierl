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
        Eshell V5.9.3  (abort with ^G)
        1> pierl_demo:start().

        [debug] [chan: 9] <send> msg: {10,8}, from: <0.39.0>
        [debug] [chan: 9] (recv) msg: {10,8}
        [debug] [chan: 10] <send> msg: ack, from: <0.41.0>
        [debug] [chan: 10] |flush| message [{109,{10,ack}}] into [<0.43.0>]
        [debug] [chan: 11] <send> msg: {'$delegate',10}, from: <0.39.0>
        [debug] [chan: 11] (recv) msg: {'$delegate',10}
        [debug] [chan: 10] (recv) msg: ack
        [debug] [chan: 9] <send> msg: {'$delegate',12}, from: <0.43.0>
        [debug] [chan: 9] (recv) msg: {'$delegate',12}
        [debug] [chan: 8] <send> msg: {'$delegate',12}, from: <0.41.0>
        [debug] [chan: 8] (recv) msg: {'$delegate',12}
        [debug] [chan: 12] <send> msg: "I failed to send you new messages, Printer.", from: <0.43.0>
        [debug] [chan: 12] (recv) msg: "I failed to send you new messages, Printer."
        Printer: aahh! Something bad happened, err: "I failed to send you new messages, Printer."
        [debug] [chan: 9] <send> msg: "You will get this message 1 more times.", from: <0.43.0>
        <..>
        [debug] [chan: 8] <send> msg: "You will get this message 0 more times.", from: <0.41.0>
        [debug] [chan: 9] <send> msg: "Ok, I am done with you.", from: <0.43.0>
        [debug] [chan: 8] (recv) msg: "You will get this message 0 more times."
        [debug] [chan: 9] (recv) msg: "Ok, I am done with you."
        Printer: oh hey, I got some message for printing: "You will get this message 0 more times."
        [debug] [chan: 8] <send> msg: "Ok, I am done with you.", from: <0.41.0>
        [debug] [chan: 8] (recv) msg: "Ok, I am done with you."
        Printer: oh hey, I got some message for printing: "Ok, I am done with you."


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
