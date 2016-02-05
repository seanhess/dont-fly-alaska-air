Don't Fly Alaska Air
====================

I was flying to SF for some meetings. I got to the airport 40 minutes early, only to find they had closed check-in 1 minute before. Instead of just giving me a ticket and letting me run for it, they made me wait in line for 20 minutes to change my flight. I still got to the gate before the plane left.

As a responsible citizen of the internet, I did my civic duty and promptly complained on twitter. But that clearly wasn't enough to reform the corrupt corporate status-quo. The only ethical choice was to write a revenge twitter account to pester them into changing their policy.

Say hello to [@DontFlyAlaska](http://twitter.com/DontFlyAlaska)

What is it?
------------

[@DontFlyAlaska](http://twitter.com/DontFlyAlaksa) is a silly revenge twitter account powered by a Haskell robot

It listens to [Twitter's Streaming API](https://dev.twitter.com/streaming/reference/post/statuses/filter), and replies to @AlaskaAir or anyone starting a conversation with them, urging them to reconsider their decision for various silly reasons. Here are some examples:

    "Don't fly @AlaksaAir. There are snakes on the plane!"

    "Don't fly @AlaksaAir. They're in league with Monsanto. Say no to genetically modified aircraft!"

    "Don't fly @AlaksaAir. The interior of the plane is carpeted like a VW bus. #VanDownByTheRiver"

    "Don't fly @AlaksaAir. Donald Trump likes them."

Technology
----------

I'm using [Haskell](https://www.haskell.org/), and got to try [Conduit](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/conduit-overview) for the first time. Conduit lets you process streams of data in constant memory. The [twitter-conduit](https://hackage.haskell.org/package/twitter-conduit-0.1.1.1/docs/Web-Twitter-Conduit.html) package provides access to the Streaming API as a conduit.

The only hangup was that twitter-conduit only allows you to filter by one thing at a time. I first tried connecting two streams, but Twitter REALLY doesn't like that and instantly rate limited me. Twitter's docs say to back off exponentially when rate limited, so I wrote [`retryWithDelay`](https://github.com/seanhess/dont-fly-alaska-air/blob/master/Main.hs#L124).

      retryWithDelay 60 (*2) $ somethingThatReturnsMaybe

I submitted [this pull request](https://github.com/himura/twitter-conduit/pull/41) to twitter-conduit to fix, and forked some functions from twitter-conduit in the meantime.

DontFlyAlaska in Action
------------------------

I left the twitter bot on for only about 30 minutes, because, well, it's actually very annoying and I'm not THAT mad, but it had some good moments.

![Cockpit Transparency](http://imgur.com/wfudmr1.png)

Follow [@DontFlyAlaska](http://twitter.com/DontFlyAlaska)

About me
--------

I create things, using Haskell and other cool technology. Read more [on my blog](http://seanhess.github.io)








