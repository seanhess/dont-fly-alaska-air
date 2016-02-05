Don't Fly Alaska Air
====================

I was flying to SF for some meetings. I got to the airport 40 minutes early, only to find they had closed check-in 1 minute before. Instead of just giving me a ticket and letting me run for it, they made me wait in line for 20 minutes to rebook my flight. I still got to the gate before the plane left.

As a responsible citizen of the internet, I did my civic duty and promptly complained on twitter. But that clearly wasn't enough to disloge the corrupt status-quo. The only honest choice was to write a revenge twitter account to pester them into changing their policy.

Say hello to [@DontFlyAlaska](http://twitter.com/DontFlyAlaska)

What is it?
------------

@DontFlyAlaska is a good-natured revenge twitter account powered by a Haskell robot

It listens to [Twitter's Streaming API](https://dev.twitter.com/streaming/reference/post/statuses/filter), and replies to @AlaskaAir or anyone starting a conversation with them, urging them to reconsider their decision for various silly reasons. Here are some examples:

    "Don't fly @AlaksaAir. There are snakes on the plane!"

    "Don't fly @AlaksaAir. They're in league with Monsanto. Say no to genetically modified aircraft!"

    "Don't fly @AlaksaAir. The interior of the plane is carpeted like a VW bus. #VanDownByTheRiver"

    "Don't fly @AlaksaAir. Donald Trump likes them."

Technology
----------

I'm using [Haskell](https://www.haskell.org/), and got try [Conduit](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/conduit-overview) for the first time. Conduit is really cool, it lets you process streams of data in constant memory. I combined that with [twitter-conduit](https://hackage.haskell.org/package/twitter-conduit-0.1.1.1/docs/Web-Twitter-Conduit.html).

It went smoothly, the only hangup was realizing that twitter-conduit didn't property support the filter API. It only allows you to filter by one thing at a time. I first tried connecting two streams, but Twitter REALLY doesn't like that and instantly rate limited me.

Twitter's docs say to back off exponentially when rate limited, so I wrote the [`retryWithDelay`](https://github.com/seanhess/dont-fly-alaska-air/blob/master/Main.hs#L130).

      retryWithDelay 60 (*2) $ somethingThatReturnsMaybe a

I submitted [this pull request](https://github.com/himura/twitter-conduit/pull/41) to twitter-conduit to fix.






