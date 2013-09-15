hrac
====

*[EXPERIMENTAL] Monads / RAC / Types*

Since RACSignals send some type of data, that type information can not be
expressed in Objective-C. So instead we express it in Haskell where
we can type check it, then generate the correct Objective-C.

![](https://pbs.twimg.com/media/BULhQrkCIAAUena.jpg:large)

By utilizing `language-c-inline` you can write Mac applications this way.
See `examples` for an example on how.
