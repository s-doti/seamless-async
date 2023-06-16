# seamless-async

[![Clojars Project](https://img.shields.io/clojars/v/com.github.s-doti/seamless-async.svg)](https://clojars.org/com.github.s-doti/seamless-async)

A Clojure library for making async.. seamless.

### seam·less (adjective)
> https://www.merriam-webster.com/dictionary/seamless
> - having no seams<br>
> - having no awkward transitions, interruptions, or indications of disparity<br>
> - perfect, flawless

## Motivation / Exposition
Software solutions are often fashioned after 3rd party APIs which are used under 
the hood, eg. whether some 3rd party API is async/blocking can have definitive 
impact over the design of our code.

This is unfortunate, as 3rd party APIs may be secondary implementation details, 
having little to do with how our solution is to be used, or to scale. Still, the 
developer may be forced to make choices very early on, whereas the consequences 
of these choices may be revealed much, much later - in production - where code 
meets reality, and reality poses its true requirements.

But in fact, it isn't just 3rd party APIs, is it? It could just as well be our 
past self, deceiving our future self; not seeing requirements for what they are, 
what they could be. Yet, can you blame that guy? after all, requirements 
naturally change over time. Oh, why must it be such a cruel choice between 
blocking and async flow? Why can't it be both?

This is the world of pain, which this library hopes to alleviate.

## Rationale / Goals
This library aims to allow the developer the freedom to engineer their solution 
properly, irrespective of 3rd party APIs, or the uncanny nature of reality when 
it comes to changing requirements.

The code should be written once, and still support different fashions of 
invocations.

Coding should be simple, and clean. Hopefully, even simpler and cleaner, 
compared to the act of coding async otherwise (somewhat subjective, I know).

A user fn, which expresses biz logic, should be allowed to maintain its purity.
This library aims to allow the seamless weaving of such user fns, with as little 
effort, and as little compromising of performance aspects to that end, as possible.

As per 3rd party APIs, this library aims to provide the means to normalize access, 
such that it is trivial to plug in an API which takes a callback, as replacement 
for one that is blocking, for instance.

Sometimes it is beneficial to exercise one invocation paradigm over the other - 
eg. we may want our tests to run in a blocking fashion (or perhaps while 
debugging); or we may want to experiment with both blocking vs async in production 
under real conditions, comparing apples to apples. On those accounts, this library 
aims to enable the seamless switching between the paradigms, even in run-time.

## Usage

#### Seamless threading of a/sync fns
```clojure
(require '[seamless-async.core :refer :all] '[clojure.core.async :refer [go <!!]])
=> nil

;we're given an async inc fn (api or otherwise, if you will)
(def async-inc #(go (inc %)))
=> #'user/async-inc

;and our biz logic demands we chain multiple calls of this async inc fn
;we can use one of our threading macros, like so:
(<!! (s-> 1
          (async-inc)
          (async-inc)))
=> 3

;but what if our biz logic demands we chain a blocking inc with the async fn?
(<!! (s-> 1
          (async-inc)
          (inc)))
=> 3

;in fact, we can chain this any way we like:
(<!! (s-> 1
          (inc)
          (async-inc)))
=> 3

;..and if there are no async calls in our flow? easy-peasy, blocking flow it is
(s-> 1
     (inc)
     (inc))
=> 3

;and the same goes to similar macros: s->>, scond->, scond->>, etc..
```

#### Seamless mapping of async logic over a collection
```clojure
(<!! (smap async-inc (range 3)))
=> [1 2 3]
(<!! (smapcat #(go (vector %)) (range 3)))
=> (0 1 2)

;so it is very easy to weave seamless logic, and later swap a/sync steps to our hearts content
;what's more, our own biz logic need not be impacted by execution considerations
;(user fns written once, unaware of how/when they're invoked)
;and we can defer the choice of execution paradigm for later. much later
(let [query async-inc
      enrich vector
      store #(go (first %))
      report-stats inc]
  (<!! (s->> 1
             (query)                                        ;async
             (enrich)                                       ;sync
             (smap async-inc)                               ;async through and through
             (filter pos?)                                  ;sync
             (store)                                        ;async
             (report-stats))))                              ;sync
=> 4
```

#### Side-effects treatment
But what of those 3rd party APIs, you ask?<br>
Here's how we may normalize main flavors, so they can be seamlessly woven into our flows as well
```clojure
;a blocking api is straight-forward
;still, we want to be sure we're not wrapping this in a go-block (you don't want to block inside 
; a go-block), use async/thread, or our streamf, which returns exc rather than nil on error
;suppose inc is a blocking api
(let [api-f inc
      sinc (streamf api-f)]
  (<!! (sinc 2)))
=> 3

;another flavor of semi-blocking apis, are those which return a future/promise
;use sderef to magically gain access to our weaving abilities, 
; and avoid forcing blocking semantics upon your flow
(let [api-f #(future %)
      streamed-f (sderef api-f)]
  (<!! (streamed-f :val)))
=> :val

;or the sderef flavor which takes timeout values
(let [api-f (constantly (promise))
      streamed-f (sderef api-f 10 :timed-out)]
  (<!! (streamed-f :val)))
=> :timed-out

;finally there are the callback-taking apis
;scallback returns a fn which pipes the outcome of the callback back into our hands
(let [api-f (fn [val callback-f] (go (callback-f val)))
      streamed-f (scallback api-f)]
  (<!! (streamed-f :val :callback)))
=> :val
;scallback optionally takes callback and err-callback implementations (default: identity)
;when invoking streamed-f, you pass :callback/:err-callback flags respectively (to flag 
;the position of the actual callbacks in the args expected by the api) 
```

#### The seamless wrapper (under the hood)
```clojure
;at the basis of our weaving, stands the seamless wrapper
;it is the real weaver which acts as seamless glue
(let [sync-f (seamless inc)
      async-f (seamless async-inc)]
  (= (sync-f (sync-f 1))
     (<!! (sync-f (async-f 1)))
     (<!! (async-f (sync-f 1)))
     (<!! (async-f (async-f 1)))))
=> true
```

#### If you must
Sometimes you actually want to invoke go-blocks and async apis directly.<br>
Perhaps a 3rd party API invocation lurks at the bottom of a nested calls stack?<br>
This requires each call in the stack to be wrapped inside a go-block. Yuck.<br>
Can't help but lock yourself to a single execution paradigm, one way or the other, eh?<br>
There is a way still, to write your code once, and later decide on the execution flavor.<br>
See below, or checkout 'demo' usage [here](test/seamless_async/t_demo.clj).
```clojure
(def runtime-async-flag (atom nil))
=> #'user/runtime-async-flag
;let foo stand for fn in our stack of nested calls
(def foo #(go-go? @runtime-async-flag %))
=> #'user/foo
(reset! runtime-async-flag false)
=> false
;foo is now sync, no go-blocks used
;if it must still invoke some inner async fn, then async blocking
; semantics (double !!) should be used inside of it per the runtime-async-flag
(foo :val)
=> :val
(reset! runtime-async-flag true)
=> true
;foo is now async, its body is wrapped inside a go-block
;if it must still invoke some inner async fn, then async parking
; semantics (single !) should be used inside of it per the runtime-async-flag
(<!! (foo :val))
=> :val

;for convenience, the global boolean flag *async?* is provided
;the idea is that you use the go-go? macro with the boolean flag where you must,
; and you use the flag inside the fn body as well to adjust blocking/parking semantics.
;it's not pretty, but hey - it's no more ugly than using async code in the first place!
;so, just as ugly, but supporting both blocking/async execution flavors
```
See [here](test/seamless_async/t_demo.clj) for more about how the go-go? macro should be used
## License

Copyright © 2023 [@s-doti](https://github.com/s-doti)

This project is licensed under the terms of [Apache License Version 2.0](http://www.apache.org/licenses/LICENSE-2.0.html).
