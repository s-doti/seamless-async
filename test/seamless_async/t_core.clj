(ns seamless-async.t-core
  (:require [midje.sweet :refer :all]
            [clojure.core.async :as async]
            [seamless-async.core :refer :all]))

(fact "verify *async?* and set-*async?* functionality"
      (let [async? *async?*]
        (set-*async?* true)
        *async?* => true
        (set-*async?* false)
        *async?* => false
        (set-*async?* async?)))

(let [rt-async-flag (atom nil)
      foo #(go-go? @rt-async-flag %)]

  (fact "foo is sync"
        (reset! rt-async-flag false)
        (foo :val) => :val)

  (fact "foo is async"
        (reset! rt-async-flag true)
        (async/<!! (foo :val)) => :val))

(fact "as-seq behaves as advertised"
      (let [ch (async/chan)
            s (as-seq ch)]
        (async/go (async/onto-chan! ch (range 1000)))
        (take 500 s) => (range 500)                         ;read 500 elements
        (async/<!! ch) => truthy                            ;channel is still open (read 1 more)
        (count s) => 999                                    ;fully realize s
        (async/<!! ch) => falsey))                          ;channel is closed

(fact "putc! is as advertised"
      (let [ch (async/chan)]
        (putc! ch :val) => true
        (async/<!! ch) => :val
        (async/<!! ch) => nil
        (putc! ch :val) => false))

(fact "sputc! is as advertised"
      (let [ch (async/chan)]
        (sputc! ch :val) => true
        (async/<!! ch) => :val
        (async/<!! ch) => nil
        (sputc! ch :val) => false)
      (let [ch (async/chan)]
        (sputc! ch nil) => nil))

(fact
  "streamf is as advertised"

  (let [sidentity (streamf identity)]
    (async/<!! (sidentity :val))) => :val

  (let [squery (streamf {:id :val})]
    (async/<!! (squery :id))) => :val

  (let [sexc (streamf #(throw (Exception. (name %))))]
    (.getMessage (async/<!! (sexc :exc)))) => "exc")

(fact
  "sderef is as advertised"

  (let [delayed-future-f (fn [latency]
                           #(future (Thread/sleep latency)
                                    %))]

    (let [f (delayed-future-f 1)
          streamed-f (sderef f)]
      (async/<!! (streamed-f :val))) => :val

    (let [f (delayed-future-f 1)
          streamed-f (sderef f 1000 :timed-out)]
      (async/<!! (streamed-f :val))) => :val

    (let [f (delayed-future-f 1000)
          streamed-f (sderef f 1 :timed-out)]
      (async/<!! (streamed-f :val))) => :timed-out)

  (let [delayed-promise-f (fn [latency]
                            #(let [pr (promise)]
                               (async/go
                                 (async/<! (async/timeout latency))
                                 (deliver pr %))
                               pr))]

    (let [f (delayed-promise-f 1)
          streamed-f (sderef f)]
      (async/<!! (streamed-f :val))) => :val

    (let [f (delayed-promise-f 1)
          streamed-f (sderef f 1000 :timed-out)]
      (async/<!! (streamed-f :val))) => :val

    (let [f (delayed-promise-f 1000)
          streamed-f (sderef f 1 :timed-out)]
      (async/<!! (streamed-f :val))) => :timed-out))

(fact
  "scallback is as advertised"

  (let [f (fn [val cb & [err]] (async/go (cb val)))
        streamed-f (scallback f)]
    (async/<!! (streamed-f :val :callback :err-callback))) => :val

  (let [f (fn [val cb & [err]] (async/go (cb val)))
        streamed-f (scallback f)]
    (async/<!! (streamed-f :val :callback))) => :val

  (let [f (fn [val cb & [err]] (async/go (err :err)))
        streamed-f (scallback f)]
    (async/<!! (streamed-f :val :callback :err-callback))) => :err

  (let [f (fn [val cb & [err]] (async/go (cb val)))
        streamed-f (scallback f :callback name)]
    (async/<!! (streamed-f :val :callback :err-callback))) => "val"

  (let [f (fn [val cb & [err]] (async/go (err :err)))
        streamed-f (scallback f :err-callback name)]
    (async/<!! (streamed-f :val :callback :err-callback))) => "err")

(fact "sresolve behaves as advertised"
      (sresolve nil) => nil
      (sresolve []) => []
      (sresolve [nil]) => [nil]
      (sresolve [1 2 3]) => [1 2 3]
      (sresolve '(1 2 3)) => '(1 2 3)
      (sresolve (range 100000)) => (range 100000)
      (async/<!! (sresolve [(async/go nil)])) => [nil]
      (async/<!! (sresolve [(async/go [1 2 3])])) => [[1 2 3]]
      (async/<!! (sresolve [1 (async/go 2) 3])) => [1 2 3]
      (async/<!! (sresolve (map #(async/go %) (range 100000)))) => (range 100000)
      (async/<!! (sresolve (map #(if (< (rand) 0.5) (async/go %) %) (range 100000)))) => (range 100000)
      (async/<!! (sresolve (cons (async/go 0) (range 100000)))) => (cons 0 (range 100000)))

(fact
  "seamless behaves as advertised"

  (let [f (seamless name)]

    (f :val) => "val"                                       ;functions identically as its source

    (-> (async/go :val)                                     ;seamlessly woven into async context
        (f)
        (async/<!!)) => "val"

    (-> (Exception. "exc")                                  ;returns exc given async context
        (async/go)
        (f)
        (async/<!!)
        (.getMessage)) => "exc"

    (f (Exception. "exc")) => (throws Exception))           ;throws exc given sync context

  ;more proof that sync/async logic can be woven in any direction
  (let [sync-f (seamless inc)
        async-f (seamless #(async/go (inc %)))]

    (-> 1
        (sync-f)
        (sync-f)) => 3

    (async/<!! (-> 1
                   (sync-f)
                   (async-f))) => 3

    (async/<!! (-> 1
                   (async-f)
                   (sync-f))) => 3

    (async/<!! (-> 1
                   (async-f)
                   (async-f))) => 3))

(fact "s-> behaves as advertised"
      (let [query #(async/go %)
            enrich #(async/go [% :enriched])
            store #(async/go (count %))]
        (async/<!! (s-> :val
                        (query)
                        (name)
                        (enrich)
                        (conj :+one)
                        (store)
                        (inc))) => 4))

(fact "s->> behaves as advertised"
      (let [query #(async/go %)
            enrich #(async/go [% :enriched])
            store #(async/go (count %))]
        (async/<!! (s->> :val
                         (query)
                         (name)
                         (enrich)
                         (map keyword?)
                         (filter true?)
                         (store)
                         (inc))) => 2))

(fact "scond-> behaves as advertised"
      (let [query #(async/go %)
            enrich #(async/go [% :enriched])
            store #(async/go (count %))]
        (async/<!! (scond-> :val
                            false (query)
                            false (name)
                            :enrich (enrich)
                            :conj (conj :+one)
                            :store (store)
                            false (inc))) => 3))

(fact "scond->> behaves as advertised"
      (let [query #(async/go %)
            enrich #(async/go [% :enriched])
            store #(async/go (count %))]
        (async/<!! (scond->> :val
                             false (query)
                             false (name)
                             :enrich (enrich)
                             :map (map keyword?)
                             :filter (filter true?)
                             :store (store)
                             :inc (inc))) => 3))

(fact "smap behaves as advertised"
      (smap inc nil) => ()
      (smap inc []) => ()
      (smap inc (range 10)) => (range 1 11)
      (smap #(async/go (inc %)) nil) => ()
      (smap #(async/go (inc %)) []) => ()
      (async/<!! (smap #(async/go (inc %)) (range 10))) => (range 1 11))

(fact "smapcat behaves as advertised"
      (smapcat (comp vector inc) nil) => ()
      (smapcat (comp vector inc) []) => ()
      (smapcat (comp vector inc) (range 10)) => (range 1 11)
      (smapcat #(async/go [(inc %)]) nil) => ()
      (smapcat #(async/go [(inc %)]) []) => ()
      (async/<!! (smapcat #(async/go [(inc %)]) (range 10))) => (range 1 11))
