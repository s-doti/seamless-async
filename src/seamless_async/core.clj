(ns seamless-async.core
  (:require [clojure.core.async :as async])
  (:import (clojure.core.async.impl.channels ManyToManyChannel)))

(def ^:dynamic
  ^{:doc "*async?* is a system global boolean flag, which may be used as
  hint to various mechanisms, that are capable of both a/sync operations,
  for impacting their execution mode. This flag reflects the overall
  general expectation of the system, as to whether its execution
  should be a/sync. This flag is merely an optional approach to
  global coordination, and may be ignored. Default: false."}
  *async?* false)

(defn set-*async?*
  "Sets the boolean state of the *async?* system global flag."
  [enabled?]
  (alter-var-root (var *async?*) (constantly enabled?)))

(def
  ^{:arglists '([x])
    :doc      "Return true if x is a ManyToManyChannel."
    :static   true}
  chan? (fn ^:static chan? [x] (instance? ManyToManyChannel x)))

(defmacro go-go?
  "Conditionally wraps code in a go block."
  [async? & body]
  `(if ~async?
     (async/go ~@body)
     ~@body))

(defn as-seq [channel]
  "Return a lazy seq of the channel values"
  (lazy-seq
    (when-let [head (async/<!! channel)]
      (cons head (as-seq channel)))))

(defn putc!
  "Put & close. Asynchronously puts a val into channel, closing
  the channel when complete (nil values are not allowed).
  Returns true unless the channel is already closed."
  [channel val]
  (async/put! channel
              val
              (fn [& args]
                (async/close! channel))))

(defn sputc!
  "Safe put & close. Asynchronously puts a val into channel, closing
  the channel when complete (closes the channel immediately if val is nil).
  Returns true unless the channel is already closed, or nil if val is nil."
  [channel val]
  (if (some? val)
    (putc! channel val)
    (async/close! channel)))

(defn streamf
  "Takes a blocking function f, eg. a db query, and returns a
  non-blocking, streaming fn.
  The returned fn has the exact same signature and logic as the original,
  but its logic will be carried out by the cached threads pool
  (see: clojure.core.async/thread-macro-executor),
  and it returns a channel which will receive the result of
  the execution when completed (throwables included), then close."
  [f]
  (fn [& args]
    (async/thread (try (apply f args)
                       (catch Throwable t t)))))

(defn sderef
  "Takes a function f which returns a derefable ref, and optionally
  timeout-ms and timeout-val, and returns a non-blocking, streaming fn,
  which also accomplishes the deref.
  The returned fn has the exact same signature and logic as the original,
  but its outcome will get deref'd by the cached threads pool
  (see: clojure.core.async/thread-macro-executor),
  and it returns a channel which will receive the result once
  available, then close."
  ([f]
   (fn [& args]
     (async/thread (deref (apply f args)))))
  ([f timeout-ms timeout-val]
   (fn [& args]
     (async/thread (deref (apply f args) timeout-ms timeout-val)))))

(defn scallback
  "Takes a function f which expects callbacks, and optionally suitable
  callbacks implementations, and returns a non-blocking, streaming fn,
  that will be piping the result of the callbacks onto a channel.
  The returned fn has the same signature and logic as the original,
  but in place of a callback/err-callback fns, it takes simple
  flags :callback/:err-callback respectively, and returns a channel
  which will receive the result of the callbacks, then close.
  If no callbacks are provided, the identity fn will be used."
  [f & {:keys [callback err-callback]
        :or   {callback identity err-callback identity}}]
  (fn [& args]
    (let [out (async/promise-chan)
          cb #(sputc! out (callback %))
          err #(sputc! out (err-callback %))]
      (->> args
           (replace {:callback cb :err-callback err})
           (apply f))
      out)))

(defn sresolve
  "Asynchronously replaces each channel encountered in coll with
  a single value taken from it.
  Returns a channel containing the outcome collection.
  If coll contains no channels, it is returned as is."
  [coll]
  (if (some chan? coll)
    (let [out (async/chan)]
      (letfn [(resolve-next
                [[elem & unresolved] resolved]
                (if (chan? elem)
                  (async/take! elem
                               #(resolve-next (conj unresolved %)
                                              resolved))
                  (let [resolved (conj resolved elem)]
                    (if (not-empty unresolved)
                      (recur unresolved resolved)
                      (putc! out resolved)))))]
        (resolve-next coll []))
      out)
    coll))

(defn seamless
  "Takes a function f and returns a fn that can be seamlessly woven
  into an asynchronous flow.
  The returned fn has the exact same signature and logic as the original,
  but if any of its arguments originate in an async manner, they will
  be first asynchronously parked until their values are made available,
  and a channel will be returned immediately to the calling thread. The
  channel will receive the result (which may be a Throwable) once
  available, then close.
  However, if all values per input args are immediately available, the
  fn behaves as the original f.
  If an input arg turns out to be a Throwable, it is returned as the result
  under asynchronous conditions, else it will be thrown."
  [f]
  (fn [& args]
    (if (some chan? args)
      (let [out (async/promise-chan)]
        (letfn [(safe-apply [f args]
                  (if-let [t (first (filter (partial instance? Throwable) args))]
                    t
                    (try (apply f args)
                         (catch Throwable t
                           t))))
                (weave [f args]
                  (let [val (safe-apply f args)]
                    (if (chan? val)
                      (async/take! val
                                   #(weave identity [%]))
                      (sputc! out val))))]
          (async/take! (sresolve args)
                       (partial weave f)))
        out)
      (if-let [t (first (filter (partial instance? Throwable) args))]
        (throw t)
        (apply f args)))))

(defmacro s->
  "Seamless -> threading, which applies seamless async glue as
  needed, in case any of the threaded expressions is asynchronous."
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `((seamless ~(first form)) ~x ~@(next form)) (meta form))
                       (list (seamless form) x))]
        (recur threaded (next forms)))
      x)))

(defmacro s->>
  "Seamless ->> threading, which applies seamless async glue as
  needed, in case any of the threaded expressions is asynchronous."
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `((seamless ~(first form)) ~@(next form) ~x) (meta form))
                       (list (seamless form) x))]
        (recur threaded (next forms)))
      x)))

(defmacro scond->
  "Seamless cond-> threading, which applies seamless async glue as
  needed, in case any of the threaded expressions is asynchronous."
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        steps (map (fn [[test step]] `(if ~test (s-> ~g ~step) ~g))
                   (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro scond->>
  "Seamless cond->> threading, which applies seamless async glue as
  needed, in case any of the threaded expressions is asynchronous."
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        steps (map (fn [[test step]] `(if ~test (s->> ~g ~step) ~g))
                   (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defn smap
  "Seamless map, which behaves as map, but in case f is an
  asynchronous op, a channel will be returned, containing the
  outcome collection, once available."
  [& args]
  (sresolve (apply map args)))

(defn smapcat
  "Seamless mapcat, which behaves as mapcat, but in case f is an
  asynchronous op, a channel will be returned, containing the
  outcome collection, once available."
  [& args]
  (s->> (apply smap args)
        (apply concat)))
