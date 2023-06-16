(ns seamless-async.t-demo
  (:require [midje.sweet :refer :all]
            [clojure.core.async :as async]
            [seamless-async.core :refer :all]
            [org.httpkit.client :as http]))

;
; auxiliary fns.
;

(defn async-http
  "Async side-effect,
   returns an async channel through which the outcome is served"
  [url]
  (let [http-fn (scallback http/get)]
    (http-fn url {} :callback)))

(defn sync-http
  "Blocking side-effect,
   returns the outcome as is"
  [url]
  @(http/get url {}))

(defn make-http-call
  "Toggle-able side-effect,
   support both a/sync flows"
  [url]
  (if *async?*
    (async-http url)
    (sync-http url)))

;
; The following shows how a biz-logic flow may be written
; once, and allow the dynamic toggling between
; blocking/async execution, even if it invokes a nested
; flow, with an execution style which we have no control
; over.
;

(defn biz-flow
  "Flow's execution style is determined solely by
   the *async?* flag, regardless of the nested-flow's style"
  [nested-flow]
  (go-go? *async?*
          (let [outcome (nested-flow "http://www.google.com")
                async-outcome? (chan? outcome)
                park? (and async-outcome? *async?*)
                block? (and async-outcome? (not *async?*))
                response (cond-> outcome
                                 park? (async/<!)
                                 block? (async/<!!))]
            (= 200 (:status response)))))

;test the toggle-ability of the biz-flow per all alternatives
(facts
  "Biz-flow is written once,
   and then may easily be toggled between a/sync,
   regardless of nested flow execution style"
  (set-*async?* false)
  (biz-flow async-http) => truthy
  (biz-flow sync-http) => truthy
  (set-*async?* true)
  (async/<!! (biz-flow async-http)) => truthy
  (async/<!! (biz-flow sync-http)) => truthy)

;
; The following shows how a biz-logic flow may be written
; once, and allow the dynamic toggling between
; blocking/async execution, along a nested flow which is
; invoked by it.
;

(defn biz-flow
  "Flow's execution style is determined by
   the nested-flow's style"
  [nested-flow]
  (s->> "http://www.google.com"
        (nested-flow)
        (:status)
        (= 200)))

;test the toggle-ability of the biz-flow
(facts
  "Biz-flow is written once,
   and then may easily be toggled between a/sync,
   along with nested flow execution style"
  (set-*async?* false)
  (biz-flow make-http-call) => truthy
  (set-*async?* true)
  (async/<!! (biz-flow make-http-call)) => truthy)
