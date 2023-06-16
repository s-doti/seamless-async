(defproject com.github.s-doti/seamless-async "1.0.1"
  :description "A Clojure library for making async.. seamless"
  :url "https://github.com/s-doti/seamless-async"
  :license {:name "Apache License, Version 2.0"
            :url  "http://www.apache.org/licenses/LICENSE-2.0.html"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.async "1.6.673"]
                 [midje/midje "1.10.9"]]
  :profiles {:dev {:dependencies [[http-kit "2.6.0"]]}}
  :repl-options {:init-ns seamless-async.core})
