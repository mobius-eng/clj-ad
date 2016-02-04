(defproject clj-auto-diff "0.1.4"
  :description "Automatic differentiation library (fork)"
  :url "http://github.com/mobius-eng/clj-auto-diff"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[net.mikera/core.matrix "0.49.0"]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "0.2.2"]]
  :profiles {:dev {:dependencies [[org.clojure/tools.namespace "0.2.11"]
                                  [midje "1.8.3"]]
                   :source-paths ["dev"]}})
