(defproject my-project-euler-lab "1.0.0-SNAPSHOT"
  :description "Solve euler problems"
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [clj-time "0.3.1"]
                 [clj-stacktrace      "0.2.4"]]
  :dev-dependencies [[midje "1.3.1"]
                     [com.intelie/lazytest "1.0.0-SNAPSHOT" :exclusions [swank-clojure]]
                     [lein-marginalia "0.7.0-SNAPSHOT"]] )
