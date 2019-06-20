(defproject org.clojars.vladimirmarkovic86/framework-lib "0.2.42"
  :description "Framework library"
  :url "https://github.com/VladimirMarkovic86/framework-lib"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojurescript "1.10.339"]
                 [org.clojars.vladimirmarkovic86/ajax-lib "0.1.11"]
                 [org.clojars.vladimirmarkovic86/htmlcss-lib "0.1.6"]
                 [org.clojars.vladimirmarkovic86/js-lib "0.1.16"]
                 [org.clojars.vladimirmarkovic86/utils-lib "0.4.9"]
                 [org.clojars.vladimirmarkovic86/language-lib "0.2.30"]
                 [org.clojars.vladimirmarkovic86/common-middle "0.2.8"]
                 [org.clojars.vladimirmarkovic86/validator-lib "0.1.31"]
                 ]

  :min-lein-version "2.0.0"
  
  :source-paths ["src/cljs"]

  :plugins [[lein-cljsbuild  "1.1.7"]
            [lein-doo "0.1.11"]
            ]

  :cljsbuild
    {:builds
      {:test
        {:source-paths ["src/cljs" "test/cljs"]
         :compiler     {:main framework-lib.test-runner
                        :optimizations :whitespace
                        :output-dir "resources/public/assets/js/out/test"
                        :output-to "resources/public/assets/js/test.js"}}
       }}
 )

