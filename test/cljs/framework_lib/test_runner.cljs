(ns framework-lib.test-runner
  (:require [framework-lib.core-test-cljs]
            [framework-lib.side-bar-menu-test]
            [framework-lib.tree-test]
            [doo.runner :refer-macros [doo-tests doo-all-tests]]))

(enable-console-print!)

(doo-tests
  'framework-lib.core-test-cljs
  'framework-lib.side-bar-menu-test
  'framework-lib.tree-test)

