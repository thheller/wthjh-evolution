(ns wthjh.react
  (:require-macros [wthjh.react])
  (:require [reagent.core :as r]))

(defn as-element [node]
  (r/as-element node))
