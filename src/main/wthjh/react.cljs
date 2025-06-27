(ns wthjh.react
  (:require-macros [wthjh.react])
  (:require
    [reagent.core :as reagent]
    ["react" :as r]))

(defn as-element [node]
  (reagent/as-element node))

(defn make-memo-fn [render]
  (fn [^js js-props]
    (let [args (.-args js-props)
          ref-args (r/useRef)
          ref-result (r/useRef)]

      ;; not using useMemo because we want = not the comparison stuff react does
      ;; this still kinda needs to account for other react elements created by other fragments
      ;; but someone else can sort that out, if anyone it interested at all
      (if (= args (.-current ref-args))
        (.-current ref-result)
        (let [result (reagent/as-element (render args))]
          (set! ref-args -current args)
          (set! ref-result -current result)
          result)))))

(defn use-memo-fn [frag-fn frag-args]
  (r/createElement frag-fn #js {:args frag-args}))