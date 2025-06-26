(ns wthjh.react
  (:require [shadow.build.compiler :as comp]))

(defn string-part? [part]
  (or (string? part)
      (number? part)
      (boolean? part)))

(defn const? [env thing]
  (or (string? thing)
      (number? thing)
      (boolean? thing)
      (keyword? thing)
      (= thing 'nil)
      (and (vector? thing) (every? #(const? env %) thing))
      (and (map? thing)
           (reduce-kv
             (fn [r k v]
               (if-not (and (const? env k) (const? env v))
                 (reduced false)
                 r))
             true
             thing))

      ;; treat foo/bar symbols as constants as they are assumed to be def'd vars
      ;; which shouldn't ever change during a lifecycle update
      ;; treat non-local symbols as constants as well since they shouldn't change either

      ;; (def some-class "foo bar")
      ;; (<< [:div {:class some-class} ...])
      ;; (<< [:div {:class styles/card} ...])
      ;; don't ever need to update :class

      (qualified-symbol? thing)
      (and (simple-symbol? thing)
           ;; FIXME: this should maybe resolve instead?
           (not (get-in env [:macro-env :locals thing])))))

(defn maybe-merge-class [attrs css]
  (assoc attrs
    :class (if-not (contains? attrs :class)
             css
             [css (:class attrs)])))

(defn last-of-vec [v]
  (if-not (seq v)
    nil
    (nth v (dec (count v)))))

(defn parse-el [el]
  (reduce-kv
    (fn [{:keys [children] :as res} idx v]
      (cond
        (zero? idx)
        res

        (and (= 1 idx) (map? v))
        (assoc res :attrs v)

        (and (= 1 idx) (seq? v) (= 'css (first v)))
        (assoc res :css v :attrs {:class v})

        (and (= 2 idx) (map? v) (:css res))
        (assoc res :attrs (maybe-merge-class v (:css res)))

        (and (string-part? v) (string-part? (last-of-vec children)))
        (update-in res [:children (dec (count children))] str v)

        :else
        (update res :children conj v)))

    {:attrs {}
     :children []}
    el))

(comment
  (parse-el '[:div (css :p-4) {:foo bar} "hello" "world"])
  (parse-el '[:div (css :p-4) {:foo bar} "hello" foo "world"])
  (parse-el '[:div (css :p-4) {:foo bar :class "yo"} "hello" foo "world"])
  (parse-el '[:div {:foo bar} "hello" 1 2 3 foo "world"])
  (parse-el '[:div {:foo bar :class (css :x)} "hello" foo "world"])
  (parse-el '[:div (css :p-4) "hello" "world"]))

(defn pop-code [{:keys [code] :as env} parent]
  (assoc env :code (conj parent code)))

(defn reduce-> [init rfn coll]
  (reduce rfn init coll))

(declare analyze-node)

(defn analyze-hiccup [env node]
  (if (const? env node)
    ;; entirely static hiccup element, extract into delay so its only created once. effectively memoized.
    (let [name (gensym "static_el_")
          extract {:name name :code node}]
      (-> env
          (update :static conj extract)
          (update :code conj `(deref ~name))
          ))

    (let [el (parse-el node)
          tag (nth node 0)

          code (:code env)]

      (-> env
          (assoc :code [tag (:attrs el)])
          (reduce-> analyze-node (:children el))
          (pop-code code)))

    ))

(defn analyze-node [env node]
  (cond
    (and (vector? node) (keyword? (first node)))
    (analyze-hiccup env node)

    :else
    (update env :code conj node)))

(defn make-fragment [macro-env body]
  (let [env {:macro-env macro-env
             :static []
             :code []}
        env (reduce analyze-node env body)]

    (when comp/*analyze-top*
      (doseq [{:keys [name code]} (:static env)]
        (comp/*analyze-top* `(def ~name (delay (as-element ~code))))))

    `(as-element ~(first (:code env)))
    ))


(defmacro << [& body]
  (make-fragment &env body))


(comment
  (require '[clojure.pprint :refer (pprint)])

  (pprint (make-fragment {:locals {'id {}}} `([:div [:h1 "hello"] [:div ~'id]]))))