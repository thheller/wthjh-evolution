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

(defn pop-current [{:keys [current] :as env} parent]
  (assoc env :current (update parent :children conj current)))

(defn reduce-> [init rfn coll]
  (reduce rfn init coll))

(defn reduce-kv-> [init rfn coll]
  (reduce-kv rfn init coll))

(defn analyze-attr [env k v]
  (if (and (const? env k) (const? env v))
    ;; :foo "bar"
    (update-in env [:current :attrs] assoc k v)

    (if (const? env k)
      ;; dynamic v, :foo code
      (let [name (gensym "attr_v_")]
        (-> env
            (update :code-order conj name)
            (update :code assoc name v)
            (update-in [:current :attrs] assoc k name)))

      (if (const? env v)
        ;; dynamic k, foo "value
        (let [name (gensym "attr_k_")]
          (-> env
              (update :code-order conj name)
              (update :code assoc name v)
              (update-in [:current :attrs] assoc name v)))

        ;; dynamic k+v, foo bar
        (let [n1 (gensym "attr_k_")
              n2 (gensym "attr_v_")]
          (-> env
              (update :code-order conj n1 n2)
              (update :code assoc n1 k n2 v)
              (update-in [:current :attrs] assoc n1 n2)))))))

(declare analyze-node)

(defn analyze-hiccup [env node]
  (if (const? env node)
    ;; entirely static hiccup element, extract into delay so its only created once. effectively memoized.
    (let [name (gensym "static_el_")]
      (-> env
          (update :static-order conj name)
          (update :static assoc name node)
          (update-in [:current :children] conj {:type :const
                                                :ref name})
          ))

    (let [el (parse-el node)
          tag (nth node 0)

          current (:current env)]

      (-> env
          (assoc :current {:type :el
                           :tag tag
                           :attrs {}
                           :children []})
          (reduce-kv-> analyze-attr (:attrs el))
          (reduce-> analyze-node (:children el))
          (pop-current current)))

    ))

(defn analyze-node [env node]
  (cond
    (and (vector? node) (keyword? (first node)))
    (analyze-hiccup env node)

    (string-part? node)
    (update-in env [:current :children] conj {:type :string :val node})

    :else
    (let [name (gensym "code_")]
      (-> env
          (update :code-order conj name)
          (update :code assoc name node)
          (update-in [:current :children] conj {:type :code :ref name})))))

(defn analyze [macro-env body]
  (let [env {:macro-env macro-env
             :static-order []
             :code-order []
             :static {}
             :current {:type :root
                       :children []}}]
    (reduce analyze-node env body)))

(defn build-node [{:keys [type children] :as node}]
  (case type
    :el
    (into [(:tag node) (:attrs node)] (map build-node) children)

    :string
    (:val node)

    :code
    (:ref node)

    :const
    `(deref ~(:ref node))

    (throw (ex-info "unknown node type" node))
    ))

(defn build-root [env]
  (let [children (get-in env [:current :children])]

    (if (= 1 (count children))
      `(as-element ~(build-node (first children)))
      `(as-element [:<> ~@(map build-node children)])
      )))

(defn make-fragment [macro-env body]
  (let [env (analyze macro-env body)]

    (if-not comp/*analyze-top*
      ::sorry-only-works-in-shadow-cljs-for-now

      (do (doseq [name (:static-order env)
                  :let [code (get-in env [:static name])]]

            (comp/*analyze-top* `(def ~name (delay (as-element ~code)))))

          (let [top-fn (gensym "memo_")
                {:keys [code-order code]} env
                root-children (get-in env [:current :children])]

            (if (and (= 1 (count root-children))
                     (= :const (:type (first root-children))))

              ;; shortcut for (<< [:total-static-hiccup])
              `(deref ~(:ref (first root-children)))
              (do (comp/*analyze-top*
                    `(def ~top-fn
                       (make-memo-fn
                         (fn [~code-order]
                           ~(build-root env)
                           ))))

                  `(use-memo-fn ~top-fn ~(mapv #(get code %) code-order)))))))
    ))

(defmacro << [& body]
  (make-fragment &env body))


(require '[clojure.pprint :refer (pprint)])

(let [test (analyze {:locals {'id {}}} `([:h1 "yo"] [:div.id1 [:h1 "hello"] [:div.hello.world {:attr ~'id :hello "world"} 1 "yo" ~'id "hi"]]))]
  (pprint (build-root test)))
