(ns Bewilde.core)

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))

(defn make-object [obj]
  {:tag obj
   :attrs {}
   :content []})

(defmacro defobject [obj & body]
  `(let [o# (make-object ~(keyword obj))]
     (def ~obj o#)))

(defobject point)

(defn new-point [[x y]]
  (assoc point :attrs {:id (gensym "point") :x x :y y}))

(defobject line)

(defn new-line [p1 p2]   
  (let [ps (map new-point [p1 p2])]
    (assoc line :attrs {:id (gensym "line")} :content (vec ps))))

(defobject radius)

(defn new-radius [r]
  (assoc radius :content [r]))

(defobject theta)

(defn new-theta [t]
  (assoc theta :content [t]))

(defobject circle)

(defn new-circle [p r t1 t2]
  (let [point (new-point p)
        rad (new-radius r)
        theta1 (new-theta t1)
        theta2 (new-theta t2)]
    (assoc circle
      :attrs {:id (gensym "circle")}
      :content [point rad theta1 theta2])))

(defobject polyline)

(defn new-polyline [p1 p2 & p3s]
  (let [p2s (conj p3s p2)
        p1s (conj p2s p1)
        ps  (map new-point p1s)]
    (assoc polyline
      :attrs {:id (gensym "polyline")}
      :content (vec ps))))

(defobject bezier)

(defn new-bezier [p1 p2 p3 p4]
  (let [ps (map new-point [p1 p2 p3 p4])]
    (assoc bezier
      :attrs {:id (gensym "bezier")}
      :content (vec ps))))

(defobject group)

(defn group-obj [o1 o2 & o3]
  (let [o2s (conj o3 o2)
        os (conj o2s o1)]
    (assoc group
      :attrs {:id (gensym "group")}
      :content (vec os))))