(ns Bewilde.core)

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))

(defn make-object [obj]
  {:tag obj
   :id nil
   :attrs nil
   :content nil})

(defmacro defobject [obj & body]
  `(let [o# (-> (make-object ~(keyword obj))
                 (assoc ~@body :id (gensym "obj")))]
     (def ~obj o#)))

(defobject point)

(defn new-point [[x y]]
  (assoc point :attrs {:x x :y y}))

(defobject line)

(defn new-line [p1 p2]
  (assoc line :content [{:point1 p1} {:point2 p2}]))

(defobject circle)

(defn new-circle [p r]
  (assoc circle :attrs {:radius r} :content [{:point p}]))

(defobject polyline)

(defn new-polyline [x] x)

(defobject bezier)