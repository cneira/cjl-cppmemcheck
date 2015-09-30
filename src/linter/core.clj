(ns linter.core
  (:gen-class)
  (:import [java.util Calendar ArrayList TimeZone List  ] )
  (:import [java.lang  Math] ))


(def list_class (new ArrayList  ) )

(Math/abs -3.4)

(Calendar/FEBRUARY)

(def list_class_dot (ArrayList.  )  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(proxy [ArrayList] []
  (sayhi [] (str "this is stupid example but pedagogic example"))
  )


(defn vp []
  (proxy [ArrayList] []
    (add [e] (do (println "Don't do this is just for showing off proxy") true  )  )))

(defn extend-class2 [name]
  (proxy [Object] []
    (toString []  (str "hello"))))

(defn proxy-example
  []
  (proxy [Object] []
    (toString [] "Overriding toString method")))

(.toString (proxy-example))

(.add (vp) "A")