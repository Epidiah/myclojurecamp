(ns mycc.announcement
  (:require
   [clojure.walk :as walk]
   [clojure.string :as str]
   [mycc.p2p.meetups :as meetups])
  (:import
   (java.time LocalTime ZoneId ZonedDateTime)))

(defn world-time-buddy-link [start-inst end-inst]
  (let [start-zdt (-> start-inst
                      .toInstant
                      (.atZone (ZoneId/of "America/Toronto")))
        end-zdt (-> end-inst
                    .toInstant
                    (.atZone (ZoneId/of "America/Toronto")))]
    (str "https://www.worldtimebuddy.com/?qm=1&lid=6167865,5809844,5856195,1850147,2950159,100&h=6167865"
         "&date=" (-> start-zdt .toLocalDate str)
         "&sln=" (.getHour start-zdt) "-" (.getHour end-zdt)
         "&hf=1")))

#_(world-time-buddy-link (java.util.Date.) (java.util.Date.))

(defn announcement [now-inst]
  [:group {:separator "\n\n"}
   "Hi everyone,"
   "Here's what's going on at Clojure Camp this month:"
   (into [:group {:separator "\n\n"}]
         (for [event (meetups/all)
               :let [all-insts (meetups/next-meetup-insts now-inst event)
                     start-inst (first all-insts)
                     end-inst (java.util.Date.
                               (+ (.getTime (last all-insts))
                                  (* 60 60 1000)))]]
           [:group {:separator "\n"}
            [:bold (:meetup/title event)]
            [:time-and-duration {:start start-inst :end end-inst}]]))
   "Clojure Camp is an online community for learning Clojure."
   [:group "To get started, visit:" [:link "https://clojure.camp"]]])

(defn vec-starts-with? [kw node]
  (and (vector? node) (= kw (first node))))

(defmulti render (fn [kw attrs & children] kw))

(defmethod render :group
  [kw {:keys [separator]} & children]
  (str/join (or separator " ") children))

(defmethod render :bold
  [kw attrs & children]
  (str "**" (str/join " " children) "**"))

(defmethod render :time-and-duration
  [kw {:keys [mode start end]} & children]
  (let [duration (/ (- (.getTime end) (.getTime start)) 60 60 1000)
        duration-string (str "(" duration " hour" (when (< 1 duration) "s") ")")]
    (case mode
      :discord (str "<t:" (/ (.getTime start) 1000)  ":F> " duration-string)
      (render :link
              {:url (world-time-buddy-link start
                                           end)}
              start
              duration-string))))

(defmethod render :link
  [kw {:keys [url]} & children]
  (str "[" (str/join " " children) "]"
       "("
       (cond
         url
         url

         (and (= 1 (count children))
              (str/starts-with? (first children) "http"))
         (first children))
       ")"))

(defn post-process [mode dsl]
  (walk/postwalk
   (fn [node]
     (if (and (vector? node) (not (map-entry? node)))
       (if (map? (second node))
         (apply render
                (first node)
                (assoc (second node) :mode mode)
                (drop 2 node))
         (apply render (first node) {:mode mode} (rest node)))
       node))
   dsl))

#_(spit "out.txt" (post-process nil (announcement (java.util.Date.))))
