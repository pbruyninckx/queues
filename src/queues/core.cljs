(ns queues.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.dom :as rd]
              ))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:queue-length 0
                          :simulating false}))

(defn increase-queue [f delay]
  (do (swap! app-state update :queue-length inc)
      (js/setTimeout f delay)))

(defn wait-time-generator [min-time-per-client max-time-per-client]
  (fn []
    (+ min-time-per-client
       (rand-int (-  max-time-per-client min-time-per-client -1)))))

(defn generate-leave-times [arrival-times wait-time-fn]
  (reduce (fn [ret arrival-time]
            (conj
              ret
              (+ (wait-time-fn)
                 (cond (empty? ret)
                       arrival-time
                       (< arrival-time (last ret))
                       (last ret)
                       :else
                       arrival-time))))
          []
          arrival-times))

(defn compute-queue-length [arrival-times leave-times]
  "Returns a vector containing the size of the queue when the n-th customer arrives.
  A customer being served is considered a part of the queue.
  The arriving customer is not considered a part of the queue."
  (loop [ret []
         qoa 0
         [arrival & rest-arrivals :as arrival-times] arrival-times
         [leave & rest-leaves :as leave-times] leave-times]
    (cond (and (nil? arrival) (nil? leave))
          ret
          (or (nil? arrival) (< leave arrival))
          (recur (conj ret (dec qoa)) (dec qoa) arrival-times rest-leaves)
          :else ; new arrival, not added to return vector
          (recur ret (inc qoa) rest-arrivals leave-times))))

(defn get-stats [v]
  {:max    (reduce max v)
   :avg    (/ (reduce + v) (count v))
   :median (->> v
                sort
                (drop (js/Math.floor (/ (count v) 2)))
                first)})

(defn compute-stats [arrival-times leave-times]
  "Computes the following stats, and returns them as a struct:

  * wait-times
    * max avg median
  * qoa: Queue size on arrival
    * max avg median"
  (let [wait-times (mapv - leave-times arrival-times)
        qoa (compute-queue-length arrival-times leave-times)]
    {:wait-times (get-stats wait-times)
     :qoa        (get-stats qoa)}))

(defn run-simulation [{:keys [clients-per-hour min-time-per-client max-time-per-client]}]
  (js/console.log "Running simulation")
  (do (swap! app-state assoc :simulating true)
      (swap! app-state assoc :queue-length 0)
      (let [arrival-times (sort (repeatedly clients-per-hour #(rand-int 60000)))
            leave-times (generate-leave-times arrival-times (wait-time-generator min-time-per-client max-time-per-client))]
        (doall
          (map #(js/setTimeout (fn [] (swap! app-state update :queue-length inc)) %) arrival-times))
        (doall
          (map #(js/setTimeout (fn [] (swap! app-state update :queue-length dec)) %) leave-times))
        (js/setTimeout #(swap! app-state assoc :simulating false) (last leave-times))
        (swap! app-state assoc :stats (compute-stats arrival-times leave-times)))))

(defn fmap [f d]
  (into {}
        (map (fn [[k v]] [k (f v)])
             d)))

(defn inputs []
  (let [fields (reagent/atom {})]
    (fn []
      [:form
       [:div.field
        [:label.label "Clients per minute"]
        [:input {:type      :number
                 :name      :clients-per-hour
                 :on-change #(swap! fields
                                    assoc :clients-per-hour (-> % .-target .-value))
                 :value     (:clients-per-hour @fields)}]]
       [:div.field
        [:label.label "Min time per client (milliseconds)"]
        [:input {:type      :number
                 :name      :min-time-per-client
                 :on-change #(swap! fields
                                    assoc :min-time-per-client (-> % .-target .-value))
                 :value     (:min-time-per-client @fields)}]]
       [:div.field
        [:label.label "Max time per client (milliseconds)"]
        [:input {:type      :number
                 :name      :max-time-per-client
                 :on-change #(swap! fields
                                    assoc :max-time-per-client (-> % .-target .-value))
                 :value     (:max-time-per-client @fields)}]]
       [:input.button.is-primary
        {:type :button
         :on-click (fn [_] (run-simulation (fmap #(js/parseInt % 10) @fields)))
         :value "Simulate"
         :disabled (:simulating @app-state)}]
       ])))


(def person "\uD83E\uDDCD")
(def idle "\uD83D\uDECE")

(defn display []
  [:div.table-container
   [:table.table.is-bordered
    [:thead
     [:tr [:th {:width "50px"} "served"] [:th "queue"]]]
    [:tbody
     [:tr
      [:td (if (pos? (:queue-length @app-state)) person idle)]
      [:td (apply str (repeat (dec (:queue-length @app-state)) person))]]]
    ]]
  )

(defn stats []
  (let [computed-stats (:stats @app-state)]
    [:div.table-container
     [:table
      [:thead [:tr [:th "Statistics"] [:th "Value"]]]
      [:tbody
       [:tr [:td "Average wait time"] [:td (get-in computed-stats [:wait-times :avg])]]
       [:tr [:td "Median wait time"] [:td (get-in computed-stats [:wait-times :median])]]
       [:tr [:td "Max wait time"] [:td (get-in computed-stats [:wait-times :max])]]
       [:tr [:td "Average queue on arrival"] [:td (get-in computed-stats [:qoa :avg])]]
       [:tr [:td "Median queue on arrival"] [:td (get-in computed-stats [:qoa :median])]]
       [:tr [:td "Max queue on arrival"] [:td (get-in computed-stats [:qoa :max])]]
       ]]]))

(defn my-page []
  [:section.section>div.container
   [:h1.title "Maison Clément Queuing system"]
   [:p.subtitle "Let's see how this might work"]
   [:div.content
    [:div.section
     [:h2.title "Configuration Parameters"]
     [inputs]]
    [:div.section
     [:h2.title "Simulation"]
     [display]]
    [:div.section
     [:h2.title "Statistics"]
     [stats]]]])

(rd/render [my-page]
           (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
