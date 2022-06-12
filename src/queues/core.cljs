(ns queues.core
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.dom :as rd]
              ))

(enable-console-print!)

(println "This text is printed from src/queues/core.cljs. It's printed on reload.")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello Pieter!"
                          :queue-length 4
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
        (js/setTimeout #(swap! app-state assoc :simulating false) (last leave-times)))))

(defn fmap [f d]
  (into {}
        (map (fn [[k v]] [k (f v)])
             d)))

(defn inputs []
  (let [fields (reagent/atom {})]
    (fn []
      [:form
       [:div.field
        [:label.label "Clients per hour"]
        [:input {:type      :number
                 :name      :clients-per-hour
                 :on-change #(swap! fields
                                    assoc :clients-per-hour (-> % .-target .-value))
                 :value     (:clients-per-hour @fields)}]]
       [:div.field
        [:label.label "Min time per client (minutes)"]
        [:input {:type      :number
                 :name      :min-time-per-client
                 :on-change #(swap! fields
                                    assoc :min-time-per-client (-> % .-target .-value))
                 :value     (:min-time-per-client @fields)}]]
       [:div.field
        [:label.label "Max time per client (minutes)"]
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
      [:th (if (pos? (:queue-length @app-state)) person idle)]
      [:th (apply str (repeat (dec (:queue-length @app-state)) person))]]]
    ]]
  )

(defn my-page []
  [:section.section>div.container
   [:h1.title "Maison Clément Queuing system"]
   [:p.subtitle "Let's see how this might work"]
   [:div.content
    [:div.section
     [:h2.title "config"]
     [inputs]]
    [:div.section
     [:h2.title "Simulation"]
     [display]]]])

(rd/render [my-page]
           (. js/document (getElementById "app")))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)
