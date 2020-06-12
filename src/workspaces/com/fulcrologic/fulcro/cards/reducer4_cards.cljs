(ns com.fulcrologic.fulcro.cards.reducer4-cards
  (:require
    ["react" :refer [createElement]]
    [nubank.workspaces.card-types.fulcro3 :as ct.fulcro]
    [nubank.workspaces.core :as ws]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.dom :as dom :refer [div button p ul]]
    [com.fulcrologic.fulcro.mutations :as m :refer [defmutation]]
    [cljs.test :as test :refer [is]]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.react.hooks :as hooks]
    [taoensso.timbre :as log])
  )

(declare Customer Sale)
(def names ["Sam" "Joe" "CoolDude" "James" "AnotherDude" "Name"])

(def subscription-fns (atom []))
(defn register-listen! [listen-fn])

(defn ident->id [ident]
  (assert (and (vector? ident) (= (count ident) 2)))
  (second ident))

(defn ident->table-id [ident]
  (assert (and (vector? ident) (= (count ident) 2)))
  (first ident))





(defn sum-sale-price [sales]
  (reduce + 0 (map :sale/amount sales)))

(defn customer-count [customers]
  (count customers))


(defn sales-extractor [app-db]
  (get app-db :sale/id))

(defn customers-extractor [app-db]
  (get app-db :customer/id))

(defn customer-extractor [customers-table customer-id]
  (get customers-table customer-id))

(defn customer-sales [sale-table customer]
  (map #(get sale-table (ident->id %)) (:customer/purchases customer)))



(defn lift-to-subscription [f input-vector output]
  {:f        (fn lift-fn* [input-map]
               (assoc input-map output (apply f (map input-map input-vector))))
   :requires input-vector
   :output   output})



(def sum-all-sales<
  [(lift-to-subscription sales-extractor [:app-db] :sales)
   (lift-to-subscription sum-sale-price [:sales] :sum)])

(defn sum-customer-sales< [customer-id]
  [(lift-to-subscription customers-extractor [:app-db] :customers)
   (lift-to-subscription #(customer-extractor % customer-id) [:customers] [:customer])
   (lift-to-subscription sales-extractor [:app-db] :sales-table)
   (lift-to-subscription customer-sales [:sales-table :customer] [:customer-sales])
   (lift-to-subscription sum-sale-price [:customer-sales] [:sum])])

(def customer-count<
  [(lift-to-subscription customer-extractor [:app-db] :customers)
   (lift-to-subscription customer-count [:customers] :count)])



(defn instance-sub [sub-vector]
  (assert (vector? sub-vector))
  (let [prior-run-result (atom {})])
  (fn sub-runner* [app-db]
    (loop )))







(defn gen-sale []
  #:sale{:id     (rand-int 99999999)
         :amount (rand-int 80)})

(defn gen-customer []
  #:customer{:id        (rand-int 99999999)
             :name      (rand-nth names)
             :purchases [(gen-sale)
                         (gen-sale)]})


(defmutation add-sale [{:keys [cust-id]}]
  (action [{:keys [state]}]
    (swap! state #(let [sale (gen-sale)
                        sale-ident [:sale/id (:sale/id sale)]]
                    (-> %
                      (merge/merge-component Sale sale)
                      (update-in [:customer/id cust-id :customer/purchases] conj sale-ident))))))


(defmutation add-cust [{:keys []}]
  (action [{:keys [state]}]
    (swap! state #(let [cust (gen-customer)
                        cust-ident [:customer/id (:customer/id cust)]]
                    (-> %
                      (merge/merge-component Customer cust)
                      (update-in [:top/id 1 :top/customers] conj cust-ident))))))



(defsc Sale [this {:keys [sale/id sale/amount] :as props}]
  {:query         [:sale/id
                   :sale/amount]
   :ident         :sale/id
   :initial-state #:sale{:id     :param/id
                         :amount :param/amount}
   }
  (str "Sale: $" amount))

(def ui-sale (comp/factory Sale {:keyfn :sale/id}))





(defsc Customer [this {:customer/keys [id name purchases] :as props}]
  {:query [:customer/id
           :customer/name
           {:customer/purchases (comp/get-query Sale)}]
   :ident :customer/id}
  (dom/div
    {}
    name
    (dom/button
      {:onClick (fn [e]
                  (comp/transact! this [(add-sale {:cust-id id})]))}
      "+ Add Sale")
    (dom/ul
      (mapv #(dom/li (ui-sale %)) purchases))))

(def ui-customer (comp/factory Customer {:keyfn :customer/id}))



(defsc Top [this {:keys [top/id top/customers] :as props}]
  {:query         [:top/id
                   {:top/customers (comp/get-query Customer)}]
   :ident         :top/id
   :initial-state (fn [_] #:top{:id        1
                                :customers [(gen-customer)
                                            (gen-customer)]})}
  (dom/div
    (dom/button
      {:onClick (fn [e]
                  (comp/transact! this [(add-cust {})]))}
      "+ Add Customer")
    (mapv ui-customer customers)))

(def ui-top (comp/factory Top {:keyfn :top/id}))




(ws/defcard reducer4-card
  (ct.fulcro/fulcro-card
    {::ct.fulcro/wrap-root? true
     ::ct.fulcro/root       Top
     ;; NOTE: Just invented tx-hook...simple add to tx-processing
     }))


