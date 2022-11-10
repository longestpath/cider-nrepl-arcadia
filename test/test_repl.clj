(ns test-repl
  (:require [clojure.test :refer :all]
            [game.repl :as repl]))

;; Middleware message the comes through:
;; - {"nrepl.middleware.print/print" "cider.nrepl.pprint/pprint", "file" "*cider-repl dependencies/cider-nrepl-arcadia-unity:localhost:37222(clj)*", "line" 35, "code" "(clojure.core/apply clojure.core/require clojure.main/repl-requires)", "nrepl.middleware.print/options" {"right-margin" 74}, "id" "4", "op" "eval", "inhibit-cider-middleware" "true", "session" "691b7fb7-720e-4a04-9bc6-c8a64f19ddb4", "nrepl.middleware.print/stream?" "1", "nrepl.middleware.print/buffer-size" 4096, "nrepl.middleware.print/quota" 1048576, "column" 1}

(def ^:const test-session-id "691b7fb7-720e-4a04-9bc6-c8a64f19ddb4")

(defn ensure-session [f]
  (swap! repl/sessions assoc (Guid/Parse test-session-id) repl/default-session-bindings)
  (f)
  (reset! repl/sessions {}))

(use-fixtures :each ensure-session)

(deftest handle-clone-test
  (let [message-sent (atom {})]
    (with-redefs [repl/send-message!
                  (fn [to-send _client]
                    (swap! message-sent
                           (fn [messages]
                             (let [clj-msg (repl/bobject->clj to-send)]
                               (assoc messages (clj-msg "op") clj-msg)))))]
      (testing "handle-clone"
        (repl/handle-message {"id" "1", "op" "clone"})))))

(deftest handle-describe-test
  (let [message-sent (atom {})]
    (with-redefs [repl/send-message!
                  (fn [to-send _client]
                    (swap! message-sent
                           (fn [messages]
                             (let [clj-msg (repl/bobject->clj to-send)]
                               (assoc messages (clj-msg "op") clj-msg)))))]
      (testing "handle-describe"
        (repl/handle-message {"id" "3", "op" "describe", "session" test-session-id})))))

(deftest handle-close-test
  (let [message-sent (atom {})]
    (with-redefs [repl/send-message!
                  (fn [to-send _client]
                    (swap! message-sent
                           (fn [messages]
                             (let [clj-msg (repl/bobject->clj to-send)]
                               (assoc messages (clj-msg "op") clj-msg)))))]
      (testing "handle-close"
        (repl/handle-message {"id" "10", "op" "close", "session" test-session-id})))))

(deftest handle-eldoc-test
  (let [message-sent (atom {})]
    (with-redefs [repl/send-message!
                  (fn [to-send _client]
                    (swap! message-sent
                           (fn [messages]
                             (let [clj-msg (repl/bobject->clj to-send)]
                               (assoc messages (clj-msg "op") clj-msg)))))]
      (testing "handle-eldoc"
        (repl/handle-message {"id" "9", "ns" "game.repl", "op" "eldoc", "session" test-session-id, "sym" "comment"})
        (is (= nil message-sent))))))
