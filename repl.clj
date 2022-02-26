(ns game.repl
  (:require [arcadia.core :as a]
            [arcadia.internal.config :as config]
            [clojure.core])
  (:import [System.Net.Sockets TcpListener]
           [BencodeNET.Parsing BencodeParser]
           [BencodeNET.Objects IBObject BDictionary BList BString BNumber]
           [System Guid]
           [System.Collections.Generic KeyValuePair]
           [clojure.lang Namespace]
           ;; Arcadia Only
           [UnityEngine GameObject]))

;; GOAL: Make this namespace a place where REPL events can be handled much like the repl callbacks of the NRepl implementation.
;; -- I just want something that I can ship with the thing in order to start an interactive coding experience.
;; Process: Find out the opcodes through Unity and just figure them out one by one

(def ^:const port 37222) ;; My own REPL

;; TODO: Better session and status handling/tracking for the future started by start-server
;; TODO: Turning off the server thread by cancelling the future? stop-server should do this
(def running (atom false))
(def client-running (atom false))
(def client (atom nil))
(def buffer (atom nil))
(def sessions (atom {}))

;; Arcadia Only
;; - This is to encapsulate a queue and eval things on it every update
;; - That way anything eval'd by this repl executes on the main thread
;;   without requiring an editor callback
(defonce repl-eval-queue (atom []))
(defn maybe-eval-queued-action []
  (let [[action & rest] @repl-eval-queue]
    (when action
      (eval action)
      (reset! repl-eval-queue rest))))
(defonce repl-main-obj (let [obj (GameObject. "NRepl")]
                         a/hook+ obj :update :repl #'maybe-eval-queued-action))

(def default-session-bindings
  {}  #_{*ns* (Namespace/findOrCreate 'user)
        *unchecked-math* false
        *warn-on-reflection* false
        *print-level* nil
        *print-length* nil
        *1 nil
        *2 nil
        *3 nil
        *e nil
        *math-context* nil})

(declare repl-future)

(defn new-session
  ([]
   (new-session default-session-bindings))
  ([bindings]
   (let [new-guid (Guid/NewGuid)]
     (swap! sessions assoc new-guid bindings)
     new-guid)))

(defn clone-session [original-guid]
  (new-session (@sessions original-guid)))

(defn send-message [message client]
  (let [bytes (.EncodeAsBytes message)]
    (-> client
        .GetStream
        (.Write bytes 0 (.Length bytes)))))

(defn bnum [n]
  (BNumber. n))

(defn bstr
  "Clojure-y way to convert a str to a BString for creating BDictionary/BList etc."
  [s]
  (if (keyword? s)
    (BString. (name s) nil)
    (BString. s nil)))

(defn bdict
  "Converts a hashmap to a BDictionary"
  [hmap]
  (let [dict (BDictionary.)]
    (doseq [[k v] hmap]
      (cond
        (string? v)
        (.Add dict (bstr k) (bstr v))

        (number? v)
        (.Add dict (bstr k) (bnum v))

        (instance? IBObject v)
        (.Add dict (bstr k) v)))
    dict))

(defn blist
  "Converts a list of strings to a BList"
  [clj-list]
  ;; Going with the stateful add approach because I'm not sure how generics work here yet, and I need a cider-compatible REPL :)
  ;; TODO: Refactor to be gooder once I'm in business
  (let [b-list (BList.)]
    (doseq [v clj-list]
      (.Add b-list v))
   b-list))

(defn bobject->clj [bobj]
  (cond
    (instance? BDictionary bobj)
    (into {} (mapv (fn [kv] [(bobject->clj (.Key kv))
                             (bobject->clj (.Value kv))])
                   bobj))
    (instance? BList bobj)
    (mapv bobject->clj bobj)

    (instance? BString bobj)
    (str bobj)

    (instance? BNumber bobj)
    (.Value bobj)

    :default
    bobj))

(defn handle-message [message]
  #_{:pre [(= (str k) "id") ;; Assumption: The first element is always "id"
           (> (count message) 0)]} ;; Assumption: There's going to be a "thing" per id of message
  (let [op-value                        (get message "op")
        auto-completion-support-enabled false
        session                         (get message "session" (new-session))]
    (case (str op-value)
      "clone"
      (let [cloned-session (clone-session session)]
        (a/log "CLONE")
        (send-message
         (bdict
          {"id"          (get message "id")
           "status"      (blist ["done"])
           "new-session" (.ToString cloned-session)})
         @client))
      "describe"
      ;; TODO: How to declare not available? Seems like lack of presence is the only way... these 0's don't work
      (let [supported-ops (bdict {"eval" 0
                                  "load-file" 1
                                  "describe" 0
                                  "clone" 1
                                  "info" 1
                                  "eldoc" 1
                                  "classpath" 1
                                  "complete" 1
                                  "macroexpand" 1})]
        (a/log "DESCRIBE")
        (send-message (bdict
                       {"id" (get message "id")
                        "session" (.ToString session)
                        "status" (blist ["done"])
                        "ops" supported-ops
                        "versions" (bdict
                                    {"clojure" (bdict
                                                clojure.core/*clojure-version*)
                                     "nrepl" (bdict
                                              {"major" 0
                                               "minor" 2
                                               "incremental" 3})})})
                      @client))
      "eval"
      ;; Example eval. This is run by cider before running the actual code (I assume this is to switch the repl context to the proper namespace)
      #_{"code" "(ns game.repl\n  (:require [arcadia.core :as a]\n            [arcadia.internal.config :as config]\n            [clojure.core])\n  (:import [System.Net.Sockets TcpListener]\n           [BencodeNET.Parsing BencodeParser]\n           [BencodeNET.Objects IBObject BDictionary BList BString BNumber]\n           [System Guid]\n           [System.Collections.Generic KeyValuePair]\n           [clojure.lang Namespace]\n           ;; Arcadia Only\n           [UnityEngine GameObject]))\n",
       "id" "21",
       "op" "eval",
       "session" "c7322193-767c-40e9-80e8-89d3739ea974"}
      (do
        (a/log "EVAL")
        (a/log (str "eval MSG: " message)))
      #_(swap! repl-eval-queue conj message)
      "load-file"
      ;; Example load-file
      #_{"file" "(ns game.core\n  (:require [arcadia.core :as a]\n            [arcadia.linear :as l]\n            [game.repl :as repl]\n            [game.utils :as utils])\n  (:import [UnityEngine\n            Application\n            QualitySettings\n            GameObject\n            Component\n            Transform]))\n\n(defn init-fps!\n  \"From Saikyun demo #2: https://www.youtube.com/watch?v=HoeUi2aOFxU&t=29s\"\n  [& _]\n  (set! (.. QualitySettings vSyncCount) 0)\n  (set! (.. Application targetFrameRate) 15))\n\n(defn load-hooks\n  \"\n  This function is to stash hooks that need to be added once while REPL developing. Add them here, then they will come back the next time.\n\n  This should possibly be an import time \\\"add\\\" and a post-import \\\"run\\\"\n  situation, so that each namespace can specify their own, but it's only\n  run once.\n  \"\n  []\n  (a/hook+ (a/object-named \"Main Camera\") :start :init-fps #'game.core/init-fps!))\n\n(defn start [& _]\n  (repl/start-server)\n  (utils/clean-scene)\n  (let [c1 (utils/create-primitive :cube \"BeheldCube\")]\n    (set! (.. c1 transform position)\n          (l/v3 3 3 3))\n    #_(a/hook+ c1 :update :moveit #'move!))\n  (utils/create-primitive :cube \"Hello World\"))\n\n;; How to persist hook adding in clojure? I feel like defonce might still be the way to do it, but the result of the hooks is def'd\n\n;; Start (does our clear on save)\n#_(start)\n\n;; Load our hooks (once)\n(defonce hooks-loaded (atom false))\n(when-not @hooks-loaded\n  (load-hooks)\n  (reset! hooks-loaded true))\n\n(comment\n  ;; This is how I added the first hook.\n  (a/hook+ (a/object-named \"Main Camera\") :start :main #'game.core/start)\n  (set! (.. (a/object-named \"Hello World\") transform position) (l/v3 3 3 3))\n  (a/destroy (a/object-named \"Hello World\"))\n\n  ;; TODO: Figure out how to ship this REPL and connect to it remotely.\n  ;; -- I think I can do it without modification of Arcadia using socket-repl, but then I'll have to use socket-repl...\n  ;; -- That said, it IS much more straightforward to run.\n  (init-fps!)\n\n  ;; Main goal: Have a space where you can manipulate the world. The way to do so, should be pluggable so it can be expanded.\n  ;; Interacting via hooks allows UnityUpdate stuff to be changed in real time.\n  ;; - So, interaction should be a hook on the character controller (or whatever that will end up being in VR)\n  ;; - That way I can develop everything interactively via the REPL.\n  (repl/start-server)\n  (repl/stop-server)\n  )\n\n(a/log \"reloaded core.clj!\")\n",
       "file-name" "core.clj",
       "file-path" "/Users/danielmosora/Development/Projects/games/golmud/Golmud/Assets/game/core.clj",
       "id" "43",
       "op" "load-file",
       "session" "c7322193-767c-40e9-80e8-89d3739ea974"}
      (a/log (str "load-file MSG: " message))
      "eldoc"
      ;; Example eldoc
      #_{"id" "25",
       "ns" "game.core",
       "op" "eldoc",
       "session" "c7322193-767c-40e9-80e8-89d3739ea974",
       "sym" "comment"}
      (a/log (str "eldoc MSG: " message))
      "info"
      ;; Info example
      ;; {"id" "113", "ns" "game.repl", "op" "info", "session" "c7322193-767c-40e9-80e8-89d3739ea974", "sym" "a/log"}
      (a/log (str "info MSG: " message))
      "complete"
      (a/log (str "complete MSG: " message))
      "classpath"
      ;; Classpath example
      ;; No idea what this does, but it happened on re-connect
      ;; {"id" "155", "op" "classpath", "session" "c7322193-767c-40e9-80e8-89d3739ea974"}
      (a/log (str "classpath MSG: " message))
      "close"
      ;; TODO: This is needed to be able to loop well
      ;; - Close and clear out the session
      ;; - Should this also stop the server?
      ;; {"id" "132", "op" "close", "session" "c7322193-767c-40e9-80e8-89d3739ea974"}
      (a/log (str "close MSG: " message))

      "macroexpand"
      ;; TODO: This is probably going to be a tough op to implement because cider won't send it unless it's provided as a middleware it seems?
      ;; Message: ‘cider-macroexpand-1’ requires the nREPL op "macroexpand" (provided by cider-nrepl)
      (a/log (str "close MSG: " message))

      ;; Default
      ;; UNIMPLEMENTED: {"id" "55", "name" "-main", "op" "ns-list-vars-by-name", "session" "c7322193-767c-40e9-80e8-89d3739ea974"}
      ;; UNIMPLEMENTED: {"id" "104", "interrupt-id" "9", "op" "interrupt", "session" "c7322193-767c-40e9-80e8-89d3739ea974"}
      (a/log (str "UNIMPLEMENTED: " message)))
    )
  )

(defn start-server
  "
  This is a direct translation of `NRepl.cs`'s `StartServer` in order to
  handle the opcodes without having to rely on the editor functionality of
  the built in nrepl.... and I guess to learn a bit about how nrepls work
  in the over-the-wire protocol!
  "
  []
  (a/log "MY nrepl starting...")
  (reset! running true)
  ;; Do stuff
  ;; TODO: Do we need to have this many futures on futures? Threads on threads?
  ;; Also... how do I stop one of these?
  (future
    (let [listener (TcpListener. (IPAddress/Loopback) port)]
      (try
        (.Start listener)
        (loop []
          (if (not (.Pending listener))
            (do (Thread/Sleep 100)
                (recur))
            ;; TODO: Refactor out this atomic state where possible
            (do
              (reset! client (.AcceptTcpClient listener))
              (reset!
               repl-future
               (future
                 (a/log (format "nrepl: connected to client %s" (.. @client Client RemoteEndPoint)))
                 (let [parser (BencodeParser.)]
                   (reset! client-running true)
                   (reset! buffer (byte-array (* 1024 8)))
                   ;; TODO: This could be a loop recur, or maybe an
                   ;; agent???
                   ;; TODO: I think since `running` is a static class
                   ;; level variable in the original code, the author was
                   ;; trying to leave the option open to close the loop
                   ;; from outside the `NRepl` class. I guess it should
                   ;; still be a public atom then???
                   (while (and @running @client-running)
                     (try
                       ;; NB> This should disposes when it's done...
                       ;; TODO: Test and conirm this
                       (with-open [ms (MemoryStream.)]
                         (loop []
                           (let [should-recur?
                                 (try
                                   (let [total (-> @client .GetStream (.Read @buffer 0 (.Length @buffer)))]
                                     (a/log (str "Total bytes::: " total))
                                     (when (= 0 total)
                                       (reset! client-running false))
                                     (when-not (= total 0)
                                       (.Write ms @buffer 0 total)
                                       (set! (.. ms Position) 0)
                                       ;; TODO: There's error handling
                                       ;; here in the NRepl.cs file, but
                                       ;; I'm not quite sure how to
                                       ;; translate it, so lets see how
                                       ;; often it occurs
                                       ;; TODO: sometimes the parse here
                                       ;; fails because the binary has 2
                                       ;; "session" keys, it fails in
                                       ;; Parse, so may be good to handle
                                       ;; that more gracefully
                                       (when-let [obj (.Parse parser ms)]
                                         (a/log "GOT A MESSAGE!!!")
                                         (let [clj-msg (bobject->clj obj)]
                                           (a/log clj-msg)
                                           (handle-message clj-msg))
                                         false)
                                       ))
                                   (catch Exception e
                                     ;; TODO: I'm assuming all exceptions are "read more", but seems that's not true, see TODO above
                                     (a/log e) ;; TODO: This could be a "big big message" exception
                                     (.Seek ms 0 SeekOrigin/End)
                                     true
                                     ))]
                             (when should-recur?
                               (recur))))
                         )
                       ;; Catch Gracefully
                       (catch SocketException e
                         (reset! client-running false))
                       (catch IOException e
                         (reset! client-running false))
                       (catch ObjectDisposedException e
                         (reset! client-running false))
                       ;; Catch and log, this is different
                       (catch Exception e
                         (a/log (format "nrepl: %s" e))
                         (reset! client-running false)))
                     ))))))
          )
        (catch Exception e
          (a/log (format "Exception in outer future: %s" e)))
        (finally
          (a/log (format "nrepl: closing port %s" port))
          (.Stop listener)))))
  )

(defn stop-server []
  (reset! @running false))

(comment
  ;; TODO: Need NRepl ops implemented
  ;; ‘cider-macroexpand-1’ requires the nREPL op "macroexpand".  Please, install (or update) cider-nrepl 0.15.1 and restart CIDER
  (validate-agent {:client-running true :client 123})

  (listener)
  (start-server)
  ;; TODO: Accept client
  ;; TODO: Log input received from cider client
  ;; TODO: Implement each op when it's not found
  ;; ...profit!

  (future (+ 1 1))
  (agent {:client-runnning false
          :port 37222
          :client nil}
         :validate-fn validate-agent)
  )

(a/log "reloaded game.repl!")
