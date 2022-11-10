(ns game.repl
  (:require [arcadia.core :as a]
            ;; TODO: Are these internal things okay to include?
            [arcadia.internal.config :as config]
            [arcadia.internal.socket-repl :as socket-repl]
            [clojure.core])
  (:import [System.Net.Sockets TcpListener]
           [BencodeNET.Parsing BencodeParser]
           [BencodeNET.Objects IBObject BDictionary BList BString BNumber]
           [System Guid]
           [System.IO TextWriter File Directory Path]
           [System.Text StringBuilder]
           [System.Collections.Generic KeyValuePair]
           [clojure.lang Namespace]
           ;; Arcadia Only
           [UnityEngine GameObject]))

;; GOAL: Make this namespace a place where REPL events can be handled much like the repl callbacks of the nREPL implementation.
;; -- I just want something that I can ship with the thing in order to start an interactive coding experience.
;; Process: Find out the opcodes through Unity and just figure them out one by one
;; Documentation: This is the structure and design of nREPL. This set of docs also contains a lot about the specifics of the transport and protocol https://nrepl.org/nrepl/design/overview.html
;; TODO: How to make cider-jack-in work? Can it work in CLR?
;; - I guess cider-jack-in probably needs a "run" command of some kind

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
(defn maybe-eval-queued-action [_ _]
  ;; TODO: In order to provide interruption, I think I will need to figure
  ;; out how to handle a cancellation token here. Update shouldn't be
  ;; throwing exceptions.
  (let [[action & rest] @repl-eval-queue]
    (when action
      (try
        (action)
        (catch Exception ex
          ;; TODO: Return this to the frontend as eval error? Check nrepl eval error handling
          (a/log ex)))
      (reset! repl-eval-queue rest))))
(defonce repl-main-obj (let [obj (GameObject. "nREPL")]
                         (a/hook+ obj :update :repl #'maybe-eval-queued-action)))

;; NB> These are the default bindings that make the eval happen in the
;; proper session context.
(def default-session-bindings
  {#'*ns* (Namespace/findOrCreate 'user)
   #'*unchecked-math* false
   #'*warn-on-reflection* false
   #'*print-level* nil
   #'*print-length* nil
   #'*1 nil
   #'*2 nil
   #'*3 nil
   #'*e nil
   #'*math-context* nil})

(declare repl-future)

(defn send-message! [message client]
  (def xmessage message)
  (def xclient client)
  (let [bytes (.EncodeAsBytes message)]
    (-> client
        .GetStream
        (.Write bytes 0 (.Length bytes)))))

(defn new-session!
  ([]
   (new-session! default-session-bindings))
  ([bindings]
   (let [new-guid (Guid/NewGuid)]
     (swap! sessions assoc new-guid bindings)
     new-guid)))

(defn get-session [message]
  (if (get message "session")
    (Guid/Parse (str (get message "session")))
    (new-session!)))

(defn clone-session! [original-guid]
  (new-session! (@sessions original-guid)))

(defn update-session! [session new-bindings]
  ;; NB> This assumes that the new-bindings are the full set of bindings
  ;; to use for the session.
  (swap! sessions update session assoc new-bindings))

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

(defn writer [kind request client]
  (let [id (str (get request "id"))
        session (str (get-session request))
        client client
        kind kind
        string-builder (StringBuilder.)]
    (proxy [TextWriter] []
      (Encoding [] Encoding/UTF8)
      (Write [value]
        (.Append string-builder value))
      (Flush []
        (send-message!
         (bdict {"id" id
                 kind (.ToString string-builder)
                 "session" session})
         client)
        (.Clear string-builder)))))

(defn find-file-ns [message]
  ;; Look up from "file" to "Assets" to get the NS.
  ;; NB> This can probably be done much easier through the message
  ;; itself with `cider`... I think we get the ns for eval but I'm going
  ;; to follow this through
  (try
    (let [[path current ns-list] (loop [path (str (get message "file"))
                                       current nil
                                        ns-list []]
                                  (if (and (some? path)
                                           (not= current "Assets")
                                           (or (File/Exists path)
                                               (Directory/Exists path)))
                                    (recur (.FullName (Directory/GetParent path))
                                           (Path/GetFileNameWithoutExtension path)
                                           (conj ns-list current))
                                    [path
                                     current
                                     (drop 1 (reverse ns-list))]))
          full-ns (clojure.string/join "." ns-list)]
     (a/log (str "Trying to find: " full-ns))
     (let [result (and (not-empty full-ns) (find-ns full-ns))]
       ;; Just logging for now to be verbose for conversion sake
       (if result
         (a/log (str "Found: " full-ns))
         (a/log ":file was not a valid ns"))
       result))
    (catch Exception ex
      (a/log (str ex))
      (a/log ":file was not a valid ns"))))

(defn do-eval [message]
  ;; TODO: This needs to run from the main thread for Unity's purposes
  ;; TODO: For some reason the communication is taking forever, I need to get a good profile to see where it's spending its time.
  (swap! repl-eval-queue
         conj
         (fn []
           (let [session          (get-session message)
                 code             (str (get message "code"))
                 session-bindings (get @sessions session)
                 out-writer       (writer "out" message @client)
                 err-writer       (writer "err" message @client)
                 ;; We want to change session context to ns in message before eval-ing
                 file-ns          nil ;; TODO: None of this seems to be working.
                 #_(get message (symbol "ns"))
                 #_(find-file-ns message)
                 eval-bindings    (cond-> (assoc session-bindings
                                                 #'*out* out-writer
                                                 #'*err* err-writer)
                                    file-ns (assoc #'*ns* file-ns))]
             (with-bindings eval-bindings
               (try
                 (let [result (eval (read-string {:read-cond :allow} code))
                       value  (pr-str result)]
                   (var-set #'*3 *2)
                   (var-set #'*2 *1)
                   (var-set #'*1 result)
                   (update-session! session (get-thread-bindings))
                   (send-message! (bdict {"id"     (get message "id")
                                          "value"   value
                                          "ns"      (str *ns*)
                                          "session" (str session)})
                                  @client)
                   (.Flush out-writer)
                   (.Flush err-writer)

                   ;; TODO: Do I need to do this?
                   (send-message! (bdict {"id" (get message "id")
                                          "status" (blist ["done"]) ;; // TODO does this have to be a list?
                                          "session" (str session)})
                                  @client)

                   )
                 (catch Exception e
                   (var-set #'*e e)
                   (update-session! session (get-thread-bindings))
                   ;; Returns
                   ;; :ex The type of exception thrown, if any. If present, then :value will be absent.
                   ;; :ns *ns*, after successful evaluation of code.
                   ;; :root-ex The type of the root exception thrown, if any. If present, then :value will be absent.
                   ;; :value The result of evaluating code, often readable. This printing is provided by the print middleware. Superseded by ex and root-ex if an exception occurs during evaluation.
                   (send-message! (bdict {"id" (get message "id")
                                          "status" (blist ["eval-error"])
                                          "session" (str session)
                                          "ex" (str (type e)) ;; TODO: Can I return the whole thing here? For cider usage
                                          })
                                  @client)
                   (send-message! (bdict {"id" (get message "id")
                                          "session" (str session)
                                          "err" (str (socket-repl/error-string e))})
                                  @client)
                   (send-message! (bdict {"id" (get message "id")
                                          "status" (blist ["done"])
                                          "session" (str session)
                                          })
                                  @client)
                   (throw e)))))))
  ;; TODO: Unsure if this is needed, but it wants a null return so lets
  ;; make it explicit
  nil
  )

(defn handle-clone [op-value message auto-completion-support-enabled session]
  (let [cloned-session (clone-session! session)]
    (send-message!
     (bdict
      {"id"          (get message "id")
       "status"      (blist ["done"])
       "new-session" (.ToString cloned-session)})
     @client)))

(defn handle-describe [op-value message auto-completion-support-enabled session]
  ;; TODO: How to declare not available? Seems like lack of presence is the only way... these 0's don't work
  (let [supported-ops (bdict {"eval" 0
                              "load-file" 1
                              "describe" 0
                              "clone" 1
                              "info" 1
                              "eldoc" 1
                              ;;"classpath" 1
                              "complete" 1
                              ;; This also doesn't seem to work to get emacs to send a macroexpand op
                              "macroexpand" 1})]
    (a/log "DESCRIBE")
    (send-message! (bdict
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
                   @client)))

(defn handle-eval [op-value message auto-completion-support-enabled session]
  ;; nREPL's eval Details: https://github.com/nrepl/nrepl/blob/8223894f6c46a2afd71398517d9b8fe91cdf715d/src/clojure/nrepl/middleware/interruptible_eval.clj#L57-L66
  ;; Example:
  ;; {"nrepl.middleware.print/print" "cider.nrepl.pprint/pr", "file" "/Users/danielmosora/Development/Projects/games/golmud/Golmud/Assets/game/core.clj", "line" 69, "code" "(+ 1 1)", "ns" "game.core", "id" "13", "op" "eval", "session" "c6e0ffcf-5fdc-407a-89df-001c4bb6c73d", "nrepl.middleware.print/stream?" [], "nrepl.middleware.print/quota" 1048576, "column" 1}
  (do
    (a/log "EVAL")
    (a/log (str "eval MSG: " message))
    #_(swap! repl-eval-queue conj (read-string (get message "code")))
    (do-eval message)))

(defn handle-load-file [op-value message auto-completion-support-enabled session]
  ;; Example load-file
  #_{"file" "(ns game.core\n  (:require [arcadia.core :as a]\n            [arcadia.linear :as l]\n            [game.repl :as repl]\n            [game.utils :as utils])\n  (:import [UnityEngine\n            Application\n            QualitySettings\n            GameObject\n            Component\n            Transform]))\n\n(defn init-fps!\n  \"From Saikyun demo #2: https://www.youtube.com/watch?v=HoeUi2aOFxU&t=29s\"\n  [& _]\n  (set! .... [full file content that was eval'd]",
     "file-name" "core.clj",
     "file-path" "/Users/danielmosora/Development/Projects/games/golmud/Golmud/Assets/game/core.clj",
     "id" "43",
     "op" "load-file",
     "session" "c7322193-767c-40e9-80e8-89d3739ea974"}
  (do
    (a/log (str "load-file MSG: " message))
    ;; TODO: Should this do things with the file? Return values?
    ;; NB> `file` contains the source of the whole file.
    ;; - NRepl.cs just wraps it in a `do` and goes to town
    (do-eval (assoc message "code"
                    (bstr (str "(do " (get message "file") " )"))))
    #_(swap! repl-eval-queue conj (read-string (get message "file")))))

(defn handle-eldoc [op-value message auto-completion-support-enabled session]
  ;; Info example
  ;; {"id" "113", "ns" "game.repl", "op" "info", "session" "c7322193-767c-40e9-80e8-89d3739ea974", "sym" "a/log"}
  ;; Example eldoc
  #_{"id"      "25",
     "ns"      "game.core",
     "op"      "eldoc",
     "session" "c7322193-767c-40e9-80e8-89d3739ea974",
     "sym"     "comment"}
  (a/log (str op-value " MSG: " message))
  (let [ns-str     (str (get message "ns"))
        symbol-str (str (or (get message "symbol") (get message "sym")))]
    ;; // Editors like Calva that support doc-on-hover sometimes will ask about empty strings or spaces
    (when-not (or (= "" symbol-str)
                  (nil? symbol-str)
                  (= " " symbol-str))
      (let [symbol-metadata (try
                              (meta (ns-resolve (find-ns (symbol ns-str)) (symbol symbol-str)))
                              (catch TypeNotFoundException ex
                                ;; // We'll just ignore this call if the type cannot be found. This happens sometimes.
                                ;; // TODO: One particular case when this happens is when querying info for a namespace.
                                ;; //       That case should be handled separately (e.g., via `find-ns`?)
                                (a/log (format "ELDOC ERROR: %s" ex))))]
        (if symbol-metadata
          (send-message! (bdict
                          (into {"id"      (get message "id")
                                 "session" (.ToString session)
                                 "status"  (blist ["done"])}
                                (->> symbol-metadata
                                     (filter #(.val %))
                                     (map (fn [entry]
                                            (let [key-str (.Substring (.ToString (.key entry)) 1)]
                                              [(case key-str
                                                 "arglists" "arglists-str"
                                                 "forms"    "forms-str"
                                                 key-str)
                                               (.ToString (.val entry))])))))))
          (send-message! (bdict
                          {"id"      (get message "id")
                           "session" (.ToString session)
                           "status"  (blist ["done", "no-info"])}))))))
  )

(defn handle-complete [op-value message auto-completion-support-enabled session]
  (a/log (str "complete MSG: " message)))

(defn handle-classpath [op-value message auto-completion-support-enabled session]
  ;; Classpath example
  ;; No idea what this does, but it happened on re-connect
  ;; - Seems like it's not implemented? https://github.com/nrepl/nrepl/search?q=classpath
  ;; {"id" "155", "op" "classpath", "session" "c7322193-767c-40e9-80e8-89d3739ea974"}
  (a/log (str "classpath MSG: " message)))

(defn handle-close [op-value message auto-completion-support-enabled session]
  ;; TODO: This is needed to be able to loop well
  ;; - Close and clear out the session
  ;; - Should this also stop the server?
  ;; {"id" "132", "op" "close", "session" "c7322193-767c-40e9-80e8-89d3739ea974"}
  (a/log (str "close MSG: " message)))

(defn handle-macroexpand [op-value message auto-completion-support-enabled session]
  ;; TODO: This is probably going to be a tough op to implement because cider won't send it unless it's provided as a middleware it seems?
  ;; Message: ‘cider-macroexpand-1’ requires the nREPL op "macroexpand" (provided by cider-nrepl)
  (a/log (str "close MSG: " message)))

(defn handle-message [message]
  (let [op-value                        (get message "op")
        auto-completion-support-enabled false
        session                         (get message "session" (new-session!))]
    ;; All ops are documented here:
    ;; - https://nrepl.org/nrepl/ops.html
    ;; TODO: How to do inspector? I could probably use clojure/reflect if needed
    (case (str op-value)
      "clone"
      (handle-clone op-value message auto-completion-support-enabled session)
      "describe"
      (handle-describe op-value message auto-completion-support-enabled session)
      "eval"
      (handle-eval op-value message auto-completion-support-enabled session)
      "load-file"
      (handle-load-file op-value message auto-completion-support-enabled session)
      "eldoc"
      (handle-eldoc op-value message auto-completion-support-enabled session)
      "info"
      (handle-eldoc op-value message auto-completion-support-enabled session)
      "complete"
      (handle-complete op-value message auto-completion-support-enabled session)

      "classpath"
      (handle-classpath op-value message auto-completion-support-enabled session)

      "close"
      (handle-close op-value message auto-completion-support-enabled session)

      "macroexpand"
      (handle-macroexpand op-value message auto-completion-support-enabled session)

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
  (def the-repl-future
    (future
      (let [listener (TcpListener. (IPAddress/Loopback) port)]
        (try
          (.Start listener)
          (loop []
            (if (not (.Pending listener))
              (do (Thread/Sleep 40)
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
            (.Stop listener))))))
  )

(defn stop-server []
  (reset! @running false))

(comment
  ;; ‘cider-macroexpand-1’ requires the nREPL op "macroexpand".  Please, install (or update) cider-nrepl 0.15.1 and restart CIDER
  (validate-agent {:client-running true :client 123})

  (listener)
  (start-server)

  (future (+ 1 1))
  (agent {:client-runnning false
          :port 37222
          :client nil}
         :validate-fn validate-agent)
  )

(a/log "reloaded game.repl!")

(comment
  (start-server)
  game.repl/message
  (future-cancel game.repl/the-repl-future)
  )
