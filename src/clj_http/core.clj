(ns clj-http.core
  "Core HTTP request/response implementation."
  (:import (org.apache.http HttpRequest HttpEntityEnclosingRequest HttpResponse Header))
  (:import (org.apache.http.util EntityUtils))
  (:import (org.apache.http.entity InputStreamEntity))
  (:import (org.apache.http.client HttpClient))
  (:import (org.apache.http.client.methods HttpGet HttpHead HttpPut HttpPost HttpDelete))
  (:import (org.apache.http.client.params CookiePolicy ClientPNames))
  (:import (org.apache.http.conn EofSensorInputStream EofSensorWatcher))
  (:import (org.apache.http.impl.client DefaultHttpClient)))

(defn- parse-headers [#^HttpResponse http-resp]
  (into {} (map (fn [#^Header h] [(.toLowerCase (.getName h)) (.getValue h)])
                (iterator-seq (.headerIterator http-resp)))))

(defn- shutdown-connections
  [#^HttpClient http-client]
  (.shutdown (.getConnectionManager http-client)))

(defn- shutdown-connections-on-close
  "Given an input stream and an HttpClient, wrap the input stream with an
EofSensorWatcher that will shutdown the HttpClient's connection manager
when the underlying input stream is closed."
  [input-stream #^HttpClient http-client]
  (EofSensorInputStream. input-stream
                         (reify EofSensorWatcher
                                (eofDetected [this stream]
                                             (shutdown-connections http-client)
                                             false)
                                (streamAbort [this stream]
                                             (shutdown-connections http-client)
                                             false)
                                (streamClosed [this stream]
                                              (shutdown-connections http-client)
                                              false))))

(defn request-with-client
  "Execute a request using the given HttpClient"
  [#^HttpClient http-client
   {:keys [request-method scheme server-name server-port uri query-string
           headers content-type character-encoding body content-length]}]
  (try
    (-> http-client
        (.getParams)
        (.setParameter ClientPNames/COOKIE_POLICY CookiePolicy/BROWSER_COMPATIBILITY))
    (let [http-url (str scheme "://" server-name
                        (if server-port (str ":" server-port))
                        uri
                        (if query-string (str "?" query-string)))
          #^HttpRequest
          http-req (case request-method
                         :get    (HttpGet. http-url)
                         :head   (HttpHead. http-url)
                         :put    (HttpPut. http-url)
                         :post   (HttpPost. http-url)
                         :delete (HttpDelete. http-url))]
      (if (and content-type character-encoding)
        (.addHeader http-req "Content-Type"
                    (str content-type "; charset=" character-encoding)))
      (if (and content-type (not character-encoding))
        (.addHeader http-req "Content-Type" content-type))
      (when content-length
        (.addHeader http-req "Content-Length" content-length))
      (.addHeader http-req "Connection" "close")
      (doseq [[header-n header-v] headers]
        (.addHeader http-req header-n header-v))
      (if body
        (let [http-body (InputStreamEntity. body (or content-length -1))]
          (.setEntity #^HttpEntityEnclosingRequest http-req http-body)))
      (let [http-resp (.execute http-client http-req)
            http-entity (.getEntity http-resp)]
        {:status (.getStatusCode (.getStatusLine http-resp))
         :headers (parse-headers http-resp)
         :body (if http-entity (.getContent http-entity))}))))

(defn request
  "Executes the HTTP request corresponding to the given Ring request map and
   returns the Ring response map corresponding to the resulting HTTP response.

   Input and output bodies are assumed to be InputStreams.

   Instantiates an HttpClient, uses it to make a request, ensures it is cleaned
   up properly and return the response."
  [req]
  (let [http-client (DefaultHttpClient.)
        resp (request-with-client http-client req)
        body (:body resp)]
    (if body
      (assoc resp :body (shutdown-connections-on-close body http-client))
      (do (shutdown-connections http-client)
          resp))))
