;; ielisp.el

(package-initialize)
(require 'json)
(require 'zmq)
(setq debug-on-error t)

(setq iel--connection-info (json-read-file (car argv)))

;; generate UUID
(defun iel--uuidgen ()
  (replace-regexp-in-string "\n" "" (shell-command-to-string "uuidgen")))

;; generate a message header given a `msg_type`
(defun iel--msg-header (msg-type)
  (json-encode-alist
   `((session . ,iel--session-id)
     (msg_id . ,(iel--uuidgen))
     (date . ,(format-time-string "%FT%T%z"))
     (msg_type . ,msg-type)
     (username . "kernel")
     (version . "5.3"))))

;; generate bind address
(defun iel--bind-addr (port)
  (let* ((transport (cdr (assoc 'transport iel--connection-info)))
         (ip (cdr (assoc 'ip iel--connection-info)))
         (port (cdr (assoc-string port iel--connection-info))))
    (format "%s://%s:%s" transport ip port)))

;; construct and send a message on socket
(defun iel--send (socket msg-type content parent-header metadata identities)
  (message "%s" content)
  (message "---")
  (let* ((header (iel--msg-header msg-type))
         (delimiter iel--delimiter)
         (signature "") ;; TODO
         (msgs (append identities (list
                                   delimiter
                                   signature
                                   header
                                   (json-encode parent-header)
                                   (json-encode metadata)
                                   (json-encode content)))))
    (message "%s" msgs)
    (zmq-send-multipart socket msgs)))

;; deserialize msgs, retuning a list of identities and an alist
(defun iel--deserialize (msgs)
  (let* ((delimiter-pos (seq-position msgs iel--delimiter))
         (identities (seq-subseq msgs 0 delimiter-pos))
         (frames (seq-subseq msgs (+ 2  delimiter-pos))))
    (list identities `((header  . ,(json-read-from-string (seq-elt frames 0)))
                       (parent_header . ,(json-read-from-string (seq-elt frames 1)))
                       (metadata . ,(json-read-from-string (seq-elt frames 2)))
                       (content . ,(json-read-from-string (seq-elt frames 3)))))))

(defun iel--shell-handler (msg)
  (let* ((deserialized-msg (iel--deserialize msg))
         (identities (car deserialized-msg))
         (msg (cadr deserialized-msg))
         (parent-header (cdr (assoc 'header msg))))
    (when (equal (cdr (assoc 'msg_type parent-header)) "kernel_info_request")
      (let* ((content '(("protocol_version" . "5.3")
                        ("implementation" . "ielisp")
                        ("implementation_version" . "0.1")
                        ("language_info" .
                         '(("name" . "elisp")
                           ("version" . "unknown")
                           ("mimetype" . "text/plain")
                           ("file_extension" . ".el")))
                        ("banner" . ""))))
        (iel--send iel--shell-socket "kernel_info_reply" content parent-header nil identities)))))
                       
    ;; (when (eq (cdr (assoc "msg_type" parent-header)) "execute_request")
    ;;   ;; publish that we are busy on IOPub
    ;;   (iel--send
    ;;    iel--iopub-socket
    ;;    "status"
    ;;    '(("execution_state" . "busy"))
    ;;    parent-header
    ;;    nil
    ;;    nil)
    ;;   ;; publish the result
    ;;   (iel--send
    ;;    iel--iopub-socket
    ;;    "execute_result"
    ;;    `(("execution_count" . ,iel--execution-count)
    ;;      ("data" . (("text/plain" . "result!")))
    ;;      ("metatadata" . nil))
    ;;    parent-header
    ;;    nil
    ;;    nil) ; parent header
    ;;   ;; reply
    ;;   (let* ((metadata nilp)
    ;;          (content `(("status" . "ok")
    ;;                     ("execution_count" . ,iel--execution-count)
    ;;                     ("user_variables" . nil)
    ;;                     ("payload" . [])
    ;;                     ("user_expressions" . nil))))
    ;;     (iel--send
    ;;      iel--shell-socket
    ;;      "execute_reply"
    ;;      content
    ;;      parent-header
    ;;      metadata       
    ;;      identities))
    ;;   (+ 1 iel--execution-count))))

(setq iel--session-id (iel--uuidgen)) ;; per session UUID
(setq iel--delimiter "<IDS|MSG>") ;; the delimiter between identities and message
(setq iel--execution-count 0)

(let* ((iel--context (zmq-context))
       (iel--shell-socket (zmq-socket iel--context zmq-ROUTER))
       (iel--iopub-socket (zmq-socket iel--context zmq-PUB))
       (iel--stdin-socket (zmq-socket iel--context zmq-ROUTER))
       (iel--control-socket (zmq-socket iel--context zmq-ROUTER))
       (iel--hb-socket (zmq-socket iel--context zmq-REP)))

  (zmq-bind iel--shell-socket (iel--bind-addr "shell_port"))
  (zmq-bind iel--iopub-socket (iel--bind-addr "iopub_port"))
  (zmq-bind iel--stdin-socket (iel--bind-addr "stdin_port"))
  (zmq-bind iel--control-socket (iel--bind-addr "control_port"))
  (zmq-bind iel--hb-socket (iel--bind-addr "hb_port"))

  (let (msg)
    (while t
      (setq msg (zmq-recv iel--hb-socket))
      (zmq-send iel--hb-socket msg)
      (setq msg (zmq-recv-multipart iel--shell-socket))
      (iel--shell-handler msg)))
)
