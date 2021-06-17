;; ielisp.el

(package-initialize)

(require 'json)
n(require 'zmq)
(require 'hex-util)
(require 'hmac-def)

(setq debug-on-error t)

(setq iel--connection-info (json-read-file (car argv)))

;; TODOS:
;; 2. Implement kernel interrupt/shutdown
;; 3. Handling errors better
;; 4. Installation

(defun iel--sha256-binary (object)
  (secure-hash 'sha256 object nil nil t))

(define-hmac-function iel--hmac-sha256 iel--sha256-binary 64 32)

(defun iel--hmac-sha256-hex (text key)
   (encode-hex-string (iel--hmac-sha256 text key)))

;; generate UUID
(defun iel--uuidgen ()
  (replace-regexp-in-string "\n" "" (shell-command-to-string "uuidgen")))

;; evaluate string and return result as string
(defun iel--eval-string (code)
  (format "%s" (eval (car (read-from-string code)))))

;; generate a message header given a `msg_type`
(defun iel--msg-header (msg-type)
  (json-encode-alist ;; TODO: maybe don't encode here
   `((session . ,iel--session-id)
     (msg_id . ,(iel--uuidgen))
     (date . ,(format-time-string "%FT%T%z"))
     (msg_type . ,msg-type)
     (username . "ashwint")
     (version . "5.3"))))

;; generate bind address
(defun iel--bind-addr (port)
  (let* ((transport (cdr (assoc 'transport iel--connection-info)))
         (ip (cdr (assoc 'ip iel--connection-info)))
         (port (cdr (assoc-string port iel--connection-info))))
    (format "%s://%s:%s" transport ip port)))

(defun iel--sign (msg key)
    (iel--hmac-sha256-hex msg key))

;; construct and send a message on socket
(defun iel--send (socket msg-type &optional content parent-header metadata identities)
  (let* ((header (iel--msg-header msg-type))
         (delimiter iel--delimiter)
         (content (if (null content) (json-encode-alist nil) (json-encode content)))
         (parent-header (if (null parent-header) (json-encode-alist nil) (json-encode parent-header)))
         (metadata (if (null metadata) (json-encode-alist nil) (json-encode metadata)))
         (key (cdr (assoc 'key iel--connection-info)))
         (signature (iel--sign (concat header parent-header metadata content) key))
         (msgs (append identities (list
                                   delimiter
                                   signature
                                   header
                                   parent-header
                                   metadata
                                   content))))
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
         (parent-header (cdr (assoc 'header msg)))
         (msg-type (cdr (assoc 'msg_type parent-header)))
         (code (cdr (assoc 'code (cdr (assoc 'content msg))))))
    (when (equal msg-type "kernel_info_request")
      (let* ((content '(("protocol_version" . "5.3")
                        ("implementation" . "ielisp")
                        ("implementation_version" . "0.1")
                        ("language_info" .(("name" . "elisp")
                                           ("version" . "unknown")
                                           ("mimetype" . "text/plain")
                                           ("file_extension" . ".el")))
                        ("banner" . ""))))
        (iel--send iel--shell-socket "kernel_info_reply" content parent-header nil identities))
      (let* ((content '(("execution_state" . "idle"))))
        (iel--send iel--iopub-socket "status" content parent-header)))

    (when (equal msg-type "execute_request")
      ;; publish that we are busy on IOPub
      (let* ((content '(("execution_state" . "busy"))))
        (iel--send iel--iopub-socket "status" content parent-header))
      ;; publish the execution input
      (let* ((content `(("execution_count" . ,iel--execution-count)
                        ("code" . ,code))))
        (iel--send iel--iopub-socket "execute_input" content parent-header))
      ;; publish the result
      (let* ((content `(("execution_count" . ,iel--execution-count)
                        ("data" . (("text/plain" . ,(iel--eval-string code))))
                        ("metatadata" . nil))))
        (iel--send iel--iopub-socket "execute_result" content parent-header))
      ;; publish that we are idle on IOPub
      (let* ((content '(("execution_state" . "idle"))))
        (iel--send iel--iopub-socket "status" content parent-header))
      ;; reply
      (let* ((content `(("status" . "ok")
                        ("execution_count" . ,iel--execution-count)
                        ("user_variables" . ,(json-encode-alist nil))
                        ("payload" . [])
                        ("user_expressions" . ,(json-encode-alist nil)))))
        (iel--send iel--shell-socket "execute_reply" content parent-header nil identities))
      (setq iel--execution-count (+ 1 iel--execution-count)))))

(setq iel--session-id (iel--uuidgen)) ;; per session UUID
(setq iel--delimiter "<IDS|MSG>") ;; the delimiter between identities and message
(setq iel--execution-count 1)

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
      (iel--shell-handler msg))))
