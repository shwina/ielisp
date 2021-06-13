;; ielisp.el

(package-initialize)
(require 'json)
(require 'zmq)
(setq debug-on-error t)

(setq iel--connection-info (json-read-file (car argv)))

(defun iel--uuidgen ()
  (replace-regexp-in-string "\n" "" (shell-command-to-string "uuidgen")))

(defun iel--msg-header (msg-type)
  (json-encode-alist
   `((session . ,iel--session-id)
     (msg_id . ,(iel--uuidgen))
     (date . ,(format-time-string "%FT%T%z"))
     (msg_type . ,msg-type)
     (version . "5.0"))))

(defun iel--send (socket msg-type content parent-header metadata identities)

(defun iel--shell-handler (msg)
  (let* (

(defun iel--bind-addr (port)
  (let* ((transport (cdr (assoc 'transport iel--connection-info)))
         (ip (cdr (assoc 'ip iel--connection-info)))
         (port (cdr (assoc-string port iel--connection-info))))
    (format "%s://%s:%s" transport ip port)))

(setq iel--session-id (iel--uuidgen))

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
      (message (format "%s"  msg))
      (message "...")))
)
