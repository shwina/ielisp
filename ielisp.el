;; ielisp.el

(package-initialize)
(require 'json)
(require 'zmq)
(setq debug-on-error t)

(setq iel--connection-info (json-read-file (car argv)))


(defun iel--bind-addr (port)
  (let* ((transport (cdr (assoc 'transport iel--connection-info)))
         (ip (cdr (assoc 'ip iel--connection-info)))
         (port (cdr (assoc-string port iel--connection-info))))
    (format "%s://%s:%s" transport ip port)))


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
      (sleep-for 1)
      (message msg)))
)
