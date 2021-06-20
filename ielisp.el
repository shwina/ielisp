;;; ielisp.el --- Emacs Lisp kernel for Jupyter

;; Copyright (C) 2021 Ashwin Srinath.

;; Author: Ashwin Srinath
;; Version: 0.1
;; Package-Requires: ((zmq "0.10.10"))
;; Keywords: tools
;; URL: https://github.com/shwina/ielisp

;;; Commentary:

;; This package provides an Emacs Lisp Kernel for Jupyter

(package-initialize)
(require 'hex-util)
(require 'hmac-def)
(require 'json)
(require 'zmq)

;; Read the connection file
(setq iel--connection-info (json-read-file (car argv)))

(defun iel--sha256-binary (object)
  "Return the SHA 256 hash (in binary form) of OBJECT, a buffer or a string."
  (secure-hash 'sha256 object nil nil t))

(define-hmac-function iel--hmac-sha256 iel--sha256-binary 64 32)

(defun iel--hmac-sha256-hex (text key)
  "Return the SHA 256 hash (as a hex string) of OBJECT, a buffer or a string."
  (encode-hex-string (iel--hmac-sha256 (string-as-unibyte text) key)))

(defun iel--uuidgen ()
  "Generate a UUID using the shell command `uuidgen'."
  (replace-regexp-in-string "\n" "" (shell-command-to-string "uuidgen")))

(defun iel--handle-interrupt ()
  "Hook to handle SIGINT"
  (throw 'iel-interrupt "Interrupted"))

(defun iel--eval-string (code)
  "Evaluate the string CODE and return its value as a string.
If an error occurs, the corresponding error string is returned"
  (add-hook 'kill-emacs-hook 'iel--handle-interrupt)
  (let ((result (catch 'iel-interrupt
                  (format "%s"
                          (condition-case err
                              (eval (car (read-from-string code)))
                            (error (format "%s" (error-message-string err))))))))
    (remove-hook  'kill-emacs-hook 'iel--handle-interrupt)
    result))

(defun iel--msg-header (msg-type)
  "Generate the message header for a given MSG-TYPE."
   `((session . ,iel--session-id)
     (msg_id . ,(iel--uuidgen))
     (date . ,(format-time-string "%FT%T%z"))
     (msg_type . ,msg-type)
     (username . "ashwint")
     (version . "5.3")))

(defun iel--bind-addr (port)
  "Generates the bind address, for the given port name PORT."
  (let* ((transport (cdr (assoc 'transport iel--connection-info)))
         (ip (cdr (assoc 'ip iel--connection-info)))
         (port (cdr (assoc-string port iel--connection-info))))
    (format "%s://%s:%s" transport ip port)))

(defun iel--sign (msg key)
  "Generate the HMAC signature for MSG, using KEY."
  (iel--hmac-sha256-hex msg key))

(defun iel--send (socket msg-type &optional content parent-header metadata identities)
  "Construct and send a message of type MSG-TYPE on SOCKET."
  (let* ((header (json-encode (iel--msg-header msg-type)))
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

(defun iel--deserialize (msg-string)
  "Deserialize MSG-STRING.
Returns a list whose first element is a list of identities,
and second element is a mapping whose keys are the names
of the message dictionaries, and values are the dictionaries
themselves (alists)."
  (let* ((delimiter-pos (seq-position msg-string iel--delimiter))
         (identities (seq-subseq msg-string 0 delimiter-pos))
         (frames (seq-subseq msg-string (+ 2  delimiter-pos))))
    (list identities `((header  . ,(json-read-from-string (seq-elt frames 0)))
                       (parent_header . ,(json-read-from-string (seq-elt frames 1)))
                       (metadata . ,(json-read-from-string (seq-elt frames 2)))
                       (content . ,(json-read-from-string (seq-elt frames 3)))))))

(defun iel--control-handler (msg)
  "Handle messages on the control socket."
  (let* ((deserialized-msg (iel--deserialize msg))
         (identities (car deserialized-msg))
         (msg (cadr deserialized-msg))
         (msg-type (cdr (assoc 'msg_type (cdr (assoc 'header msg))))))
    (when (equal msg-type "shutdown_request")
      (setq iel--running nil))))

(defun iel--shell-handler (msg)
  "Handle messages on the shell socket."
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

(defconst iel--session-id (iel--uuidgen) "per session UUID")
(defconst iel--delimiter "<IDS|MSG>")
(defvar iel--execution-count 1)
(defvar iel--running 't)

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
  
  (let ((poller (zmq-poller))
        (timeout 1000)
        (msg))
    (zmq-poller-add poller iel--hb-socket (list zmq-POLLIN))
    (zmq-poller-add poller iel--control-socket (list zmq-POLLIN))
    (zmq-poller-add poller iel--shell-socket (list zmq-POLLIN))
    (while iel--running
      (let* ((socks-events (zmq-poller-wait-all poller 1 -1))
             (hb-events (cdr (assoc iel--hb-socket socks-events)))
             (control-events (cdr (assoc iel--control-socket socks-events)))
             (shell-events (cdr (assoc iel--shell-socket socks-events))))
        (when hb-events
          (setq msg (zmq-recv iel--hb-socket))
          (zmq-send iel--hb-socket msg))
        (when control-events
          (setq msg (zmq-recv-multipart iel--control-socket))
          (iel--control-handler msg))
        (when shell-events
          (setq msg (zmq-recv-multipart iel--shell-socket))
          (iel--shell-handler msg))))))

