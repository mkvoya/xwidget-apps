;;; xwidget-apps.el --- Apps based on Emacs xwidget -*- lexical-binding:t -*-
;;; Current available apps:
;;;   - Org Mind Map Live (org-mindmap-live)
;;; Commentary:

(require 'websocket)
(require 'org-element)
(require 'elnode)
(require 'xwidget)
(require 'xwidget-dict)

;;; Code:

;;; Websocket server for xwidget-apps

(defcustom xwidget-apps/ws-server-port 12302
  "Websocket server port."
  :type 'number)

(defcustom xwidget-apps/http-server-port 12301
  "Http server port."
  :type 'number)

(defvar xwidget-apps/ws-server-conn nil
  "Websocket connection.")

(cl-defstruct xwidget-app
  name
  httpd-path
  httpd-dispatch
  start
  stop
  ws-on-message
  ws-on-open
  ws-on-close
  )

(defvar xwidget-app-list (list) "A list of xwidget apps.")

(defun xwidget-apps/httpd-handler (httpcon)
  "The handler to serve html mindmap file through HTTPCON."
  ;; (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  ;; (elnode-send-file httpcon "./App.html")

  (let* ((path (elnode-http-pathinfo httpcon))
         ;; find the app serving the path
         (app (-find #'(lambda (x) (string-prefix-p (xwidget-app-httpd-path x) path))
                     xwidget-app-list)))
    (if app
        ;; use app's httpd routes
        (funcall (xwidget-app-httpd-dispatch app) httpcon)
      (elnode-http-start httpcon 404)
      )
    ))

(defun xwidget-apps/ws-on-message (ws frame)
  "Handle message of FRAME on WS."
  ;; (print (websocket-frame-text frame))
  (let ((json (org-mindmap-live/export-current-org-to-json)))
    ;; (message json)
    (ignore-errors
      (websocket-send-text ws json))))

(defun xwidget-apps/ws-on-close (ws frame)
  "Handle message of FRAME on WS."
  (websocket-send-text
   ws (websocket-frame-text frame)))

;;; Abstract class for xwidget app.
(defclass xwidget-app () ; No superclasses
  ()
  "An abstract class for xwidget-app."
  )

(defun xwidget-apps/ws-on-open (ws)
  "Handle open of WS."
  (message "OPEN"))

(defun reduce-class-method (method object)
  "Reduce the first parameter of METHOD  with given OBJECT and passes REST"
  (lambda (&rest args)
    (apply method object args))
  )

(defun xwidget-apps/start ()
  "Start xwidget apps servers."
  (xwidget-apps/stop)
  (setq xwidget-apps/ws-server-conn
        (websocket-server
         xwidget-apps/ws-server-port
         :host 'local
         :on-open #'xwidget-apps/ws-on-open
         :on-message #'xwidget-apps/ws-on-message)
        )

  (elnode-start #'xwidget-apps/httpd-handler
                :port xwidget-apps/http-server-port :host "localhost")

  (-each xwidget-app-list #'(lambda (app) (funcall (xwidget-app-start app))))

  ;; (xwidget-webkit-new-session (format "http://localhost:%d/" xwidget-apps/http-server-port))

  )

(defun xwidget-apps/stop ()
  "Stop the xwidget apps servers."
  (-each xwidget-app-list #'(lambda (app) (funcall (xwidget-app-stop app))))
  (if xwidget-apps/ws-server-conn
      (progn
        (websocket-server-close xwidget-apps/ws-server-conn)
        (setq xwidget-apps/ws-server-conn nil))
    )
  (elnode-stop xwidget-apps/http-server-port)
  )

;; (websocket-send-text org-mindmap-live/ws-server-conn "HILO")

;;;###autoload
(define-minor-mode xwidget-apps-mode
  "Xwidget-apps enables some plugins that displays html content via xwidget."
  :global t
  (if xwidget-apps-mode
      (xwidget-apps/start)
    (xwidget-apps/stop)))

(provide 'xwidget-apps)
;;; xwidget-apps.el ends here
