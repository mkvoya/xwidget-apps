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

(defvar xwidget-apps/httpd-routes
  `(
    ("js/d3@7.js" . ,(elnode-make-send-file "./js/d3@7.js"))
    ("js/markmap-view.js" . ,(elnode-make-send-file "./js/markmap-view.js"))
    ("/" . ,(elnode-make-send-file "./App.html"))))

(cl-defmethod on-ws-message ((app xwidget-app) ws frame)
  "Handle message of FRAME on WS for the app APP."
  )

(cl-defmethod on-ws-close ((app xwidget-app) ws frame)
  "Handle close of FRAME on WS for the app APP."
  )

(cl-defmethod on-ws-open ((app xwidget-app) ws frame)
  "Handle open of FRAME on WS for the app APP."
  )

(cl-defmethod start((app xwidget-app))
  "Start for the app APP."
  )
(cl-defmethod stop((app xwidget-app))
  "Stop for the app APP."
  )


(defun xwidget-apps/httpd-handler (httpcon)
  "The handler to serve html mindmap file through HTTPCON."
  ;; (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  ;; (elnode-send-file httpcon "./App.html")
  (elnode-hostpath-dispatcher httpcon xwidget-apps/httpd-routes))


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

(defvar xwidget-app-list () "A list of xwidget apps.")

(defun xiwdget-apps/register (class)
  "Register an object given CLASS name."
  (add-to-list 'xwidget-app-list (make-instance class))
  )

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

  (-each xwidget-app-list #'(lambda (app) (start app)))

  ;; (xwidget-webkit-new-session (format "http://localhost:%d/" xwidget-apps/http-server-port))

  ;; (xwidget-dict-enable)
  )

(defun xwidget-apps/stop ()
  "Stop the xwidget apps servers."
  ;; (xwidget-dict-disable)
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
