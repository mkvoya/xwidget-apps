;;; xwidget-apps.el --- Apps based on Emacs xwidget -*- lexical-binding:t -*-
;;; Current available apps:
;;;   - Org Mind Map Live (org-mindmap-live)
;;; Commentary:

(require 'websocket)
(require 'org-element)
(require 'elnode)
(require 'xwidget)

;;; Code:

(defun org-mindmap-live/build-node (node children)
  "Generate a NODE with CHILDREN."
  `((t . "heading")  ; type
    (v . ,node)      ; label
    (d . 1)          ; depth
    (p . nil)        ; depth
    (c . ,children)) ; children
  )

(defun org-mindmap-live/export-hl-subtree (hl)
  "Export a headline subtree of HL."
  (let* ((title (org-element-property :title hl))
         (children (vector)))
    (dolist (sub-hl (nthcdr 2 hl) children)
      (let ((subtree (org-mindmap-live/export-hl-subtree sub-hl)))
        (setq children (vconcat children (vector subtree)))))
    (org-mindmap-live/build-node title children)))

(defun org-mindmap-live/export-tree ()
  "Export tree of org doc."
  (let ((top-nodes (vector)))
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (hl)
        (let ((parent (org-element-property :parent hl)))
          (unless (eq (org-element-type parent) 'headline)
            (setq top-nodes (vconcat top-nodes (vector (org-mindmap-live/export-hl-subtree hl))))))))
    (org-mindmap-live/build-node "Main" top-nodes)))

(defun org-mindmap-live/export-json ()
  "Export json of org doc."
  (json-encode (org-mindmap-live/export-tree))
  )

(defun org-mindmap-live/export-current-org-to-json ()
  "Export if current buffer is org."
  (if (equal major-mode 'org-mode)
      (org-mindmap-live/export-json)
    ""
  ))


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

(defun xwidget-apps/ws-on-open (ws)
  "Handle open of WS."
  (message "OPEN")
  )

(defun xwidget-apps/httpd-handler (httpcon)
  "The handler to serve html mindmap file through HTTPCON."
  ;; (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
  ;; (elnode-send-file httpcon "./App.html")
  (elnode-hostpath-dispatcher httpcon xwidget-apps/httpd-routes))


(defun xwidget-apps/start ()
  "Start xwidget apps servers."
  (xwidget-apps/stop)
  (setq xwidget-apps/ws-server-conn
        (websocket-server
         xwidget-apps/ws-server-port
         :host 'local
         :on-open #'xwidget-apps/ws-on-open
         :on-message #'xwidget-apps/ws-on-message))

  (elnode-start #'xwidget-apps/httpd-handler
                :port xwidget-apps/http-server-port :host "localhost")

  (xwidget-webkit-new-session (format "http://localhost:%d/" xwidget-apps/http-server-port))
  )

(defun xwidget-apps/stop ()
  "Stop the xwidget apps servers."
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
