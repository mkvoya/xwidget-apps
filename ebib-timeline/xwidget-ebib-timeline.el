;;; xwidget-ebib-timeline.el --- Add timeline to Ebib based on Emacs xwidget -*- lexical-binding:t -*-
;;; Commentary:
;;; This mimics the timeline feature of Zotero.

(require 'websocket)
(require 'org-element)
(require 'elnode)
(require 'xwidget)
(require 'xwidget-dict)

;;; Code:

(defun ebib-timeline/build-node (node children)
  "Generate a NODE with CHILDREN."
  `((t . "heading")  ; type
    (v . ,node)      ; label
    (d . 1)          ; depth
    (p . nil)        ; depth
    (c . ,children)) ; children
  )

(defun ebib-timeline/export-hl-subtree (hl)
  "Export a headline subtree of HL."
  (let* ((title (org-element-property :title hl))
         (children (vector)))
    (dolist (sub-hl (nthcdr 2 hl) children)
      (let ((subtree (ebib-timeline/export-hl-subtree sub-hl)))
        (setq children (vconcat children (vector subtree)))))
    (ebib-timeline/build-node title children)))

(defun ebib-timeline/export-tree ()
  "Export tree of org doc."
  (let ((top-nodes (vector)))
    (org-element-map (org-element-parse-buffer 'headline) 'headline
      (lambda (hl)
        (let ((parent (org-element-property :parent hl)))
          (unless (eq (org-element-type parent) 'headline)
            (setq top-nodes (vconcat top-nodes (vector (ebib-timeline/export-hl-subtree hl))))))))
    (ebib-timeline/build-node "Main" top-nodes)))

(defun ebib-timeline/export-json ()
  "Export json of org doc."
  (json-encode (ebib-timeline/export-tree))
  )

(defun ebib-timeline/export-current-org-to-json ()
  "Export if current buffer is org."
  (if (equal major-mode 'org-mode)
      (ebib-timeline/export-json)
    ""
  ))


;;; Websocket server for xwidget-apps

(defun ebib-timeline/ws-on-message (ws frame)
  "Handle message of FRAME on WS."
  ;; (print (websocket-frame-text frame))
  (let ((json (org-mindmap-live/export-current-org-to-json)))
    ;; (message json)
    (ignore-errors
      (websocket-send-text ws json))))

(defun ebib-timeline/ws-on-close (ws frame)
  "Handle message of FRAME on WS."
  (websocket-send-text
   ws (websocket-frame-text frame)))

(defun ebib-timeline/ws-on-open (ws)
  "Handle open of WS."
  (message "OPEN")
  )


(defun ebib-timeline/start ()
  "Callback on server start."
  (setq xwidget-apps/ws-server-conn
        (websocket-server
         xwidget-apps/ws-server-port
         :host 'local
         :on-open #'xwidget-apps/ws-on-open
         :on-message #'xwidget-apps/ws-on-message)
        )

  (elnode-start #'xwidget-apps/httpd-handler
                :port xwidget-apps/http-server-port :host "localhost")

  (xwidget-webkit-new-session (format "http://localhost:%d/" xwidget-apps/http-server-port))

  (xwidget-dict-enable)
  )

(defun ebib-timeline/stop ()
  "Callback on server stop."
  (xwidget-dict-disable)
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
