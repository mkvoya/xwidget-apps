;;; org-mindmap-live.el --- Org Mind Map Live Server -*- lexical-binding:t -*-
;;; Commentary:

(require 'websocket)

;;; Code:

(defun org-mindmap/ws-on-message (ws frame)
  "Handle message of FRAME on WS."
  (message "ih")
  (message (websocket-frame-text frame))
  (websocket-send-text
   ws (websocket-frame-text frame)))

(defun org-mindmap/ws-on-close (_ws)
  "Handle message of FRAME on WS."
  (websocket-send-text
   ws (websocket-frame-text frame)))

(defvar org-mindmap/ws-server-conn)

(if org-mindmap/ws-server-conn
    (websocket-server-close org-mindmap/ws-server-conn))
(setq org-mindmap/ws-server-conn
      (websocket-server
       12302
       :host 'local
       :on-message #'org-mindmap/ws-on-message))

(websocket-send-text
 org-mindmap/ws-server-conn
 "HILO")

(provide 'org-mindmap-live)
;;; org-mindmap-live.el ends here
