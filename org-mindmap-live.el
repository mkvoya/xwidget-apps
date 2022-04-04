;;; org-mindmap-live.el --- Org Mind Map Live Server -*- lexical-binding:t -*-
;;; Commentary:

(require 'websocket)
(require 'org-element)

;;; Code:

(defun org-mindmap-live/build-node (node children)
  "Generate a NODE with CHILDREN."
  `((name . ,node)
    (children . ,children))
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
    (vector (org-mindmap-live/build-node "Main" top-nodes))))

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

(defun org-mindmap-live/ws-on-message (ws frame)
  "Handle message of FRAME on WS."
  (print (websocket-frame-text frame)
  (let ((json (org-mindmap-live/export-current-org-to-json)))
    (message json)
    (websocket-send-text ws json))))

(defun org-mindmap-live/ws-on-close (ws frame)
  "Handle message of FRAME on WS."
  (websocket-send-text
   ws (websocket-frame-text frame)))

(defun org-mindmap-live/ws-on-open (ws)
  "Handle open of WS."
  (message "OPEN")
  )

(defvar org-mindmap-live/ws-server-conn nil
  "Websocket connection.")

(if org-mindmap-live/ws-server-conn
    (websocket-server-close org-mindmap-live/ws-server-conn))

(setq org-mindmap-live/ws-server-conn
      (websocket-server
       12302
       :host 'local
       :on-open #'org-mindmap-live/ws-on-open
       :on-message #'org-mindmap-live/ws-on-message))

;; (websocket-send-text org-mindmap-live/ws-server-conn "HILO")

(provide 'org-mindmap-live)
;;; org-mindmap-live.el ends here
