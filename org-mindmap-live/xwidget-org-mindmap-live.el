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

(defclass org-mindmap-live (xwidget-app)
  ()
  "The org-mindmap-live app inheriting xwidget-app.")

(cl-defmethod on-ws-message ((app org-mindmap-live) ws frame)
  "Handle message of FRAME on WS for the app APP."
  ;; (print (websocket-frame-text frame))
  (let ((json (org-mindmap-live/export-current-org-to-json)))
    ;; (message json)
    (ignore-errors
      (websocket-send-text ws json))))
  )

(cl-defmethod on-ws-close ((app org-mindmap-live) ws frame)
  "Handle close of FRAME on WS for the app APP."
  (websocket-send-text
   ws (websocket-frame-text frame)))
  )

(cl-defmethod on-ws-open ((app org-mindmap-live) ws frame)
  "Handle open of FRAME on WS for the app APP."
  (message "OPEN on org-mindmap-live")
  )

(cl-defmethod start ((app org-mindmap-live))
"Start."
()
)

(provide 'xwidget-org-mindmap-live)
;;; xwidget-apps.el ends here
