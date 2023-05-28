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

(defun org-mindmap-live/ws-on-message (ws frame)
  "Handle message of FRAME on WS."
  ;; (print (websocket-frame-text frame))
  (let ((json (org-mindmap-live/export-current-org-to-json)))
    ;; (message json)
    (ignore-errors
      (websocket-send-text ws json))))

(defun org-mindmap-live/ws-on-close (ws frame)
  "Handle close of FRAME on WS."
  (websocket-send-text
   ws (websocket-frame-text frame)))

(defun org-mindmap-live/ws-on-open (ws frame)
  "Handle open of FRAME on WS."
  (message "OPEN on org-mindmap-live")
  )

(defun org-mindmap-live/start ()
  "Start org-mindmap-live."
  (message "start org-mindmap-live.")
  )

(defun org-mindmap-live/stop ()
  "Stop org-mindmap-live."
  (message "stop org-mindmap-live.")
  )


(defconst org-mindmap-live/name "org-mindmap-live")
(defconst org-mindmap-live/httpd-path "/org-mindmap-live")
(defconst org-mindmap-live/base-path (file-name-directory
                                      (or load-file-name buffer-file-name)))

(defun org-mindmap-live/httpd-dispatch (httpcon)
  "Dispatch HTTPCON."
  (let* ((path (elnode-http-pathinfo httpcon))
         (relpath (substring path (length org-mindmap-live/httpd-path)))
         (filepath nil))
    (cl-assert (string-prefix-p org-mindmap-live/httpd-path path)
               t "path is wrong: %s")
    (print relpath)
    (print (expand-file-name "."))
    (cond
     ((string= relpath "/")
      (setq filepath "./App.html"))
     ((string= relpath "/js/d3@7.js")
      (setq filepath "./js/d3@7.js"))
     ((string= relpath "/js/markmap-view.js")
      (setq filepath "./js/markmap-view.js"))
     )
    (if filepath
        (progn
          (elnode-http-start httpcon 200 '("Content-type" . "text/html"))
          (elnode-send-file httpcon (expand-file-name filepath org-mindmap-live/base-path))))
        (elnode-http-start httpcon 404)))


(defun org-mindmap-live/register (class)
  "Register an object given CLASS name."
  (add-to-list 'xwidget-app-list
               (make-xwidget-app
                :name org-mindmap-live/name
                :httpd-path org-mindmap-live/httpd-path
                :httpd-dispatch #'org-mindmap-live/httpd-dispatch
                :start #'org-mindmap-live/start
                :stop #'org-mindmap-live/stop
                :ws-on-open #'org-mindmap-live/ws-on-open
                :ws-on-close #'org-mindmap-live/ws-on-close
                :ws-on-message #'org-mindmap-live/ws-on-message
                ))
  )

(defun org-mindmap-live/unregister (class)
  "Unregister an object given CLASS name."
  (setq xwidget-app-list
        (-remove (lambda (app) (string= "org-mindmap-live" (xwidget-app-name app)))
                 xwidget-app-list))
  )


(provide 'xwidget-org-mindmap-live)
;;; xwidget-apps.el ends here
