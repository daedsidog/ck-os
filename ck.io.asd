(defsystem #:ck.io
  :components ((:module "source"
                :components ((:file "environment")
                             (:file "io" :depends-on ("environment")))))
  :depends-on (#:ck.clle))
