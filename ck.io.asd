(defsystem #:ck.io
  :components
  ((:module "source"
    :components
    ((:file "environment"))))
  :depends-on (#:ck.core))
