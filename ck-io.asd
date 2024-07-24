(defsystem #:ck-io
  :components ((:module "source"
                :components ((:file "environment")
                             #+os-windows (:file "windows")
                             (:file "io" :depends-on ("environment")))))
  :depends-on (#:ck-clle #:cffi))
