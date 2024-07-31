(defsystem #:ck-os
  :components ((:module "source"
                :components ((:file "windows" :if-feature :win32)
                             (:file "unix" :if-feature :unix)
                             (:file "os" :depends-on ("unix" "windows")))))
  :depends-on (#:ck-clle 
               (:if-feature (:or :unix :win32) #:cffi)))
