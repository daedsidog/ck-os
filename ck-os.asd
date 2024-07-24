(defsystem #:ck-os
  :components ((:module "source"
                :components (#+win32 (:file "windows")
                             #+unix (:file "unix")
                             (:file "os" :depends-on (#+unix "unix"
                                                      #+win32 "windows")))))
  :depends-on (#:ck-clle
               #+(or win32 unix) #:cffi))
