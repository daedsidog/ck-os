(defsystem #:ck-os
  :components ((:module "source"
                :components ((:module "win32" :if-feature :win32
                              :components ((:file "common")
                                           (:file "user32" :depends-on ("common"))
                                           (:file "gdi32" :depends-on ("common"))
                                           (:file "os" :depends-on ("common" "user32" "gdi32"))))
                             (:module "unix" :if-feature :unix
                              :components ((:file "os")))
                             (:file "os" :depends-on ("unix" "win32")))))
  :depends-on (#:ck-clle 
               #+(or win32 unix) #:cffi
               #+(or win32 unix) #:trivial-garbage))
