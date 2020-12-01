(asdf:defsystem "tagmov"
  :depends-on (:unix-opts :str :parse-float)
  :components ((:file "tagmov"))
  :build-operation program-op
  :build-pathname "tagmov"
  :entry-point "tagmov:main")
