(asdf:defsystem "tagmov"
  :depends-on (:unix-opts :str)
  :components ((:file "tagmov"))
  :build-operation program-op
  :build-pathname "tagmov"
  :entry-point "tagmov:main")
