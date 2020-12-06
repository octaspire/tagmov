(asdf:defsystem "tagmov"
  :depends-on (:alexandria :unix-opts :parse-float)
  :components ((:file "tagmov"))
  :build-operation program-op
  :build-pathname "tagmov"
  :entry-point "tagmov:main")
