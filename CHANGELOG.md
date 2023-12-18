# 1.2.0

- The plugin now records much more of the module's compilation. Previously, the
  span was only recording the `installCoreToDos` phase. With this version, we
  record starting at the very beginning of compilation (before parsing,
  TemplateHaskell, etc), and we stop recording at the end of all linking steps.
- GHC support for < 9.6 was dropped. Further patches should restore this
  support if desired. 

# 1.1.0

- You can now set `OTEL_GHC_PLUGIN_RECORD_PASSES` to have the plugin report
  spans for compilation passes. By default, it will only record the
  module-level spans. By setting it to `t` or `true`, it will record the same level of granularity as the prior version.

# 1.0.0

- Initial release
