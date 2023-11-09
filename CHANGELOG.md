# 1.1.0

- You can now set `OTEL_GHC_PLUGIN_RECORD_PASSES` to have the plugin report
  spans for compilation passes. By default, it will only record the
  module-level spans. By setting it to `t` or `true`, it will record the same level of granularity as the prior version.

# 1.0.0

- Initial release
