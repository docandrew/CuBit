"""Opt-in classic-script compile/execution elapsed timing; no JIT or policy changes."""

def apply(edit):
    edit("components/script/dom/globalscope/script_execution.rs",
        '        let mut source = if let Some(unminified_js_dir) = self.unminified_js_dir() {',
        '        let penny_source_bytes = source.len();\n        let mut source = if let Some(unminified_js_dir) = self.unminified_js_dir() {',
        1)
    edit("components/script/dom/globalscope/script_execution.rs",
        '        rooted!(&in(cx) let compiled_script = unsafe { Compile1(cx, compilation_options.ptr, &mut source) });',
        '        let penny_profile_started = servo_config::opts::get().debug\n            .is_enabled(servo_config::opts::DiagnosticsLoggingOption::ProfileScriptEvents)\n            .then(std::time::Instant::now);\n        rooted!(&in(cx) let compiled_script = unsafe { Compile1(cx, compilation_options.ptr, &mut source) });\n        if let Some(started) = penny_profile_started {\n            warn!("PENNY-JS: stage=compile elapsed_us={} bytes={} success={} source={}",\n                started.elapsed().as_micros(), penny_source_bytes,\n                !compiled_script.get().is_null(), url);\n        }',
        1)
    edit("components/script/dom/globalscope/script_execution.rs",
        '                    result = unsafe { JS_ExecuteScript(cx, record.handle(), return_value) };',
        '                    let penny_profile_started = servo_config::opts::get().debug\n                        .is_enabled(servo_config::opts::DiagnosticsLoggingOption::ProfileScriptEvents)\n                        .then(std::time::Instant::now);\n                    result = unsafe { JS_ExecuteScript(cx, record.handle(), return_value) };\n                    if let Some(started) = penny_profile_started {\n                        warn!("PENNY-JS: stage=execute elapsed_us={} success={} document={}",\n                            started.elapsed().as_micros(), result, self.get_url());\n                    }',
        1)
