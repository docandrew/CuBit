"""Preserve uncatchable SpiderMonkey cancellation during known shutdown."""
def apply(edit):
    edit('components/script/dom/bindings/error.rs',
         'use crate::dom::types::QuotaExceededError;',
         'use crate::dom::types::QuotaExceededError;\nuse crate::dom::bindings::inheritance::Castable;\nuse crate::dom::window::Window;\nuse crate::dom::workerglobalscope::WorkerGlobalScope;\nuse crate::event_loop::script_thread::ScriptThread;', 1)
    edit('components/script/dom/bindings/error.rs',
         '        Err(JsEngineError::JSFailed) => unsafe {\n            assert!(JS_IsExceptionPending(cx));\n        },',
         """        Err(JsEngineError::JSFailed) => unsafe {
            // A shutdown interrupt stops execution without throwing a catchable
            // JS exception. Preserve that failure for the binding's false return.
            // Do not manufacture an exception that page code could catch to keep
            // executing after shutdown, or relax the invariant for live globals.
            let closing = if let Some(worker) = global.downcast::<WorkerGlobalScope>() {
                worker.is_closing()
            } else if global.is::<Window>() {
                !ScriptThread::can_continue_running()
            } else {
                false
            };
            if !JS_IsExceptionPending(cx) && closing {
                log::warn!("PENNY-SHUTDOWN: preserving uncatchable JavaScript cancellation");
                return;
            }
            assert!(JS_IsExceptionPending(cx));
        },""", 1)
