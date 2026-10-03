// Initialize the statically linked media registry before starting any threads.
// Media bytes come from Servo's capability-scoped networking and source adapter.
use std::cell::Cell;
use std::ffi::{c_void, CStr};
use std::marker::PhantomData;
use std::rc::Rc;
thread_local! {
    static SESSION: Cell<*mut c_void> = const { Cell::new(std::ptr::null_mut()) };
    static FAILED: Cell<bool> = const { Cell::new(false) };
}
// Declared before Servo in main, so its destructor runs after the engine.
// Rc marker keeps initialization, polling and shutdown on the owner thread.
pub struct AudioGuard { session: *mut c_void, _owner: PhantomData<Rc<()>> }
impl Drop for AudioGuard {
    fn drop(&mut self) {
        SESSION.with(|slot| { assert_eq!(slot.replace(std::ptr::null_mut()), self.session); });
        unsafe { penny_browser_audio_close(self.session); }
    }
}
pub fn poll() {
    if FAILED.with(Cell::get) { return; }
    SESSION.with(|slot| {
        let session = slot.get();
        if session.is_null() { return; }
        let mut error = std::ptr::null_mut();
        if unsafe { penny_audio_session_poll(session, &mut error) } == 0 {
            FAILED.with(|failed| failed.set(true));
            if !error.is_null() {
                let message = unsafe { CStr::from_ptr((*error).message) }.to_string_lossy();
                crate::say(&format!("PENNY-AUDIO: output unavailable: {message}"));
            } else { crate::say("PENNY-AUDIO: output unavailable"); }
        }
        if !error.is_null() { unsafe { gstreamer::glib::ffi::g_error_free(error); } }
    });
}
unsafe extern "C" {
    fn penny_browser_audio_new() -> *mut c_void;
    fn penny_browser_audio_close(session: *mut c_void);
    fn penny_audio_session_poll(session: *mut c_void, error: *mut *mut gstreamer::glib::ffi::GError) -> i32;
    fn gst_plugin_opus_register();
    fn gst_plugin_audioconvert_register();
    fn gst_plugin_audioresample_register();
    fn gst_plugin_audiomixer_register();
    fn gst_plugin_volume_register();
    fn gst_plugin_coreelements_register();
    fn gst_plugin_app_register();
    fn gst_plugin_playback_register();
    fn gst_plugin_typefindfunctions_register();
    fn gst_plugin_videoconvertscale_register();
    fn gst_plugin_matroska_register();
    fn gst_plugin_vpx_register();
}
pub fn initialize() -> AudioGuard {
    crate::cubit_desktop::initialize_native();
    SESSION.with(|slot| assert!(slot.get().is_null(), "media already initialized"));
    FAILED.with(|failed| failed.set(false));
    // No on-disk registry cache, plugin scanning, or external scanner process.
    // Must run before other threads: changing environment later is unsafe.
    unsafe {
        std::env::set_var("GST_REGISTRY_DISABLE", "yes");
        std::env::set_var("GST_PLUGIN_SYSTEM_PATH_1_0", "");
        std::env::set_var("GST_PLUGIN_PATH_1_0", "");
    }
    gstreamer::init().expect("initialize Penny media");
    unsafe {
        gst_plugin_coreelements_register();
        gst_plugin_app_register();
        gst_plugin_playback_register();
        gst_plugin_typefindfunctions_register();
        gst_plugin_videoconvertscale_register();
        gst_plugin_matroska_register();
        gst_plugin_vpx_register();
        gst_plugin_opus_register();
        gst_plugin_audioconvert_register();
        gst_plugin_audioresample_register();
        gst_plugin_audiomixer_register();
        gst_plugin_volume_register();
    }
    let session = unsafe { penny_browser_audio_new() };
    if session.is_null() { crate::say("PENNY-AUDIO: session unavailable"); }
    SESSION.with(|slot| slot.set(session));
    AudioGuard { session, _owner: PhantomData }
}
