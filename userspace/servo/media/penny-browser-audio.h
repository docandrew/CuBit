#ifndef PENNY_BROWSER_AUDIO_H
#define PENNY_BROWSER_AUDIO_H
#include "penny-audio-player.h"
/* Call after native Ada host and GStreamer initialization. Own one session
 * on the browser event-loop thread; poll it there. Creation is lazy.
 * Close after media players retire. Any surviving player retains the transport
 * until it is destroyed; the Ada runtime must outlive all media threads. */
PennyAudioSession *penny_browser_audio_new(void);
void penny_browser_audio_close(PennyAudioSession *);
#endif
