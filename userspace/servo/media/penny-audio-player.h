#ifndef PENNY_AUDIO_PLAYER_H
#define PENNY_AUDIO_PLAYER_H
#include "penny-audio-sink.h"
typedef struct PennyAudioSession PennyAudioSession;
/* The supplied transport context outlives the session and every player sink. */
PennyAudioSession *penny_audio_session_new(const PennyAudioTransport *,void *);
void penny_audio_session_unref(PennyAudioSession *);
gboolean penny_audio_session_poll(PennyAudioSession *,GError **);
/* Register the process-local factory. Existing players retain their session
 * when it is cleared or replaced; new unbound players fail to start. */
gboolean penny_audio_player_register(PennyAudioSession *);
void penny_audio_player_unregister(PennyAudioSession *);
/* A fixed S16LE/48kHz/stereo sink. Upstream supplies conversion/resampling. */
GstElement *penny_audio_player_new(PennyAudioSession *);
#endif
