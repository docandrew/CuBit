#ifndef PENNY_AUDIO_HUB_H
#define PENNY_AUDIO_HUB_H
#include <gst/gst.h>
typedef struct PennyAudioHub PennyAudioHub;
typedef struct PennyAudioInput PennyAudioInput;
/* One hub owns one device sink. It outlives its inputs. Control operations
 * (new/add/remove/free) are serialized by the owner. push may run concurrently
 * with remove, provided its caller retains the input until push returns. */
PennyAudioHub *penny_audio_hub_new(GstElement *output);
PennyAudioInput *penny_audio_hub_add(PennyAudioHub *hub);
GstFlowReturn penny_audio_input_push(PennyAudioInput *,GstBuffer *buffer);
/* Single producer, serialized with finish. Always consumes buffer. A full
 * queue returns GST_FLOW_CUSTOM_SUCCESS without enqueueing it. */
GstFlowReturn penny_audio_input_try_push(PennyAudioInput *,GstBuffer *buffer);
/* Finish is serialized with this input's pushes. Drained: 1 complete,
 * 0 pending, -1 unavailable/error. Other inputs may continue indefinitely. */
GstFlowReturn penny_audio_input_finish(PennyAudioInput *);
gint penny_audio_input_drained(PennyAudioInput *);
void penny_audio_input_remove(PennyAudioInput *);
void penny_audio_input_free(PennyAudioInput *);
/* Owner event loop must poll: a failed device stops the common pipeline,
 * unblocking all producers. Failure is sticky for this hub. */
gboolean penny_audio_hub_poll(PennyAudioHub *,GError **error);
void penny_audio_hub_free(PennyAudioHub *);
GstClockTime penny_audio_hub_time(PennyAudioHub *);
guint penny_audio_hub_inputs(PennyAudioHub *);
GstClockTime penny_audio_hub_render_delay(PennyAudioHub *);
#endif
