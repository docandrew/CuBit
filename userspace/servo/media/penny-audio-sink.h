#ifndef PENNY_AUDIO_SINK_H
#define PENNY_AUDIO_SINK_H
#include <gst/gst.h>
/* Transport callbacks run serialized. write/queued must be nonblocking.
 * open creates a stopped 48 kHz stereo S16LE stream; close discards its queue.
 * The browser mixer owns one sink, rather than allocating one per tab.
 * Context remains valid until the returned element is finalized. */
typedef struct {
 gboolean (*open)(void *);
 guint (*write)(void *, const guint8 *, guint frames);
 void (*start)(void *);
 gint64 (*queued)(void *);
 void (*close)(void *);
 /* Optional bounded transport reserve, in 48kHz frames; negative is failure. */
 gint64 (*capacity)(void *);
} PennyAudioTransport;
GstElement *penny_audio_sink_new(const PennyAudioTransport *, void *context);
/* Completed sample-frame position on a continuous 48kHz output timeline.
 * Includes device DMA backlog, not host codec/amplifier latency. */
GstFlowReturn penny_audio_sink_progress(GstElement *,guint64 *frame);
GstClockTime penny_audio_sink_reserve(GstElement *);
#endif
