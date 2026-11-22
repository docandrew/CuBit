#include "penny-audio-sink.h"
#include <gst/base/gstbasesink.h>
#include <limits.h>

typedef struct {
 GstBaseSink parent;
 GMutex lock;
 GCond wake;
 PennyAudioTransport transport;
 void *context;
 gboolean opened, started, flushing;
 guint64 submitted, end_frame;
} PennyAudioSink;
typedef struct { GstBaseSinkClass parent; } PennyAudioSinkClass;
G_DEFINE_TYPE(PennyAudioSink, penny_audio_sink, GST_TYPE_BASE_SINK)

static gboolean start(GstBaseSink *base) {
 PennyAudioSink *s=(PennyAudioSink *)base;
 g_mutex_lock(&s->lock);
 s->flushing=FALSE;s->started=FALSE;s->submitted=0;s->end_frame=0;
 s->opened=s->transport.open(s->context);
 gboolean ok=s->opened;
 g_mutex_unlock(&s->lock);
 if(!ok)GST_ELEMENT_ERROR(s,RESOURCE,OPEN_WRITE,("Cannot open audio output"),(NULL));
 return ok;
}
static gboolean stop(GstBaseSink *base) {
 PennyAudioSink *s=(PennyAudioSink *)base;
 g_mutex_lock(&s->lock);
 s->flushing=TRUE;g_cond_broadcast(&s->wake);
 if(s->opened)s->transport.close(s->context);
 s->opened=FALSE;s->started=FALSE;
 g_mutex_unlock(&s->lock);
 return TRUE;
}
static gboolean unlock(GstBaseSink *base) {
 PennyAudioSink *s=(PennyAudioSink *)base;
 g_mutex_lock(&s->lock);s->flushing=TRUE;g_cond_broadcast(&s->wake);g_mutex_unlock(&s->lock);
 return TRUE;
}
static gboolean unlock_stop(GstBaseSink *base) {
 PennyAudioSink *s=(PennyAudioSink *)base;
 g_mutex_lock(&s->lock);s->flushing=FALSE;g_mutex_unlock(&s->lock);
 return TRUE;
}
// Own no copied PCM queue: retain only the current upstream buffer until its
// suffix is accepted. A stopped or failed output cannot spin or hold teardown.
static GstFlowReturn render(GstBaseSink *base,GstBuffer *buffer) {
 PennyAudioSink *s=(PennyAudioSink *)base;
 GstMapInfo map;
 if(!gst_buffer_map(buffer,&map,GST_MAP_READ)){
  GST_ELEMENT_ERROR(s,RESOURCE,READ,("Cannot map audio buffer"),(NULL));return GST_FLOW_ERROR;
 }
 if(map.size%4 || map.size/4>UINT_MAX){
  gst_buffer_unmap(buffer,&map);
  GST_ELEMENT_ERROR(s,STREAM,FORMAT,("Invalid stereo audio buffer size"),(NULL));return GST_FLOW_ERROR;
 }
 if(!GST_BUFFER_PTS_IS_VALID(buffer)){
  gst_buffer_unmap(buffer,&map);
  GST_ELEMENT_ERROR(s,STREAM,FORMAT,("Missing audio output timestamp"),(NULL));return GST_FLOW_ERROR;
 }
 guint64 first_frame=GST_BUFFER_OFFSET_IS_VALID(buffer)?GST_BUFFER_OFFSET(buffer):
  gst_util_uint64_scale_round(GST_BUFFER_PTS(buffer),48000,GST_SECOND);
 const char *failure=NULL;
 guint offset=0,frames=map.size/4;
 GstFlowReturn result=GST_FLOW_OK;
 gint64 deadline=g_get_monotonic_time()+2*G_TIME_SPAN_SECOND;
 g_mutex_lock(&s->lock);
 if(s->submitted&&first_frame!=s->end_frame){
  failure="Discontinuous audio output timeline";result=GST_FLOW_ERROR;
 }
 while(result==GST_FLOW_OK&&offset<frames) {
  if(s->flushing){result=GST_FLOW_FLUSHING;break;}
  if(!s->opened){failure="Audio output is closed";result=GST_FLOW_ERROR;break;}
  guint accepted=s->transport.write(s->context,map.data+(gsize)offset*4,frames-offset);
  if(accepted>frames-offset){failure="Invalid audio write acknowledgement";result=GST_FLOW_ERROR;break;}
  if(accepted){
   offset+=accepted;s->submitted+=accepted;s->end_frame=first_frame+offset;deadline=g_get_monotonic_time()+2*G_TIME_SPAN_SECOND;
   if(!s->started){s->transport.start(s->context);s->started=TRUE;}
  }else{
   gint64 now=g_get_monotonic_time();
   if(now>=deadline){failure="Audio output made no progress";result=GST_FLOW_ERROR;break;}
   g_cond_wait_until(&s->wake,&s->lock,MIN(deadline,now+2000));
  }
 }
 g_mutex_unlock(&s->lock);gst_buffer_unmap(buffer,&map);
 // Posting may invoke a synchronous bus handler; never do it under our lock.
 if(failure)GST_ELEMENT_ERROR(s,RESOURCE,WRITE,("%s",failure),(NULL));
 return result;
}
static GstFlowReturn wait_event(GstBaseSink *base,GstEvent *event) {
 GstFlowReturn result=GST_BASE_SINK_CLASS(penny_audio_sink_parent_class)->wait_event(base,event);
 if(result!=GST_FLOW_OK || GST_EVENT_TYPE(event)!=GST_EVENT_EOS)return result;
 PennyAudioSink *s=(PennyAudioSink *)base;
 const char *failure=NULL;
 g_mutex_lock(&s->lock);
 gint64 deadline=g_get_monotonic_time()+2*G_TIME_SPAN_SECOND;
 for(;;){
  if(s->flushing){result=GST_FLOW_FLUSHING;break;}
  if(!s->opened){failure="Audio output closed before drain";result=GST_FLOW_ERROR;break;}
  gint64 queued=s->transport.queued(s->context);
  if(queued<0){failure="Audio playback status unavailable";result=GST_FLOW_ERROR;break;}
  if(!queued)break;
  gint64 now=g_get_monotonic_time();
  if(now>=deadline){failure="Audio output drain timed out";result=GST_FLOW_ERROR;break;}
  g_cond_wait_until(&s->wake,&s->lock,MIN(deadline,now+2000));
 }
 g_mutex_unlock(&s->lock);
 // EOS event failures do not automatically create a pipeline error message.
 if(failure)GST_ELEMENT_ERROR(s,RESOURCE,WRITE,("Cannot drain audio output"),("%s",failure));
 return result;
}
static gboolean event(GstBaseSink *base,GstEvent *event) {
 PennyAudioSink *s=(PennyAudioSink *)base;
 if(GST_EVENT_TYPE(event)==GST_EVENT_FLUSH_STOP){
  g_mutex_lock(&s->lock);
  if(s->opened){
   s->transport.close(s->context);s->opened=s->transport.open(s->context);s->started=FALSE;s->submitted=0;s->end_frame=0;
  }
  gboolean ok=s->opened;
  g_mutex_unlock(&s->lock);
  if(!ok){
   GST_ELEMENT_ERROR(s,RESOURCE,OPEN_WRITE,("Cannot reopen audio output after flush"),(NULL));
   gst_event_unref(event);return FALSE;
  }
 }
 return GST_BASE_SINK_CLASS(penny_audio_sink_parent_class)->event(base,event);
}
static void finalize(GObject *object) {
 PennyAudioSink *s=(PennyAudioSink *)object;
 g_cond_clear(&s->wake);g_mutex_clear(&s->lock);
 G_OBJECT_CLASS(penny_audio_sink_parent_class)->finalize(object);
}
static void penny_audio_sink_class_init(PennyAudioSinkClass *klass) {
 GstElementClass *element=GST_ELEMENT_CLASS(klass);
 GstBaseSinkClass *sink=GST_BASE_SINK_CLASS(klass);
 GstCaps *caps=gst_caps_from_string("audio/x-raw,format=S16LE,rate=48000,channels=2,layout=interleaved");
 gst_element_class_add_pad_template(element,gst_pad_template_new("sink",GST_PAD_SINK,GST_PAD_ALWAYS,caps));gst_caps_unref(caps);
 gst_element_class_set_static_metadata(element,"Penny mixer output","Sink/Audio","Bounded CuBit audio transport","CuBit");
 sink->start=start;sink->stop=stop;sink->unlock=unlock;sink->unlock_stop=unlock_stop;
 sink->render=render;sink->wait_event=wait_event;sink->event=event;
 G_OBJECT_CLASS(klass)->finalize=finalize;
}
static void penny_audio_sink_init(PennyAudioSink *s) {
 g_mutex_init(&s->lock);g_cond_init(&s->wake);
 gst_base_sink_set_sync(GST_BASE_SINK(s),TRUE);
}
GstElement *penny_audio_sink_new(const PennyAudioTransport *transport,void *context) {
 if(!transport || !transport->open || !transport->write || !transport->start || !transport->queued || !transport->close)return NULL;
 PennyAudioSink *s=g_object_new(penny_audio_sink_get_type(),NULL);
 s->transport=*transport;s->context=context;return GST_ELEMENT(s);
}

GstFlowReturn penny_audio_sink_progress(GstElement *element,guint64 *frame) {
 if(!element||!G_TYPE_CHECK_INSTANCE_TYPE(element,penny_audio_sink_get_type())||!frame)return GST_FLOW_ERROR;
 PennyAudioSink *s=(PennyAudioSink *)element;GstFlowReturn result=GST_FLOW_OK;
 g_mutex_lock(&s->lock);
 if(!s->opened||s->flushing)result=GST_FLOW_FLUSHING;
 else if(!s->submitted)*frame=0;
 else{
  gint64 queued=s->transport.queued(s->context);
  if(queued<0||(guint64)queued>s->submitted)result=GST_FLOW_ERROR;
  else *frame=s->end_frame-(guint64)queued;
 }
 g_mutex_unlock(&s->lock);return result;
}

GstClockTime penny_audio_sink_reserve(GstElement *element){
 if(!element||!G_TYPE_CHECK_INSTANCE_TYPE(element,penny_audio_sink_get_type()))return GST_CLOCK_TIME_NONE;
 PennyAudioSink *s=(PennyAudioSink *)element;
 g_mutex_lock(&s->lock);
 gint64 frames=s->opened?(s->transport.capacity?s->transport.capacity(s->context):0):-1;
 g_mutex_unlock(&s->lock);
 if(frames<0||frames>10240)return GST_CLOCK_TIME_NONE;
 return gst_util_uint64_scale((guint64)frames,GST_SECOND,48000);
}
