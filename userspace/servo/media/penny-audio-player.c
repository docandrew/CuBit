#include "penny-audio-player.h"
#include "penny-audio-hub.h"
#include <gst/base/gstbasesink.h>

struct PennyAudioSession {gint refs;GMutex control;PennyAudioHub *hub;GstElement *pending_output;PennyAudioTransport transport;void *context;gboolean failed;};
typedef struct {
 GstBaseSink parent;
 PennyAudioSession *session;
 PennyAudioInput *input;
 GMutex lock;GCond wake;
 gboolean flushing,anchored,finished;
 GstClockTime media_origin,base_origin;
 guint64 output_origin,generation;
} PennyAudioPlayer;
typedef struct {GstBaseSinkClass parent;} PennyAudioPlayerClass;
G_DEFINE_TYPE(PennyAudioPlayer,penny_audio_player,GST_TYPE_BASE_SINK)
static GMutex factory_lock;
static PennyAudioSession *factory_session;
PennyAudioSession *penny_audio_session_new(const PennyAudioTransport *ops,void *context){
 GstElement *output=penny_audio_sink_new(ops,context);if(!output)return NULL;
 PennyAudioSession *s=g_new0(PennyAudioSession,1);g_mutex_init(&s->control);s->refs=1;
 s->pending_output=output;s->transport=*ops;s->context=context;return s;
}
void penny_audio_session_unref(PennyAudioSession *s){
 if(g_atomic_int_dec_and_test(&s->refs)){if(s->hub)penny_audio_hub_free(s->hub);if(s->pending_output)gst_object_unref(s->pending_output);g_mutex_clear(&s->control);g_free(s);}
}
gboolean penny_audio_session_poll(PennyAudioSession *s,GError **error){
 g_mutex_lock(&s->control);gboolean ok=!s->failed;
 if(s->failed)g_set_error_literal(error,g_quark_from_static_string("penny-audio-session"),1,"Audio output startup failed");
 else if(s->hub)ok=penny_audio_hub_poll(s->hub,error);
 g_mutex_unlock(&s->control);return ok;
}
// Called under the per-player mutex. No producer blocks in appsrc while
// holding it: a full queue is retried through a cancellable condition wait.
static void drop_input(PennyAudioPlayer *s){
 if(s->input){g_mutex_lock(&s->session->control);penny_audio_input_free(s->input);s->input=NULL;
  // All inputs have stopped or completed their own drain. Destroy the live
  // mixer and close its device before allowing another player to reopen it.
  // Other players retain an input while rendering, so their hub stays alive.
  if(penny_audio_hub_inputs(s->session->hub)==0){
   penny_audio_hub_free(s->session->hub);s->session->hub=NULL;
  }
  g_mutex_unlock(&s->session->control);}
 s->anchored=FALSE;s->finished=FALSE;s->generation++;
}
static gboolean ensure_input(PennyAudioPlayer *s){
 if(!s->session)return FALSE;
 if(!s->input){g_mutex_lock(&s->session->control);
  if(!s->session->hub&&!s->session->failed){
   // Hub takes ownership even on failure. Idle sessions have no mixer thread
   // or open device; start it only when the first player starts.
   if(!s->session->pending_output)
    s->session->pending_output=penny_audio_sink_new(&s->session->transport,s->session->context);
   s->session->hub=penny_audio_hub_new(s->session->pending_output);
   s->session->pending_output=NULL;
   s->session->failed=s->session->hub==NULL;
  }
  if(s->session->hub)s->input=penny_audio_hub_add(s->session->hub);
  g_mutex_unlock(&s->session->control);}
 return s->input!=NULL;
}
static gboolean start(GstBaseSink *base){
 PennyAudioPlayer *s=(PennyAudioPlayer *)base;g_mutex_lock(&s->lock);s->flushing=FALSE;
 gboolean ok=ensure_input(s);
 GstClockTime delay=ok?penny_audio_hub_render_delay(s->session->hub):GST_CLOCK_TIME_NONE;
 ok=ok&&GST_CLOCK_TIME_IS_VALID(delay);g_mutex_unlock(&s->lock);
 if(ok)gst_base_sink_set_render_delay(base,delay);
 if(!ok)GST_ELEMENT_ERROR(s,RESOURCE,OPEN_WRITE,("Cannot create browser audio input"),(NULL));
 return ok;
}
static gboolean stop(GstBaseSink *base){
 PennyAudioPlayer *s=(PennyAudioPlayer *)base;g_mutex_lock(&s->lock);s->flushing=TRUE;g_cond_broadcast(&s->wake);drop_input(s);g_mutex_unlock(&s->lock);return TRUE;
}
static gboolean unlock(GstBaseSink *base){
 PennyAudioPlayer *s=(PennyAudioPlayer *)base;
 g_mutex_lock(&s->lock);s->flushing=TRUE;g_cond_broadcast(&s->wake);g_mutex_unlock(&s->lock);
 return TRUE;
}
static gboolean unlock_stop(GstBaseSink *base){
 PennyAudioPlayer *s=(PennyAudioPlayer *)base;g_mutex_lock(&s->lock);s->flushing=FALSE;g_mutex_unlock(&s->lock);return TRUE;
}
// Called from render/wait_event with BaseSink's preroll lock held.
// Release our own mutex so unlock_stop can clear the interruption while the
// base class completes PAUSED. A true flush/stop must never revive old work.
static GstFlowReturn wait_interruption(PennyAudioPlayer *s,GstBaseSink *base,
                                      guint64 generation,gint64 *deadline){
 while(s->flushing&&s->generation==generation){
  g_mutex_unlock(&s->lock);
  GstFlowReturn result=gst_base_sink_wait_preroll(base);
  g_mutex_lock(&s->lock);
  if(result!=GST_FLOW_OK)return result;
  *deadline=g_get_monotonic_time()+2*G_TIME_SPAN_SECOND;
 }
 return s->generation==generation?GST_FLOW_OK:GST_FLOW_FLUSHING;
}
static GstFlowReturn render(GstBaseSink *base,GstBuffer *buffer){
 PennyAudioPlayer *s=(PennyAudioPlayer *)base;
 gsize size=gst_buffer_get_size(buffer);
 if(size%4||!GST_BUFFER_PTS_IS_VALID(buffer)){
  GST_ELEMENT_ERROR(s,STREAM,FORMAT,("Invalid browser audio buffer"),(NULL));return GST_FLOW_ERROR;
 }
 GstClockTime running=gst_segment_to_running_time(&base->segment,GST_FORMAT_TIME,GST_BUFFER_PTS(buffer));
 if(!GST_CLOCK_TIME_IS_VALID(running))return GST_FLOW_OK; // clipped segment
 GstFlowReturn result=GST_FLOW_OK;const char *failure=NULL;
 g_mutex_lock(&s->lock);
 guint64 generation=s->generation;
 gint64 deadline=g_get_monotonic_time()+2*G_TIME_SPAN_SECOND;
 result=wait_interruption(s,base,generation,&deadline);
 if(result!=GST_FLOW_OK){g_mutex_unlock(&s->lock);return result;}
 if(!ensure_input(s)){g_mutex_unlock(&s->lock);GST_ELEMENT_ERROR(s,RESOURCE,OPEN_WRITE,("Cannot create browser audio input"),(NULL));return GST_FLOW_ERROR;}
 if(!s->anchored){
  GstClockTime now=penny_audio_hub_time(s->session->hub);
  if(!GST_CLOCK_TIME_IS_VALID(now)){g_mutex_unlock(&s->lock);GST_ELEMENT_ERROR(s,RESOURCE,FAILED,("Shared audio clock unavailable"),(NULL));return GST_FLOW_ERROR;}
  s->base_origin=gst_element_get_base_time(GST_ELEMENT(s));s->media_origin=running;s->output_origin=gst_util_uint64_scale(now+40*GST_MSECOND,48000,GST_SECOND);s->anchored=TRUE;
 }
 if(running<s->media_origin){g_mutex_unlock(&s->lock);GST_ELEMENT_ERROR(s,STREAM,FORMAT,("Audio timestamp moved backwards without a flush"),(NULL));return GST_FLOW_ERROR;}
 for(gsize offset=0;offset<size;){
  result=wait_interruption(s,base,generation,&deadline);if(result!=GST_FLOW_OK)break;
  // The shared output clock keeps running while this media pipeline pauses.
  // GStreamer shifts this element's base time on resume; shift our output
  // anchor by the same amount, including when resuming a partial buffer.
  GstClockTime current_base=gst_element_get_base_time(GST_ELEMENT(s));
  if(current_base<s->base_origin){failure="Audio base time moved backwards without a flush";result=GST_FLOW_ERROR;break;}
  s->output_origin+=gst_util_uint64_scale_round(current_base-s->base_origin,48000,GST_SECOND);
  s->base_origin=current_base;
  guint64 first=s->output_origin+gst_util_uint64_scale_round(running-s->media_origin,48000,GST_SECOND);
  gsize bytes=MIN(size-offset,(gsize)1920);
  GstBuffer *part=gst_buffer_copy_region(buffer,GST_BUFFER_COPY_MEMORY,offset,bytes);
  if(!part){failure="Cannot reference audio samples";result=GST_FLOW_ERROR;break;}
  GST_BUFFER_PTS(part)=gst_util_uint64_scale(first+offset/4,GST_SECOND,48000);
  GST_BUFFER_DURATION(part)=gst_util_uint64_scale(bytes/4,GST_SECOND,48000);
  result=penny_audio_input_try_push(s->input,part);
  if(result==GST_FLOW_OK){offset+=bytes;deadline=g_get_monotonic_time()+2*G_TIME_SPAN_SECOND;}
  else if(result==GST_FLOW_CUSTOM_SUCCESS){
   gint64 now=g_get_monotonic_time();
   if(now>=deadline){failure="Browser audio input stalled";result=GST_FLOW_ERROR;break;}
   g_cond_wait_until(&s->wake,&s->lock,MIN(deadline,now+2000));result=GST_FLOW_OK;
  }else{failure="Browser audio input rejected samples";break;}
 }
 g_mutex_unlock(&s->lock);
 if(failure)GST_ELEMENT_ERROR(s,RESOURCE,WRITE,("%s",failure),(NULL));
 return result;
}
static GstFlowReturn wait_event(GstBaseSink *base,GstEvent *event){
 GstFlowReturn result=GST_BASE_SINK_CLASS(penny_audio_player_parent_class)->wait_event(base,event);
 if(result!=GST_FLOW_OK||GST_EVENT_TYPE(event)!=GST_EVENT_EOS)return result;
 PennyAudioPlayer *s=(PennyAudioPlayer *)base;const char *failure=NULL;
 g_mutex_lock(&s->lock);
 guint64 generation=s->generation;
 gint64 deadline=g_get_monotonic_time()+2*G_TIME_SPAN_SECOND;
 while(TRUE){
  result=wait_interruption(s,base,generation,&deadline);if(result!=GST_FLOW_OK)break;
  if(!s->input)break;
  if(!s->finished){result=penny_audio_input_finish(s->input);s->finished=TRUE;if(result!=GST_FLOW_OK){failure="Cannot finish browser audio input";break;}}
  gint drained=penny_audio_input_drained(s->input);if(drained==1)break;
  if(drained<0){failure="Browser audio completion unavailable";result=GST_FLOW_ERROR;break;}
  gint64 now=g_get_monotonic_time();if(now>=deadline){failure="Browser audio completion timed out";result=GST_FLOW_ERROR;break;}
  g_cond_wait_until(&s->wake,&s->lock,MIN(deadline,now+2000));
 }
 if(s->flushing||s->generation!=generation)result=GST_FLOW_FLUSHING;
 g_mutex_unlock(&s->lock);if(failure)GST_ELEMENT_ERROR(s,RESOURCE,WRITE,("%s",failure),(NULL));
 return result;
}
static gboolean event(GstBaseSink *base,GstEvent *event){
 PennyAudioPlayer *s=(PennyAudioPlayer *)base;
 if(GST_EVENT_TYPE(event)==GST_EVENT_FLUSH_STOP){g_mutex_lock(&s->lock);drop_input(s);g_mutex_unlock(&s->lock);}
 return GST_BASE_SINK_CLASS(penny_audio_player_parent_class)->event(base,event);
}
static void finalize(GObject *object){
 PennyAudioPlayer *s=(PennyAudioPlayer *)object;
 drop_input(s);if(s->session)penny_audio_session_unref(s->session);g_cond_clear(&s->wake);g_mutex_clear(&s->lock);
 G_OBJECT_CLASS(penny_audio_player_parent_class)->finalize(object);
}
static void penny_audio_player_class_init(PennyAudioPlayerClass *klass){
 GstElementClass *element=GST_ELEMENT_CLASS(klass);GstBaseSinkClass *sink=GST_BASE_SINK_CLASS(klass);
 GstCaps *caps=gst_caps_from_string("audio/x-raw,format=S16LE,rate=48000,channels=2,layout=interleaved");
 gst_element_class_add_pad_template(element,gst_pad_template_new("sink",GST_PAD_SINK,GST_PAD_ALWAYS,caps));gst_caps_unref(caps);
 gst_element_class_set_static_metadata(element,"Penny shared audio input","Sink/Audio","Per-player input to CuBit output","CuBit");
 sink->start=start;sink->stop=stop;sink->unlock=unlock;sink->unlock_stop=unlock_stop;sink->render=render;sink->wait_event=wait_event;sink->event=event;
 G_OBJECT_CLASS(klass)->finalize=finalize;
}
static void penny_audio_player_init(PennyAudioPlayer *s){
 g_mutex_init(&s->lock);g_cond_init(&s->wake);gst_base_sink_set_sync(GST_BASE_SINK(s),TRUE);
 g_mutex_lock(&factory_lock);s->session=factory_session;
 if(s->session)g_atomic_int_inc(&s->session->refs);
 g_mutex_unlock(&factory_lock);
}
GstElement *penny_audio_player_new(PennyAudioSession *session){
 if(!session)return NULL;
 PennyAudioPlayer *s=g_object_new(penny_audio_player_get_type(),NULL);
 if(s->session)penny_audio_session_unref(s->session);
 g_atomic_int_inc(&session->refs);s->session=session;return GST_ELEMENT(s);
}

gboolean penny_audio_player_register(PennyAudioSession *session){
 if(!session)return FALSE;
 GstElementFactory *factory=gst_element_factory_find("pennyaudiosink");
 gboolean compatible=!factory||gst_element_factory_get_element_type(factory)==penny_audio_player_get_type();
 if(factory)gst_object_unref(factory);
 if(!compatible)return FALSE;
 if(!factory&&!gst_element_register(NULL,"pennyaudiosink",GST_RANK_NONE,penny_audio_player_get_type()))return FALSE;
 g_atomic_int_inc(&session->refs);
 g_mutex_lock(&factory_lock);PennyAudioSession *old=factory_session;factory_session=session;g_mutex_unlock(&factory_lock);
 if(old)penny_audio_session_unref(old);
 return TRUE;
}
void penny_audio_player_unregister(PennyAudioSession *session){
 g_mutex_lock(&factory_lock);gboolean matched=factory_session==session;
 if(matched)factory_session=NULL;
 g_mutex_unlock(&factory_lock);
 if(matched&&session)penny_audio_session_unref(session);
}
