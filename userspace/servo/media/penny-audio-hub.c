#include "penny-audio-hub.h"
#include "penny-audio-sink.h"
#include <gst/app/gstappsrc.h>
#include <gst/base/gstbasesink.h>
#include <gst/base/gstaggregator.h>
struct PennyAudioHub { GstElement *pipeline,*mixer,*output; GstBus *bus; GError *failure; guint inputs; };
struct PennyAudioInput {
 GMutex lock;
 PennyAudioHub *hub;
 GstElement *source;
 GstPad *pad;
 gboolean finished, has_data;
 guint64 end_frame;
};
PennyAudioHub *penny_audio_hub_new(GstElement *output) {
 if(!output)return NULL;
 if(!GST_IS_BASE_SINK(output)){gst_object_unref(output);return NULL;}
 // This live output has no preroll requirement; individual media players
 // retain their own preroll independently.
 gst_base_sink_set_async_enabled(GST_BASE_SINK(output),FALSE);
 // Keep the idle output clock-paced, but fill ahead of presentation so the
 // device ring has a bounded reserve instead of receiving samples just in time.
 gst_base_sink_set_ts_offset(GST_BASE_SINK(output),-20*(gint64)GST_MSECOND);

 PennyAudioHub *h=g_new0(PennyAudioHub,1);
 h->output=output;h->pipeline=gst_pipeline_new(NULL);
 h->mixer=gst_element_factory_make_full("audiomixer","force-live",TRUE,NULL);
 if(!h->pipeline||!h->mixer){
  if(h->pipeline)gst_object_unref(h->pipeline);
  if(h->mixer)gst_object_unref(h->mixer);
  gst_object_unref(output);g_free(h);return NULL;
 }
 // ZERO keeps first_buffer pending during forced-live silence; the first
 // arriving input can then reset an already-running output position to zero.
 // NOW selects the live origin before emitting silence and clears that flag.
 // Allow bounded scheduling jitter before the live mixer discards late input.
 // Keep this wait budget visible to each player through render_delay below.
 g_object_set(h->mixer,"ignore-inactive-pads",TRUE,"start-time-selection",GST_AGGREGATOR_START_TIME_SELECTION_NOW,"latency",(guint64)(40*GST_MSECOND),NULL);
 gst_bin_add_many(GST_BIN(h->pipeline),h->mixer,output,NULL);
 if(!gst_element_link(h->mixer,output)||gst_element_set_state(h->pipeline,GST_STATE_PLAYING)==GST_STATE_CHANGE_FAILURE){
  gst_element_set_state(h->pipeline,GST_STATE_NULL);gst_object_unref(h->pipeline);g_free(h);return NULL;
 }
 h->bus=gst_element_get_bus(h->pipeline);
 return h;
}
PennyAudioInput *penny_audio_hub_add(PennyAudioHub *h) {
 if(h->failure)return NULL;
 PennyAudioInput *i=g_new0(PennyAudioInput,1);g_mutex_init(&i->lock);i->hub=h;
 i->source=gst_element_factory_make("appsrc",NULL);
 if(!i->source){g_mutex_clear(&i->lock);g_free(i);return NULL;}
 GstCaps *caps=gst_caps_from_string("audio/x-raw,format=S16LE,rate=48000,channels=2,layout=interleaved");
 gst_app_src_set_caps(GST_APP_SRC(i->source),caps);gst_caps_unref(caps);
 // Bounded per-input buffering. Removal first stops appsrc, releasing a
 // producer waiting for room; it never waits while holding the input mutex.
 g_object_set(i->source,"format",GST_FORMAT_TIME,"is-live",TRUE,"max-bytes",(guint64)7680,"max-buffers",(guint64)4,"block",TRUE,NULL);
 gst_bin_add(GST_BIN(h->pipeline),i->source);
 i->pad=gst_element_request_pad_simple(h->mixer,"sink_%u");
 GstPad *src=gst_element_get_static_pad(i->source,"src");
 gboolean linked=i->pad&&gst_pad_link(src,i->pad)==GST_PAD_LINK_OK;gst_object_unref(src);
 if(!linked||!gst_element_sync_state_with_parent(i->source)){
  gst_element_set_state(i->source,GST_STATE_NULL);
  if(i->pad){gst_element_release_request_pad(h->mixer,i->pad);gst_object_unref(i->pad);}
  gst_bin_remove(GST_BIN(h->pipeline),i->source);g_mutex_clear(&i->lock);g_free(i);return NULL;
 }
 h->inputs++;return i;
}
static GstFlowReturn push(PennyAudioInput *i,GstBuffer *buffer,gboolean wait) {
 // At most 40ms in one push: appsrc's byte limit alone allows a single large
 // buffer to exceed the limit. PTS is on the hub's running-time clock.
 if(gst_buffer_get_size(buffer)>7680||gst_buffer_get_size(buffer)%4||!GST_BUFFER_PTS_IS_VALID(buffer)){
  gst_buffer_unref(buffer);return GST_FLOW_ERROR;
 }
 guint64 end_frame=gst_util_uint64_scale_round(GST_BUFFER_PTS(buffer),48000,GST_SECOND)+gst_buffer_get_size(buffer)/4;
 g_mutex_lock(&i->lock);GstElement *source=i->source&&!i->finished?gst_object_ref(i->source):NULL;g_mutex_unlock(&i->lock);
 if(!source){gst_buffer_unref(buffer);return GST_FLOW_FLUSHING;}
 if(!wait&&(gst_app_src_get_current_level_bytes(GST_APP_SRC(source))+gst_buffer_get_size(buffer)>7680||
             gst_app_src_get_current_level_buffers(GST_APP_SRC(source))>=4)){
  gst_object_unref(source);gst_buffer_unref(buffer);return GST_FLOW_CUSTOM_SUCCESS;
 }
 GstFlowReturn result=gst_app_src_push_buffer(GST_APP_SRC(source),buffer);gst_object_unref(source);
 if(result==GST_FLOW_OK){g_mutex_lock(&i->lock);i->end_frame=MAX(i->end_frame,end_frame);i->has_data=TRUE;g_mutex_unlock(&i->lock);}
 return result;
}
GstFlowReturn penny_audio_input_push(PennyAudioInput *i,GstBuffer *buffer){return push(i,buffer,TRUE);}
GstFlowReturn penny_audio_input_try_push(PennyAudioInput *i,GstBuffer *buffer){return push(i,buffer,FALSE);}
GstFlowReturn penny_audio_input_finish(PennyAudioInput *i) {
 g_mutex_lock(&i->lock);
 GstElement *source=i->source?gst_object_ref(i->source):NULL;i->finished=TRUE;
 g_mutex_unlock(&i->lock);
 if(!source)return GST_FLOW_FLUSHING;
 GstFlowReturn result=gst_app_src_end_of_stream(GST_APP_SRC(source));gst_object_unref(source);return result;
}
gint penny_audio_input_drained(PennyAudioInput *i) {
 g_mutex_lock(&i->lock);
 gboolean active=i->source!=NULL,finished=i->finished,has_data=i->has_data;guint64 target=i->end_frame;
 g_mutex_unlock(&i->lock);
 if(!active)return -1;
 if(!finished)return 0;
 if(!has_data)return 1;
 guint64 played=0;
 if(penny_audio_sink_progress(i->hub->output,&played)!=GST_FLOW_OK)return -1;
 return played>=target?1:0;
}
void penny_audio_input_remove(PennyAudioInput *i) {
 g_mutex_lock(&i->lock);GstElement *source=i->source;i->source=NULL;g_mutex_unlock(&i->lock);
 if(!source)return;
 gst_element_set_state(source,GST_STATE_NULL);
 GstPad *src=gst_element_get_static_pad(source,"src");gst_pad_unlink(src,i->pad);gst_object_unref(src);
 gst_element_release_request_pad(i->hub->mixer,i->pad);gst_object_unref(i->pad);i->pad=NULL;
 gst_bin_remove(GST_BIN(i->hub->pipeline),source);i->hub->inputs--;
}
void penny_audio_input_free(PennyAudioInput *i) {penny_audio_input_remove(i);g_mutex_clear(&i->lock);g_free(i);}
gboolean penny_audio_hub_poll(PennyAudioHub *h,GError **error) {
 if(!h->failure){
  // Bound each owner-loop visit while servicing dynamic latency negotiation.
  for(guint count=0;count<32&&!h->failure;count++){
   GstMessage *message=gst_bus_pop_filtered(h->bus,GST_MESSAGE_ERROR|GST_MESSAGE_LATENCY);
   if(!message)break;
   if(GST_MESSAGE_TYPE(message)==GST_MESSAGE_LATENCY){
    if(!gst_bin_recalculate_latency(GST_BIN(h->pipeline)))
     g_set_error_literal(&h->failure,GST_CORE_ERROR,GST_CORE_ERROR_CLOCK,"Cannot negotiate shared audio latency");
   }else gst_message_parse_error(message,&h->failure,NULL);
   gst_message_unref(message);
   if(h->failure)gst_element_set_state(h->pipeline,GST_STATE_NULL);
  }
 }
 if(h->failure&&error)*error=g_error_copy(h->failure);
 return h->failure==NULL;
}
void penny_audio_hub_free(PennyAudioHub *h) {
 g_return_if_fail(h->inputs==0);
 gst_element_set_state(h->pipeline,GST_STATE_NULL);gst_object_unref(h->pipeline);gst_object_unref(h->bus);g_clear_error(&h->failure);g_free(h);
}
GstClockTime penny_audio_hub_time(PennyAudioHub *h) {
 GstClock *clock=gst_element_get_clock(h->pipeline);if(!clock)return GST_CLOCK_TIME_NONE;
 GstClockTime now=gst_clock_get_time(clock),base=gst_element_get_base_time(h->pipeline);gst_object_unref(clock);
 return now>=base?now-base:0;
}
guint penny_audio_hub_inputs(PennyAudioHub *h) {
 GST_OBJECT_LOCK(h->mixer);guint count=GST_ELEMENT(h->mixer)->numsinkpads;GST_OBJECT_UNLOCK(h->mixer);
 return count;
}

GstClockTime penny_audio_hub_render_delay(PennyAudioHub *h){
 GstClockTime reserve=penny_audio_sink_reserve(h->output);
 if(!GST_CLOCK_TIME_IS_VALID(reserve))return GST_CLOCK_TIME_NONE;
 guint64 duration=0,allowance=0;g_object_get(h->mixer,"output-buffer-duration",&duration,"latency",&allowance,NULL);
 if(duration>GST_SECOND||allowance>GST_SECOND)return GST_CLOCK_TIME_NONE;
 return reserve+40*GST_MSECOND+duration+allowance;
}
