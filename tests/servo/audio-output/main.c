#include "penny-audio-sink.h"
#include <gst/app/gstappsrc.h>
#include <cubit/debug.h>
#include <stdio.h>
#include <string.h>
GST_PLUGIN_STATIC_DECLARE(app);
GST_PLUGIN_STATIC_DECLARE(opus);
GST_PLUGIN_STATIC_DECLARE(audioconvert);
GST_PLUGIN_STATIC_DECLARE(audioresample);
GST_PLUGIN_STATIC_DECLARE(audiomixer);
static void log_line(const char *s){cubit_debug_write(s,strlen(s));}
#define CHECK(x) do{if(!(x)){char msg[128];snprintf(msg,sizeof(msg),"GSTREAMER-OUTPUT: FAIL mode=%d line=%d\n",mode,__LINE__);log_line(msg);return 0;}}while(0)
typedef enum {NORMAL, QUERY_ERROR, DRAIN_TIMEOUT, WRITE_TIMEOUT, OVER_ACCEPT, CANCEL_WRITE, CANCEL_DRAIN, DENIED} Mode;
typedef struct {
 unsigned opens,closes,starts,calls,frames,queued,drains;
 gint entered,draining;
 Mode mode;
 gboolean bad;
 gint64 capacity;
} Transport;
static gboolean open_output(void *context){Transport *t=context;t->opens++;return t->mode!=DENIED;}
static guint write_output(void *context,const guint8 *data,guint frames){
 Transport *t=context;t->calls++;g_atomic_int_set(&t->entered,1);
 if(t->mode==OVER_ACCEPT)return frames+1;
 if(t->mode==CANCEL_WRITE||t->mode==WRITE_TIMEOUT||t->calls%3==0)return 0;
 guint count=MIN(frames,7);
 for(guint f=0;f<count;f++)for(guint b=0;b<4;b++)if(data[f*4+b]!=(guint8)((t->frames+f)*4+b))t->bad=TRUE;
 t->frames+=count;t->queued=count;return count;
}
static void start_output(void *context){Transport *t=context;t->starts++;if(!t->frames)t->bad=TRUE;}
static gint64 queued_output(void *context){
 Transport *t=context;g_atomic_int_set(&t->draining,1);
 if(t->mode==QUERY_ERROR)return -1;
 if(t->mode==DRAIN_TIMEOUT||t->mode==CANCEL_DRAIN)return 1;
 if(t->queued){t->queued--;t->drains++;return t->queued+1;}return 0;
}
static void close_output(void *context){Transport *t=context;t->closes++;t->queued=0;}
static const PennyAudioTransport ops={open_output,write_output,start_output,queued_output,close_output,NULL};
static int run(Mode mode){
 Transport t={0};t.mode=mode;
 GstElement *pipeline=gst_pipeline_new(NULL),*source=gst_element_factory_make("appsrc",NULL),*sink=penny_audio_sink_new(&ops,&t);CHECK(pipeline&&source&&sink);
 gst_pipeline_set_auto_flush_bus(GST_PIPELINE(pipeline),FALSE);
 GstCaps *caps=gst_caps_from_string("audio/x-raw,format=S16LE,rate=48000,channels=2,layout=interleaved");gst_app_src_set_caps(GST_APP_SRC(source),caps);gst_caps_unref(caps);
 g_object_set(source,"format",GST_FORMAT_TIME,"max-bytes",(guint64)3840,"block",TRUE,NULL);
 gst_bin_add_many(GST_BIN(pipeline),source,sink,NULL);CHECK(gst_element_link(source,sink));
 GstBus *bus=gst_element_get_bus(pipeline);
 GstStateChangeReturn state=gst_element_set_state(pipeline,GST_STATE_PLAYING);
 if(mode==DENIED){CHECK(state==GST_STATE_CHANGE_FAILURE);}
 else{
  CHECK(state!=GST_STATE_CHANGE_FAILURE);
  guint8 data[960*4];for(unsigned i=0;i<sizeof(data);i++)data[i]=(guint8)i;
  GstBuffer *buffer=gst_buffer_new_allocate(NULL,sizeof(data),NULL);CHECK(buffer);gst_buffer_fill(buffer,0,data,sizeof(data));GST_BUFFER_PTS(buffer)=0;GST_BUFFER_DURATION(buffer)=20*GST_MSECOND;
  CHECK(gst_app_src_push_buffer(GST_APP_SRC(source),buffer)==GST_FLOW_OK);
  CHECK(gst_app_src_end_of_stream(GST_APP_SRC(source))==GST_FLOW_OK);
 }
 gboolean cancel=mode==CANCEL_WRITE||mode==CANCEL_DRAIN;
 if(cancel){
  gint *entered=mode==CANCEL_WRITE?&t.entered:&t.draining;
  gint64 deadline=g_get_monotonic_time()+G_TIME_SPAN_SECOND;
  while(!g_atomic_int_get(entered)&&g_get_monotonic_time()<deadline)g_usleep(1000);
  CHECK(g_atomic_int_get(entered));
 }else{
  GstMessage *message=gst_bus_timed_pop_filtered(bus,5*GST_SECOND,GST_MESSAGE_EOS|GST_MESSAGE_ERROR);
  CHECK(message);
  if(mode==NORMAL){CHECK(GST_MESSAGE_TYPE(message)==GST_MESSAGE_EOS);}
  else{
   CHECK(GST_MESSAGE_TYPE(message)==GST_MESSAGE_ERROR);
   CHECK(GST_MESSAGE_SRC(message)==GST_OBJECT(sink));
   GError *error=NULL;gst_message_parse_error(message,&error,NULL);
   CHECK(error&&error->domain==GST_RESOURCE_ERROR);g_error_free(error);
  }
  gst_message_unref(message);
 }
 gint64 before=g_get_monotonic_time();CHECK(gst_element_set_state(pipeline,GST_STATE_NULL)!=GST_STATE_CHANGE_FAILURE);
 CHECK(g_get_monotonic_time()-before<G_TIME_SPAN_SECOND);
 if(cancel){GstMessage *error=gst_bus_pop_filtered(bus,GST_MESSAGE_ERROR);CHECK(!error);}
 gst_object_unref(bus);gst_object_unref(pipeline);
 CHECK(t.opens==1&&t.closes==(mode==DENIED?0:1));CHECK(!t.bad);
 if(mode==NORMAL||mode==QUERY_ERROR||mode==DRAIN_TIMEOUT||mode==CANCEL_DRAIN)CHECK(t.frames==960&&t.starts==1&&t.calls>960/7);
 else CHECK(t.frames==0&&t.starts==0);
 char message[96];snprintf(message,sizeof(message),"GSTREAMER-OUTPUT: mode=%d PASS\n",mode);log_line(message);return 1;
}
static gint64 capacity_output(void *context){return ((Transport *)context)->capacity;}
static int capacity_cases(void){
 Mode mode=NORMAL;Transport t={0};PennyAudioTransport callbacks=ops;callbacks.capacity=capacity_output;
 GstElement *sink=penny_audio_sink_new(&callbacks,&t);CHECK(sink);
 CHECK(penny_audio_sink_reserve(sink)==GST_CLOCK_TIME_NONE);
 CHECK(gst_element_set_state(sink,GST_STATE_PAUSED)!=GST_STATE_CHANGE_FAILURE);
 t.capacity=0;CHECK(penny_audio_sink_reserve(sink)==0);
 t.capacity=10240;CHECK(penny_audio_sink_reserve(sink)==gst_util_uint64_scale(10240,GST_SECOND,48000));
 t.capacity=-1;CHECK(penny_audio_sink_reserve(sink)==GST_CLOCK_TIME_NONE);
 t.capacity=10241;CHECK(penny_audio_sink_reserve(sink)==GST_CLOCK_TIME_NONE);
 t.capacity=G_MAXINT64;CHECK(penny_audio_sink_reserve(sink)==GST_CLOCK_TIME_NONE);
 CHECK(gst_element_set_state(sink,GST_STATE_NULL)!=GST_STATE_CHANGE_FAILURE);
 CHECK(penny_audio_sink_reserve(sink)==GST_CLOCK_TIME_NONE);CHECK(t.opens==1&&t.closes==1);gst_object_unref(sink);
 log_line("GSTREAMER-OUTPUT: capacity bounds and closed-state rejection PASS\n");return 1;
}
int main(void){
 g_setenv("GST_REGISTRY_DISABLE","yes",TRUE);g_setenv("GST_PLUGIN_SYSTEM_PATH_1_0","",TRUE);g_setenv("GST_PLUGIN_PATH_1_0","",TRUE);
 if(!gst_init_check(NULL,NULL,NULL))goto done;
 GST_PLUGIN_STATIC_REGISTER(app);
 GST_PLUGIN_STATIC_REGISTER(opus);
 GST_PLUGIN_STATIC_REGISTER(audioconvert);
 GST_PLUGIN_STATIC_REGISTER(audioresample);
 GST_PLUGIN_STATIC_REGISTER(audiomixer);
 const char *factories[]={"opusdec","audioconvert","audioresample","audiomixer"};
 for(unsigned i=0;i<G_N_ELEMENTS(factories);i++){
  GstElement *element=gst_element_factory_make(factories[i],NULL);
  if(!element){log_line("GSTREAMER-OUTPUT: FAIL missing audio element\n");goto done;}
  gst_object_unref(element);
 }
 log_line("GSTREAMER-OUTPUT: audio registry PASS\n");
 for(Mode mode=NORMAL;mode<=DENIED;mode++)if(!run(mode))goto done;
 if(capacity_cases()&&run(NORMAL))log_line("GSTREAMER-OUTPUT: PASS\n");
done:for(;;)g_usleep(1000000);
}
