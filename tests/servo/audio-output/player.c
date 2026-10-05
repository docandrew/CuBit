#include "penny-audio-player.h"
#include <gst/app/gstappsrc.h>
#include <cubit/debug.h>
#include <stdio.h>
#include <string.h>
GST_PLUGIN_STATIC_DECLARE(app);
GST_PLUGIN_STATIC_DECLARE(audiomixer);
static void log_line(const char *s){cubit_debug_write(s,strlen(s));}
#define CHECK(x) do{if(!(x)){char m[100];snprintf(m,sizeof(m),"GSTREAMER-PLAYER-OUTPUT: FAIL line=%d\n",__LINE__);log_line(m);return 0;}}while(0)
typedef struct {GMutex lock;unsigned opens,closes;gint64 sum;guint64 bits[9],writes;gboolean bad,deny_open;} Output;
static gboolean open_output(void *p){Output *o=p;o->opens++;return !o->deny_open;}
static guint write_output(void *p,const guint8 *data,guint frames){
 Output *o=p;g_mutex_lock(&o->lock);o->writes++;
 for(unsigned f=0;f<frames;f++){int l=(short)(data[f*4]|data[f*4+1]<<8),r=(short)(data[f*4+2]|data[f*4+3]<<8);if(l!=-r||l<0)o->bad=TRUE;o->sum+=l;for(unsigned bit=0;bit<9;bit++)if(l&(1<<bit))o->bits[bit]++;}
 g_mutex_unlock(&o->lock);return frames;
}
static void start_output(void *p){(void)p;}
static gint64 queued_output(void *p){(void)p;return 0;}
static void close_output(void *p){Output *o=p;o->closes++;}
static const PennyAudioTransport ops={open_output,write_output,start_output,queued_output,close_output,NULL};
static GstBuffer *buffer(unsigned frames,int value,GstClockTime pts){
 GstBuffer *b=gst_buffer_new_allocate(NULL,frames*4,NULL);GstMapInfo map;if(!b||!gst_buffer_map(b,&map,GST_MAP_WRITE))return NULL;
 for(unsigned f=0;f<frames;f++){map.data[f*4]=value&255;map.data[f*4+1]=(value>>8)&255;map.data[f*4+2]=(-value)&255;map.data[f*4+3]=((-value)>>8)&255;}
 gst_buffer_unmap(b,&map);GST_BUFFER_PTS(b)=pts;GST_BUFFER_DURATION(b)=gst_util_uint64_scale(frames,GST_SECOND,48000);return b;
}
static int run(PennyAudioSession *session,Output *output,unsigned count){
 unsigned opens_before=output->opens,closes_before=output->closes;
 GstElement *pipelines[9];GstBus *buses[9];gboolean ended[9]={0};gint64 expected=0;guint64 before_bits[9];
 g_mutex_lock(&output->lock);gint64 before=output->sum;memcpy(before_bits,output->bits,sizeof(before_bits));g_mutex_unlock(&output->lock);
 for(unsigned i=0;i<count;i++){
  GstElement *pipeline=gst_pipeline_new(NULL),*source=gst_element_factory_make("appsrc",NULL),*sink=gst_element_factory_make("pennyaudiosink",NULL);CHECK(pipeline&&source&&sink);pipelines[i]=pipeline;
  GstCaps *caps=gst_caps_from_string("audio/x-raw,format=S16LE,rate=48000,channels=2,layout=interleaved");gst_app_src_set_caps(GST_APP_SRC(source),caps);gst_caps_unref(caps);
  g_object_set(source,"format",GST_FORMAT_TIME,"max-bytes",(guint64)3840,"block",TRUE,NULL);
  gst_bin_add_many(GST_BIN(pipeline),source,sink,NULL);CHECK(gst_element_link(source,sink));buses[i]=gst_element_get_bus(pipeline);
  CHECK(gst_element_set_state(pipeline,GST_STATE_PLAYING)!=GST_STATE_CHANGE_FAILURE);
  CHECK(gst_app_src_push_buffer(GST_APP_SRC(source),buffer(960,1<<i,0))==GST_FLOW_OK);
  CHECK(gst_app_src_end_of_stream(GST_APP_SRC(source))==GST_FLOW_OK);expected+=960*(1<<i);
 }
 gint64 deadline=g_get_monotonic_time()+5*G_TIME_SPAN_SECOND;unsigned done=0;
 while(done<count&&g_get_monotonic_time()<deadline){
  GError *error=NULL;CHECK(penny_audio_session_poll(session,&error));
  for(unsigned i=0;i<count;i++)if(!ended[i]){
   GstMessage *message=gst_bus_pop_filtered(buses[i],GST_MESSAGE_EOS|GST_MESSAGE_ERROR);
   if(message){
    if(GST_MESSAGE_TYPE(message)==GST_MESSAGE_ERROR){gst_message_parse_error(message,&error,NULL);log_line(error->message);log_line("\n");}
    CHECK(GST_MESSAGE_TYPE(message)==GST_MESSAGE_EOS);gst_message_unref(message);ended[i]=TRUE;done++;
   }
  }
  g_usleep(1000);
 }
 CHECK(done==count);
 g_mutex_lock(&output->lock);gint64 actual=output->sum-before;gboolean bad=output->bad;guint64 after_bits[9];memcpy(after_bits,output->bits,sizeof(after_bits));g_mutex_unlock(&output->lock);
 for(unsigned bit=0;bit<count;bit++){char msg[120];snprintf(msg,sizeof(msg),"GSTREAMER-PLAYER-OUTPUT: contribution bit=%u frames=%llu expected=960\n",bit,(unsigned long long)(after_bits[bit]-before_bits[bit]));log_line(msg);}
 CHECK(actual==expected&&!bad);
 for(unsigned bit=0;bit<count;bit++)CHECK(after_bits[bit]-before_bits[bit]==960);
 for(unsigned i=0;i<count;i++){CHECK(gst_element_set_state(pipelines[i],GST_STATE_NULL)!=GST_STATE_CHANGE_FAILURE);gst_object_unref(buses[i]);gst_object_unref(pipelines[i]);}
 CHECK(output->opens==opens_before+1&&output->closes==closes_before+1);
 g_mutex_lock(&output->lock);guint64 writes=output->writes;g_mutex_unlock(&output->lock);
 g_usleep(100000);CHECK(penny_audio_session_poll(session,NULL));
 g_mutex_lock(&output->lock);gboolean quiet=output->writes==writes;g_mutex_unlock(&output->lock);
 CHECK(quiet);log_line("GSTREAMER-PLAYER-OUTPUT: empty session closes device and stops writes PASS\n");
 char message[100];snprintf(message,sizeof(message),"GSTREAMER-PLAYER-OUTPUT: %u players exact sample sum, EOS and teardown\n",count);log_line(message);return 1;
}
static int reopen_failure(void){
 Output output={0};g_mutex_init(&output.lock);
 PennyAudioSession *session=penny_audio_session_new(&ops,&output);
 CHECK(session&&penny_audio_player_register(session));CHECK(run(session,&output,1));
 output.deny_open=TRUE;
 GstElement *sink=gst_element_factory_make("pennyaudiosink",NULL);CHECK(sink);
 CHECK(gst_element_set_state(sink,GST_STATE_PAUSED)==GST_STATE_CHANGE_FAILURE);
 GError *error=NULL;CHECK(!penny_audio_session_poll(session,&error)&&error);g_clear_error(&error);
 CHECK(output.opens==2&&output.closes==1);
 gst_element_set_state(sink,GST_STATE_NULL);gst_object_unref(sink);
 penny_audio_player_unregister(session);penny_audio_session_unref(session);
 CHECK(output.opens==2&&output.closes==1);g_mutex_clear(&output.lock);
 log_line("GSTREAMER-PLAYER-OUTPUT: device reopen failure releases idle session PASS\n");return 1;
}
static gboolean seek_data(GstAppSrc *source,guint64 offset,gpointer user){(void)source;(void)offset;g_atomic_int_inc((gint *)user);return TRUE;}
static int await_eos(PennyAudioSession *session,GstBus *bus){
 gint64 deadline=g_get_monotonic_time()+5*G_TIME_SPAN_SECOND;
 while(g_get_monotonic_time()<deadline){
  CHECK(penny_audio_session_poll(session,NULL));GstMessage *m=gst_bus_pop_filtered(bus,GST_MESSAGE_EOS|GST_MESSAGE_ERROR);
  if(m){if(GST_MESSAGE_TYPE(m)==GST_MESSAGE_ERROR){GError *error=NULL;gst_message_parse_error(m,&error,NULL);log_line(error->message);log_line("\n");}
   CHECK(GST_MESSAGE_TYPE(m)==GST_MESSAGE_EOS);gst_message_unref(m);return 1;}
  g_usleep(1000);
 }CHECK(FALSE);return 0;
}
static gint64 sum(Output *output){g_mutex_lock(&output->lock);gint64 result=output->sum;g_mutex_unlock(&output->lock);return result;}
static int seek_pause(PennyAudioSession *session,Output *output){
 gint seeks=0;GstAppSrcCallbacks callbacks={.seek_data=seek_data};
 GstElement *pipeline=gst_pipeline_new(NULL),*source=gst_element_factory_make("appsrc",NULL),*sink=gst_element_factory_make("pennyaudiosink",NULL);CHECK(pipeline&&source&&sink);
 GstCaps *caps=gst_caps_from_string("audio/x-raw,format=S16LE,rate=48000,channels=2,layout=interleaved");gst_app_src_set_caps(GST_APP_SRC(source),caps);gst_caps_unref(caps);
 g_object_set(source,"format",GST_FORMAT_TIME,"max-bytes",(guint64)3840,"block",TRUE,NULL);
 gst_app_src_set_stream_type(GST_APP_SRC(source),GST_APP_STREAM_TYPE_SEEKABLE);gst_app_src_set_callbacks(GST_APP_SRC(source),&callbacks,&seeks,NULL);
 gst_bin_add_many(GST_BIN(pipeline),source,sink,NULL);CHECK(gst_element_link(source,sink));GstBus *bus=gst_element_get_bus(pipeline);
 gint64 expected=sum(output);CHECK(gst_element_set_state(pipeline,GST_STATE_PLAYING)!=GST_STATE_CHANGE_FAILURE);
 CHECK(gst_app_src_push_buffer(GST_APP_SRC(source),buffer(960,1100,0))==GST_FLOW_OK);CHECK(gst_app_src_end_of_stream(GST_APP_SRC(source))==GST_FLOW_OK);CHECK(await_eos(session,bus));expected+=960*1100;CHECK(sum(output)==expected);
 CHECK(gst_element_seek_simple(pipeline,GST_FORMAT_TIME,GST_SEEK_FLAG_FLUSH,0));
 CHECK(gst_app_src_push_buffer(GST_APP_SRC(source),buffer(960,2200,0))==GST_FLOW_OK);CHECK(gst_app_src_end_of_stream(GST_APP_SRC(source))==GST_FLOW_OK);CHECK(await_eos(session,bus));expected+=960*2200;CHECK(sum(output)==expected);
 CHECK(gst_element_seek_simple(pipeline,GST_FORMAT_TIME,GST_SEEK_FLAG_FLUSH,0));
 CHECK(gst_element_set_state(pipeline,GST_STATE_PAUSED)!=GST_STATE_CHANGE_FAILURE);
 CHECK(gst_app_src_push_buffer(GST_APP_SRC(source),buffer(960,3300,0))==GST_FLOW_OK);
 g_usleep(50000);CHECK(sum(output)==expected);
 CHECK(gst_element_set_state(pipeline,GST_STATE_PLAYING)!=GST_STATE_CHANGE_FAILURE);
 CHECK(gst_app_src_end_of_stream(GST_APP_SRC(source))==GST_FLOW_OK);CHECK(await_eos(session,bus));expected+=960*3300;CHECK(sum(output)==expected);
 CHECK(gst_element_seek_simple(pipeline,GST_FORMAT_TIME,GST_SEEK_FLAG_FLUSH,0));
 CHECK(gst_app_src_push_buffer(GST_APP_SRC(source),buffer(960,4400,5*GST_SECOND))==GST_FLOW_OK);g_usleep(20000);
 gint64 before=g_get_monotonic_time();CHECK(gst_element_set_state(pipeline,GST_STATE_NULL)!=GST_STATE_CHANGE_FAILURE);CHECK(g_get_monotonic_time()-before<G_TIME_SPAN_SECOND);CHECK(sum(output)==expected);
 CHECK(g_atomic_int_get(&seeks)>=4);gst_object_unref(bus);gst_object_unref(pipeline);
 log_line("GSTREAMER-PLAYER-OUTPUT: seek replay, paused preroll and scheduled-buffer cancellation PASS\n");return 1;
}
static int interrupted_render(PennyAudioSession *session,Output *output,unsigned mode){
 GstElement *pipeline=gst_pipeline_new(NULL),*source=gst_element_factory_make("appsrc",NULL),*sink=gst_element_factory_make("pennyaudiosink",NULL);CHECK(pipeline&&source&&sink);
 GstCaps *caps=gst_caps_from_string("audio/x-raw,format=S16LE,rate=48000,channels=2,layout=interleaved");gst_app_src_set_caps(GST_APP_SRC(source),caps);gst_caps_unref(caps);
 g_object_set(source,"format",GST_FORMAT_TIME,NULL);
 gint seeks=0;GstAppSrcCallbacks callbacks={.seek_data=seek_data};
 gst_app_src_set_stream_type(GST_APP_SRC(source),GST_APP_STREAM_TYPE_SEEKABLE);gst_app_src_set_callbacks(GST_APP_SRC(source),&callbacks,&seeks,NULL);
 gst_bin_add_many(GST_BIN(pipeline),source,sink,NULL);CHECK(gst_element_link(source,sink));GstBus *bus=gst_element_get_bus(pipeline);
 gint64 before=sum(output);CHECK(gst_element_set_state(pipeline,GST_STATE_PLAYING)!=GST_STATE_CHANGE_FAILURE);
 /* One buffer spans many render iterations: interruption must preserve its remainder. */
 CHECK(gst_app_src_push_buffer(GST_APP_SRC(source),buffer(96000,64,0))==GST_FLOW_OK);
 CHECK(gst_app_src_end_of_stream(GST_APP_SRC(source))==GST_FLOW_OK);
 gint64 deadline=g_get_monotonic_time()+G_TIME_SPAN_SECOND;
 while(sum(output)==before&&g_get_monotonic_time()<deadline){CHECK(penny_audio_session_poll(session,NULL));g_usleep(1000);}
 CHECK(sum(output)>before&&sum(output)<before+96000*64);
 CHECK(gst_element_set_state(pipeline,GST_STATE_PAUSED)!=GST_STATE_CHANGE_FAILURE);
 GstState current,pending;CHECK(gst_element_get_state(pipeline,&current,&pending,GST_SECOND)==GST_STATE_CHANGE_SUCCESS);CHECK(current==GST_STATE_PAUSED);
 /* Allow bounded already-submitted audio to drain, then require silence. */
 g_usleep(150000);gint64 paused=sum(output);g_usleep(80000);CHECK(sum(output)==paused);
 CHECK(paused<before+96000*64);
 if(mode==2){
  CHECK(gst_element_seek_simple(pipeline,GST_FORMAT_TIME,GST_SEEK_FLAG_FLUSH,0));
  CHECK(gst_app_src_push_buffer(GST_APP_SRC(source),buffer(960,128,0))==GST_FLOW_OK);
  CHECK(gst_app_src_end_of_stream(GST_APP_SRC(source))==GST_FLOW_OK);
  CHECK(gst_element_set_state(pipeline,GST_STATE_PLAYING)!=GST_STATE_CHANGE_FAILURE);CHECK(await_eos(session,bus));
  CHECK(sum(output)==paused+960*128);CHECK(g_atomic_int_get(&seeks)>=2);
  CHECK(gst_element_set_state(pipeline,GST_STATE_NULL)!=GST_STATE_CHANGE_FAILURE);
 }else if(mode==1){
  gint64 start=g_get_monotonic_time();CHECK(gst_element_set_state(pipeline,GST_STATE_NULL)!=GST_STATE_CHANGE_FAILURE);CHECK(g_get_monotonic_time()-start<G_TIME_SPAN_SECOND);
  g_usleep(100000);CHECK(sum(output)==paused);
 }else{
  CHECK(gst_element_set_state(pipeline,GST_STATE_PLAYING)!=GST_STATE_CHANGE_FAILURE);CHECK(await_eos(session,bus));
  CHECK(sum(output)==before+96000*64);CHECK(gst_element_set_state(pipeline,GST_STATE_NULL)!=GST_STATE_CHANGE_FAILURE);
 }
 gst_object_unref(bus);gst_object_unref(pipeline);
 log_line(mode==2?"GSTREAMER-PLAYER-OUTPUT: paused in-flight flushing seek PASS\n":mode==1?"GSTREAMER-PLAYER-OUTPUT: paused in-flight buffer cancellation PASS\n":"GSTREAMER-PLAYER-OUTPUT: in-flight pause/resume exact samples PASS\n");return 1;
}
int main(void){
 g_setenv("GST_REGISTRY_DISABLE","yes",TRUE);g_setenv("GST_PLUGIN_SYSTEM_PATH_1_0","",TRUE);g_setenv("GST_PLUGIN_PATH_1_0","",TRUE);
 if(!gst_init_check(NULL,NULL,NULL))goto done;
 GST_PLUGIN_STATIC_REGISTER(app);GST_PLUGIN_STATIC_REGISTER(audiomixer);
 if(!reopen_failure())goto done;
 Output absent={0};g_mutex_init(&absent.lock);absent.deny_open=TRUE;
 PennyAudioSession *unavailable=penny_audio_session_new(&ops,&absent);
 if(!unavailable||!penny_audio_player_register(unavailable)||absent.opens)goto done;
 GstElement *denied=gst_element_factory_make("pennyaudiosink",NULL);
 if(!denied||gst_element_set_state(denied,GST_STATE_PAUSED)!=GST_STATE_CHANGE_FAILURE)goto done;
 GError *error=NULL;
 if(penny_audio_session_poll(unavailable,&error)||!error)goto done;
 g_clear_error(&error);gst_element_set_state(denied,GST_STATE_NULL);gst_object_unref(denied);
 penny_audio_player_unregister(unavailable);penny_audio_session_unref(unavailable);
 if(absent.opens!=1||absent.closes)goto done;
 log_line("GSTREAMER-PLAYER-OUTPUT: missing device fails playback and releases session PASS\n");
 Output output={0};g_mutex_init(&output.lock);PennyAudioSession *session=penny_audio_session_new(&ops,&output);
 if(!session||output.opens||!penny_audio_session_poll(session,NULL))goto done;
 PennyAudioSession *idle=penny_audio_session_new(&ops,&output);
 if(!idle)goto done;
 penny_audio_session_unref(idle);
 if(output.opens||output.closes)goto done;
 log_line("GSTREAMER-PLAYER-OUTPUT: idle session never opens device PASS\n");
 if(session&&penny_audio_player_register(session)&&run(session,&output,9)&&run(session,&output,1)&&run(session,&output,9)&&seek_pause(session,&output)&&interrupted_render(session,&output,0)&&interrupted_render(session,&output,1)&&interrupted_render(session,&output,2)){
  GstElement *held=gst_element_factory_make("pennyaudiosink",NULL);
  if(!held||gst_element_set_state(held,GST_STATE_PAUSED)==GST_STATE_CHANGE_FAILURE)goto done;
  penny_audio_player_unregister(session);penny_audio_session_unref(session);
  if(output.opens!=output.closes+1)goto done;
  gst_element_set_state(held,GST_STATE_NULL);
  gst_object_unref(held);
  GstElement *unbound=gst_element_factory_make("pennyaudiosink",NULL);
  if(!unbound||gst_element_set_state(unbound,GST_STATE_PAUSED)!=GST_STATE_CHANGE_FAILURE)goto done;
  gst_element_set_state(unbound,GST_STATE_NULL);gst_object_unref(unbound);
  if(output.closes==output.opens)log_line("GSTREAMER-PLAYER-OUTPUT: factory lifetime and unbound rejection PASS\nGSTREAMER-PLAYER-OUTPUT: PASS\n");
 }
 done:for(;;)g_usleep(1000000);
}
