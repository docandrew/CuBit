#include "penny-audio-hub.h"
#include "penny-audio-sink.h"
#include <gst/app/gstappsink.h>
#include <cubit/debug.h>
#include <stdio.h>
#include <string.h>
GST_PLUGIN_STATIC_DECLARE(app);
GST_PLUGIN_STATIC_DECLARE(audiomixer);
static void log_line(const char *s){cubit_debug_write(s,strlen(s));}
#define CHECK(x) do{if(!(x)){char m[100];snprintf(m,sizeof(m),"GSTREAMER-HUB: FAIL line=%d\n",__LINE__);log_line(m);return 0;}}while(0)
static GstBuffer *buffer(int value,GstClockTime pts){
 guint8 data[480*4];for(unsigned i=0;i<480;i++){data[i*4]=value&255;data[i*4+1]=(value>>8)&255;data[i*4+2]=(-value)&255;data[i*4+3]=((-value)>>8)&255;}
 GstBuffer *b=gst_buffer_new_allocate(NULL,sizeof(data),NULL);gst_buffer_fill(b,0,data,sizeof(data));GST_BUFFER_PTS(b)=pts;GST_BUFFER_DURATION(b)=10*GST_MSECOND;return b;
}
static int capture(GstAppSink *sink,int value){
 unsigned heard=0;
 for(unsigned n=0;n<80&&heard<480;n++){
  GstSample *sample=gst_app_sink_try_pull_sample(sink,GST_SECOND);CHECK(sample);GstMapInfo map;GstBuffer *b=gst_sample_get_buffer(sample);CHECK(gst_buffer_map(b,&map,GST_MAP_READ));CHECK(map.size%4==0);
  for(unsigned i=0;i<map.size;i+=4){int l=(short)(map.data[i]|map.data[i+1]<<8),r=(short)(map.data[i+2]|map.data[i+3]<<8);CHECK((l==0&&r==0)||(l==value&&r==-value));if(l)heard++;}
  gst_buffer_unmap(b,&map);gst_sample_unref(sample);
 }
 CHECK(heard==480);return 1;
}
typedef struct {PennyAudioInput *input;gint entered,done;GstFlowReturn result;GstClockTime pts;} Writer;
static gpointer write_until_blocked(gpointer data){Writer *w=data;for(unsigned n=0;n<32;n++){g_atomic_int_set(&w->entered,n+1);w->result=penny_audio_input_push(w->input,buffer(1000,w->pts+n*10*GST_MSECOND));if(w->result!=GST_FLOW_OK)break;}g_atomic_int_set(&w->done,1);return NULL;}
static int run(void){
 GstElement *sink=gst_element_factory_make("appsink",NULL);CHECK(sink);
 GstCaps *caps=gst_caps_from_string("audio/x-raw,format=S16LE,rate=48000,channels=2,layout=interleaved");gst_app_sink_set_caps(GST_APP_SINK(sink),caps);gst_caps_unref(caps);
 gst_app_sink_set_max_buffers(GST_APP_SINK(sink),2);g_object_set(sink,"sync",FALSE,NULL);
 PennyAudioHub *hub=penny_audio_hub_new(sink);CHECK(hub);
 PennyAudioInput *inputs[9];for(unsigned i=0;i<9;i++){inputs[i]=penny_audio_hub_add(hub);CHECK(inputs[i]);}
 CHECK(penny_audio_hub_inputs(hub)==9);
 GstClockTime pts=penny_audio_hub_time(hub)+100*GST_MSECOND;pts=gst_util_uint64_scale(gst_util_uint64_scale(pts,48000,GST_SECOND),GST_SECOND,48000);
 for(unsigned i=0;i<9;i++)CHECK(penny_audio_input_push(inputs[i],buffer(100*(i+1),pts))==GST_FLOW_OK);
 CHECK(capture(GST_APP_SINK(sink),4500));
 for(unsigned i=0;i<9;i++){penny_audio_input_free(inputs[i]);}
 CHECK(penny_audio_hub_inputs(hub)==0);
 log_line("GSTREAMER-HUB: nine sources exact, all request pads released\n");
 for(unsigned cycle=0;cycle<3;cycle++){
  PennyAudioInput *input=penny_audio_hub_add(hub);CHECK(input);
  pts=penny_audio_hub_time(hub)+100*GST_MSECOND;pts=gst_util_uint64_scale(gst_util_uint64_scale(pts,48000,GST_SECOND),GST_SECOND,48000);
  CHECK(penny_audio_input_push(input,buffer(2000,pts))==GST_FLOW_OK);CHECK(capture(GST_APP_SINK(sink),2000));penny_audio_input_free(input);CHECK(penny_audio_hub_inputs(hub)==0);
 }
 log_line("GSTREAMER-HUB: three late joins exact, empty hub reused\n");
 // Keep the output consuming while specifically filling a future input.
 // An indefinitely blocked capture sink is a different failure condition.
 g_object_set(sink,"drop",TRUE,NULL);
 PennyAudioInput *input=penny_audio_hub_add(hub);CHECK(input);
 Writer w={.input=input,.pts=penny_audio_hub_time(hub)+10*GST_SECOND};GThread *thread=g_thread_new("blocked-audio",write_until_blocked,&w);
 gint64 deadline=g_get_monotonic_time()+G_TIME_SPAN_SECOND;
 while(g_atomic_int_get(&w.entered)<8&&g_get_monotonic_time()<deadline)g_usleep(1000);
 CHECK(g_atomic_int_get(&w.entered)>=5&&!g_atomic_int_get(&w.done));
 gint64 before=g_get_monotonic_time();penny_audio_input_remove(input);g_thread_join(thread);CHECK(g_get_monotonic_time()-before<G_TIME_SPAN_SECOND);CHECK(w.result==GST_FLOW_FLUSHING);
 CHECK(penny_audio_input_push(input,buffer(1000,0))==GST_FLOW_FLUSHING);penny_audio_input_free(input);CHECK(penny_audio_hub_inputs(hub)==0);
 penny_audio_hub_free(hub);log_line("GSTREAMER-HUB: blocked producer cancelled, teardown PASS\n");return 1;
}
static gboolean open_blocked(void *p){(void)p;return TRUE;}
static guint write_blocked(void *p,const guint8 *data,guint frames){(void)p;(void)data;(void)frames;return 0;}
static void start_blocked(void *p){(void)p;}
static gint64 queued_blocked(void *p){(void)p;return 0;}
static void close_blocked(void *p){(*(unsigned *)p)++;}
static int device_failure(void){
 unsigned closes=0;
 PennyAudioTransport ops={open_blocked,write_blocked,start_blocked,queued_blocked,close_blocked,NULL};
 GstElement *output=penny_audio_sink_new(&ops,&closes);CHECK(output);
 PennyAudioHub *hub=penny_audio_hub_new(output);CHECK(hub);
 PennyAudioInput *input=penny_audio_hub_add(hub);CHECK(input);
 Writer w={.input=input,.pts=penny_audio_hub_time(hub)+10*GST_SECOND};GThread *thread=g_thread_new("device-stall",write_until_blocked,&w);
 GError *error=NULL;gint64 deadline=g_get_monotonic_time()+5*G_TIME_SPAN_SECOND;
 while(penny_audio_hub_poll(hub,&error)&&g_get_monotonic_time()<deadline)g_usleep(1000);
 CHECK(error&&error->domain==GST_RESOURCE_ERROR);g_error_free(error);
 CHECK(!penny_audio_hub_add(hub));
 g_thread_join(thread);CHECK(w.result==GST_FLOW_FLUSHING);CHECK(closes==1);
 penny_audio_input_free(input);CHECK(penny_audio_hub_inputs(hub)==0);penny_audio_hub_free(hub);
 log_line("GSTREAMER-HUB: device stall reported, blocked producer released\n");return 1;
}
typedef struct {gint written,played;unsigned closes;} ClockedTransport;
static gboolean clocked_open(void *p){(void)p;return TRUE;}
static guint clocked_write(void *p,const guint8 *data,guint frames){(void)data;ClockedTransport *t=p;g_atomic_int_add(&t->written,frames);return frames;}
static gint64 clocked_queued(void *p){ClockedTransport *t=p;return g_atomic_int_get(&t->written)-g_atomic_int_get(&t->played);}
static void clocked_close(void *p){ClockedTransport *t=p;t->closes++;}
static int independent_drain(void){
 ClockedTransport t={0};PennyAudioTransport ops={clocked_open,clocked_write,start_blocked,clocked_queued,clocked_close,NULL};
 GstElement *output=penny_audio_sink_new(&ops,&t);CHECK(output);PennyAudioHub *hub=penny_audio_hub_new(output);CHECK(hub);
 PennyAudioInput *short_input=penny_audio_hub_add(hub),*long_input=penny_audio_hub_add(hub);CHECK(short_input&&long_input);
 GstClockTime pts=penny_audio_hub_time(hub)+100*GST_MSECOND;
 guint64 start_frame=gst_util_uint64_scale(pts,48000,GST_SECOND);pts=gst_util_uint64_scale(start_frame,GST_SECOND,48000);
 CHECK(penny_audio_input_push(short_input,buffer(1000,pts))==GST_FLOW_OK);
 CHECK(penny_audio_input_finish(short_input)==GST_FLOW_OK);
 CHECK(penny_audio_input_push(short_input,buffer(1000,pts))==GST_FLOW_FLUSHING);
 for(unsigned n=0;n<4;n++)CHECK(penny_audio_input_push(long_input,buffer(2000,pts+n*10*GST_MSECOND))==GST_FLOW_OK);
 CHECK(penny_audio_input_finish(long_input)==GST_FLOW_OK);
 gint64 deadline=g_get_monotonic_time()+2*G_TIME_SPAN_SECOND;
 while(g_atomic_int_get(&t.written)<(gint)(start_frame+1920)&&g_get_monotonic_time()<deadline){CHECK(penny_audio_hub_poll(hub,NULL));g_usleep(1000);}
 CHECK(g_atomic_int_get(&t.written)>=(gint)(start_frame+1920));
 guint64 origin=0;CHECK(penny_audio_sink_progress(output,&origin)==GST_FLOW_OK);CHECK(origin<=start_frame);
 CHECK(penny_audio_input_drained(short_input)==0&&penny_audio_input_drained(long_input)==0);
 g_atomic_int_set(&t.played,start_frame-origin+479);CHECK(penny_audio_input_drained(short_input)==0);
 g_atomic_int_set(&t.played,start_frame-origin+480);CHECK(penny_audio_input_drained(short_input)==1&&penny_audio_input_drained(long_input)==0);
 penny_audio_input_free(short_input);CHECK(penny_audio_hub_inputs(hub)==1);
 g_atomic_int_set(&t.played,start_frame-origin+1919);CHECK(penny_audio_input_drained(long_input)==0);
 g_atomic_int_set(&t.played,start_frame-origin+1920);CHECK(penny_audio_input_drained(long_input)==1);
 penny_audio_input_free(long_input);penny_audio_hub_free(hub);CHECK(t.closes==1);
 log_line("GSTREAMER-HUB: independent drain exact sample boundaries PASS\n");return 1;
}
int main(void){g_setenv("GST_REGISTRY_DISABLE","yes",TRUE);g_setenv("GST_PLUGIN_SYSTEM_PATH_1_0","",TRUE);g_setenv("GST_PLUGIN_PATH_1_0","",TRUE);if(gst_init_check(NULL,NULL,NULL)){GST_PLUGIN_STATIC_REGISTER(app);GST_PLUGIN_STATIC_REGISTER(audiomixer);if(run()&&device_failure()&&independent_drain())log_line("GSTREAMER-HUB: PASS\n");}for(;;)g_usleep(1000000);}
