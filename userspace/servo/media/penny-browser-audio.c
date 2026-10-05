#include "penny-browser-audio.h"
#include <stdint.h>
extern int penny_audio_open(void);
extern unsigned penny_audio_write(const void *,unsigned);
extern int64_t penny_audio_queued(void);
extern int64_t penny_audio_capacity(void);
extern void penny_audio_start(void);
extern void penny_audio_close(void);
static gboolean open_output(void *context){(void)context;return penny_audio_open()!=0;}
static guint write_output(void *context,const guint8 *data,guint frames){(void)context;return penny_audio_write(data,frames);}
static void start_output(void *context){(void)context;penny_audio_start();}
static gint64 queued_output(void *context){(void)context;return penny_audio_queued();}
static gint64 capacity_output(void *context){(void)context;return penny_audio_capacity();}
static void close_output(void *context){(void)context;penny_audio_close();}
PennyAudioSession *penny_browser_audio_new(void){
 static const PennyAudioTransport ops={open_output,write_output,start_output,queued_output,close_output,capacity_output};
 PennyAudioSession *session=penny_audio_session_new(&ops,NULL);
 if(session&&!penny_audio_player_register(session)){penny_audio_session_unref(session);return NULL;}
 return session;
}
void penny_browser_audio_close(PennyAudioSession *session){
 if(!session)return;
 penny_audio_player_unregister(session);penny_audio_session_unref(session);
}
