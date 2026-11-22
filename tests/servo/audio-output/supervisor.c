/* Disposable launcher only. The media image is spawned with no policy flag
   or capability grants, rather than running as the privileged boot service. */
#include "abi.h"
extern unsigned char media_start[],media_end[];
int main(void) {
 U pid=call6(60,(U)media_start,(U)(media_end-media_start),5,0,0,0);
 if(!pid||pid==~0UL||call(73,pid,0,0)) {
  say("GSTREAMER-SMOKE: FAIL ordinary child launch\n");
 } else say("MEDIA-SUPERVISOR: ordinary child resumed\n");
 for(;;) call(28,1000,0,0);
}
