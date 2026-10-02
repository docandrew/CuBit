#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <SDL.h>
#include <assert.h>
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
static unsigned delivered, painted, batch, yields;
static int done;
static int elapsed(void) { return getenv("CUBIT_FLOOD_ELAPSED") != NULL; }
Uint64 SDL_GetTicks64(void) {
    static Uint64 tick;
    return elapsed() ? tick++ : 0;
}
int SDL_PollEvent(SDL_Event *event) {
    memset(event, 0, sizeof(*event));
    if (done) { event->type = SDL_QUIT; return 1; }
    /* Never report an empty input queue. A queue-draining implementation
       fails this guard before reaching a presentation opportunity. */
    assert(++batch <= 32);
    event->type = SDL_TEXTINPUT;
    event->text.text[0] = (char)('a' + delivered % 26);
    delivered++;
    return 1;
}
void SDL_RenderPresent(SDL_Renderer *renderer) {
    void (*real_present)(SDL_Renderer *) = dlsym(RTLD_NEXT, "SDL_RenderPresent");
    assert(real_present != NULL);
    real_present(renderer);
    if (batch == 0) return;
    assert(batch == (elapsed() ? 1u : 32u));
    batch = 0;
    if (++painted == 4) done = 1;
}
int sched_yield(void) {
    int (*real_yield)(void) = dlsym(RTLD_NEXT, "sched_yield");
    assert(real_yield != NULL);
    yields++;
    return real_yield();
}
void SDL_Delay(Uint32 milliseconds) {
    void (*real_delay)(Uint32) = dlsym(RTLD_NEXT, "SDL_Delay");
    assert(real_delay != NULL);
    /* Local backlog must not be turned into a timer wait. */
    assert(done || milliseconds == 0);
    real_delay(milliseconds);
}
__attribute__((destructor)) static void verify(void) {
    assert(painted == 4);
    assert(delivered == (elapsed() ? 4u : 128u));
    assert(yields >= 3);
    fprintf(stderr, "PASS Workbench continuous-input %s: %u events, %u frames, %u yields\n",
            elapsed() ? "elapsed clock" : "frozen clock", delivered, painted, yields);
}
