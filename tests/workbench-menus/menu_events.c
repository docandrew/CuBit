/* Linux-only SDL event driver for the Workbench menu bar test (run.sh).
   Loaded with LD_PRELOAD, never linked into CuBit: it feeds the real shared
   Workbench loop one event per wait and saves every presented frame as
   frame-NN.bmp, NN being the number of events delivered before it. */
#include <SDL.h>
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static unsigned stage;
static SDL_Event queued_event;
static int generated_event, have_event;

/* The script: one event per stage. */
enum action { KEY, ALT_KEY, TEXT, PRESS, RELEASE, QUIT };
struct step { enum action action; SDL_Keycode key; char text; int x, y; };
#define BAR_Y 33            /* inside the menu bar, under the preview's 22-pixel title */
#define FILE_TITLE_X 16     /* inside "File", the first title */
static const struct step script[] = {
    {KEY, SDLK_F10, 0, 0, 0},          /*  1: File, nothing selected */
    {KEY, SDLK_RIGHT, 0, 0, 0},        /*  2: Edit */
    {KEY, SDLK_RIGHT, 0, 0, 0},        /*  3: View */
    {KEY, SDLK_RIGHT, 0, 0, 0},        /*  4: Run */
    {KEY, SDLK_RIGHT, 0, 0, 0},        /*  5: Help */
    {KEY, SDLK_ESCAPE, 0, 0, 0},       /*  6: closed */
    {ALT_KEY, SDLK_v, 0, 0, 0},        /*  7: View, REPL selected */
    {KEY, SDLK_RETURN, 0, 0, 0},       /*  8: the REPL shows */
    {KEY, SDLK_F6, 0, 0, 0},           /*  9: and hides (its shortcut) */
    {ALT_KEY, SDLK_h, 0, 0, 0},        /* 10: Help */
    {TEXT, 0, 'a', 0, 0},              /* 11: About (its mnemonic) */
    {PRESS, 0, 0, FILE_TITLE_X, BAR_Y},   /* 12: File opens on the press */
    {RELEASE, 0, 0, FILE_TITLE_X, BAR_Y}, /* 13: and stays open */
    {PRESS, 0, 0, 600, 300},           /* 14: an outside press only closes it */
    {RELEASE, 0, 0, 600, 300},         /* 15: its release is the menu's too */
    {QUIT, 0, 0, 0, 0},
};
#define STEPS (sizeof(script) / sizeof(script[0]))

int SDL_PushEvent(SDL_Event *event)
{
    static int (*push)(SDL_Event *);
    if (generated_event) {
        generated_event = 0;
        queued_event = *event;
        have_event = 1;
        return 1;
    }
    if (push == NULL) push = dlsym(RTLD_NEXT, "SDL_PushEvent");
    if (push == NULL) abort();
    return push(event);
}

int SDL_PollEvent(SDL_Event *event)
{
    static int (*poll)(SDL_Event *);
    if (have_event) {
        if (event != NULL) { *event = queued_event; have_event = 0; }
        return 1;
    }
    if (poll == NULL) poll = dlsym(RTLD_NEXT, "SDL_PollEvent");
    if (poll == NULL) abort();
    return poll(event);
}

void SDL_RenderPresent(SDL_Renderer *renderer)
{
    static void (*present)(SDL_Renderer *);
    const char *directory = getenv("CCL_TEST_CAPTURE");
    if (present == NULL) present = dlsym(RTLD_NEXT, "SDL_RenderPresent");
    if (present == NULL || directory == NULL) abort();
    int width, height;
    if (SDL_GetRendererOutputSize(renderer, &width, &height) != 0) abort();
    SDL_Surface *surface = SDL_CreateRGBSurfaceWithFormat
        (0, width, height, 32, SDL_PIXELFORMAT_ARGB8888);
    if (surface == NULL) abort();
    if (SDL_RenderReadPixels(renderer, NULL, surface->format->format,
                             surface->pixels, surface->pitch) != 0) abort();
    char path[1024];
    int length = snprintf(path, sizeof(path), "%s/frame-%02u.bmp", directory, stage);
    if (length < 0 || (size_t)length >= sizeof(path) || SDL_SaveBMP(surface, path) != 0) abort();
    SDL_FreeSurface(surface);
    present(renderer);
}

static void deliver(SDL_Event *event)
{
    const struct step *s = &script[stage < STEPS ? stage : STEPS - 1];
    memset(event, 0, sizeof(*event));
    switch (s->action) {
    case KEY:
    case ALT_KEY:
        event->type = SDL_KEYDOWN;
        event->key.keysym.sym = s->key;
        event->key.keysym.mod = s->action == ALT_KEY ? KMOD_LALT : KMOD_NONE;
        break;
    case TEXT:
        event->type = SDL_TEXTINPUT;
        event->text.text[0] = s->text;
        break;
    case PRESS:
    case RELEASE:
        event->type = s->action == PRESS ? SDL_MOUSEBUTTONDOWN : SDL_MOUSEBUTTONUP;
        event->button.button = SDL_BUTTON_LEFT;
        event->button.clicks = 1;
        event->button.x = s->x;
        event->button.y = s->y;
        break;
    case QUIT:
        event->type = SDL_QUIT;
        break;
    }
    ++stage;
    generated_event = 1;
}

int SDL_WaitEvent(SDL_Event *event)
{
    deliver(event);
    return 1;
}

int SDL_WaitEventTimeout(SDL_Event *event, int timeout)
{
    (void)timeout;
    deliver(event);
    return 1;
}
