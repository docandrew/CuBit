/* The hosted Text window's SDL2 glue (tests/text-editor/text_window.adb):
   a window whose surface the app draws into, events flattened into one
   struct, and presents of the damaged rectangle only. */
#include <SDL2/SDL.h>
#include <stdint.h>
#include <string.h>

enum text_sdl_kind {
  TEXT_SDL_NONE = 0, TEXT_SDL_KEY = 1, TEXT_SDL_TEXT = 2, TEXT_SDL_DOWN = 3, TEXT_SDL_UP = 4,
  TEXT_SDL_MOVE = 5, TEXT_SDL_WHEEL = 6, TEXT_SDL_RESIZE = 7, TEXT_SDL_QUIT = 8
};

/* Keep in step with SDL_Event in text_window.adb. */
struct text_sdl_event {
  int32_t kind;
  int32_t scancode;   /* USB HID usage (SDL_Scancode) */
  int32_t shift, control, alt;
  int32_t x, y;
  int32_t wheel;      /* positive: away from the user */
  int32_t button;
  uint32_t time_ms;
  char text[32];
};

static SDL_Window *window;

int text_sdl_open(int width, int height) {
  if (SDL_Init(SDL_INIT_VIDEO) != 0) return 0;
  window = SDL_CreateWindow("CuBit Text (hosted)", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
                            width, height, SDL_WINDOW_RESIZABLE);
  if (!window) return 0;
  SDL_StartTextInput();
  return 1;
}

/* The window's pixels (XRGB8888 is what the toolkit draws); 0 if the
   surface has another format. */
int text_sdl_surface(void **pixels, int *width, int *height, int *pitch) {
  SDL_Surface *s = SDL_GetWindowSurface(window);
  if (!s || s->format->BytesPerPixel != 4) return 0;
  *pixels = s->pixels; *width = s->w; *height = s->h; *pitch = s->pitch;
  return 1;
}

void text_sdl_present(int x, int y, int w, int h) {
  SDL_Rect r = { x, y, w, h };
  SDL_UpdateWindowSurfaceRects(window, &r, 1);
}

uint64_t text_sdl_ticks_us(void) {
  return SDL_GetPerformanceCounter() * 1000000ull / SDL_GetPerformanceFrequency();
}

static void modifiers(struct text_sdl_event *e, uint16_t mod) {
  e->shift = (mod & KMOD_SHIFT) != 0;
  e->control = (mod & KMOD_CTRL) != 0;
  e->alt = (mod & KMOD_ALT) != 0;
}

/* Waits at most timeout_ms (0: only what is queued) and flattens one event. */
int text_sdl_next(struct text_sdl_event *e, int timeout_ms) {
  SDL_Event ev;
  memset(e, 0, sizeof *e);
  if (timeout_ms > 0 ? !SDL_WaitEventTimeout(&ev, timeout_ms) : !SDL_PollEvent(&ev)) return 0;
  e->time_ms = ev.common.timestamp;
  switch (ev.type) {
  case SDL_QUIT: e->kind = TEXT_SDL_QUIT; break;
  case SDL_KEYDOWN:
    e->kind = TEXT_SDL_KEY; e->scancode = ev.key.keysym.scancode; modifiers(e, ev.key.keysym.mod); break;
  case SDL_TEXTINPUT:
    e->kind = TEXT_SDL_TEXT; SDL_strlcpy(e->text, ev.text.text, sizeof e->text); break;
  case SDL_MOUSEBUTTONDOWN: case SDL_MOUSEBUTTONUP:
    e->kind = ev.type == SDL_MOUSEBUTTONDOWN ? TEXT_SDL_DOWN : TEXT_SDL_UP;
    e->x = ev.button.x; e->y = ev.button.y; e->button = ev.button.button;
    modifiers(e, SDL_GetModState()); break;
  case SDL_MOUSEMOTION: e->kind = TEXT_SDL_MOVE; e->x = ev.motion.x; e->y = ev.motion.y; break;
  case SDL_MOUSEWHEEL: {
    int x, y; SDL_GetMouseState(&x, &y);
    e->kind = TEXT_SDL_WHEEL; e->wheel = ev.wheel.y; e->x = x; e->y = y; break; }
  case SDL_WINDOWEVENT:
    if (ev.window.event == SDL_WINDOWEVENT_SIZE_CHANGED || ev.window.event == SDL_WINDOWEVENT_EXPOSED) {
      e->kind = TEXT_SDL_RESIZE;
      SDL_GetWindowSize(window, &e->x, &e->y);
    }
    break;
  default: break;
  }
  return 1;
}

/* Called from the host file loader task: wakes the window's
   loop; SDL_PushEvent is safe from any thread. */
void text_sdl_wake(void) {
  SDL_Event ev;
  memset(&ev, 0, sizeof ev);
  ev.type = SDL_USEREVENT;
  SDL_PushEvent(&ev);
}

void text_sdl_close(void) {
  if (window) SDL_DestroyWindow(window);
  SDL_Quit();
}
