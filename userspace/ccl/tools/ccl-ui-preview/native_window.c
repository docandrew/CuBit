/* HOSTED/LINUX Workbench adapter only.  CuBit presents the same shared canvas
 * through display-service IPC and does not link SDL. */
#define SDL_MAIN_HANDLED
#include <SDL2/SDL.h>
#include <stdint.h>
#include <stdio.h>

struct ccl_window {
    SDL_Window *window;
    SDL_Renderer *renderer;
    SDL_Texture *texture;
    int frames_left;
    const char *screenshot_path;
    int debug_input;
    Uint64 last_click_ms;
    int click_origin_x;
    int click_origin_y;
    unsigned int click_count;
};

enum {
    CCL_MULTI_CLICK_MS = 500,
    CCL_MULTI_CLICK_RADIUS = 5
};

static unsigned int ccl_count_click(struct ccl_window *state, int x, int y)
{
    Uint64 now = SDL_GetTicks64();
    int dx = x - state->click_origin_x;
    int dy = y - state->click_origin_y;
    int continues = state->click_count > 0 && state->click_count < 3 &&
                    now - state->last_click_ms <= CCL_MULTI_CLICK_MS &&
                    dx >= -CCL_MULTI_CLICK_RADIUS &&
                    dx <= CCL_MULTI_CLICK_RADIUS &&
                    dy >= -CCL_MULTI_CLICK_RADIUS &&
                    dy <= CCL_MULTI_CLICK_RADIUS;

    if (continues)
        ++state->click_count;
    else {
        state->click_count = 1;
        state->click_origin_x = x;
        state->click_origin_y = y;
    }
    state->last_click_ms = now;
    return state->click_count;
}

void *ccl_window_open(int width, int height)
{
    struct ccl_window *state = NULL;
    SDL_Rect display_bounds;
    int window_width = width;
    int window_height = height;

    SDL_SetMainReady();
    SDL_SetHint(SDL_HINT_VIDEO_HIGHDPI_DISABLED, "1");
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_EVENTS) != 0) return NULL;
    state = SDL_calloc(1, sizeof(*state));
    if (state == NULL) { SDL_Quit(); return NULL; }
    {
        const char *frames = SDL_getenv("CCL_UI_PREVIEW_FRAMES");
        state->frames_left = frames != NULL ? SDL_atoi(frames) : -1;
        state->screenshot_path = SDL_getenv("CCL_UI_SCREENSHOT");
        state->debug_input = SDL_getenv("CCL_UI_DEBUG_INPUT") != NULL;
    }
    if (SDL_GetDisplayUsableBounds(0, &display_bounds) == 0) {
        int scale_width = (display_bounds.w * 4 / 5) / width;
        int scale_height = (display_bounds.h * 4 / 5) / height;
        int scale = scale_width < scale_height ? scale_width : scale_height;
        if (scale > 1) { window_width = width * scale; window_height = height * scale; }
    }
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "nearest");
    state->window = SDL_CreateWindow("CCL Workbench - Linux preview",
        SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, window_width,
        window_height, SDL_WINDOW_SHOWN | SDL_WINDOW_RESIZABLE);
    if (state->window == NULL) goto fail;
    state->renderer = SDL_CreateRenderer(state->window, -1,
        SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
    if (state->renderer == NULL)
        state->renderer = SDL_CreateRenderer(state->window, -1, SDL_RENDERER_SOFTWARE);
    if (state->renderer == NULL ||
        SDL_RenderSetLogicalSize(state->renderer, width, height) != 0 ||
        SDL_RenderSetIntegerScale(state->renderer, SDL_TRUE) != 0) goto fail;
    state->texture = SDL_CreateTexture(state->renderer, SDL_PIXELFORMAT_ARGB8888,
        SDL_TEXTUREACCESS_STREAMING, width, height);
    if (state->texture == NULL) goto fail;
    SDL_StartTextInput();
    return state;
fail:
    SDL_DestroyTexture(state->texture); SDL_DestroyRenderer(state->renderer);
    SDL_DestroyWindow(state->window); SDL_free(state); SDL_Quit(); return NULL;
}

/* Event kinds include 23 undo, 24 redo, and 25 run source. */
int ccl_window_poll(void *handle, int *kind, unsigned int *character,
                    unsigned int *modifiers, int *x, int *y)
{
    SDL_Event event;
    struct ccl_window *state = handle;
    SDL_Keymod mods;
    if (state->frames_left == 0) { *kind = 1; return 1; }
    while (SDL_PollEvent(&event) != 0) {
        if (event.type == SDL_QUIT) {
            if (state->debug_input)
                fprintf(stderr, "close event type=%u\n", event.type);
            *kind = 1; return 1;
        }
        if (event.type == SDL_TEXTINPUT && (unsigned char)event.text.text[0] < 128) {
            if (state->debug_input)
                fprintf(stderr, "text input byte=%u ('%c')\n",
                        (unsigned char)event.text.text[0], event.text.text[0]);
            *kind = 2; *character = (unsigned char)event.text.text[0];
            *modifiers = 0; return 1;
        }
        if (event.type == SDL_MOUSEBUTTONDOWN &&
            event.button.button == SDL_BUTTON_LEFT) {
            unsigned int click_count;
            *x = event.button.x; *y = event.button.y;
            click_count = ccl_count_click(state, *x, *y);
            if (state->debug_input)
                fprintf(stderr, "mouse down window=%d,%d logical=%d,%d clicks=%u\n",
                        event.button.x, event.button.y, *x, *y,
                        click_count);
            mods = SDL_GetModState();
            *modifiers = ((mods & KMOD_SHIFT) != 0 ? 1u : 0u) |
                         ((mods & KMOD_CTRL) != 0 ? 2u : 0u) |
                         ((mods & KMOD_ALT) != 0 ? 4u : 0u);
            if (click_count == 3)
                *kind = 15;
            else if (click_count == 2)
                *kind = 14;
            else
                *kind = 11;
            return 1;
        }
        if (event.type == SDL_MOUSEMOTION) {
            *x = event.motion.x; *y = event.motion.y;
            if (state->debug_input)
                fprintf(stderr, "mouse motion window=%d,%d logical=%d,%d\n",
                        event.motion.x, event.motion.y, *x, *y);
            *kind = (event.motion.state & SDL_BUTTON_LMASK) != 0 ? 12 : 26;
            return 1;
        }
        if (event.type == SDL_MOUSEBUTTONUP &&
            event.button.button == SDL_BUTTON_LEFT) {
            *x = event.button.x; *y = event.button.y;
            if (state->debug_input)
                fprintf(stderr, "mouse up window=%d,%d logical=%d,%d\n",
                        event.button.x, event.button.y, *x, *y);
            *kind = 13; return 1;
        }
        if (event.type == SDL_MOUSEWHEEL) {
            *kind = event.wheel.y > 0 ? 18 : 19;
            return 1;
        }
        if (event.type != SDL_KEYDOWN) continue;
        mods = event.key.keysym.mod;
        if (state->debug_input)
            fprintf(stderr, "key down sym=%d scancode=%d modifiers=%u\n",
                    event.key.keysym.sym, event.key.keysym.scancode,
                    (unsigned int)mods);
        *modifiers = ((mods & KMOD_SHIFT) != 0 ? 1u : 0u) |
                     ((mods & KMOD_CTRL) != 0 ? 2u : 0u) |
                     ((mods & KMOD_ALT) != 0 ? 4u : 0u);
        if (event.key.keysym.sym == SDLK_z && (*modifiers & 2u) != 0) {
            *kind = (*modifiers & 1u) != 0 ? 24 : 23; return 1;
        }
        if (event.key.keysym.sym == SDLK_y && (*modifiers & 2u) != 0) {
            *kind = 24; return 1;
        }
        if (event.key.keysym.sym == SDLK_F5 ||
            ((event.key.keysym.sym == SDLK_RETURN ||
              event.key.keysym.sym == SDLK_KP_ENTER) &&
             (*modifiers & 2u) != 0)) {
            *kind = 25; return 1;
        }
        if (event.key.keysym.sym == SDLK_ESCAPE) { *kind = 22; return 1; }
        if (event.key.keysym.sym == SDLK_BACKSPACE) { *kind = 3; return 1; }
        if (event.key.keysym.sym == SDLK_RETURN ||
            event.key.keysym.sym == SDLK_KP_ENTER) { *kind = 4; return 1; }
        if (event.key.keysym.sym == SDLK_LEFT) { *kind = 5; return 1; }
        if (event.key.keysym.sym == SDLK_RIGHT) { *kind = 6; return 1; }
        if (event.key.keysym.sym == SDLK_HOME) { *kind = 7; return 1; }
        if (event.key.keysym.sym == SDLK_END) { *kind = 8; return 1; }
        if (event.key.keysym.sym == SDLK_DELETE) { *kind = 9; return 1; }
        if (event.key.keysym.sym == SDLK_UP) { *kind = 16; return 1; }
        if (event.key.keysym.sym == SDLK_DOWN) { *kind = 17; return 1; }
        if (event.key.keysym.sym == SDLK_PAGEUP) { *kind = 20; return 1; }
        if (event.key.keysym.sym == SDLK_PAGEDOWN) { *kind = 21; return 1; }
        if (event.key.keysym.sym == SDLK_a && (*modifiers & 2u) != 0) {
            *kind = 10; return 1;
        }
    }
    return 0;
}

int ccl_window_present(void *handle, const uint32_t *pixels, int pitch)
{
    struct ccl_window *state = handle;
    if (SDL_UpdateTexture(state->texture, NULL, pixels, pitch) != 0 ||
        SDL_RenderClear(state->renderer) != 0 ||
        SDL_RenderCopy(state->renderer, state->texture, NULL, NULL) != 0) {
        fprintf(stderr, "CCL Workbench presentation failed: %s\n", SDL_GetError());
        return 1;
    }
    if (state->screenshot_path != NULL) {
        int width;
        int height;
        SDL_Surface *capture;
        if (SDL_GetRendererOutputSize(state->renderer, &width, &height) != 0)
            return 1;
        capture = SDL_CreateRGBSurfaceWithFormat(0, width, height, 32,
                                                 SDL_PIXELFORMAT_ARGB8888);
        if (capture == NULL ||
            SDL_RenderReadPixels(state->renderer, NULL, SDL_PIXELFORMAT_ARGB8888,
                                 capture->pixels, capture->pitch) != 0 ||
            SDL_SaveBMP(capture, state->screenshot_path) != 0) {
            SDL_FreeSurface(capture);
            return 1;
        }
        SDL_FreeSurface(capture);
        state->screenshot_path = NULL;
    }
    SDL_RenderPresent(state->renderer);
    if (state->frames_left > 0) --state->frames_left;
    return 0;
}

void ccl_window_wait(void) { SDL_Delay(10); }

uint64_t ccl_window_ticks(void) { return SDL_GetTicks64(); }

void ccl_window_close(void *handle)
{
    struct ccl_window *state = handle;
    SDL_StopTextInput(); SDL_DestroyTexture(state->texture);
    SDL_DestroyRenderer(state->renderer); SDL_DestroyWindow(state->window);
    SDL_free(state); SDL_Quit();
}
