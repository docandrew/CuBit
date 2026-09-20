/* Linux-only SDL event driver. Loaded by the test, never linked into CuBit.
   Exercise the real shared Workbench loop and capture its presented frames. */
#include <SDL.h>
#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static unsigned stage;
static SDL_Event queued_event;
static int generated_event, have_event;

/* Keep synthetic events at the SDL2 application boundary. In particular, do
   not send artificial text payloads through SDL2-compat's SDL3 event queue. */
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

int SDL_WaitEvent(SDL_Event *event)
{
    static const char filename[] = "clock.ccl";
    memset(event, 0, sizeof(*event));
    event->type = SDL_KEYDOWN;
    const char *syntax_source = getenv("CCL_TEST_SYNTAX");
    const char *button_source = getenv("CCL_TEST_BUTTON");
    const char *unsaved = getenv("CCL_TEST_UNSAVED");
    if (unsaved != NULL) {
        if (stage == 0) { event->key.keysym.sym = SDLK_a; event->key.keysym.mod = KMOD_CTRL; }
        else if (stage <= 3) { event->type = SDL_TEXTINPUT; event->text.text[0] = '7'; }
        else if (stage == 4) { event->key.keysym.sym = SDLK_o; event->key.keysym.mod = KMOD_CTRL; }
        else {
            event->type = SDL_QUIT;
            SDL_Keycode key = SDLK_UNKNOWN;
            SDL_Keymod mod = KMOD_NONE;
            if (strcmp(unsaved, "cancel") == 0) {
                if (stage == 5) key = SDLK_ESCAPE;
                if (stage == 6) { key = SDLK_o; mod = KMOD_CTRL; }
            } else if (strcmp(unsaved, "discard") == 0) {
                if (stage == 5 || stage == 9) key = SDLK_TAB;
                if (stage == 6 || stage == 10 || stage == 11) key = SDLK_RETURN;
                if (stage == 7) key = SDLK_ESCAPE;
                if (stage == 8 || stage == 12) { key = SDLK_o; mod = KMOD_CTRL; }
            } else if (strcmp(unsaved, "save") == 0) {
                if (stage == 5 || stage == 6 || stage == 11) key = SDLK_RETURN;
                if (stage == 7) key = SDLK_ESCAPE;
                if (stage == 8 || stage == 12) { key = SDLK_o; mod = KMOD_CTRL; }
                if (stage == 9 || stage == 10) key = SDLK_DOWN;
            } else if (strcmp(unsaved, "save-cancel") == 0) {
                if (stage == 5 || stage == 10) key = SDLK_RETURN;
                if (stage == 6 || stage == 8) key = SDLK_ESCAPE;
                if (stage == 7) { key = SDLK_o; mod = KMOD_CTRL; }
                if (stage == 9) { key = SDLK_s; mod = KMOD_CTRL; }
            } else if (strcmp(unsaved, "save-fail") == 0) {
                if (stage == 5 || stage == 16) key = SDLK_RETURN;
                if (stage == 6) { key = SDLK_a; mod = KMOD_CTRL; }
                if (stage >= 7 && stage <= 15) {
                    event->type = SDL_TEXTINPUT; event->text.text[0] = "hello.ccl"[stage - 7];
                }
                if (stage == 17) key = SDLK_ESCAPE;
                if (stage == 18) { key = SDLK_o; mod = KMOD_CTRL; }
            } else if (strcmp(unsaved, "open-fail") == 0) {
                if (stage == 5 || stage == 7) key = SDLK_TAB;
                if (stage == 6 || stage == 20) key = SDLK_RETURN;
                if (stage == 8) { key = SDLK_a; mod = KMOD_CTRL; }
                if (stage >= 9 && stage <= 19) {
                    event->type = SDL_TEXTINPUT; event->text.text[0] = "missing.ccl"[stage - 9];
                }
                if (stage == 21) key = SDLK_ESCAPE;
                if (stage == 22) { key = SDLK_o; mod = KMOD_CTRL; }
            }
            if (key != SDLK_UNKNOWN) {
                event->type = SDL_KEYDOWN; event->key.keysym.sym = key; event->key.keysym.mod = mod;
            }
        }
        ++stage;
        generated_event = 1;
        return 1;
    }
    if (button_source != NULL) {
        unsigned length = (unsigned)strlen(button_source);
        if (stage == 0) { event->key.keysym.sym = SDLK_a; event->key.keysym.mod = KMOD_CTRL; }
        else if (stage <= length) {
            event->type = SDL_TEXTINPUT;
            event->text.text[0] = button_source[stage - 1];
        } else {
            unsigned action = stage - length - 1;
            if (action == 0 || action == 6) event->key.keysym.sym = SDLK_F5;
            else if (action == 3) { event->key.keysym.sym = SDLK_a; event->key.keysym.mod = KMOD_CTRL; }
            else if (action == 4 || action == 5) {
                event->type = SDL_TEXTINPUT;
                event->text.text[0] = action == 4 ? '4' : '2';
            } else if (action <= 12) {
                int down = action == 1 || action == 7 || action == 9 || action == 11;
                event->type = down ? SDL_MOUSEBUTTONDOWN : SDL_MOUSEBUTTONUP;
                event->button.button = SDL_BUTTON_LEFT;
                event->button.x = action == 9 || action == 10 ? 200 : 410;
                event->button.y = 60;
            } else event->type = SDL_QUIT;
        }
        ++stage;
        generated_event = 1;
        return 1;
    }
    if (syntax_source != NULL) {
        unsigned length = (unsigned)strlen(syntax_source);
        if (stage == 0) { event->key.keysym.sym = SDLK_a; event->key.keysym.mod = KMOD_CTRL; }
        else if (stage <= length) {
            event->type = SDL_TEXTINPUT;
            event->text.text[0] = syntax_source[stage - 1];
        } else if (getenv("CCL_TEST_SYNTAX_VM") != NULL) {
            unsigned action = stage - length - 1;
            if (action < 4) {
                event->type = action % 2 == 0 ? SDL_MOUSEBUTTONDOWN : SDL_MOUSEBUTTONUP;
                event->button.button = SDL_BUTTON_LEFT;
                event->button.x = action < 2 ? 115 : 240;
                event->button.y = 60;
            } else if (action == 4 || action == 5) {
                event->key.keysym.sym = SDLK_F8;
                if (action == 5) event->key.keysym.mod = KMOD_SHIFT;
            } else event->type = SDL_QUIT;
        } else if (stage == length + 1) event->key.keysym.sym = SDLK_F8;
        else if (stage == length + 2) event->key.keysym.sym = SDLK_F5;
        else if (stage == length + 3) event->key.keysym.sym = SDLK_F8;
        else if (stage == length + 4) event->key.keysym.sym = SDLK_F5;
        else event->type = SDL_QUIT;
        ++stage;
        generated_event = 1;
        return 1;
    }
    if (getenv("CCL_TEST_UI_HOOKS") != NULL) {
        static const char *commands[] = {
            "(ui.label-value 42)", "(ui.label-visible false)",
            "(clock.monotonic-ms)",
            "(define (greet (name String)) String (concat \"Hello, \" name)) "
            "(ui.label-text (greet \"Cubie\"))",
            "(ui.output-append \"Hello from CCL!\")",
            "(ui.output-append (to-string (clock.monotonic-ms)))",
            "(ui.output-clear)",
            "(ui.output-append \"line 01\\nline 02\\nline 03\\nline 04\\nline 05\\nline 06\\nline 07\\nline 08\\nline 09\\nline 10\")"
        };
        unsigned offset = 1;
        if (stage == 0) event->key.keysym.sym = SDLK_F6;
        else {
            event->type = SDL_QUIT;
            for (unsigned i = 0; i < sizeof(commands) / sizeof(commands[0]); ++i) {
                unsigned length = (unsigned)strlen(commands[i]);
                if (stage >= offset && stage < offset + length) {
                    event->type = SDL_TEXTINPUT;
                    event->text.text[0] = commands[i][stage - offset];
                    break;
                }
                if (stage == offset + length) {
                    event->type = SDL_KEYDOWN;
                    event->key.keysym.sym = SDLK_RETURN;
                    break;
                }
                offset += length + 1;
            }
            if (event->type == SDL_QUIT && stage >= offset) {
                unsigned action = stage - offset;
                if (action < 2 || action == 3 || action == 5 ||
                    (action >= 6 && action < 10)) {
                    int down = action == 0 || action == 3 || action == 6 || action == 8;
                    event->type = down ? SDL_MOUSEBUTTONDOWN : SDL_MOUSEBUTTONUP;
                    event->button.button = SDL_BUTTON_LEFT;
                    event->button.x = action == 3 || action == 5 ? 910 :
                        action == 6 || action == 7 ? 890 : 300;
                    event->button.y = action == 3 ? 560 : action == 5 ? 535 :
                        action == 6 || action == 7 ? 503 : 530;
                } else if (action == 2) {
                    event->type = SDL_MOUSEWHEEL;
                    event->wheel.y = 1;
                } else if (action == 4) {
                    event->type = SDL_MOUSEMOTION;
                    event->motion.state = SDL_BUTTON_LMASK;
                    event->motion.x = 910;
                    event->motion.y = 535;
                } else if (action == 10) {
                    event->type = SDL_TEXTINPUT;
                    event->text.text[0] = 'X';
                } else if (action == 11) {
                    event->type = SDL_KEYDOWN;
                    event->key.keysym.sym = SDLK_F6;
                }
            }
        }
        ++stage;
        generated_event = 1;
        return 1;
    }
    const char *probe = getenv("CCL_TEST_COMPLETION_PROBE");
    if (probe != NULL) {
        size_t length = strlen(probe);
        if (stage == 0) event->key.keysym.sym = SDLK_F6;
        else if (stage <= length) {
            event->type = SDL_TEXTINPUT;
            event->text.text[0] = probe[stage - 1];
        } else if (stage == length + 1) {
            const char *move = getenv("CCL_TEST_COMPLETION_MOVE");
            if (move != NULL && strcmp(move, "select") == 0) {
                event->key.keysym.sym = SDLK_a;
                event->key.keysym.mod = KMOD_CTRL;
            } else if (move != NULL && strcmp(move, "left") == 0) {
                event->key.keysym.sym = SDLK_LEFT;
            } else {
                event->key.keysym.sym = SDLK_SPACE;
                event->key.keysym.mod = KMOD_CTRL;
            }
        } else if (stage == length + 2) event->key.keysym.sym = SDLK_TAB;
        else event->type = SDL_QUIT;
        ++stage;
        generated_event = 1;
        return 1;
    }
    if (getenv("CCL_TEST_COMPLETION") != NULL) {
        /* Compare completion with the exact same text entered manually. */
        static const char prefix[] = "(clock.mon";
        static const char complete[] = "(clock.monotonic-ms";
        if (stage == 0) event->key.keysym.sym = SDLK_F6;
        else if (stage >= 1 && stage <= strlen(prefix)) {
            event->type = SDL_TEXTINPUT;
            event->text.text[0] = prefix[stage - 1];
        } else if (stage == 11) {
            event->key.keysym.sym = SDLK_SPACE;
            event->key.keysym.mod = KMOD_CTRL;
        } else if (stage == 12) {
            event->key.keysym.sym = SDLK_TAB;
        } else if (stage == 13) {
            event->key.keysym.sym = SDLK_a;
            event->key.keysym.mod = KMOD_CTRL;
        } else if (stage >= 14 && stage < 14 + strlen(complete)) {
            event->type = SDL_TEXTINPUT;
            event->text.text[0] = complete[stage - 14];
        } else event->type = SDL_QUIT;
        ++stage;
        generated_event = 1;
        return 1;
    }
    if (getenv("CCL_TEST_REPL") != NULL) {
        /* Nine text events, including both string delimiters. */
        static const char expression[] = "\"hello!!\"";
        switch (stage) {
        case 0: case 13: case 15: event->key.keysym.sym = SDLK_F6; break;
        case 10: event->key.keysym.sym = SDLK_RETURN; break;
        case 11: event->key.keysym.sym = SDLK_UP; break;
        case 12: event->key.keysym.sym = SDLK_DOWN; break;
        case 14: event->key.keysym.sym = SDLK_F5; break;
        default:
            if (stage >= 1 && stage <= 9) {
                event->type = SDL_TEXTINPUT;
                event->text.text[0] = expression[stage - 1];
            } else event->type = SDL_QUIT;
            break;
        }
        ++stage;
        generated_event = 1;
        return 1;
    }
    switch (stage) {
    case 0:
        event->type = SDL_MOUSEMOTION;
        event->motion.x = 18; event->motion.y = 60;
        break;
    case 1:
        event->key.keysym.sym = SDLK_o;
        event->key.keysym.mod = KMOD_CTRL;
        break;
    case 2: event->key.keysym.sym = SDLK_DOWN; break;
    case 3: event->key.keysym.sym = SDLK_ESCAPE; break;
    case 4:
        event->key.keysym.sym = SDLK_s;
        event->key.keysym.mod = KMOD_CTRL;
        break;
    case 14: event->key.keysym.sym = SDLK_RETURN; break;
    case 15:
        event->key.keysym.sym = SDLK_o;
        event->key.keysym.mod = KMOD_CTRL;
        break;
    case 16: case 17: event->key.keysym.sym = SDLK_DOWN; break;
    case 18: event->key.keysym.sym = SDLK_RETURN; break;
    default:
        if (stage >= 5 && stage < 14) {
            event->type = SDL_TEXTINPUT;
            event->text.text[0] = filename[stage - 5];
        } else event->type = SDL_QUIT;
        break;
    }
    ++stage;
    generated_event = 1;
    return 1;
}
