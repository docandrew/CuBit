#include "gb.h"
#include "cubit.h"
#include "cubit_desktop.h"
#include "cubit_audio.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* All native messages cross existing endpoint/grant boundaries. The emulator
 * has no networking, device, camera, process-launch or writable-file authority. */
struct __attribute__((packed)) service_entry {
    uint8_t kind, rights; uint16_t slot; uint32_t role; uint64_t parameter;
};
static const struct __attribute__((packed)) {
    uint32_t magic; uint16_t version, count; struct service_entry entries[3];
} manifest __attribute__((section(".cubit.caps"), used)) = {
    0x43424954, 1, 3, {{CUBIT_REQ_SERVICE, CUBIT_RIGHT_RW, 1, 6, 0},
                     {CUBIT_REQ_SERVICE, CUBIT_RIGHT_RW, CAP_SLOT_MIXER, 9, 0},
                     {CUBIT_REQ_SERVICE, CUBIT_RIGHT_RW, 21, 15, 0}}
};
struct __attribute__((packed)) access_entry {
    uint8_t rights, length, flags, reserved; uint16_t uid, gid;
    char prefix[64]; uint64_t reserved64;
};
_Static_assert(sizeof(struct access_entry) == 80, "access wire size");
static const struct __attribute__((packed)) {
    uint32_t magic; uint16_t version, count, uid, gid;
    uint8_t trust, sandbox; uint16_t reserved; struct access_entry entries[1];
} access_manifest __attribute__((section(".cubit.access"), used)) = {
    CUBIT_ACCESS_MAGIC, 1, 1, 0, 0, 0, 0, 0,
    {{CUBIT_ACL_READ, 8, 0, 0, 0, 0, "sameboy/", 0}}
};
static const unsigned char identity[] __attribute__((section(".cubit.id"), used)) = {
    'C','B','I','D',1,0,1,0, 2,17,0,'i','d',
    'c','o','m','.','c','u','b','i','t','.','s','a','m','e','b','o','y'
};
static const unsigned char streams[] __attribute__((section(".cubit.streams"), used)) = {
    'C','B','S','T',1,0,1,0, 4,0,4,0,1,0,0,0
};

enum { HELLO=0x0800, BYE=0x0801, CREATE=0x0810, DESTROY=0x0811,
       PRESENT=0x0812, POLL=0x0821, LIMITS=0x0841, TITLE=0x0842 };
enum { WIDTH=160, HEIGHT=144, SCALE=3, VIEW_W=WIDTH*SCALE, VIEW_H=HEIGHT*SCALE,
       WINDOW_W=VIEW_W+20, WINDOW_H=VIEW_H+44, MAX_ROM=8*1024*1024, ROM_SLOTS=16 };
static GB_gameboy_t *gameboy;
static uint32_t pixels[WIDTH*HEIGHT];
static uint32_t *surface_pixels;
static uint64_t surface, serial;
static unsigned frames, selected_rom;
static int paused, change_rom, running=1;
static int frame_ready;
static unsigned char keys_down[128];
static void title(const char *text);
enum { AUDIO_RATE=48000, AUDIO_BATCH=2048, AUDIO_PREFILL=1536 };
static GB_sample_t audio_samples[AUDIO_BATCH];
static unsigned audio_count, audio_first, audio_primed;
static unsigned volume=70;
static int audio_enabled, audio_started, audio_overflow, muted;
static uint64_t audio_progress;
_Static_assert(sizeof(GB_sample_t)==4, "native stereo frame layout");

static void audio_reset(void)
{
    cubit_audio_close();
    audio_count=audio_first=audio_primed=0;
    audio_started=audio_overflow=0;
    audio_enabled=!paused && cubit_audio_open();
    if(audio_enabled) cubit_audio_volume(muted?0:volume);
    audio_progress=cubit_gettime_ms();
}
static void audio_sample(GB_gameboy_t *gb, GB_sample_t *sample)
{
    (void)gb;
    if(!audio_enabled) return;
    if(audio_count==AUDIO_BATCH) { audio_overflow=1; return; }
    audio_samples[audio_count++]=*sample;
}
/* Retain partial writes. Backpressure is handled by the outer event loop, not
 * by blocking inside the emulator's per-sample callback. */
static void audio_flush(void)
{
    if(!audio_enabled) return;
    unsigned written=cubit_audio_write(audio_samples+audio_first,audio_count-audio_first);
    audio_first+=written;
    if(written) audio_progress=cubit_gettime_ms();
    if(!audio_started) {
        audio_primed+=written;
        if(audio_primed>=AUDIO_PREFILL) {
            cubit_audio_start(); audio_started=1;
            puts("sameboy: native audio started (48000 Hz stereo)");
        }
    }
    if(audio_first==audio_count) audio_first=audio_count=0;
    if(audio_overflow || (audio_count && cubit_gettime_ms()-audio_progress>250)) {
        puts("sameboy: audio stalled/overflowed; continuing silently");
        cubit_audio_close(); audio_enabled=0; audio_first=audio_count=0;
    }
}
static void volume_changed(void)
{
    cubit_audio_volume(muted?0:volume);
    char text[24];
    snprintf(text,sizeof(text),"SameBoy - %s%u%%",muted?"mute ":"",volume);
    title(text);
    printf("sameboy: volume=%u mute=%d\n",volume,muted);
}
extern const uint8_t sb_dmg_boot[], sb_dmg_boot_end[], sb_cgb_boot[], sb_cgb_boot_end[];

static int call(unsigned op, uint64_t a, uint64_t b, uint64_t c, uint64_t d,
                cubit_async_message_t *reply)
{
    cubit_async_message_t msg={0};
    msg.tag.label=op; msg.tag.length=4;
    msg.words[0]=a; msg.words[1]=b; msg.words[2]=c; msg.words[3]=d;
    if (syscall2(SYSCALL_CALL_VIA_ENDPOINT_CAPABILITY, CAP_SLOT_DESKTOP, &msg)==-1)
        return -1;
    if (reply) *reply=msg;
    return msg.tag.label==op ? 0 : -1;
}
static void title(const char *text)
{
    uint64_t words[3]={0};
    size_t length=strlen(text); if (length>23) length=23;
    for(size_t i=0;i<length;i++) words[i/8]|=(uint64_t)(uint8_t)text[i]<<(8*(i%8));
    words[2]|=(uint64_t)length<<56;
    call(TITLE,surface,words[0],words[1],words[2],NULL);
}
static void shutdown(void)
{
    cubit_audio_close();
    if(surface) { call(DESTROY,surface,0,0,0,NULL); surface=0; }
    call(BYE,0,0,0,0,NULL);
}
static int window(void)
{
    cubit_async_message_t msg;
    if(call(HELLO,1ULL<<32,0,0,0,&msg)<0 || !msg.words[0]) return -1;
    if(call(CREATE,WINDOW_W,WINDOW_H,2,0,&msg)<0 || !msg.words[0]) return -1;
    surface=msg.words[0];
    uint64_t size=WINDOW_W|((uint64_t)WINDOW_H<<32);
    if(call(LIMITS,surface,size,size,1|4|16|128,NULL)<0) return -1;
    size_t pages=(VIEW_W*VIEW_H*4+4095)/4096;
    void *raw=cubit_sbrk(pages*4096+4096);
    if(raw==(void *)-1) return -1;
    surface_pixels=(uint32_t *)(((uintptr_t)raw+4095)&~(uintptr_t)4095);
    memset(surface_pixels,0,pages*4096);
    long grant=syscall4(SYSCALL_CREATE_SHARED_MEMORY_GRANT_VIA_CAPABILITY,
                       CAP_SLOT_DESKTOP,surface_pixels,pages,0);
    if(grant<0 || cubit_desktop_attach_buffer(surface,grant,VIEW_W,VIEW_H,VIEW_W*4)<0)
        return -1;
    title("SameBoy");
    return 0;
}
static uint32_t rgb(GB_gameboy_t *gb, uint8_t r, uint8_t g, uint8_t b)
{ (void)gb; return 0xff000000u|((uint32_t)r<<16)|((uint32_t)g<<8)|b; }
static void log_message(GB_gameboy_t *gb, const char *text, GB_log_attributes_t attr)
{ (void)gb; (void)attr; cubit_stream_print(CUBIT_STREAM_LOG,text); }
static void boot_rom(GB_gameboy_t *gb, GB_boot_rom_t type)
{
    (void)type;
    if(GB_is_cgb(gb)) GB_load_boot_rom_from_buffer(gb,sb_cgb_boot,sb_cgb_boot_end-sb_cgb_boot);
    else GB_load_boot_rom_from_buffer(gb,sb_dmg_boot,sb_dmg_boot_end-sb_dmg_boot);
}
static void vblank(GB_gameboy_t *gb, GB_vblank_type_t type)
{ (void)gb; (void)type; frame_ready=1; }
static uint64_t run_frame(void)
{
    /* GB_run reports 8 MHz cycle units, including CGB double-speed mode.
     * Do not use GB_run_frame's sync counter: disabled host timekeeping
     * resets it at VBlank. Count exactly the cycles we actually emulate. */
    uint64_t cycles=0;
    frame_ready=0;
    while(!frame_ready) cycles+=GB_run(gameboy);
    return cycles*1000000000ULL/(2ULL*GB_get_clock_rate(gameboy));
}
static int load_rom(unsigned index)
{
    char path[32]; snprintf(path,sizeof(path),"sameboy/%02u.gb",index);
    FILE *file=fopen(path,"rb"); if(!file) return -1;
    if(fseek(file,0,SEEK_END)) { fclose(file); return -1; }
    long length=ftell(file);
    if(length<0x150 || length>MAX_ROM || fseek(file,0,SEEK_SET)) {
        fclose(file); return -1;
    }
    uint8_t *rom=malloc((size_t)length);
    if(!rom) { fclose(file); return -1; }
    size_t got=fread(rom,1,(size_t)length,file); fclose(file);
    if(got!=(size_t)length) { free(rom); return -1; }
    GB_gameboy_t *allocated=GB_alloc();
    if(!allocated) { free(rom); return -1; }
    if(gameboy) { GB_free(gameboy); GB_dealloc(gameboy); gameboy=NULL; }
    GB_model_t model=(rom[0x143]&0x80)?GB_MODEL_CGB_E:GB_MODEL_DMG_B;
    gameboy=GB_init(allocated,model);
    GB_set_log_callback(gameboy,log_message);
    GB_set_rgb_encode_callback(gameboy,rgb);
    GB_set_pixels_output(gameboy,pixels);
    GB_set_boot_rom_load_callback(gameboy,boot_rom);
    GB_set_vblank_callback(gameboy,vblank);
    GB_apu_set_sample_callback(gameboy,audio_sample);
    GB_set_sample_rate(gameboy,AUDIO_RATE);
    GB_set_turbo_mode(gameboy,true,true);
    GB_load_rom_from_buffer(gameboy,rom,(size_t)length);
    free(rom);
    selected_rom=index; frames=0; paused=0;
    audio_reset();
    if(!audio_enabled) puts("sameboy: mixer unavailable; continuing silently");
    memset(keys_down,0,sizeof(keys_down));
    char text[24]; snprintf(text,sizeof(text),"SameBoy - ROM %02u",index);
    title(text);
    printf("sameboy: loaded ROM %02u (%ld bytes)\n",index,length);
    return 0;
}
static void input(void)
{
    for(unsigned n=0;n<32;n++) {
        cubit_async_message_t msg;
        if(call(POLL,surface,serial,0,0,&msg)<0) { running=0; return; }
        if(!cubit_desktop_input_reply_valid(POLL,msg.tag.label,msg.tag.length,
            msg.tag.flags,msg.tag.reserved,msg.words)) { running=0; return; }
        if(!msg.words[0]) return;
        serial=msg.words[1];
        if(msg.tag.flags) {
            GB_set_key_mask(gameboy,0);
            memset(keys_down,0,sizeof(keys_down));
        }
        if(msg.words[0]!=1 && msg.words[0]!=2) continue;
        int down=msg.words[0]==1; unsigned scan=msg.words[2];
        int pressed=down && !keys_down[scan];
        keys_down[scan]=down;
        GB_key_t key=GB_KEY_MAX;
        switch(scan) {
        case 0x48:key=GB_KEY_UP;break; case 0x50:key=GB_KEY_DOWN;break;
        case 0x4b:key=GB_KEY_LEFT;break; case 0x4d:key=GB_KEY_RIGHT;break;
        case 0x2c:key=GB_KEY_B;break; case 0x2d:key=GB_KEY_A;break;
        case 0x0f:key=GB_KEY_SELECT;break; case 0x1c:key=GB_KEY_START;break;
        case 0x01:if(down) running=0;break;
        case 0x19:if(pressed) { paused=!paused; audio_reset(); title(paused?"SameBoy - paused":"SameBoy"); }break;
        case 0x3c:if(pressed) change_rom=1;break;
        case 0x3f:if(pressed) { GB_reset(gameboy); audio_reset(); }break;
        case 0x42:if(pressed) { muted=!muted; volume_changed(); }break; /* F8 */
        case 0x43:if(pressed) { if(volume>=5) volume-=5; volume_changed(); }break; /* F9 */
        case 0x44:if(pressed) { if(volume<=95) volume+=5; volume_changed(); }break; /* F10 */
        }
        if(key!=GB_KEY_MAX) GB_set_key_state(gameboy,key,down);
    }
}
static void present(void)
{
    for(unsigned y=0;y<VIEW_H;y++) for(unsigned x=0;x<VIEW_W;x++)
        surface_pixels[y*VIEW_W+x]=pixels[(y/SCALE)*WIDTH+x/SCALE];
    if(call(PRESENT,surface,0,0,0,NULL)<0) running=0;
}
static int run(void)
{
    if(window()<0) { puts("sameboy: cannot create native window"); return 1; }
    puts("sameboy: native window ready");
    if(load_rom(0)<0) { puts("sameboy: cannot read sameboy/00.gb"); return 1; }
    puts("sameboy: arrows, Z/B, X/A, Enter/Start, Tab/Select; P pause, F2 next ROM, F5 reset, Esc close");
    puts("sameboy: F8 mute, F9 quieter, F10 louder (this app only)");
    uint64_t target=cubit_gettime_ms()*1000000ULL;
    while(running) {
        input(); if(!running) break;
        if(change_rom) {
            change_rom=0;
            unsigned next=(selected_rom+1)%ROM_SLOTS;
            if(load_rom(next)<0 && next) (void)load_rom(0);
            target=cubit_gettime_ms()*1000000ULL;
        }
        if(!paused) {
            audio_flush();
            if(audio_count) { cubit_sleep_ms(1); continue; }
            uint64_t elapsed=run_frame();
            audio_flush();
            present();
            target+=elapsed;
            if(++frames==120) puts("sameboy: 120 emulated frames");
        } else target=cubit_gettime_ms()*1000000ULL+10000000ULL;
        uint64_t now=cubit_gettime_ms()*1000000ULL;
        if(target>now) cubit_sleep_ms((target-now)/1000000ULL);
        else if(now-target>100000000ULL) target=now;
    }
    GB_free(gameboy); GB_dealloc(gameboy);
    return 0;
}

int main(void)
{
    atexit(shutdown);
    int status=run();
    /* CuBit's C CRT returns directly to process exit; returning from main does
     * not run libc's atexit list. Release service-owned resources explicitly. */
    shutdown();
    if(!status) puts("sameboy: clean exit");
    return status;
}
