/*
 * DOOM sound module for CuBit OS
 * Copyright (C) 2026 Jon Andrew
 *
 * Thin C shim that constructs DOOM's sound_module_t and music_module_t
 * structs. The audio processing is done in Ada (CuBit.Doom_Sound).
 *
 * This file bridges DOOM's C types (sfxinfo_t, etc.) to the Ada exports.
 */

#include "doomtype.h"
#include "i_sound.h"
#include "w_wad.h"

/* Ada-exported functions (from CuBit.Doom_Sound) */
extern int  cubit_snd_init(void);
extern void cubit_snd_shutdown(void);
extern void cubit_snd_update(void);
extern int  cubit_snd_start_channel(const void *data,
                                     unsigned int data_len,
                                     unsigned int sample_rate,
                                     int channel, int vol, int sep);
extern void cubit_snd_stop_channel(int channel);
extern int  cubit_snd_is_playing(int channel);
extern void cubit_snd_update_params(int channel, int vol, int sep);

/*---------------------------------------------------------------------------
 * Sound module implementation
 *---------------------------------------------------------------------------*/

static boolean DG_SndInit(boolean use_sfx_prefix)
{
    (void)use_sfx_prefix;
    return cubit_snd_init() != 0;
}

static void DG_SndShutdown(void)
{
    cubit_snd_shutdown();
}

static int DG_GetSfxLumpNum(sfxinfo_t *sfx)
{
    char namebuf[9];
    /* Prepend "DS" to sound name for WAD lookup */
    namebuf[0] = 'D';
    namebuf[1] = 'S';
    int i;
    for (i = 0; i < 6 && sfx->name[i]; i++)
        namebuf[2 + i] = sfx->name[i];
    namebuf[2 + i] = '\0';

    return W_GetNumForName(namebuf);
}

static void DG_SndUpdate(void)
{
    cubit_snd_update();
}

static void DG_SndUpdateParams(int channel, int vol, int sep)
{
    cubit_snd_update_params(channel, vol, sep);
}

static int DG_SndStartSound(sfxinfo_t *sfx, int channel,
                             int vol, int sep)
{
    int lumpnum = sfx->lumpnum;
    unsigned char *lumpdata = W_CacheLumpNum(lumpnum, 1);
    int lumplen = W_LumpLength(lumpnum);

    /* Validate DOOM sound lump header:
     *   Bytes 0-1: magic 0x0003
     *   Bytes 2-3: sample rate (LE16)
     *   Bytes 4-7: data length (LE32)
     *   Bytes 8+:  raw 8-bit unsigned PCM
     */
    if (lumplen < 8)
        return -1;

    unsigned int magic = lumpdata[0] | (lumpdata[1] << 8);
    if (magic != 0x0003)
        return -1;

    unsigned int sample_rate = lumpdata[2] | (lumpdata[3] << 8);
    unsigned int data_len = lumpdata[4] | (lumpdata[5] << 8)
                          | (lumpdata[6] << 16) | (lumpdata[7] << 24);

    /* Sanity check */
    if (data_len + 8 > (unsigned int)lumplen)
        data_len = lumplen - 8;
    if (data_len < 8)
        return -1;

    /* Skip padding (DMX skips first 16 and last 16 samples) */
    unsigned int skip = 16;
    if (data_len > skip * 2) {
        lumpdata += skip;
        data_len -= skip * 2;
    }

    return cubit_snd_start_channel(lumpdata + 8, data_len,
                                    sample_rate, channel,
                                    vol, sep);
}

static void DG_SndStopSound(int channel)
{
    cubit_snd_stop_channel(channel);
}

static boolean DG_SndIsPlaying(int channel)
{
    return cubit_snd_is_playing(channel) != 0;
}

static void DG_SndCacheSounds(sfxinfo_t *sounds, int num_sounds)
{
    /* No precaching needed - WAD data is memory-mapped */
    (void)sounds;
    (void)num_sounds;
}

/* Device list: claim to be a Sound Blaster */
static snddevice_t snd_devices[] = { SNDDEVICE_SB };

sound_module_t DG_sound_module = {
    snd_devices,
    sizeof(snd_devices) / sizeof(snd_devices[0]),
    DG_SndInit,
    DG_SndShutdown,
    DG_GetSfxLumpNum,
    DG_SndUpdate,
    DG_SndUpdateParams,
    DG_SndStartSound,
    DG_SndStopSound,
    DG_SndIsPlaying,
    DG_SndCacheSounds
};

/*---------------------------------------------------------------------------
 * Music module (stub - no music playback yet)
 *---------------------------------------------------------------------------*/

static boolean DG_MusInit(void) { return false; }
static void DG_MusShutdown(void) {}
static void DG_MusSetVolume(int vol) { (void)vol; }
static void DG_MusPause(void) {}
static void DG_MusResume(void) {}
static void *DG_MusRegister(void *data, int len)
    { (void)data; (void)len; return NULL; }
static void DG_MusUnregister(void *handle) { (void)handle; }
static void DG_MusPlay(void *handle, boolean looping)
    { (void)handle; (void)looping; }
static void DG_MusStop(void) {}
static boolean DG_MusIsPlaying(void) { return false; }
static void DG_MusPoll(void) {}

static snddevice_t mus_devices[] = { SNDDEVICE_SB };

music_module_t DG_music_module = {
    mus_devices,
    sizeof(mus_devices) / sizeof(mus_devices[0]),
    DG_MusInit,
    DG_MusShutdown,
    DG_MusSetVolume,
    DG_MusPause,
    DG_MusResume,
    DG_MusRegister,
    DG_MusUnregister,
    DG_MusPlay,
    DG_MusStop,
    DG_MusIsPlaying,
    DG_MusPoll
};
