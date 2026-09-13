#ifndef CUBIT_AUDIO_H
#define CUBIT_AUDIO_H
/* Native Ada-backed single-stream adapter, 48000 Hz stereo signed 16-bit LE.
 * Serialized calls only. Open is initially stopped: prefill before start.
 * Write is nonblocking and returns frames accepted; retain any unwritten suffix.
 * Close discards queued samples. Volume affects ONLY this stream (0..100%).
 * Requires the mixer service endpoint in CAP_SLOT_MIXER; no device authority. */
int cubit_audio_open(void);
unsigned cubit_audio_write(const void *samples, unsigned frames);
void cubit_audio_start(void);
void cubit_audio_close(void);
void cubit_audio_volume(unsigned percent);
#endif
