#!/usr/bin/env python3
"""Summarize CuBit instrumented runs; quantiles are histogram upper bounds."""
import argparse
import array
import json
import io
import math
import re
import statistics
import sys
import struct
import wave
from pathlib import Path


def fields(line):
    return {k: int(v) for k, v in re.findall(r"(\w+)=\s*(\d+)", line)}


def serial_report(text):
    rate = 0
    phase = "unlabelled"
    timings, audio, inputs = [], [], []
    load_progress = None
    for line in text.splitlines():
        if "BENCH: phase=" in line:
            phase = line.split("phase=", 1)[1].split()[0]
        if "TIMING: calibration" in line:
            rate = fields(line).get("ticks_per_ms", 0)
        elif "TIMING: " in line:
            body = line.split("TIMING: ", 1)[1]
            sample = dict(name=body.split()[0], phase=phase, **fields(body))
            if rate:
                sample["microseconds"] = {
                    k.removesuffix("_ticks"): round(v * 1000 / rate, 4)
                    for k, v in fields(body).items() if k.endswith("_ticks")
                }
            timings.append(sample)
        elif "AUDIO-BENCH: phase " in line or "AUDIO-BENCH: counters " in line:
            audio.append(fields(line))
        elif "INPUT-BENCH: scenario=" in line:
            inputs.append(dict(scenario=line.split("scenario=", 1)[1].split()[0],
                               **fields(line)))
        elif "BENCH-LOAD: COMPLETE" in line and "batches=" in line:
            load_progress = fields(line)
    load_present = "BENCH-LOAD: START" in text
    overlap = load_overlaps_measurement(text) if load_present else None
    input_valid = valid_input_run(text, rate, timings, inputs)
    return dict(ticks_per_guest_ms=rate, timings=timings, audio=audio, inputs=inputs,
                input_scope=("Closed-loop normalized publication to focused-app receipt; "
                             "not IRQ-to-app, an admitted workload guarantee, or key-to-photon."
                             if "INPUT-BENCH:" in text else None),
                input_integrity_valid=input_valid,
                input_observed_target_met=(input_valid and
                    (not load_present or overlap is True) and all(
                    item["misses_1ms"] * 100 <= item["delivered"] for item in inputs)),
                load_covers_measurement=overlap,
                load_progress=load_progress,
                warnings=["Calibrated against guest milliseconds, not an external clock.",
                          "Quantiles are bucket upper bounds; maxima are observed samples.",
                          "SMP conversion assumes synchronized virtual TSCs."])


def valid_input_run(text, rate, timings, inputs):
    """Fail closed on incomplete or duplicated fixtures; never censor losses."""
    scenarios = {"CONTINUOUS", "PACED", "REPAINTING"}
    if (rate <= 0 or len(inputs) != len(scenarios) or
            {item["scenario"] for item in inputs} != scenarios or
            text.count("INPUT-BENCH: START") != 1 or
            text.count("INPUT-BENCH: COMPLETE") != 1 or
            "BENCH: PASS input integrity" not in text or "BENCH: FAIL input" in text or
            text.index("INPUT-BENCH: START") > text.index("INPUT-BENCH: COMPLETE")):
        return False
    for item in inputs:
        hist = [h for h in timings if h["name"] == "input-" + item["scenario"]]
        if (len(hist) != 1 or item.get("delivered", 0) < 2048 or
                hist[0].get("count") != item["delivered"] or
                item.get("failures") != 0 or
                not 0 <= item.get("misses_1ms", -1) <= item["delivered"]):
            return False
    return True


def load_overlaps_measurement(text):
    if "INPUT-BENCH:" in text:
        markers = ("INPUT-BENCH: START", "INPUT-BENCH: COMPLETE")
    elif "AUDIO-BENCH:" in text:
        markers = ("AUDIO-BENCH: START", "AUDIO-BENCH: COMPLETE")
    else:
        markers = ("BENCH: phase=untraced", "BENCH: PASS ipc")
    positions = [text.find(marker) for marker in
                 ("BENCH-LOAD: START", *markers, "BENCH-LOAD: COMPLETE")]
    return all(p >= 0 for p in positions) and positions == sorted(positions)


def wave_report(path):
    payload = bytearray(path.read_bytes())
    unfinalized = False
    # QEMU's WAV backend can leave zero chunk lengths after SIGTERM. Recognize
    # only its exact PCM header and repair lengths IN MEMORY, not the artifact.
    if (len(payload) >= 44 and payload[:4] == b"RIFF" and
            payload[4:8] == b"\0" * 4 and payload[8:16] == b"WAVEfmt " and
            payload[16:20] == b"\x10\0\0\0" and payload[36:40] == b"data" and
            payload[40:44] == b"\0" * 4):
        if (len(payload) - 44) % 4:
            raise ValueError("unfinalized capture ends in a partial stereo frame")
        struct.pack_into("<I", payload, 4, len(payload) - 8)
        struct.pack_into("<I", payload, 40, len(payload) - 44)
        unfinalized = True
    with wave.open(io.BytesIO(payload), "rb") as source:
        channels, width, rate = source.getnchannels(), source.getsampwidth(), source.getframerate()
        if width != 2 or channels != 2 or source.getcomptype() != "NONE":
            raise ValueError("expected uncompressed stereo S16 WAV")
        data = array.array("h", source.readframes(source.getnframes()))
    if sys.byteorder != "little":
        data.byteswap()
    samples = data[::channels]
    peak = max(map(abs, samples), default=0)
    if peak < 32:
        raise ValueError("capture contains no measurable test signal")
    active = [i for i, value in enumerate(samples) if abs(value) > peak / 100]
    first, last = active[0], active[-1]
    # Exclude start/stop transitions, but not interior silence.
    begin, end = first + rate // 5, last - rate // 5
    if end - begin < 2 * rate:
        raise ValueError("test signal too short for analysis")
    block = rate // 50  # 20 ms windows
    rms = [math.sqrt(sum(x * x for x in samples[i:i + block]) / block)
           for i in range(begin, end - block + 1, block)]
    median_rms = statistics.median(rms)
    quiet = sum(value < median_rms / 10 for value in rms)
    crossings = [i for i in range(begin + 1, end)
                 if samples[i - 1] <= 0 < samples[i]]
    gaps = [b - a for a, b in zip(crossings, crossings[1:]) if b > a]
    frequency = rate / statistics.median(gaps) if gaps else 0
    return dict(rate_hz=rate, channels=channels, unfinalized_capture=unfinalized,
                duration_s=len(samples) / rate,
                active_span_s=(last - first + 1) / rate, peak=peak,
                median_rms=round(median_rms, 3), estimated_tone_hz=round(frequency, 3),
                quiet_20ms_windows=quiet, windows=len(rms),
                clipped_samples=sum(abs(x) >= 32767 for x in samples[begin:end]),
                note="Signal/capture diagnostic, not speaker latency or an exhaustive distortion test.")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("serial", type=Path)
    parser.add_argument("--wav", type=Path)
    parser.add_argument("--require-load", action="store_true")
    parser.add_argument("--require-input-integrity", action="store_true")
    parser.add_argument("--require-input-target", action="store_true",
                        help="Require observed <1ms p99 in every input scenario; not an SLA guarantee")
    args = parser.parse_args()
    result = serial_report(args.serial.read_text(errors="replace"))
    if args.wav:
        result["wave"] = wave_report(args.wav)
    print(json.dumps(result, indent=2))
    if args.require_load and result["load_covers_measurement"] is not True:
        sys.exit("benchmark load did not cover measurement; not a valid loaded result")
    if args.require_input_integrity and not result["input_integrity_valid"]:
        sys.exit("input benchmark incomplete or invalid")
    if args.require_input_target and not result["input_observed_target_met"]:
        sys.exit("input benchmark invalid or observed p99 target missed")


if __name__ == "__main__":
    main()
