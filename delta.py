import math

import librosa
import numpy
import soundfile as sf


def preprocess_audio(
        filename: str, target_sample_rate: int, normalize: float,
        normalization_percentile: int) -> numpy.ndarray:
    """Upscale input audio to target sample rate and normalize signal."""

    data, _ = librosa.load(filename, sr=target_sample_rate, mono=True)

    max_value = numpy.percentile(data, normalization_percentile)
    data /= max_value
    data *= normalize

    return data


def downsample_audio(simulated_audio, original_audio, input_rate, output_rate,
                     noise_output=False):
    """Downscale the 1MHz simulated audio output suitable for writing as .wav

    :arg simulated_audio The simulated audio data to downsample
    :arg original_audio The original audio data that was simulated
    :arg input_rate Sample rate of input audio
    :arg output_rate Desired sample rate of output audio
    :arg noise_output Whether to also produce a noise waveform, i.e. difference
      between input and output audio

    :returns Tuple of downsampled audio and noise data (or None
    if noise_output==False)
    """
    downsampled_output = librosa.resample(
        numpy.array(simulated_audio, dtype=numpy.float32),
        orig_sr=input_rate,
        target_sr=output_rate)

    downsampled_noise = None
    if noise_output:
        noise_len = min(len(simulated_audio), len(original_audio))
        downsampled_noise = librosa.resample(
            numpy.array(
                simulated_audio[:noise_len] - original_audio[:noise_len]),
            orig_sr=input_rate,
            target_sr=output_rate)

    return downsampled_output, downsampled_noise


PUSH_PHASE = [
    False, False, False, True,
    False, True, False, True,
    False, True, False, True,
    False, True, False, False,
    False, False, False, False,
    False, False, False, False,
    False, False, False, False,
    False, False, False, False
]

BATCH_SIZE = 240 + 2
STEP_SIZE = 12


def main():
    sample_rate = int(1020400/32)
    input_audio = preprocess_audio("kris.wav", sample_rate, 0.8, 99)

    pos = 0.0
    output = []
    voltages = []
    voltage = False
    for i, sample in enumerate(input_audio):
        pos_up = pos + (1.0 - pos) / STEP_SIZE
        pos_down = pos + (-1.0 - pos) / STEP_SIZE

        dist_up = math.fabs(pos_up - sample)
        dist_down = math.fabs(pos_down - sample)

        if i % BATCH_SIZE >= (BATCH_SIZE - 2):
            # We always skip 2x32 cycles when stepping heads
            want_up = voltage
        else:
            want_up = dist_up < dist_down

        if want_up:
            pos = pos_up
        else:
            pos = pos_down

        if want_up != voltage:
            output.append(True)
            voltage = not voltage
        else:
            output.append(False)
        
        pos = numpy.clip(pos, -1.0, 1.0)
        voltages.append(pos)

    output_audio = librosa.resample(
        numpy.array(voltages, dtype=numpy.float32),
        orig_sr=sample_rate,
        target_sr=sample_rate)

    with sf.SoundFile("out.wav", "w", sample_rate, channels=1, format='WAV') as f:
        f.write(output_audio)

    chunks = []
    pos = 0
    while True:
        chunk = output[pos:pos+BATCH_SIZE]
        if len(chunk) < BATCH_SIZE:
            chunk.extend([0] * (BATCH_SIZE - len(chunk)))
            break
        pos += len(chunk)
        chunks.append(chunk)

    bytestream = []
    for i, chunk in enumerate(chunks):
        chunk = chunk[:BATCH_SIZE - 2]
    
        push_phase = PUSH_PHASE[i % 32]

        if not push_phase:
            disk_chunk = [0b11011101 if s else 0b11011100 for s in chunk]
            disk_chunk[-1] = 0xff
            bytestream.extend(disk_chunk)
            continue

        mux_chunk = [0b10011101 if s else 0b10011100 for s in chunk]

        if i < len(chunks) - 1:
            next_chunk = chunks[i+1][:BATCH_SIZE - 2]
        else:
            next_chunk = [0] * (BATCH_SIZE - 2)

        for j, s in enumerate(reversed(next_chunk)):
            if s:
                mux_chunk[j] ^= 0b01000000
        mux_chunk[-1] = 0xff
        bytestream.extend(mux_chunk)

    with open("out.a2d", "wb+") as f:
        f.write(bytes(bytestream))


if __name__ == "__main__":
    main()