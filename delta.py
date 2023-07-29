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
    #False, False, False, False,
    False, False, False, True,

    # False, True, False, True,
    False, True, False, True,

    # False, True, False, True,
    False, True, False, True,

    # False, True, False, True,
    False, True, False, False,

    # False, False, False, False,
    False, False, False, False,

    # False, False, False, False,
    False, False, False, False,

    # False, False, False, False,
    False, False, False, False,

    # False, False, False, False,
    False, False, False, False
]


BATCH_SIZE = 254 + 2
STEP_SIZE = 8


class Tracks:
    NIBBLE_6_2 = [
        0x96, 0x97, 0x9a, 0x9b, 0x9d, 0x9e, 0x9f, 0xa6,
        0xa7, 0xab, 0xac, 0xad, 0xae, 0xaf, 0xb2, 0xb3,
        0xb4, 0xb5, 0xb6, 0xb7, 0xb9, 0xba, 0xbb, 0xbc,
        0xbd, 0xbe, 0xbf, 0xcb, 0xcd, 0xce, 0xcf, 0xd3,
        0xd6, 0xd7, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde,
        0xdf, 0xe5, 0xe6, 0xe7, 0xe9, 0xea, 0xeb, 0xec,
        0xed, 0xee, 0xef, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6,
        0xf7, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff
    ]

    def __init__(self, input_audio) -> None:
        self._input_audio = input_audio

        self._toggles = [None, None, None]
        self._output_audio = [None, None, None]

    def encode(self):
        max_track_len = 0
        for i, audio in enumerate(self._input_audio):
            self._toggles[i], self._output_audio[i] = self._compute_speaker(
                audio)
            if len(self._toggles[i]) > max_track_len:
                max_track_len = len(self._toggles[i])

        # normalize length so all tracks are the same length
        for i in range(3):
            if len(self._toggles[i]) >= max_track_len:
                continue
            self._toggles[i].extend([0] * (max_track_len - len(self._toggles[i])))

        for chunk0, chunk1, chunk2 in zip(
            self._encode_track(self._toggles[0]),
            self._encode_track(self._toggles[1]),
            self._encode_track(self._toggles[2]),
        ):
            # 6 bit quantity
            merged = chunk0 ^ (chunk1 << 2) + (chunk2 << 4)
            # print(merged, chunk0, chunk1, chunk2)
            
            nibbles = [self.NIBBLE_6_2[m] for m in merged]
            nibbles[-1] = 0xAA  # EOS marker

            yield from nibbles

    def _encode_track(self, track):
        chunks = self._chunks(track)

        disk_chunks = []
        for i, chunk in enumerate(chunks):
            # We drop the last 2 samples because the write process writes two dummy values
            # after writing the data, so we need to skip over them
            # TODO: handle this in the chunking instead
            chunk = chunk[:BATCH_SIZE - 2]
            push_phase = PUSH_PHASE[i % len(PUSH_PHASE)]

            # encode chunk into bit 1
            disk_chunk = numpy.array([0b10 if s else 0b00 for s in chunk])
            
            if not push_phase:
                disk_chunks.append(disk_chunk)
                continue

            # If we're encoding for a push phase, we need to grab the next
            # chunk, reverse it, and encode it into bit 0
            if i < len(chunks) - 1:
                next_chunk = numpy.array(chunks[i+1][:BATCH_SIZE - 2])
            else:
                next_chunk = numpy.zeros(BATCH_SIZE - 2)
            for j, s in enumerate(reversed(next_chunk)):
                if s:
                    disk_chunk[j] ^= 0b01
            disk_chunks.append(disk_chunk)
        return disk_chunks

    @staticmethod
    def _chunks(data):
        chunks = []
        pos = 0
        while True:
            chunk = data[pos:pos+BATCH_SIZE]
            if len(chunk) < BATCH_SIZE:
                chunk.extend([0] * (BATCH_SIZE - len(chunk)))
                break
            pos += len(chunk)
            chunks.append(chunk)
        return chunks

    def _compute_speaker(self, audio):
        pos = -1.0
        toggles = []
        positions = []
        voltage = -1.0
        for i, sample in enumerate(audio):
            if sample > pos:
                new_voltage = 1.0
            else:
                new_voltage = -1.0

            if new_voltage == voltage:
                toggles.append(False)
            else:
                toggles.append(True)

            pos += (new_voltage - pos) / STEP_SIZE
            voltage = new_voltage

            pos = numpy.clip(pos, -1.0, 1.0)
            positions.append(pos)

        return toggles, positions


def main():
    # TODO: why is playback about 20% slower?  Even if we are sometimes polling for reads, that can't slow down the drive speed.
    sample_rate = int(1020400/32)
    input_audio = [
        preprocess_audio("kris.wav", sample_rate, 0.8, 99),
        preprocess_audio("testing.wav", sample_rate, 0.8, 99),
        preprocess_audio("record.wav", sample_rate, 0.8, 99),
    ]

    null_audio = [0] * 150*1024
    input_audio = [null_audio, null_audio, null_audio]

    tracks = Tracks(input_audio)
    
    # output_audio = librosa.resample(
    #     numpy.array(voltages, dtype=numpy.float32),
    #     orig_sr=sample_rate,
    #     target_sr=sample_rate)

    # with sf.SoundFile("out.wav", "w", sample_rate, channels=1, format='WAV') as f:
    #     f.write(output_audio)

    with open("out.a2d", "wb+") as f:
        f.write(bytes(tracks.encode()))


if __name__ == "__main__":
    main()