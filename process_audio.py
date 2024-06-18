import wave
from scipy.io.wavfile import read, write
import numpy as np
from scipy.signal import buttord, butter, filtfilt, resample
import rpdb

def trim_audio(start_time,end_time,output_file):
    '''
    start_time: start time seconds
    end_time: end time in seconds
    '''
    output_wav = wave.open(output_file, 'r')
    nchannels = output_wav.getnchannels()
    sampwidth = output_wav.getsampwidth()
    framerate = output_wav.getframerate()
    output_wav.setpos(int(start_time * framerate))
    data_trim = output_wav.readframes(int((end_time - start_time) * framerate))

    with wave.open(output_file, 'w') as outfile:
        outfile.setnchannels(nchannels)
        outfile.setsampwidth(sampwidth)
        outfile.setframerate(framerate)
        outfile.setnframes(int(len(data_trim) / sampwidth))
        outfile.writeframes(data_trim)
        outfile.close()

def fade_in_out(rate, data, length, output_file):

    fadein = np.power(np.linspace(0, 1, round(1 + (length / 1000.0 * rate))), 2.0)
    fadeout = fadein[::-1]
    data[:len(fadein)] = data[:len(fadein)] * fadein
    data[-len(fadeout):] = data[-len(fadeout):] * fadeout

    return data

def freq2nyq(freq, fs):
    # find the nyquist fq from fs
    # frequencies for filter design must be expressed as a fraction of Nyquist
    nyquist = fs / 2.0
    return freq / nyquist

def build_filter_b(Wn):
    b, a = butter(2, Wn, btype='band', analog=False, output='ba')
    return b, a

def bandpass_filter(signal, fs, lower, upper):

    Wn1 = [freq2nyq(lower, fs), freq2nyq(upper, fs)]
    b1, a1 = build_filter_b(Wn1)
    filtered_signal1 = filtfilt(b1, a1, signal)
    filtered_signal2 = filtfilt(b1, a1, filtered_signal1)
    return filtered_signal2


