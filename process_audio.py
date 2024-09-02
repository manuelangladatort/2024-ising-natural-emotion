import wave
from scipy.io.wavfile import read, write
import numpy as np
from scipy.signal import buttord, butter, filtfilt, resample
import rpdb

def trim_audio(start_time,end_time,output_file):
    '''
    start_time: start time seconds
    end_time: end time in seconds
    output_file: path to output WAV file
    '''
    # Step 1: Read the audio data
    with wave.open(output_file, 'r') as output_wav:
        nchannels = output_wav.getnchannels()
        sampwidth = output_wav.getsampwidth()
        framerate = output_wav.getframerate()
        output_wav.setpos(int(start_time * framerate))
        data_trim = output_wav.readframes(int((end_time - start_time) * framerate))

    # Step 2: Write the trimmed data back to the same file
    with wave.open(output_file, 'w') as outfile:
        outfile.setnchannels(nchannels)
        outfile.setsampwidth(sampwidth)
        outfile.setframerate(framerate)
        outfile.writeframes(data_trim)


def fade_in_out(rate, data, length):
    '''
    rate: sample rate of the audio
    data: numpy array of audio data
    length: length of fade in/out in milliseconds
    '''
    fadein_len = int(length / 1000.0 * rate)
    fadeout_len = fadein_len
    fadein = np.power(np.linspace(0, 1, fadein_len), 2.0)
    fadeout = np.power(np.linspace(1, 0, fadeout_len), 2.0)
    
    data[:fadein_len] = data[:fadein_len] * fadein
    data[-fadeout_len:] = data[-fadeout_len:] * fadeout

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
    '''
    signal: numpy array of audio data
    fs: sample rate of the audio
    lower: lower bound of the frequency range
    upper: upper bound of the frequency range
    '''
    Wn = [freq2nyq(lower, fs), freq2nyq(upper, fs)]
    b, a = build_filter_b(Wn)
    filtered_signal = filtfilt(b, a, signal)
    return filtered_signal


def normalize_audio(data, target_peak=0.99):
    """
    Normalize the audio signal to a target peak level.
    
    data: numpy array of audio data, expected to be in floating-point format
    target_peak: target peak amplitude (default is 0.99 to avoid clipping)
    """
    max_val = np.max(np.abs(data))
    if max_val > 0:
        scaling_factor = target_peak / max_val
        data = data * scaling_factor
        
        # Ensure the data stays within the range [-1, 1]
        data = np.clip(data, -1.0, 1.0)
        
    return data