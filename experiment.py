# imports
from statistics import mean
from markupsafe import Markup
import tempfile
import json
import numpy as np
import math
import random
from scipy.io.wavfile import read, write

# psynet
import psynet.experiment
from psynet.js_synth import JSSynth, Note, HarmonicTimbre, InstrumentTimbre
from psynet.asset import DebugStorage, LocalStorage, ExperimentAsset, Asset, CachedAsset
from psynet.consent import NoConsent
from .goldsmiths_consent import GoldsmithsConsent, GoldsmithsAudioConsent, GoldsmithsOpenScienceConsent
from psynet.modular_page import AudioPrompt, AudioRecordControl, ModularPage, SurveyJSControl
from psynet.page import InfoPage, SuccessfulEndPage,join
from psynet.timeline import Event, ProgressDisplay, ProgressStage, Timeline, CodeBlock, conditional, FailedValidation
from psynet.trial.audio import (
    AudioImitationChainTrial,
    AudioImitationChainTrialMaker,
)
from psynet.experiment import scheduled_task
from psynet.bot import Bot
from psynet.trial.imitation_chain import ImitationChainNode
from psynet.utils import get_logger
logger = get_logger()

# sing4me
from sing4me import singing_extract as sing
from sing4me import melodies
from .params import singing_2intervals
from .instructions import welcome, requirements_mic
from .questionnaire import questionnaire, questionnaire_emotion
from .pre_screens import (
    mic_test,
    # recording_example,
    singing_performance
)
from .process_audio import fade_in_out, trim_audio, bandpass_filter, normalize_audio

import warnings
warnings.filterwarnings("ignore", category=DeprecationWarning)


########################################################################################################################
# Prolific parameters
########################################################################################################################

def get_prolific_settings():
    with open("qualification_prolific_en.json", "r") as f:
        qualification = json.dumps(json.load(f))
    return {
        "recruiter": "prolific", # prolific vs hotair,
        # "id": "singing-nets",
        "prolific_estimated_completion_minutes": 14,
        "prolific_recruitment_config": qualification,
        "base_payment": 2.20,
        "auto_recruit": False,
        "currency": "£",
        "wage_per_hour": 0.01
    }


########################################################################################################################
# Global parameters
########################################################################################################################

DEBUG = False
RUN_BOT = True
INSTRUCTION = "emotion" # imitate vs emotion


# recruitment
INITIAL_RECRUITMENT_SIZE = 10
NUM_TRIALS_PARTICIPANT = 20 
NUM_ITERATIONS_CHAIN = 11

DESIGN = "across"  # within vs across
NUM_PARTICIPANTS_EXPERIMENT = 50  # only active in within
NUM_CHAINS_PARTICIPANT = 3   # only active in within


# Fixed vs variable number of notes
VARIABLE_NUM_NOTES = False  

if VARIABLE_NUM_NOTES:
    MIN_NUM_NOTES = 4
    MAX_NUM_NOTES = 13
    NUM_NOTE_CONDITIONS = list(range(MIN_NUM_NOTES, (MAX_NUM_NOTES + 1)))
    NUM_CHAINS_CONDITION   = 20  # if 20, then 200 chains; if 30, then 300 chains
    NUM_CHAINS_EXPERIMENT = len(NUM_NOTE_CONDITIONS) * NUM_CHAINS_CONDITION
    TIME_ESTIMATE_TRIAL = 20
    ADD_TIME_AFTER_SINGING = 3
else:
    NUM_NOTE_CONDITIONS = 1
    NUM_NOTES = 5
    NUM_CHAINS_EXPERIMENT = 200 # only active in across
    TIME_ESTIMATE_TRIAL = 18  
    ADD_TIME_AFTER_SINGING = 2


# Instructions
if INSTRUCTION == "imitate":
    intrusction_show_trial = "Sing back the melody"
    instruction_instructions = "In each trial, you will listen to a melody and asked to sing it back as accurately as possible."
elif INSTRUCTION == "emotion":
    intrusction_show_trial = "Sing back the melody with emotion"
    # instruction_instructions = "In each trial, you will listen to a melody and asked to sing it back accurately <b><b>while also conveying any type of emotion</b></b>"


# singing
config = singing_2intervals  # params singing extraction (from sing4me)
reference_mode = "pitch_mode"  # pitch_mode vs previous_note vs first_note
roving_width = 2.5
roving_mean = dict(
    default=55,  # it was 55.5
    low=49,  # it was 49.5 (male)
    high=61  # it was 61.5 (female)
    )


# singing technology
SYLLABLE = 'TA'
MAX_ABS_INT_ERROR_ALLOWED = 999  # set to 999 if NUM_INT > 2
MAX_INT_SIZE = 999
MAX_MELODY_PITCH_RANGE = 999  # deactivated
MAX_INTERVAL2REFERENCE = 7.5  # set to 7.5 if NUM_INT > 2


# timbre
# TIMBRE = InstrumentTimbre("piano")
TIMBRE = dict(
    default=HarmonicTimbre(
        attack=0.01,  # Attack phase duration in seconds
        decay=0.025,  # Decay phase duration in seconds
        sustain_amp=0.7,  # Amplitude fraction to decay to relative to max amplitude --> 0.4, 0.7
        release=0.25,  # Release phase duration in seconds
        num_harmonics=10,  # Acd ctual number of partial harmonics to use
        roll_off=14,  # Roll-off in units of dB/octave,
    )
)

# durations
# note_duration_tonejs = 0.8
MIN_NOTE_SILENCE = 0.2
ADD_TIME_AFTER_SINGING = 2
MIN_ISI = 0.5
MAX_ISI = 1.5
LOG_SAMPLE_ISI = False


# conditional parameters
NUM_PARTICIPANTS_EXPERIMENT = 50  # only active in within
NUM_CHAINS_PARTICIPANT = 3   # only active in within

if DEBUG:
    num_iterations_per_chain = 10
    num_trials_per_participant = NUM_TRIALS_PARTICIPANT
    num_chains_per_participant = NUM_CHAINS_PARTICIPANT # only active in within
    target_num_participants = NUM_PARTICIPANTS_EXPERIMENT  # only active in within
    num_chains = 10  # only active in across
    repeat_same_chain = True
    save_plot = True
    NUM_CHAINS_EXPERIMENT = 5
else:
    num_iterations_per_chain = NUM_ITERATIONS_CHAIN
    num_trials_per_participant = NUM_TRIALS_PARTICIPANT
    num_chains_per_participant = NUM_CHAINS_PARTICIPANT   # only active in within
    target_num_participants = NUM_PARTICIPANTS_EXPERIMENT  # only active in within
    num_chains = NUM_CHAINS_EXPERIMENT # only active in across
    repeat_same_chain = False
    save_plot = True


if DESIGN == "within":
    DESIGN_PARAMS = {
        "num_trials_per_participant": (int(num_trials_per_participant) + 10),
        "num_trials_practice_feedback": 3,
        "num_iterations_per_chain": num_iterations_per_chain,
        "trials_per_node": 1,
        "balance_across_chains": True,
        "chain_type": "within",
        "num_chains_per_participant": num_chains_per_participant,
        "recruit_mode": "n_participants",
        "target_num_participants": target_num_participants,
        "num_chains_per_exp": None,
        "repeat_same_chain": repeat_same_chain
    }
else:
    DESIGN_PARAMS = {
        "num_trials_per_participant": int(num_trials_per_participant),
        "num_trials_practice_feedback": 3,
        "num_iterations_per_chain": num_iterations_per_chain,
        "trials_per_node": 1,
        "balance_across_chains": False,
        "chain_type": "across",
        "num_chains_per_participant": None,
        "recruit_mode": "n_trials",
        "target_num_participants": None,
        "num_chains_per_exp": NUM_CHAINS_EXPERIMENT,
        "repeat_same_chain": repeat_same_chain
    }


########################################################################################################################
# methods
########################################################################################################################
def estimate_time_per_trial(note_durations, num_pitches, silence_duration, extra_time_singing):
    melody_duration = sum(note_durations) + (silence_duration*num_pitches) + 0.5
    singing_duration = melody_duration + extra_time_singing
    return melody_duration, singing_duration


# sample ISI for a given tempo
def sample_ISIs_for_tempo(min_isi, max_isi, silent_gap, num_pitches):
    isis = []
    note_durations = []
    for _ in range(num_pitches - 1):  
        isi = random.uniform(min_isi, max_isi)
        isis.append(isi)
        # now you can get note duration
        note_duration = isi - silent_gap
        note_durations.append(note_duration)
    # finally, add the note duration of the final note
    final_note_duration = random.uniform(min_isi, max_isi) - silent_gap
    note_durations.append(final_note_duration)
    return isis, note_durations


# sample ISI continously and randomly
def sample_random_ISI(min_isi, max_isi, is_log_sampling):
    if is_log_sampling:
        isis = math.exp(random.uniform(math.log(min_isi), math.log(max_isi)))
    else:
        isis = random.uniform(min_isi, max_isi)
    return isis


def sample_random_ISIs(min_isi, max_isi, num_pitches, is_log_sampling):
    isis = []
    for i in range(num_pitches):
        target_pitch = sample_random_ISI(
            min_isi=min_isi,
            max_isi=max_isi,
            is_log_sampling=is_log_sampling
        )
        isis.append(target_pitch)
    return (isis)


def get_note_durations(ISIs, note_silence):
    note_durations = []
    curr_onset = 0
    for note in ISIs:
        curr_onset = note - note_silence
        note_durations.append(curr_onset)
    return note_durations


def compute_stats_time(sung_note_durations, target_note_durations, sung_ISIs, target_ISIs):
    raw_note_durations_diffs = sing.diff_across(sung_note_durations, target_note_durations)
    raw_ISIs_diffs = sing.diff_across(sung_ISIs, target_ISIs)

    note_duration_max_error, note_duration_root_mean_squared = sing.compute_error(raw_note_durations_diffs)
    ISIs_max_error, ISIs_root_mean_squared = sing.compute_error(raw_ISIs_diffs)

    return {
        "raw_note_durations_diffs": raw_note_durations_diffs,
        "mean_note_duration_diffs": float(np.mean(raw_note_durations_diffs) if raw_note_durations_diffs else 999),
        "note_duration_max_error": note_duration_max_error,
        "note_duration_root_mean_squared": float(note_duration_root_mean_squared),
        "raw_ISIs_diffs": raw_ISIs_diffs,
        "mean_ISIs_diff": float(np.mean(raw_ISIs_diffs) if raw_ISIs_diffs else 999),
        "ISIs_max_error": ISIs_max_error,
        "ISIs_root_mean_squared": float(ISIs_root_mean_squared)
    }


def convert_numpy(data):
    if isinstance(data, dict):
        return {k: convert_numpy(v) for k, v in data.items()}
    elif isinstance(data, list):
        return [convert_numpy(i) for i in data]
    elif isinstance(data, np.ndarray):
        return data.tolist()
    elif isinstance(data, np.generic):  # Covers numpy scalars like np.float64
        return data.item()
    else:
        return data


# create random melodies
def create_seed_melody(num_notes):
    reference_pitch = melodies.sample_reference_pitch(
        roving_mean["high"],
        roving_width,
    )
    target_pitches = melodies.sample_absolute_pitches(
        reference_pitch=reference_pitch,
        max_interval2reference=MAX_INTERVAL2REFERENCE,
        num_pitches=num_notes
    )
    target_intervals = melodies.convert_absolute_pitches_to_interval_sequence(target_pitches, "previous_note")
    ISIs, note_durations = sample_ISIs_for_tempo(
        min_isi=MIN_ISI,
        max_isi=MAX_ISI,
        silent_gap=MIN_NOTE_SILENCE,
        num_pitches=num_notes
    )
    onsets = []
    curr_onset = 0
    for note in note_durations:
        onsets.extend([curr_onset])
        curr_onset += note + MIN_NOTE_SILENCE
    return dict(
        register="high",  # all melodies are generated in the high register
        reference_pitch=reference_pitch,
        max_interval2reference=MAX_INTERVAL2REFERENCE,
        max_interval_size=MAX_ABS_INT_ERROR_ALLOWED,
        target_pitches=target_pitches,
        target_intervals=target_intervals,
        target_ISIs=ISIs,
        target_note_durations=note_durations,
        num_target_pitches=num_notes,
        trial_type="source_trial",
        reference_mode=reference_mode,
        onsets=onsets
    )


########################################################################################################################
# Experiment parts
########################################################################################################################
def create_singing_trial(show_current_trial, time_estimate, melody_duration, singing_duration, stimulus, target_num_notes):
    singing_page = ModularPage(
        "singing",
        AudioPrompt(
            stimulus,
            Markup(
                f"""
                <h3>Sing back the melody</h3>
                <hr>
                <b><b>This melody has {target_num_notes} notes</b></b>: Sing each note using the syllable '{SYLLABLE}' and leave short gaps between notes.
                <br><br>
                {show_current_trial}
                <hr>
                """
            ),
        ),
        control=AudioRecordControl(
            duration=singing_duration,
            show_meter=True,
            controls=False,
            auto_advance=False,
            bot_response_media="audio_5notes.wav",
        ),
        events={
            "promptStart": Event(is_triggered_by="trialStart", delay = 0.5),
            "recordStart": Event(is_triggered_by="promptEnd", delay = 0.25),
        },
        progress_display=ProgressDisplay(
            stages=[
                ProgressStage(melody_duration + 0.5, "Listen to the melody...", "orange"),
                ProgressStage(singing_duration, "Recording...SING THE MELODY!", "red"),
                ProgressStage(0.5, "Done!", "green", persistent=True),
            ],
        ),
        time_estimate=time_estimate,
    )
    return singing_page


def create_singing_trial_seed(show_current_trial, target_pitches, note_durations, time_estimate, melody_duration, singing_duration):
    singing_page = ModularPage(
        "singing",
        JSSynth(
            Markup(
                f"""
                <h3>{intrusction_show_trial}</h3>
                <hr>
                <b><b>This melody has {len(target_pitches)} notes</b></b>: Sing each note using the syllable '{SYLLABLE}' and leave short gaps between notes.
                <br><br>
                {show_current_trial}
                <hr>
                """
            ),
            [Note(pitch, duration=duration) for pitch, duration in
             zip(target_pitches, note_durations)],
            timbre=TIMBRE,
            default_silence=MIN_NOTE_SILENCE,
        ),
        control=AudioRecordControl(
            duration=singing_duration,
            show_meter=True,
            controls=False,
            auto_advance=False,
            bot_response_media="audio_5notes.wav",
        ),
        events={
            "promptStart": Event(is_triggered_by="trialStart"),
            "recordStart": Event(is_triggered_by="promptEnd", delay=0.25),
        },
        progress_display=ProgressDisplay(
            stages=[
                ProgressStage((melody_duration + 0.25), "Listen to the melody...", "orange"),
                ProgressStage(singing_duration, "Recording...SING THE MELODY!", "red"),
                ProgressStage(0.5, "Done!", "green", persistent=True),
            ],
        ),
        time_estimate=time_estimate,
    )
    return singing_page


class CustomTrialAnalysis(AudioImitationChainTrial):
    def analyze_recording(self, audio_file: str, output_plot: str):
        if self.degree == 0:
            if self.participant.var.register == "high":
                target_pitches = self.definition["target_pitches"]
            else:
                target_pitches = [(i - 12) for i in self.definition["target_pitches"]]
        else:
            target_pitches = self.definition["target_pitches"]

        target_note_durations = self.definition["target_note_durations"]
        target_ISIs = self.definition["target_ISIs"]

        raw = sing.analyze(
            audio_file,
            config,
            target_pitches=target_pitches,
            plot_options=sing.PlotOptions(
                save=save_plot, path=output_plot, format="png"
            ),
        )

        raw = [{key: melodies.as_native_type(value) for key, value in x.items()} for x in raw]

        sung_pitches = [x["median_f0"] for x in raw]
        sung_intervals = melodies.convert_absolute_pitches_to_interval_sequence(
            sung_pitches,
            "previous_note"
        )
        target_intervals = melodies.convert_absolute_pitches_to_interval_sequence(
            target_pitches,
            "previous_note"
        )
        
        # get start/ end onsets
        start_onsets = [x["start_tt"] for x in raw]
        end_onsets = [x["end_tt"] for x in raw]

        # get ISI
        ISI_ms = np.diff(start_onsets)
        sung_ISIs = [(i / 1000) for i in ISI_ms]

        # note durations
        note_durations_ms = [element2 - element1 for (element2, element1) in zip(end_onsets, start_onsets)]
        sung_note_durations = [(i / 1000) for i in note_durations_ms]

        time_stats = compute_stats_time(
            sung_note_durations, 
            target_note_durations, 
            sung_ISIs, 
            target_ISIs)

        pitch_stats = sing.compute_stats(
            sung_pitches,
            target_pitches,
            sung_intervals,
            target_intervals
        )

        time_stats = convert_numpy(time_stats)
        pitch_stats = convert_numpy(pitch_stats)

        num_sung_pitches = pitch_stats["num_sung_pitches"]
        num_target_pitches = pitch_stats["num_target_pitches"]
        direction_accuracy = pitch_stats["direction_accuracy"]
        correct_num_notes = num_sung_pitches == num_target_pitches

        if VARIABLE_NUM_NOTES:
            if len(target_pitches) in range(3, 6): # if melodies are 3 to 5 notes long, allow 2 mistakes
                MAX_NUM_DIFF_PITCHES_ALLOWED = 2
            elif len(target_pitches) in range(6, 9): # if melodies are 6 to 8 notes long, allow 3 mistakes
                MAX_NUM_DIFF_PITCHES_ALLOWED = 3
            elif len(target_pitches) in range(9, 15): # if melodies are 9 to 15 notes long, allow 4 mistakes
                MAX_NUM_DIFF_PITCHES_ALLOWED = 4
            else:
                MAX_NUM_DIFF_PITCHES_ALLOWED = 0
            
            # failing criteria based on max number of different pitches allowed
            is_failed = melodies.failing_criteria_unconstrained_notes(pitch_stats, MAX_NUM_DIFF_PITCHES_ALLOWED)
            failed = is_failed["failed"]
            reason = is_failed["reason"]

        else:
            if direction_accuracy >= 75 and correct_num_notes:
                failed = False
                reason = "All good"
            elif not correct_num_notes:
                failed = True
                reason = f"Wrong number of sung notes: {num_sung_pitches} sung out of {num_target_pitches} notes in melody"
            elif direction_accuracy < 75:
                failed = True
                reason = f"Direction accuracy is too low: {direction_accuracy}%"
            else:
                failed = True
                reason = f"Unknown issue: num sung pitches {num_sung_pitches}, direction accuracy {direction_accuracy}%"

        if RUN_BOT:
            failed = False
            reason = "All good"

        analysis_dict = {
            "failed": failed,
            "reason": reason,
            "register": self.participant.var.register,
            "target_pitches": target_pitches,
            "num_target_pitches": len(target_pitches),
            "target_intervals": target_intervals,
            "sung_pitches": sung_pitches,
            "num_sung_pitches": len(sung_pitches),
            "sung_intervals": sung_intervals,
            "raw": raw,
            "save_plot": save_plot,
            "pitch_stats": pitch_stats,
            "time_stats": time_stats,
            "target_note_durations": target_note_durations,
            "target_ISIs": target_ISIs,
            "sung_note_durations":sung_note_durations,
            "sung_ISIs":sung_ISIs,
            "raw_audio": audio_file,
            "ISI_rms_error": time_stats["ISIs_root_mean_squared"],
            "interval_rms_error":  pitch_stats["root_mean_squared_interval"],
            "direction_accuracy": direction_accuracy,
        }

        analysis_dict = convert_numpy(analysis_dict)
        
        return analysis_dict


class CustomTrial(CustomTrialAnalysis):

    time_estimate = TIME_ESTIMATE_TRIAL

    def show_trial(self, experiment, participant):
        current_trial = self.position + 1

        if self.trial_maker_id == "sing_practice":
            total_num_trials = DESIGN_PARAMS["num_trials_practice_feedback"]
        else:
            total_num_trials = DESIGN_PARAMS["num_trials_per_participant"] 

        show_current_trial = f'<br><i>Trial number {current_trial} out of {total_num_trials} possible maximum trials.</i>'

        if self.degree == 0:
            logger.info("********** Register of participant: {0} **********".format(participant.var.register))
        
            # convert to right register
            if self.participant.var.register == "high":
                target_pitches = self.definition["target_pitches"]
            else:
                target_pitches = [(i - 12) for i in self.definition["target_pitches"]]

            target_note_durations = self.definition["target_note_durations"]

            melody_duration, singing_duration = estimate_time_per_trial(
                target_note_durations,
                len(target_pitches) + 1,
                MIN_NOTE_SILENCE,
                ADD_TIME_AFTER_SINGING
            )

            singing_page = create_singing_trial_seed(
                show_current_trial,
                target_pitches,
                target_note_durations,
                TIME_ESTIMATE_TRIAL,
                melody_duration,
                singing_duration
            )

        else:
            stimulus = self.assets['stimulus']
            raw_analysis = self.definition['raw_analysis']
            target_num_notes = self.definition['num_target_pitches']
            start_time = (raw_analysis[0][0]['start_tt'] - 100) /1000
            end_time = (raw_analysis[0][target_num_notes-1]['end_tt'] + 100)/1000
            tot_duration = end_time-start_time

            singing_page = create_singing_trial(
                show_current_trial,
                TIME_ESTIMATE_TRIAL,
                (tot_duration + 1),
                (tot_duration + ADD_TIME_AFTER_SINGING),
                stimulus,
                target_num_notes
            )

        return singing_page
    

class CustomTrialPractice(CustomTrial):

    def show_trial(self, experiment, participant):

        current_trial = self.position + 1
        total_num_trials = DESIGN_PARAMS["num_trials_practice_feedback"]
        show_current_trial = f'Trial number {current_trial} out of {total_num_trials} trials.'

        if self.participant.var.register == "high":
            target_pitches = self.definition["target_pitches"]
        else:
            target_pitches = [(i - 12) for i in self.definition["target_pitches"]]

        target_note_durations = self.definition["target_note_durations"]

        melody_duration, singing_duration = estimate_time_per_trial(
            target_note_durations,
            len(target_pitches) + 1,
            MIN_NOTE_SILENCE,
            ADD_TIME_AFTER_SINGING
        )

        singing_page = create_singing_trial_seed(
            show_current_trial,
            target_pitches,
            target_note_durations,
            TIME_ESTIMATE_TRIAL,
            melody_duration,
            singing_duration
        )

        return singing_page

    def gives_feedback(self, experiment, participant):
        return True

    def show_feedback(self, experiment, participant):
        output_analysis = self.analysis
        num_sung_pitches = len(output_analysis["sung_pitches"])
        num_target_pitches = len(output_analysis["target_pitches"])
        direction_accuracy = output_analysis["direction_accuracy"]
        correct_num_notes = num_sung_pitches == num_target_pitches

        if direction_accuracy >= 75 and correct_num_notes:
            return InfoPage(
                Markup(
                    f"""
                    <h3>Your performance is great!</h3>
                    <hr>
                    We detected {num_sung_pitches} notes in your recording with an accuracy of {direction_accuracy}%.
                    <hr>
                    """
                ),
                time_estimate=5
            )
        elif correct_num_notes and 50 <= direction_accuracy < 75:
            return InfoPage(
                Markup(
                    f"""
                    <h3>You can do better...</h3>
                    <hr>
                    The accuracy of your performance is {direction_accuracy}%, and should be improved.
                    <br><br>
                    Please try to do one or more of the following:
                    <ol><li>Make sure you are in a quiet room.</li>
                        <li>Sing each note as accurately as possible using the syllable 'TA'.</li>
                        <li>Leave short gaps between notes.</li>
                    </ol>
                    <b><b>To continue with the experiment, you will need to improve your performance.</b></b>
                    <hr>
                    """
                ),
                time_estimate=5
            )
        elif correct_num_notes and direction_accuracy < 50:
            return InfoPage(
                Markup(
                    f"""
                    <h3>You can do better...</h3>
                    <hr>
                    The accuracy of your performance is too low: {direction_accuracy}%.
                    <br><br>
                    Please try to do one or more of the following:
                    <ol><li>Make sure you are in a quiet room.</li>
                        <li>Sing each note as accurately as possible using the syllable 'TA'.</li>
                        <li>Leave short gaps between notes.</li>
                    </ol>
                    <b><b>To continue with the experiment, you will need to improve your performance.</b></b>
                    <hr>
                    """
                ),
                time_estimate=5
            )
        else:
            return InfoPage(
                Markup(
                    f"""
                    <h3>You can do better...</h3>
                    <hr>
                    We detected {num_sung_pitches} notes in your recording.
                    <br><br>
                    Please try to do one or more of the following:
                    <ol><li>Make sure you are in a quiet room.</li>
                        <li>Make sure your microphone is working.</li>
                        <li>Sing each note as accurately as possible using the syllable 'TA'.</li>
                        <li>Leave short gaps between notes.</li>
                    </ol>
                    <b><b>To continue with the experiment, you will need to improve your performance.</b></b>
                    <hr>
                    """
                ),
                time_estimate=5
            )


class CustomNode(ImitationChainNode):

    def summarize_trials(self, trials: list, experiment, participant):
        sung_intervals = [trial.analysis["sung_intervals"] for trial in trials]
        sung_pitches = [trial.analysis["sung_pitches"] for trial in trials]
        sung_note_durations = [trial.analysis["sung_note_durations"] for trial in trials]
        sung_ISIs = [trial.analysis["sung_ISIs"] for trial in trials]

        raw_analysis = [trial.analysis["raw"] for trial in trials]
        raw_audio = [trial.analysis["raw_audio"] for trial in trials]

        target_intervals = [mean(x) for x in zip(*sung_intervals)]
        target_pitches = [mean(x) for x in zip(*sung_pitches)]
        target_note_durations = [mean(x) for x in zip(*sung_note_durations)]
        target_ISIs = [mean(x) for x in zip(*sung_ISIs)]

        return dict(
            num_target_pitches=len(target_pitches),
            target_pitches=target_pitches,
            target_intervals=target_intervals,
            target_note_durations=target_note_durations,
            target_ISIs = target_ISIs,
            trial_type="node_trial",
            raw_audio=raw_audio,
            raw_analysis=raw_analysis
        )
    
    def synthesize_target(self, output_file):
        if self.degree == 0:
            target_pitches = self.seed['target_pitches']
            durations = self.seed['target_note_durations']
            onsets = self.seed['onsets']

            sing.generate_sine_tones(
                target_pitches,
                durations,
                onsets,
                config["sample_rate"],
                output_file
            )

        else:
            raw_analysis = self.definition['raw_analysis']
            target_num_notes = self.definition['num_target_pitches']
            start_time = (raw_analysis[0][0]['start_tt'] - 150) / 1000
            end_time = (raw_analysis[0][target_num_notes - 1]['end_tt'] + 150) / 1000

            self.parent.alive_trials[0].assets["singing"].export(output_file)

            if start_time < 0:
                start_time = 0

            trim_audio(start_time, end_time, output_file)
            rate, data = read(output_file)
            data = fade_in_out(rate, data, 100)
            data = bandpass_filter(data, rate, 300, 6000)
            data = normalize_audio(data)

            data = 0.1 * data / np.max(np.abs(data)) # Scale the audio so that its maximum value is 0.1

            data = np.int16(data * 32767) # Convert to int16 format for writing to wav file

            write(output_file, rate, data)

    def async_on_deploy(self):
        if self.degree == 0:
            return
        else:
            logger.info("Synthesizing media for node %i and degree %i", self.id, self.degree)

            with tempfile.NamedTemporaryFile() as temp_file:
                self.synthesize_target(temp_file.name)
                asset = ExperimentAsset(
                    local_key="stimulus",
                    input_path=temp_file.name,
                    extension=".wav",
                    parent=self,
                )
                asset.deposit()


# create random meldoies for intial set of nodes
if VARIABLE_NUM_NOTES:
    start_nodes = [
        CustomNode(definition=create_seed_melody(num_notes_melody))
        for num_notes_melody in NUM_NOTE_CONDITIONS
        for _ in range(NUM_CHAINS_CONDITION)
        ]
else:
    start_nodes = [
        CustomNode(definition=create_seed_melody(NUM_NOTES))
        for _ in range(NUM_CHAINS_EXPERIMENT)
        ]


# practice
class CustomNodePractice(ImitationChainNode):
    def create_definition_from_seed(self, seed, experiment, participant):
        return seed

    def summarize_trials(self, trials: list, experiment, participant):
        sung_intervals = [trial.analysis["sung_intervals"] for trial in trials]
        register = [trial.analysis["register"] for trial in trials]

        sung_intervals = [mean(x) for x in zip(*sung_intervals)]

        return dict(
            register=register.pop(0),
            sung_intervals=sung_intervals,
            trial_type="node_trial_practice"
        )

    def create_initial_seed(self, experiment, participant):

        if VARIABLE_NUM_NOTES:
            num_notes_practice = melodies.sample_num_pitches(4, 7)
        else:
            num_notes_practice = NUM_NOTES

        reference_pitch = melodies.sample_reference_pitch(
            roving_mean["high"],
            roving_width,
        )

        target_pitches = melodies.sample_absolute_pitches(
            reference_pitch=reference_pitch,
            max_interval2reference=MAX_INTERVAL2REFERENCE,
            num_pitches=num_notes_practice
        )
        target_intervals = melodies.convert_absolute_pitches_to_interval_sequence(target_pitches, "previous_note")

        ISIs, note_durations = sample_ISIs_for_tempo(
            min_isi=MIN_ISI,
            max_isi=MAX_ISI,
            silent_gap=MIN_NOTE_SILENCE,
            num_pitches=num_notes_practice
        )

        return dict(
            register="high",  # all melodies are generated in the high register
            reference_pitch=reference_pitch,
            max_interval2reference=MAX_INTERVAL2REFERENCE,
            max_interval_size=MAX_ABS_INT_ERROR_ALLOWED,
            target_pitches=target_pitches,
            target_intervals=target_intervals,
            target_ISIs=ISIs,
            target_note_durations=note_durations,
            num_target_pitches=num_notes_practice,
            trial_type="source_trial_practice",
            reference_mode=reference_mode
        )

class SingingImitationTrialMakerPractice(AudioImitationChainTrialMaker):
    performance_check_type = "performance"
    performance_threshold = 0.5
    give_end_feedback_passed = False


if VARIABLE_NUM_NOTES:
    practice_instructions = "You will now practice singing longer melodies composed of different number of notes."
else:
    practice_instructions = f"You will now practice singing longer melodies consisting of {NUM_NOTES} notes."


# practice_singing = join(
#     InfoPage("We can now start the singing practice. Please pay attention to the instructions.", time_estimate=2),
#     InfoPage(Markup(f"""
#             <h3>Instructions Practice</h3>
#             <hr>
#             {practice_instructions}
#             <br><br>
#             In each trial, you will listen to a melody and asked to sing it back as accurately as possible. 
#             <br><br>
#             <b><b>Remember</b></b>: Sing each note clearly to the syllable 'TA' 
#             and leave short gaps between notes.
#             <hr>
#             """
#             ),
#             time_estimate = 3
#             ),
#     InfoPage(Markup(f"""
#             <h3>Attention</h3>
#             <hr>
#             You will take a total of {DESIGN_PARAMS["num_trials_practice_feedback"]} practice trials and receive feedback after each trial.
#             <br><br>
#             If your performance is not good enough, the experiment will terminate earlier.
#             <hr>
#             """
#             ),
#             time_estimate = 3
#             ),
#     SingingImitationTrialMakerPractice(
#         id_="sing_practice",
#         trial_class=CustomTrialPractice,
#         node_class=CustomNodePractice,
#         chain_type="within",
#         expected_trials_per_participant=DESIGN_PARAMS["num_trials_practice_feedback"],
#         max_trials_per_participant=DESIGN_PARAMS["num_trials_practice_feedback"],
#         chains_per_participant=DESIGN_PARAMS["num_trials_practice_feedback"],  # set to None if chain_type="across"
#         max_nodes_per_chain = 1,  # only relevant in within chains
#         chains_per_experiment=None,  # set to None if chain_type="within"
#         trials_per_node=1,
#         balance_across_chains=True,
#         check_performance_at_end=True,
#         check_performance_every_trial=False,
#         recruit_mode="n_participants",
#         target_n_participants=0,
#         wait_for_networks=True,
#         propagate_failure=False
#     ),
# )


practice_singing_emotion = join(
    InfoPage("We can now start the singing practice. Please pay attention to the instructions.", time_estimate=2),
    InfoPage(Markup(f"""
            <h3>Instructions</h3>
            <hr>
            In this study, you will hear short 5-note-melodies. After each melody, your task is to <b><b>sing it back with emotion.</b></b>
            <br><br>
            Try to imitate the melody as accurately as you can. It might be challenging, but it is important that you keep trying and 
            that you add emotion to the melody. This can be any emotion, whatever you feel fits best to the melody you just heard.
            <br><br>
            Please ensure you are in a quiet environment, sing each note clearly to the syllable 'TA', and leave short gaps between notes.
            <hr>
            """
            ),
            time_estimate = 3
            ),
    InfoPage(Markup(f"""
            We will begin with {DESIGN_PARAMS["num_trials_practice_feedback"]} practice trials where you will receive feedback on your singing.
            <br><br>
            If your performance is not good enough, we will not be able to continue with the experiment.
            <hr>
            """
            ),
            time_estimate = 3
            ),
    SingingImitationTrialMakerPractice(
        id_="sing_practice_emotion",
        trial_class=CustomTrialPractice,
        node_class=CustomNodePractice,
        chain_type="within",
        expected_trials_per_participant=DESIGN_PARAMS["num_trials_practice_feedback"],
        max_trials_per_participant=DESIGN_PARAMS["num_trials_practice_feedback"],
        chains_per_participant=DESIGN_PARAMS["num_trials_practice_feedback"],  # set to None if chain_type="across"
        max_nodes_per_chain = 1,  # only relevant in within chains
        chains_per_experiment=None,  # set to None if chain_type="within"
        trials_per_node=1,
        balance_across_chains=True,
        check_performance_at_end=True,
        check_performance_every_trial=False,
        recruit_mode="n_participants",
        target_n_participants=0,
        wait_for_networks=True,
        propagate_failure=False
    ),
)


# main_singing = join(
#     InfoPage("We can now start with the main singing task.", time_estimate=2),
#     InfoPage(
#         Markup(
#             f"""
#             <h3>Instructions</h3>
#             <hr>
#             You will listen to a total of {num_trials_per_participant} musical melodies. 
#             <br><br>
#             {instruction_instructions}
#             <br><br>
#             <b><b>Note</b></b>: Melodies can either be played by a piano or sung by other particaipnts.
#             <hr>
#             """
#         ),
#         time_estimate=3
#     ),
#     AudioImitationChainTrialMaker(
#         id_="imitation_chain",
#         trial_class=CustomTrial,
#         node_class=CustomNode,
#         start_nodes=start_nodes,
#         chain_type=DESIGN_PARAMS["chain_type"],
#         max_nodes_per_chain=DESIGN_PARAMS["num_iterations_per_chain"],
#         max_trials_per_participant=DESIGN_PARAMS["num_trials_per_participant"],
#         expected_trials_per_participant=DESIGN_PARAMS["num_trials_per_participant"],
#         chains_per_participant=DESIGN_PARAMS["num_chains_per_participant"],
#         chains_per_experiment=DESIGN_PARAMS["num_chains_per_exp"],
#         trials_per_node=1,
#         balance_across_chains=DESIGN_PARAMS["balance_across_chains"],
#         check_performance_at_end=False,
#         check_performance_every_trial=False,
#         recruit_mode=DESIGN_PARAMS["recruit_mode"],
#         target_n_participants=DESIGN_PARAMS["target_num_participants"],
#         allow_revisiting_networks_in_across_chains=DESIGN_PARAMS["repeat_same_chain"]
#     ),
# )

main_singing_emotion = join(
    InfoPage("We can now start with the main singing task.", time_estimate=2),
    InfoPage(
        Markup(
            f"""
            <h3>Instructions</h3>
            <hr>
            You will hear a total of {num_trials_per_participant} 5-note musical melodies. 
            After each melody, your task is to <b><b>sing it back with emotion.</b></b>
            <br><br>
            Please ensure you are in a quiet environment, sing each note clearly to the syllable 'TA', and leave short gaps between notes.
            <hr>
            """
        ),
        time_estimate=3
    ),
    AudioImitationChainTrialMaker(
        id_="imitation_chain_emotion",
        trial_class=CustomTrial,
        node_class=CustomNode,
        start_nodes=start_nodes,
        chain_type=DESIGN_PARAMS["chain_type"],
        max_nodes_per_chain=DESIGN_PARAMS["num_iterations_per_chain"],
        max_trials_per_participant=DESIGN_PARAMS["num_trials_per_participant"],
        expected_trials_per_participant=DESIGN_PARAMS["num_trials_per_participant"],
        chains_per_participant=DESIGN_PARAMS["num_chains_per_participant"],
        chains_per_experiment=DESIGN_PARAMS["num_chains_per_exp"],
        trials_per_node=1,
        balance_across_chains=DESIGN_PARAMS["balance_across_chains"],
        check_performance_at_end=False,
        check_performance_every_trial=False,
        recruit_mode=DESIGN_PARAMS["recruit_mode"],
        target_n_participants=DESIGN_PARAMS["target_num_participants"],
        allow_revisiting_networks_in_across_chains=DESIGN_PARAMS["repeat_same_chain"]
    ),
)


########################################################################################################################
# Timeline
########################################################################################################################
class Exp(psynet.experiment.Experiment):
    label = "iterated naturalistic singing"

    asset_storage = LocalStorage()

    config = {
        **get_prolific_settings(),
        "initial_recruitment_size": INITIAL_RECRUITMENT_SIZE,
        "title": "Singing experiment (Chrome browser, ~14 mins)",
        "description": "This is a singing experiment. You will be asked to sing musical melodies.",
        "contact_email_on_error": "m.angladatort@gold.ac.uk",
        "organization_name": "Goldsmiths, University of London",
        "docker_image_base_name": "docker.io/manuelangladatort/iterated-singing",
        "show_reward": False
    }

    if DEBUG:
        timeline = Timeline(
            GoldsmithsConsent(),
            GoldsmithsAudioConsent(),
            GoldsmithsOpenScienceConsent(),
            CodeBlock(lambda participant: participant.var.set("register", "low")),  # set singing register to high
            welcome(),
            # mic_test(),
            # practice_singing,
            practice_singing_emotion,
            # main_singing,
            # main_singing_emotion,
            # questionnaire(),
            questionnaire_emotion(),
            SuccessfulEndPage()
        )

    else:
        timeline = Timeline(
            GoldsmithsConsent(),
            GoldsmithsAudioConsent(),
            GoldsmithsOpenScienceConsent(),
            welcome(),
            requirements_mic(),
            mic_test(),
            singing_performance(),  # here we 1) screen bad participants and 2) select singing register (8 trials)
            conditional( 
                label="assign_register",
                condition=lambda experiment, participant: participant.var.predicted_register == "undefined",
                logic_if_true=CodeBlock(
                    lambda experiment, participant: participant.var.set(
                        "register", random.choice(["low", "high"]))
                ),
                logic_if_false=CodeBlock(lambda experiment, participant: participant.var.set(
                    "register", participant.var.predicted_register)
                                            ),
                fix_time_credit=False
            ),
            # practice_singing,
            practice_singing_emotion,
            # main_singing,
            main_singing_emotion,
            # questionnaire(),
            questionnaire_emotion(),
            SuccessfulEndPage(),
        )

    if RUN_BOT: 
        @staticmethod # MAKE SURE TO COMMENT OUT IN REAL EXPERIMENTS!
        @scheduled_task("interval", minutes=40 / 60, max_instances=1)
        def run_bot_participant():
            # Every 7 seconds, runs a bot participant.
            from psynet.experiment import is_experiment_launched

            if is_experiment_launched():
                bot = Bot()
                bot.take_experiment()

    def __init__(self, session=None):
        super().__init__(session)
        self.initial_recruitment_size = INITIAL_RECRUITMENT_SIZE