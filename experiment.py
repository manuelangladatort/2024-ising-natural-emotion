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
from psynet.consent import NoConsent, MainConsent, OpenScienceConsent, AudiovisualConsent
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
from .questionnaire import questionnaire
from .pre_screens import (
    mic_test,
    recording_example,
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
        "recruiter": RECRUITER,
        # "id": "singing-nets",
        "prolific_estimated_completion_minutes": 14,
        "prolific_maximum_allowed_minutes": 40,
        "prolific_recruitment_config": qualification,
        "base_payment": 2.1,
        "auto_recruit": False,
        "currency": "£",
        "wage_per_hour": 0.01
    }


########################################################################################################################
# Global parameters
########################################################################################################################

DEBUG = False
RUN_BOT = False

RECRUITER = "prolific" # prolific vs hotair
DESIGN = "across"  # within vs across
INITIAL_RECRUITMENT_SIZE = 5
TIME_ESTIMATE_TRIAL = 18  

NUM_NOTES = 5
NUM_INT = (NUM_NOTES - 1)
SYLLABLE = 'TA'

MIN_ISI = 0.5
MAX_ISI = 1.5
LOG_SAMPLE_ISI = False

# trials
NUM_TRIALS_PARTICIPANT = 20 
NUM_PARTICIPANTS_EXPERIMENT = 50  # only active in within
NUM_CHAINS_PARTICIPANT = 3   # only active in within
NUM_CHAINS_EXPERIMENT = 100 # only active in across


MAX_ABS_INT_ERROR_ALLOWED = 999  # set to 999 if NUM_INT > 2
MAX_INT_SIZE = 999
MAX_MELODY_PITCH_RANGE = 999  # deactivated
MAX_INTERVAL2REFERENCE = 7.5  # set to 7.5 if NUM_INT > 2

# singing
config = singing_2intervals  # params singing extraction (from sing4me)
reference_mode = "pitch_mode"  # pitch_mode vs previous_note vs first_note
roving_width = 2.5
roving_mean = dict(
    default=55,  # it was 55.5
    low=49,  # it was 49.5 (male)
    high=61  # it was 61.5 (female)
    )

# timbre
TIMBRE = InstrumentTimbre("piano")
# note_duration_tonejs = 0.8
MIN_NOTE_SILENCE = 0.2
ADD_TIME_AFTER_SINGING = 2


# conditional parameters
if DEBUG:
    num_iterations_per_chain = 5
    num_trials_per_participant = NUM_TRIALS_PARTICIPANT
    num_chains_per_participant = NUM_CHAINS_PARTICIPANT # only active in within
    target_num_participants = NUM_PARTICIPANTS_EXPERIMENT  # only active in within
    num_chains = 10  # only active in across
    repeat_same_chain = True
    save_plot = True
else:
    num_iterations_per_chain = 10
    num_trials_per_participant = NUM_TRIALS_PARTICIPANT
    num_chains_per_participant = NUM_CHAINS_PARTICIPANT   # only active in within
    target_num_participants = NUM_PARTICIPANTS_EXPERIMENT  # only active in within
    num_chains = NUM_CHAINS_EXPERIMENT # only active in across
    repeat_same_chain = False
    save_plot = True


if DESIGN == "within":
    DESIGN_PARAMS = {
        "num_trials_per_participant": (int(num_trials_per_participant) + 10),
        "num_trials_practice_test": 2,
        "num_trials_practice_feedback": 2,
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
        "num_trials_practice_test": 2,
        "num_trials_practice_feedback": 2,
        "num_iterations_per_chain": num_iterations_per_chain,
        "trials_per_node": 1,
        "balance_across_chains": False,
        "chain_type": "across",
        "num_chains_per_participant": None,
        "recruit_mode": "n_trials",
        "target_num_participants": None,
        "num_chains_per_exp": num_chains,
        "repeat_same_chain": repeat_same_chain
    }


# methods
def estimate_time_per_trial(note_durations, num_pitches, silence_duration, extra_time_singing):
    melody_duration = sum(note_durations) + (silence_duration*num_pitches) + 0.5
    singing_duration = melody_duration + extra_time_singing
    return melody_duration, singing_duration


def sample_ISI(min_isi, max_isi, is_log_sampling):
    if is_log_sampling:
        isis = math.exp(random.uniform(math.log(min_isi), math.log(max_isi)))
    else:
        isis = random.uniform(min_isi, max_isi)
    return isis


def sample_ISIs(min_isi, max_isi, num_pitches, is_log_sampling):
    isis = []
    for i in range(num_pitches):
        target_pitch = sample_ISI(
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


########################################################################################################################
# Experiment parts
########################################################################################################################

def create_singing_trial(show_current_trial, time_estimate, melody_duration, singing_duration, stimulus):
    singing_page = ModularPage(
        "singing",
        AudioPrompt(
            stimulus,
            Markup(
                f"""
              <h3>Sing back the melody</h3>
                <hr>
                <b><b>This melody has {NUM_NOTES} notes</b></b>: Sing each note using the syllable '{SYLLABLE}' and leave silent gaps between notes.
                <br><br>
                <div class="alert alert-primary">
                    <ul>
                        <li>Click 'Record from start' to start the recording again.</li>
                        <li>Click 'Play melody' to listen to the target melody.</li>
                        <li>Click 'Play recording' to listen to your recording.</li>
                    </ul>
                </div>
                {show_current_trial}
                <hr>
                """
            ),
        ),
        control=AudioRecordControl(
            duration=singing_duration,
            show_meter=True,
            controls=True,
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
                <h3>Sing back the melody</h3>
                <hr>
                <b><b>This melody has {NUM_NOTES} notes</b></b>: Sing each note using the syllable '{SYLLABLE}' and leave silent gaps between notes.
                <br><br>
                <div class="alert alert-primary">
                    <ul>
                        <li>Click 'Record from start' to start the recording again.</li>
                        <li>Click 'Play recording' to listen to your recording.</li>
                    </ul>
                </div>
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
            controls=True,
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

        # sung_silence_durations, sung_note_durations, sung_ISIs = sing.extract_onsets(raw, MIN_NOTE_SILENCE) # TODO: check extraction, why are silences 0.25?
        
        # timings

        # get start/ end onsets
        start_onsets = [x["start_tt"] for x in raw]
        end_onsets = [x["end_tt"] for x in raw]

        # get ISI
        ISI_ms = np.diff(start_onsets)
        sung_ISIs = [(i / 1000) for i in ISI_ms]

        # note durations
        note_durations_ms = [element2 - element1 for (element2, element1) in zip(end_onsets, start_onsets)]
        sung_note_durations = [(i / 1000) for i in note_durations_ms]

        # silent durations
        silence_durations_ms = [element2 - element1 for (element2, element1) in zip(ISI_ms, note_durations_ms)]
        sung_silence_durations = [(i / 1000) for i in silence_durations_ms]

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
        correct_num_notes = num_sung_pitches == num_target_pitches

        if correct_num_notes:
            failed = False
            reason = "All good"
        else:
            failed = True
            reason = f"Wrong number of sung notes: {num_sung_pitches}  sung out of {num_target_pitches} notes in melody"

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
            "sung_silence_durations":sung_silence_durations,
            "sung_note_durations":sung_note_durations,
            "sung_ISIs":sung_ISIs,
            "raw_audio": audio_file
        }

        analysis_dict = convert_numpy(analysis_dict)

        # for key, value in analysis_dict.items():
        #     if isinstance(value, np.ndarray):
        #         analysis_dict[key] = value.tolist()
        
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
                NUM_NOTES + 1,
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
            start_time = (raw_analysis[0][0]['start_tt'] - 100) /1000
            end_time = (raw_analysis[0][NUM_NOTES-1]['end_tt'] + 100)/1000
            tot_duration = end_time-start_time

            singing_page = create_singing_trial(
                show_current_trial,
                TIME_ESTIMATE_TRIAL,
                (tot_duration + 1),
                (tot_duration + ADD_TIME_AFTER_SINGING),
                stimulus
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
            NUM_NOTES + 1,
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

        if num_sung_pitches == num_target_pitches:
            return InfoPage(
                Markup(
                    f"""
                    <h3>Your performance is great!</h3>
                    <hr>
                    We detected {num_sung_pitches} notes in your recording.
                    <hr>
                    """
                ),
                time_estimate=5
            )
        elif num_sung_pitches == (num_target_pitches - 1) or num_sung_pitches == (num_target_pitches + 1):
            return InfoPage(
                Markup(
                    f"""
                    <h3>You can do better...</h3>
                    <hr>
                    We detected {num_sung_pitches} notes in your recording, but we asked you to sing {num_target_pitches} notes.
                    <br>
                    Please try to do one or more of the following:
                    <ol><li>Sing each note clearly using the syllable 'TA'.</li>
                        <li>Make sure you computer microphone is working and you are in a quiet environment.</li>
                        <li>Leave a silent gap between the notes.</li>
                        <li>Sing each note for about 1 second.</li>
                    </ol>
                    <b><b>If you don't improve your performance, the experiment will terminate.</b></b>
                    <hr>
                    """
                ),
                time_estimate=5
            )
        else:
            return InfoPage(
                Markup(
                    f"""
                   <h3>Your performance is bad...</h3>
                    <hr>
                    We could not detect any note in your recording.<br><br>
                    Please try to do one or more of the following:
                    <ol><li>Sing each note clearly using the syllable 'TA'.</li>
                        <li>Make sure you computer microphone is working and you are in a quiet environment.</li>
                        <li>Leave a silent gap between the notes.</li>
                        <li>Sing each note for about 1 second.</li>
                    </ol>
                    <b><b>If you don't improve your performance, the experiment will terminate.</b></b>
                    <hr>
                    """
                ),
                time_estimate=5
            )


class CustomNode(ImitationChainNode):
    def create_definition_from_seed(self, seed, experiment, participant):
        return seed

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
            # register=self.participant.var.register,
            num_target_pitches=len(target_pitches),
            target_pitches=target_pitches,
            target_intervals=target_intervals,
            target_note_durations=target_note_durations,
            target_ISIs = target_ISIs,
            trial_type="node_trial",
            raw_audio=raw_audio,
            raw_analysis=raw_analysis
        )

    def create_initial_seed(self, experiment, participant):

        reference_pitch = melodies.sample_reference_pitch(
            roving_mean["high"],
            roving_width,
        )

        target_pitches = melodies.sample_absolute_pitches(
            reference_pitch=reference_pitch,
            max_interval2reference=MAX_INTERVAL2REFERENCE,
            num_pitches=NUM_NOTES
        )
        target_intervals = melodies.convert_absolute_pitches_to_interval_sequence(target_pitches, "previous_note")

        ISIs = sample_ISIs(
            min_isi=MIN_ISI,
            max_isi=MAX_ISI,
            num_pitches=NUM_NOTES,
            is_log_sampling=LOG_SAMPLE_ISI
        )
        
        note_durations = get_note_durations(ISIs, MIN_NOTE_SILENCE)
        
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
            num_target_pitches=NUM_NOTES,
            trial_type="source_trial",
            reference_mode=reference_mode,
            onsets=onsets
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
            start_time = (raw_analysis[0][0]['start_tt'] - 150) / 1000
            end_time = (raw_analysis[0][NUM_NOTES - 1]['end_tt'] + 150) / 1000

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
        logger.info("Synthesizing media for node %i ", self.id, "and degree %i", self.degree)

        with tempfile.NamedTemporaryFile() as temp_file:

            self.synthesize_target(temp_file.name)
            asset = ExperimentAsset(
                local_key="stimulus",
                input_path=temp_file.name,
                extension=".wav",
                parent=self,
            )
            asset.deposit()


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

        reference_pitch = melodies.sample_reference_pitch(
            roving_mean["high"],
            roving_width,
        )

        target_pitches = melodies.sample_absolute_pitches(
            reference_pitch=reference_pitch,
            max_interval2reference=MAX_INTERVAL2REFERENCE,
            num_pitches=NUM_NOTES
        )
        target_intervals = melodies.convert_absolute_pitches_to_interval_sequence(target_pitches, "previous_note")

        ISIs = sample_ISIs(
            min_isi=MIN_ISI,
            max_isi=MAX_ISI,
            num_pitches=NUM_NOTES,
            is_log_sampling=LOG_SAMPLE_ISI
        )
        
        note_durations = get_note_durations(ISIs, MIN_NOTE_SILENCE)

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
            num_target_pitches=NUM_NOTES,
            trial_type="source_trial_practice",
            reference_mode=reference_mode,
            onsets=onsets
        )

class SingingImitationTrialMakerPractice(AudioImitationChainTrialMaker):
    performance_check_type = "performance"
    performance_threshold = 0.25
    give_end_feedback_passed = False


practice_singing = join(
    InfoPage("We can now start the main singing practice. Please pay attention to the instructions.", time_estimate=2),
    InfoPage(Markup(f"""
            <h3>Instructions Practice</h3>
            <hr>
            You will now practice singing to longer melodies consisting of {NUM_NOTES} notes.
            <br><br>
            In each trial, you will first listen to a melody and rate it.<br>
            You will then listen to the melody again and sing it back as accurately as possible. 
            <br><br>
            <b><b>Remember</b></b>: Sing each note clearly to the syllable 'TA' 
            and leave silent gaps between notes.
            <hr>
            """
            ),
            time_estimate = 3
            ),
    SingingImitationTrialMakerPractice(
        id_="sing_practice",
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


main_singing = join(
    InfoPage("We can now start with the main singing task. Please pay attention to the instructions.", time_estimate=2),
    InfoPage(
        Markup(
            f"""
            <h3>Instructions</h3>
            <hr>
            You will listen to a total of {num_trials_per_participant} musical melodies. 
            <br><br>
            <b><b>Note</b></b>: Melodies can either be played by a piano or sung by other particaipnts.
            <br><br>
            In each trial, you will first listen to a melody and rate it.<br>
            You will then be asked to sing the melody back as accurately as possible. 
            <hr>
            """
        ),
        time_estimate=3
    ),
    AudioImitationChainTrialMaker(
        id_="imitation_chain",
        trial_class=CustomTrial,
        node_class=CustomNode,
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
        "description": "This is a singing experiment. You will be asked to listen to and sing melodies.",
        "contact_email_on_error": "m.angladatort@gold.ac.uk",
        "organization_name": "Max Planck Institute for Empirical Aesthetics",
        "show_reward": False
    }

    if DEBUG:
        timeline = Timeline(
            NoConsent(),
            CodeBlock(lambda participant: participant.var.set("register", "low")),  # set singing register to low
            welcome(),
            requirements_mic(),
            mic_test(),
            practice_singing,
            main_singing,
            questionnaire(),
            SuccessfulEndPage()
        )

    else:
        timeline = Timeline(
            MainConsent(),
            AudiovisualConsent(),
            OpenScienceConsent(),
            welcome(),
            requirements_mic(),
            mic_test(),
            # recording_example(),
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
            practice_singing,
            main_singing,
            questionnaire(),
            InfoPage(Markup(
                f"""
                <h3>Thank you for participating!</h3>
                <hr>
                You have successfully completed the experiment. 
                <br><br>
                <b><b>Completion code</b></b>: we are aware of a problem in Prolific where some participants do not get a completion code at the end of the study. 
                If this is the case, please use the NOCODE option and we will manually send you the full payment. Thank you.
                <hr>
                """
                ), time_estimate=2),
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