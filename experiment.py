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
from psynet.timeline import Event, ProgressDisplay, ProgressStage, Timeline, CodeBlock, conditional
from psynet.trial.audio import (
    AudioImitationChainTrial,
    AudioImitationChainTrialMaker,
)
from psynet.trial.imitation_chain import ImitationChainNode
from psynet.utils import get_logger
logger = get_logger()

# sing4me
from sing4me import singing_extract as sing
from sing4me import melodies
from .params import singing_2intervals
from .instructions import instructions, requirements_mic
from .questionnaire import questionnaire
from .pre_screens import (
    mic_test,
    recording_example,
    singing_performance
)
from .process_audio import fade_in_out, trim_audio, bandpass_filter

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

DEBUG = True
RECRUITER = "hotair" # "prolific"
RUN_BOT = False
DESIGN = "within"  # within vs across
INITIAL_RECRUITMENT_SIZE = 5

SYLLABLE = 'TA'
NUM_NOTES = 5
NUM_INT = (NUM_NOTES - 1)

MIN_ISI = 0.5
MAX_ISI = 1.5
LOG_SAMPLE_ISI = False

# trials
NUM_PARTICIPANTS_EXPERIMENT = 50  # only active in within
NUM_TRIALS_PARTICIPANT = 30 
NUM_CHAINS_EXPERIMENT = 100 # only active in across

TIME_ESTIMATE_TRIAL = 22  # increase if NUM_INT > 2
TIME_ESTIMATE_LISTEN = 10
TIME_ESTIMATE_SING = 12

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
note_silence_tonejs = 0.2
ADD_TIME_AFTER_SINGING = 2

# values ratings
rating_response = [
    {"value": "1", "text": "1"},
    {"value": "2", "text": "2"},
    {"value": "3", "text": "3"},
    {"value": "4", "text": "4"},
    {"value": "5", "text": "5"},
    {"value": "6", "text": "6"},
    {"value": "7", "text": "7"},
    {"value": "8", "text": "8"},
    {"value": "9", "text": "9"},
    {"value": "10", "text": "10"}
    ]

# conditional parameters
if DEBUG:
    num_iterations_per_chain = 5
    num_chains_per_participant = 3
    num_trials_per_participant = 15
    max_num_failed_trials_allowed = 2
    target_num_participants = 10
    num_chains = 5  # only active in across
    repeat_same_chain = True
    save_plot = True
else:
    num_iterations_per_chain = 10
    num_chains_per_participant = 3   # only active in within
    num_trials_per_participant = NUM_TRIALS_PARTICIPANT
    max_num_failed_trials_allowed = 5  # TODO: implement
    target_num_participants = NUM_PARTICIPANTS_EXPERIMENT  # only active in across
    num_chains = NUM_CHAINS_EXPERIMENT
    repeat_same_chain = False
    save_plot = True


if DESIGN == "within":
    DESIGN_PARAMS = {
        "num_trials_per_participant": (int(num_trials_per_participant) + 10),
        "num_trials_practice_test": 3,
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
        "num_trials_practice_test": 3,
        "num_trials_practice_feedback": 3,
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
        curr_onset = note - note_silence_tonejs
        note_durations.append(curr_onset)
    return note_durations


########################################################################################################################
# Experiment parts
########################################################################################################################
# def create_listen_and_rate_trial(show_current_trial, time_estimate, stimulus, stimulus_duration):
#     listen_and_rate_page = ModularPage(
#         "listen_and_rate_trial",
#         AudioPrompt(
#             stimulus,
#             Markup(
#                 f"""
#                 <h3>Listen to and rate the melody</h3>
#                 <hr>
#                 Please use the ratings scales below'.<br>
#                 {show_current_trial}
#                 <hr>
#                 """
#             ),
#             controls=False,
#         ),
#         control=SurveyJSControl(
#                 {
#                     "logoPosition": "right",
#                     "pages": [
#                         {
#                             "name": "rate_emotions",
#                             "elements": [
#                                 {
#                                     "type": "rating",
#                                     "name": "valence",
#                                     "title": "Please rate whether the song evoked negative or positive feelings.",
#                                     "rateValues": rating_response,
#                                     "minRateDescription": "Very negative",
#                                     "maxRateDescription": "Very positive",
#                                 },
#                                  {
#                                     "type": "rating",
#                                     "name": "arousal",
#                                     "title": "Please rate the level of energy or excitement you felt while listening to the song.",
#                                     "rateValues": rating_response,
#                                     "minRateDescription": "Very calm",
#                                     "maxRateDescription": "Very exciting",
#                                 },
#                             ],
#                         },
#                     ],
#                 },
#             ),
#         events={"promptStart": Event(is_triggered_by="trialStart", delay=1)},
#         progress_display=ProgressDisplay(
#             stages=[ProgressStage(stimulus_duration, "Listen to and rate the melody", "green")],
#         ),
#         time_estimate=time_estimate,
#     )
#     return listen_and_rate_page


# def create_listen_and_rate_trial_seed(show_current_trial, time_estimate, target_pitches, note_durations, stimulus_duration):
#     listen_and_rate_page = ModularPage(
#         "listen_and_rate_trial_seed",
#         JSSynth(
#             Markup(
#                 f"""
#                 <h3>Listen to and rate the melody</h3>
#                 <hr>
#                 Please use the ratings scales below'.<br>
#                 {show_current_trial}
#                 <hr>
#                 """
#             ),
#             [Note(pitch, duration=duration) for pitch, duration in
#              zip(target_pitches, note_durations)],
#             timbre=TIMBRE,
#             default_silence=note_silence_tonejs,
#         ),

#         control=SurveyJSControl(
#                 {
#                     "logoPosition": "right",
#                     "pages": [
#                         {
#                             "name": "rate_emotions",
#                             "elements": [
#                                 {
#                                     "type": "rating",
#                                     "name": "valence",
#                                     "title": "Please rate whether the song evoked negative or positive feelings.",
#                                     "rateValues": rating_response,
#                                     "minRateDescription": "Very negative",
#                                     "maxRateDescription": "Very positive",
#                                 },
#                                  {
#                                     "type": "rating",
#                                     "name": "arousal",
#                                     "title": "Please rate the level of energy or excitement you felt while listening to the song.",
#                                     "rateValues": rating_response,
#                                     "minRateDescription": "Very calm",
#                                     "maxRateDescription": "Very exciting",
#                                 },
#                             ],
#                         },
#                     ],
#                 },
#             ),
#         events={"promptStart": Event(is_triggered_by="trialStart", delay=1)},
#         progress_display=ProgressDisplay(
#             stages=[ProgressStage(stimulus_duration, "Listen to and rate the melody", "green")],
#         ),
#         time_estimate=time_estimate,
#     )
#     return listen_and_rate_page


def create_singing_trial(show_current_trial, time_estimate, melody_duration, singing_duration, stimulus):
    singing_page = ModularPage(
        "singing",
        AudioPrompt(
            stimulus,
            Markup(
                f"""
                <h3>Sing back the melody</h3>
                <hr>
                <b><b>This melody has {NUM_NOTES} notes </b></b>: Sing each note clearly using the syllable '{SYLLABLE}'.
                <br><i>Leave silent gaps between notes.</i>
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
            bot_response_media="audio_5notes.wav.wav",
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
                <b><b>This melody has {NUM_NOTES} notes</b></b>: Sing each note clearly using the syllable '{SYLLABLE}'.
                <br><i>Leave silent gaps between notes.</i>
                <br><br>
                {show_current_trial}
                <hr>
                """
            ),
            [Note(pitch, duration=duration) for pitch, duration in
             zip(target_pitches, note_durations)],
            timbre=TIMBRE,
            default_silence=note_silence_tonejs,
        ),
        control=AudioRecordControl(
            duration=singing_duration,
            show_meter=True,
            controls=False,
            auto_advance=False,
            bot_response_media="example_audio.wav",
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

        raw = sing.analyze(
            audio_file,
            config,
            target_pitches=target_pitches,
            plot_options=sing.PlotOptions(
                save=save_plot, path=output_plot, format="png"
            ),
        )

        raw = [
            {key: melodies.as_native_type(value) for key, value in x.items()} for x in raw
        ]
        silence_durations, note_durations, ISIs = sing.extract_onsets(raw, 0.25)
        sung_pitches = [x["median_f0"] for x in raw]
        sung_intervals = melodies.convert_absolute_pitches_to_interval_sequence(
            sung_pitches,
            "previous_note"
        )
        target_intervals = melodies.convert_absolute_pitches_to_interval_sequence(
            target_pitches,
            "previous_note"
        )

        stats = sing.compute_stats(
            sung_pitches,
            target_pitches,
            sung_intervals,
            target_intervals
        )

        num_sung_pitches = stats["num_sung_pitches"]
        num_target_pitches = stats["num_target_pitches"]
        correct_num_notes = num_sung_pitches == num_target_pitches

        if correct_num_notes:
            failed = False
            reason = "All good"
        else:
            failed = True
            reason = f"Wrong number of sung notes: {num_sung_pitches}  sung out of {num_target_pitches} notes in melody"

        return {
            "failed": failed,
            "reason": reason,
            "register": self.participant.var.register,
            # "reference_pitch": reference_pitch,
            "target_pitches": target_pitches,
            "num_target_pitches": len(target_pitches),
            "target_intervals": target_intervals,
            "sung_pitches": sung_pitches,
            "num_sung_pitches": len(sung_pitches),
            "sung_intervals": sung_intervals,
            "raw": raw,
            "save_plot": save_plot,
            "stats": stats,
            "silence_durations":silence_durations,
            "note_durations":note_durations,
            "ISIs":ISIs,
            "raw_audio": audio_file
        }


class CustomTrial(CustomTrialAnalysis):
    time_estimate = TIME_ESTIMATE_TRIAL

    def show_trial(self, experiment, participant):
        current_trial = self.position + 1
        total_num_trials = DESIGN_PARAMS["num_trials_per_participant"]
        show_current_trial = f'<br><br>Trial number {current_trial} out of {total_num_trials} possible maximum trials.'

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
                note_silence_tonejs,
                ADD_TIME_AFTER_SINGING
            )

            singing_page = create_singing_trial_seed(
                show_current_trial,
                target_pitches,
                target_note_durations,
                TIME_ESTIMATE_SING,
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
                TIME_ESTIMATE_SING,
                (tot_duration + 1),
                (tot_duration + ADD_TIME_AFTER_SINGING),
                stimulus
            )

        return singing_page


class CustomNode(ImitationChainNode):
    def create_definition_from_seed(self, seed, experiment, participant):
        return seed

    def summarize_trials(self, trials: list, experiment, participant):
        target_pitches = [trial.analysis["target_pitches"] for trial in trials]
        sung_intervals = [trial.analysis["sung_intervals"] for trial in trials]
        note_durations = [trial.analysis["note_durations"] for trial in trials]
        ISIs = [trial.analysis["ISIs"] for trial in trials]
        raw_analysis = [trial.analysis["raw"] for trial in trials]
        raw_audio = [trial.analysis["raw_audio"] for trial in trials]

        target_pitches = [mean(x) for x in zip(*target_pitches)]
        target_intervals = [mean(x) for x in zip(*sung_intervals)]
        note_durations = [mean(x) for x in zip(*note_durations)]
        ISIs = [mean(x) for x in zip(*ISIs)]

        return dict(
            register=self.participant.var.register,
            num_target_pitches=len(target_pitches),
            target_pitches=target_pitches,
            target_intervals=target_intervals,
            target_note_durations=note_durations,
            ISIs = ISIs,
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
        target_intervals2reference = melodies.convert_absolute_pitches_to_intervals2reference(
            target_pitches, reference_pitch
        )
        ISIs = sample_ISIs(
            min_isi=MIN_ISI,
            max_isi=MAX_ISI,
            num_pitches=NUM_NOTES,
            is_log_sampling=LOG_SAMPLE_ISI
        )
        
        note_durations = get_note_durations(ISIs, note_silence_tonejs)

        # note_durations = sample_note_durations(
        #     min_note_duration=MIN_ISI,
        #     max_note_duration=MAX_ISI,
        #     num_pitches=NUM_NOTES,
        #     is_log_sampling=LOG_SAMPLE_ISI
        # )
        
        onsets = []
        curr_onset = 0
        for note in note_durations:
            onsets.extend([curr_onset])
            curr_onset += note + note_silence_tonejs

        return dict(
            register="high",  # all melodies are generated in the high register
            reference_pitch=reference_pitch,
            max_interval2reference=MAX_INTERVAL2REFERENCE,
            max_interval_size=MAX_ABS_INT_ERROR_ALLOWED,
            target_pitches=target_pitches,
            target_intervals=target_intervals,
            ISIs=ISIs,
            target_note_durations=note_durations,
            target_intervals2reference=target_intervals2reference,
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
            start_time = (raw_analysis[0][0]['start_tt']-150)/1000
            end_time = (raw_analysis[0][NUM_NOTES-1]['end_tt']+150)/1000
            self.parent.alive_trials[0].assets["singing"].export(output_file)
            if start_time < 0:
                start_time = 0

            trim_audio(start_time,end_time,output_file)
            rate, data = read(output_file)
            data = fade_in_out(rate, data, 100, output_file)
            data = bandpass_filter(data, rate, 300, 6000)
            data = np.array(data,dtype=np.int16)

            write(output_file, rate, data)

    def async_on_deploy(self):
        logger.info("Synthesizing media for node %i...", self.id)

        with tempfile.NamedTemporaryFile() as temp_file:

            self.synthesize_target(temp_file.name)
            asset = ExperimentAsset(
                local_key="stimulus",
                input_path=temp_file.name,
                extension=".wav",
                parent=self,
            )
            asset.deposit()


main_singing = join(
    InfoPage("We can now start with the main singing task. Please pay attention to the instructions.", time_estimate=2),
    InfoPage(
        Markup(
            f"""
            <h3>Instructions</h3>
            <hr>
            You will listen to a total of {num_trials_per_participant} musical melodies. Each melody is composed of {NUM_NOTES} notes.
            <br><br>
            Listen to each melody carefully and sing it back as accurately as possible. 
            <br><br>
            <b><b>Important</b></b>: Sing each note clearly to the syllable 'TA' 
            and leave silent gaps between notes.
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
        allow_revisiting_networks_in_across_chains=DESIGN_PARAMS["repeat_same_chain"],
        assets={"5s_silence": CachedAsset(input_path="assets/5s_silence.wav")}
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
        "title": "Singing experiment (Chrome browser, ~15 mins)",
        "description":
            "This is a singing experiment. You will be asked to replicate melodies. Please use incognito mode.",
        "contact_email_on_error": "computational.audition+online_running_manu@gmail.com",
        "organization_name": "Max Planck Institute for Empirical Aesthetics",
        "show_reward": False
    }

    if DEBUG:
            timeline = Timeline(
                NoConsent(),
                instructions(),
                CodeBlock(lambda participant: participant.var.set("register", "low")),  # set singing register to low
                main_singing,
                SuccessfulEndPage()
            )

    else:
        timeline = Timeline(
            MainConsent(),
            AudiovisualConsent(),
            OpenScienceConsent(),
            requirements_mic(),
            mic_test(),
            recording_example(),
            singing_performance(),  # here we 1) screen bad participants and 2) select singing register
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
            main_singing,
            questionnaire(),
            SuccessfulEndPage(),
        )

    # if RUN_BOT: # TODO: implement BOTDS
    #     @staticmethod # MAKE SURE TO COMMENT OUT IN REAL EXPERIMENTS!
    #     @scheduled_task("interval", minutes=40 / 60, max_instances=1)
    #     def run_bot_participant():
    #         # Every 7 seconds, runs a bot participant.
    #         from psynet.experiment import is_experiment_launched

    #         if is_experiment_launched():
    #             bot = Bot()
    #             bot.take_experiment()

    def __init__(self, session=None):
        super().__init__(session)
        self.initial_recruitment_size = INITIAL_RECRUITMENT_SIZE