import random
from dominate import tags

from psynet.demography.general import Age, Gender
from psynet.demography.gmsi import GMSI
from psynet.modular_page import ModularPage, TextControl, SurveyJSControl, PushButtonControl
from psynet.page import InfoPage
from psynet.timeline import join, FailedValidation


def introduction():
    html = tags.div()
    with html:
        tags.p(
            "Congratulations, you completed the main experiment!"
        )
        tags.p(
            "Before we finish, we would like to ask you a few questions about you and the study. ",
            "They should only take a couple of minutes to complete.",
        )
    return InfoPage(html, time_estimate=10)


def questionnaire():
    return join(
        introduction(),
        Age(),
        Gender(),
        GMSI(subscales=["Musical Training", "Emotions"]),
        feedback(),
        debrief()
    )

def questionnaire_emotion():
    return join(
        introduction(),
        add_emotion_in_singing(),
        feedback_emotion(),
        Age(),
        Gender(),
        GMSI(subscales=["Musical Training", "Emotions"]),
        feedback(),
        debrief_emotion()
    )


STOMPR_rating_values = [
    {"value": "1", "text": "1 (Dislike Strongly)"},
    {"value": "2", "text": "2"},
    {"value": "3", "text": "3"},
    {"value": "4", "text": "4"},
    {"value": "5", "text": "5"},
    {"value": "6", "text": "6"},
    {"value": "7", "text": "7 (Like Strongly)"}
]

music_genres = [
    {"value": "Alternative", "text": "Alternative"},
    {"value": "Bluegrass", "text": "Bluegrass"},
    {"value": "Blues", "text": "Blues"},
    {"value": "Classical", "text": "Classical"},
    {"value": "Country", "text": "Country"},
    {"value": "Dance/Electronica", "text": "Dance/Electronica"},
    {"value": "Folk", "text": "Folk"},
    {"value": "Funk", "text": "Funk"},
    {"value": "Gospel", "text": "Gospel"},
    {"value": "Heavy Metal", "text": "Heavy Metal"},
    {"value": "World", "text": "World"},
    {"value": "Jazz", "text": "Jazz"},
    {"value": "New Age", "text": "New Age"},
    {"value": "Oldies", "text": "Oldies"},
    {"value": "Opera", "text": "Opera"},
    {"value": "Pop", "text": "Pop"},
    {"value": "Punk", "text": "Punk"},
    {"value": "Rap/hip-hop", "text": "Rap/hip-hop"},
    {"value": "Reggae", "text": "Reggae"},
    {"value": "Religious", "text": "Religious"},
    {"value": "Rock", "text": "Rock"},
    {"value": "Soul/R&B", "text": "Soul/R&B"},
    {"value": "Soundtracks/theme song", "text": "Soundtracks/theme song"}
]


class STOMPR(ModularPage):
    def __init__(self):
        super().__init__(
            "stompr",
            "Rate your preferences for each music genre on a scale from 1 (Dislike Strongly) to 7 (Like Strongly). You will earn 0.15 dollars.",
            SurveyJSControl(
                {
                    "logoPosition": "right",
                    "pages": [
                        {
                            "name": "page1",
                            "elements": [
                                {
                                    "type": "matrix",
                                    "name": "STOMPR_choices",
                                    "title": "Please indicate your basic preference for each of the following genres using the scale provided.",
                                    "isRequired": True,
                                    "columns": STOMPR_rating_values,
                                    "rows": music_genres,
                                },
                            ],
                        },
                    ],
                },
            ),
            time_estimate=55,
            bot_response=lambda: {"rating": "5",},
        )

    def validate(self, response, **kwargs):
        n_responses = len(response.answer["STOMPR_choices"])

        if n_responses < len(music_genres):
            return FailedValidation("Please answer all the questions.")

        return None


TIPI_rating_values = [
    {"value": "1", "text": "1 (Disagree Strongly)"},
    {"value": "2", "text": "2"},
    {"value": "3", "text": "3"},
    {"value": "4", "text": "4"},
    {"value": "5", "text": "5"},
    {"value": "6", "text": "6"},
    {"value": "7", "text": "7 (Agree Strongly)"}
]

tipi_genres = [
    {"value": "Extraverted", "text": "Extraverted, enthusiastic"},
    {"value": "Critical", "text": "Critical, quarrelsome."},
    {"value": "Dependable", "text": "Dependable, self-disciplined."},
    {"value": "Anxious", "text": "Anxious, easily upset."},
    {"value": "Open", "text": "Open to new experiences, complex."},
    {"value": "Reserved", "text": "Reserved, quiet."},
    {"value": "Sympathetic", "text": "Sympathetic, warm."},
    {"value": "Disorganized", "text": "Disorganized, careless."},
    {"value": "Calm", "text": "Calm, emotionally stable."},
    {"value": "Conventional", "text": "Conventional, uncreative."},
]

class TIPI(ModularPage):
    def __init__(self):
        super().__init__(
            "tipi",
            "Here are a number of personality traits that may or may not apply to you. Please rate each statement to indicate the extent to which you agree/ disagree with them on a scale from 1 (Disagree Strongly) to 7 (Agree Strongly). You will earn 0.15 dollars.",
            SurveyJSControl(
                {
                    "logoPosition": "right",
                    "pages": [
                        {
                            "name": "page1",
                            "elements": [
                                {
                                    "type": "matrix",
                                    "name": "TIPI_choices",
                                    "title": "I see myself as:",
                                    "isRequired": True,
                                    "columns": TIPI_rating_values,
                                    "rows": tipi_genres,
                                },
                            ],
                        },
                    ],
                },
            ),
            time_estimate=55,
            bot_response=lambda: {"rating": "5",},
        )

    def validate(self, response, **kwargs):
        n_responses = len(response.answer["TIPI_choices"])

        if n_responses < len(tipi_genres):
            return FailedValidation("Please answer all the questions.")

        return None


def add_emotion_in_singing():
    return ModularPage(
        "add_emotion_sing",
        "Did you try to sing the melodies with emotion?",
        PushButtonControl(
            choices=["yes", "no"],
            labels=["Yes", "No"],
            arrange_vertically=False,
        ),
        time_estimate=3,
        bot_response="I am just a bot, I don't have any feedback for you.",
        save_answer="feedback_emotion",
    )


def feedback_emotion():
    return ModularPage(
        "feedback",
        "What was your strategy to add emotion to the melodies?",
        TextControl(one_line=False),
        bot_response="I am just a bot, I don't have any feedback for you.",
        save_answer="feedback_emotion",
        time_estimate=5,
    )

def feedback():
    return ModularPage(
        "feedback",
        "Do you have any feedback to give us about the experiment?",
        TextControl(one_line=False),
        bot_response="I am just a bot, I don't have any feedback for you.",
        save_answer="feedback",
        time_estimate=5,
    )


def debrief():
    html = tags.div()

    with html:
        tags.p(
            """
            Thank you for participating in this experiment. The purpose of the experiment was to collect data on how we 
            perceive and sing melodies (sequences of musical tones), such as the ones you have been listening to. In particular, 
            we are interested in studying the role of emotions in the perception of melodies.
            """
        )
        tags.p(
            """
            The data collected during this experiment will help to better understand how people derive emotions from 
            melodies, studying for the first time all possible melodic combinations and listeners' individual 
            differences at a large scale (testing many melodies and participants from different backgrounds).
            """
        )

    return InfoPage(html, time_estimate=5)


def debrief_emotion():
    html = tags.div()

    with html:
        tags.p(
            """
            Thank you for taking part in this experiment and your contribution to science!
            The aim of this experiment is to identify the role of emotions in the oral transmission music. Similar to the Telephone Game, 
            the melodies you heard were sung and passed from one participant to another by singing. This procedure will allow us to identify 
            how the emotionality of melodies develops through transmission and what impact different instructions have on the resulting melodies. 
            """
        )
        tags.p(
            """
            The data collected during this experiment will help to better understand how people derive emotions from 
            melodies, and how this shape music perception and evolution. Your data is stored anonymously, and you cannot withdraw it anymore.
            If you were unexpectedly affected by taking part in the study, please feel free to send feedback to us.
            """
        )
        tags.p(
            """
            If you wish to find out more about this experiment, please get in touch. You can contact me, Dr Manuel Anglada-Tort, at m.anglada-tort@gold.ac.uk. 
            If you have any ethical concerns, you can contact the Chair of the Psychology Ethics Committee at Goldsmiths, University of London, 
            Dr Maria Herrojo Ruiz, m.herrojo-ruiz@gold.ac.uk. 
            """
        )

    return InfoPage(html, time_estimate=5)