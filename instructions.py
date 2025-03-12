from markupsafe import Markup
from psynet.page import InfoPage


def welcome():
    return InfoPage(
        Markup(
            """
            <h3>Welcome page</h3>
            <hr>
            Thank you for your interest and support for our research on singing and emotions.<br><br>
            We are exploring how singing and emotions interact. In this study, you will hear melodies imitate them while conveying an emotion of your choice.
            <hr>
            """
        ),
        time_estimate=3
    )


def requirements_mic():
    return InfoPage(
        Markup(
            """
            <h3>Requirements</h3>
            <hr>
            <ul>
                <li>You must use a working microphone, either from your headphones or computer.</li>
                <li>You must be in a quiet room (with no background noises).</li>
            </ul>
            If you cannot meet these requirements, please return the study.
            <hr>
            """
        ),
        time_estimate=3
    )
