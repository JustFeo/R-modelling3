from manim import *


config.background_color = "#0b0b10"


class ProjectIntro(Scene):
    def construct(self):
        title = Text("Modelling 3 Project", font_size=48, color=BLUE_B)
        subtitle = Text("Robot-assisted motor learning", font_size=30, color=GRAY_C)
        header = VGroup(title, subtitle).arrange(DOWN, aligned_edge=LEFT)
        header.to_edge(UP, buff=0.6)

        self.play(FadeIn(title, shift=UP), FadeIn(subtitle, shift=UP))
        self.wait(0.5)

        left_lines = VGroup(
            Text("Data: 100 participants", font_size=30, color=WHITE),
            Text("Stages: BL, STR, LTR", font_size=30, color=WHITE),
            Text("Traits: AC, FS, TC, TB", font_size=30, color=WHITE),
            Text("Groups: Control vs Experimental", font_size=30, color=WHITE),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.25)
        left_lines.to_edge(LEFT, buff=0.8).shift(DOWN * 0.2)

        self.play(LaggedStart(*[Write(line) for line in left_lines], lag_ratio=0.2))
        self.wait(0.5)

        formula = MathTex(
            r"AveAbsError_{i,s} = \frac{1}{40}\sum_{k=1}^{20}(|Error|_{i,s,1,k}+|Error|_{i,s,2,k})"
        ).scale(0.7)
        formula.to_edge(DOWN, buff=0.8)
        self.play(Write(formula))
        self.wait(0.5)

        code_lines = [
            'd <- read.csv("dataset.csv")',
            'stage_data <- subset(d, Stage %in% c("BL","STR","LTR"))',
            "AveAbsError <- aggregate(AbsError ~ ParticipantID + Stage,",
            "                         data=stage_data, mean)",
        ]
        code_mobs = VGroup(
            *[Text(line, font="Monospace", font_size=22, color=YELLOW_B) for line in code_lines]
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.1)

        code_box = SurroundingRectangle(code_mobs, color=GRAY_B, buff=0.3)
        code_label = Text("Code snapshot", font_size=24, color=GRAY_C)
        code_group = VGroup(code_label, code_box, code_mobs).arrange(DOWN, buff=0.15)
        code_group.to_edge(RIGHT, buff=0.8).shift(DOWN * 0.1)

        self.play(FadeIn(code_group, shift=RIGHT))
        self.wait(1.0)

        question = Text("Question: Does guidance improve learning?", font_size=30, color=BLUE_A)
        question.next_to(left_lines, DOWN, buff=0.5, aligned_edge=LEFT)
        self.play(Write(question))
        self.wait(1.0)

        self.play(FadeOut(code_group), FadeOut(question), FadeOut(formula))
        self.wait(0.3)

