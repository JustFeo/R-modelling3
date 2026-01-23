from manim import *


config.background_color = "#0b0b10"


class ProjectFull(Scene):
    def construct(self):
        # Scene 1: Title + Research Question
        title = Text("Modelling 3 Project", font_size=54, color=BLUE_B)
        subtitle = Text("Robot-assisted motor learning", font_size=30, color=GRAY_C)
        header = VGroup(title, subtitle).arrange(DOWN, aligned_edge=LEFT, buff=0.2)
        header.to_edge(UP, buff=0.6)

        question = Text(
            "Question: Does guidance improve learning,\n"
            "and does personality matter?",
            font_size=30,
            color=WHITE,
            line_spacing=1.2,
        )
        question.to_edge(LEFT, buff=0.9).shift(DOWN * 0.4)

        self.play(FadeIn(title, shift=UP), FadeIn(subtitle, shift=UP))
        self.wait(0.6)
        self.play(Write(question))
        self.wait(1.2)

        # Scene 2: Dataset structure
        data_title = Text("Dataset", font_size=42, color=BLUE_A)
        data_title.to_edge(UP, buff=0.6)
        data_lines = VGroup(
            Text("100 participants", font_size=30, color=WHITE),
            Text("Groups: Control vs Experimental", font_size=30, color=WHITE),
            Text("Stages: BL, STR, LTR", font_size=30, color=WHITE),
            Text("Each stage: 2 runs × 20 targets", font_size=30, color=WHITE),
            Text("Traits: AC, FS, TC, TB (0–100)", font_size=30, color=WHITE),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.25)
        data_lines.to_edge(LEFT, buff=0.9).shift(DOWN * 0.2)

        formula = MathTex(
            r"AveAbsError_{i,s}=\frac{1}{40}\sum_{k=1}^{20}"
            r"(|Error|_{i,s,1,k}+|Error|_{i,s,2,k})"
        ).scale(0.75)
        formula.to_edge(DOWN, buff=0.8)

        self.play(FadeOut(question), FadeOut(header))
        self.play(FadeIn(data_title, shift=UP))
        self.play(LaggedStart(*[Write(l) for l in data_lines], lag_ratio=0.15))
        self.wait(0.6)
        self.play(Write(formula))
        self.wait(1.2)

        # Scene 3: Week 1 distributions
        self.play(FadeOut(data_lines), FadeOut(formula), FadeOut(data_title))
        w1_title = Text("Week 1: Personality distributions", font_size=42, color=BLUE_A)
        w1_title.to_edge(UP, buff=0.6)

        traits = VGroup(
            Text("AC → Weibull", font_size=30, color=WHITE),
            Text("FS → Beta", font_size=30, color=WHITE),
            Text("TC → Beta", font_size=30, color=WHITE),
            Text("TB → Normal", font_size=30, color=WHITE),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.25)
        traits.to_edge(LEFT, buff=0.9).shift(DOWN * 0.2)

        hist_bars = VGroup(
            Rectangle(width=0.25, height=1.2, color=YELLOW_B, fill_opacity=0.8),
            Rectangle(width=0.25, height=0.8, color=GREEN_B, fill_opacity=0.8),
            Rectangle(width=0.25, height=1.5, color=BLUE_B, fill_opacity=0.8),
            Rectangle(width=0.25, height=1.0, color=RED_B, fill_opacity=0.8),
        ).arrange(RIGHT, buff=0.25)
        hist_bars.to_edge(RIGHT, buff=1.2).shift(DOWN * 0.2)

        w1_notes = Text(
            "Histograms, Q–Q plots, KS tests",
            font_size=26,
            color=GRAY_B,
        ).to_edge(DOWN, buff=0.8)

        self.play(FadeIn(w1_title, shift=UP))
        self.play(Write(traits))
        self.play(GrowFromCenter(hist_bars))
        self.play(Write(w1_notes))
        self.wait(1.2)

        # Scene 4: Week 2 comparisons
        self.play(FadeOut(w1_title), FadeOut(traits), FadeOut(hist_bars), FadeOut(w1_notes))
        w2_title = Text("Week 2: Group comparisons", font_size=42, color=BLUE_A)
        w2_title.to_edge(UP, buff=0.6)

        w2_points = VGroup(
            Text("BL: no significant difference", font_size=30, color=WHITE),
            Text("STR: Experimental lower error", font_size=30, color=WHITE),
            Text("LTR: Experimental lower error", font_size=30, color=WHITE),
            Text("Learning effects: E > C", font_size=30, color=WHITE),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.25)
        w2_points.to_edge(LEFT, buff=0.9).shift(DOWN * 0.1)

        bars = VGroup(
            Rectangle(width=0.35, height=1.3, color=GRAY_B, fill_opacity=0.8),
            Rectangle(width=0.35, height=1.1, color=BLUE_B, fill_opacity=0.8),
            Rectangle(width=0.35, height=0.9, color=GRAY_B, fill_opacity=0.8),
            Rectangle(width=0.35, height=0.7, color=BLUE_B, fill_opacity=0.8),
            Rectangle(width=0.35, height=1.1, color=GRAY_B, fill_opacity=0.8),
            Rectangle(width=0.35, height=0.85, color=BLUE_B, fill_opacity=0.8),
        ).arrange(RIGHT, buff=0.2)
        bars.to_edge(RIGHT, buff=1.2).shift(DOWN * 0.2)

        stage_labels = VGroup(
            Text("BL", font_size=22, color=GRAY_C),
            Text("STR", font_size=22, color=GRAY_C),
            Text("LTR", font_size=22, color=GRAY_C),
        ).arrange(RIGHT, buff=1.0)
        stage_labels.next_to(bars, DOWN, buff=0.4)

        self.play(FadeIn(w2_title, shift=UP))
        self.play(Write(w2_points))
        self.play(GrowFromCenter(bars), FadeIn(stage_labels))
        self.wait(1.2)

        # Scene 5: Week 3 regression models
        self.play(FadeOut(w2_title), FadeOut(w2_points), FadeOut(bars), FadeOut(stage_labels))
        w3_title = Text("Week 3: Regression models", font_size=42, color=BLUE_A)
        w3_title.to_edge(UP, buff=0.6)

        eq1 = MathTex(
            r"AveAbsError_{i,s}=\beta_0+\beta_1 AC_i+\beta_2 FS_i+\beta_3 TC_i+\beta_4 TB_i+\varepsilon_{i,s}"
        ).scale(0.7)
        eq2 = MathTex(
            r"+(\beta_5+\beta_6 AC_i+\beta_7 FS_i+\beta_8 TC_i+\beta_9 TB_i)\,1_{\{Group_i=E\}}"
        ).scale(0.7)
        eq_group = VGroup(eq1, eq2).arrange(DOWN, buff=0.25)
        eq_group.to_edge(LEFT, buff=0.6).shift(DOWN * 0.2)

        w3_notes = VGroup(
            Text("FS ↑ error", font_size=30, color=WHITE),
            Text("TC ↓ error", font_size=30, color=WHITE),
            Text("Guidance interacts with traits", font_size=30, color=WHITE),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.2)
        w3_notes.to_edge(RIGHT, buff=1.0).shift(DOWN * 0.1)

        self.play(FadeIn(w3_title, shift=UP))
        self.play(Write(eq_group))
        self.play(Write(w3_notes))
        self.wait(1.2)

        # Scene 6: Week 4 power analysis
        self.play(FadeOut(w3_title), FadeOut(eq_group), FadeOut(w3_notes))
        w4_title = Text("Week 4: Power analysis", font_size=42, color=BLUE_A)
        w4_title.to_edge(UP, buff=0.6)

        w4_text = VGroup(
            Text("Target power: 80%", font_size=30, color=WHITE),
            Text("Effect size (d): 0.8", font_size=30, color=WHITE),
            Text("Required ≈ 26 per group", font_size=30, color=WHITE),
            Text("We have 50 per group → adequate", font_size=30, color=WHITE),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.25)
        w4_text.to_edge(LEFT, buff=0.9).shift(DOWN * 0.1)

        self.play(FadeIn(w4_title, shift=UP))
        self.play(Write(w4_text))
        self.wait(1.2)

        # Scene 7: Extension — trait dependence
        self.play(FadeOut(w4_title), FadeOut(w4_text))
        ext_title = Text("Extension: Trait dependence", font_size=42, color=BLUE_A)
        ext_title.to_edge(UP, buff=0.6)

        ext_lines = VGroup(
            Text("Correlation matrices (AC, FS, TC, TB)", font_size=30, color=WHITE),
            Text("BMA (bicreg) models for trait prediction", font_size=30, color=WHITE),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.25)
        ext_lines.to_edge(LEFT, buff=0.9).shift(DOWN * 0.1)

        squares = VGroup(*[
            Square(side_length=0.35, fill_opacity=0.85, color=BLUE_D)
            for _ in range(16)
        ]).arrange_in_grid(4, 4, buff=0.05)
        squares.to_edge(RIGHT, buff=1.1).shift(DOWN * 0.1)

        self.play(FadeIn(ext_title, shift=UP))
        self.play(Write(ext_lines))
        self.play(FadeIn(squares))
        self.wait(1.2)

        # Scene 8: Workflow + closing
        self.play(FadeOut(ext_title), FadeOut(ext_lines), FadeOut(squares))
        close_title = Text("Workflow", font_size=42, color=BLUE_A)
        close_title.to_edge(UP, buff=0.6)

        flow = VGroup(
            Text("Data → Week 1 → Week 2 → Week 3 → Week 4 → Extension", font_size=28, color=WHITE),
            Text("Outcome: Guidance improves learning, traits matter", font_size=28, color=WHITE),
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.3)
        flow.to_edge(LEFT, buff=0.9).shift(DOWN * 0.1)

        self.play(FadeIn(close_title, shift=UP))
        self.play(Write(flow))
        self.wait(2.0)

        self.play(FadeOut(flow), FadeOut(close_title))
        self.wait(0.5)

