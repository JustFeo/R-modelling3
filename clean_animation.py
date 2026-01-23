"""
Robot-Assisted Learning: Clean, Sequential Visual Analysis
One concept at a time, slow and methodical
Duration: ~5 minutes with proper pacing
"""

from manim import *
import numpy as np

class CleanStatisticalAnalysis(Scene):
    def construct(self):
        self.camera.background_color = "#0a0a0a"
        
        # PART 1: Introduction (30s)
        self.intro_scene()
        
        # PART 2: Show ONE distribution at a time (60s total)
        self.show_distributions()
        
        # PART 3: Learning effect - ONE comparison (60s)
        self.show_learning()
        
        # PART 4: Regression - step by step (70s)
        self.show_regression()
        
        # PART 5: Power analysis - slow build (50s)
        self.show_power()
        
        # PART 6: Correlations - one at a time (40s)
        self.show_correlations()
        
        # PART 7: Conclusion (20s)
        self.conclusion()
    
    
    def intro_scene(self):
        """Simple, clear introduction"""
        # Title
        title = Text("Robot-Assisted Motor Learning", font_size=48, color=BLUE)
        self.play(Write(title), run_time=2)
        self.wait(2)
        self.play(FadeOut(title), run_time=1)
        
        # Study design - ONE element at a time
        design1 = Text("100 participants", font_size=36)
        self.play(FadeIn(design1), run_time=1.5)
        self.wait(2)
        
        design2 = Text("2 groups: Control vs Experimental", font_size=32)
        design2.next_to(design1, DOWN, buff=0.8)
        self.play(FadeIn(design2), run_time=1.5)
        self.wait(2)
        
        design3 = Text("3 stages: Baseline → Training → Retention", font_size=28)
        design3.next_to(design2, DOWN, buff=0.8)
        self.play(FadeIn(design3), run_time=1.5)
        self.wait(2)
        
        # Clear everything
        self.play(FadeOut(VGroup(design1, design2, design3)), run_time=1)
        self.wait(1)
    
    
    def show_distributions(self):
        """Show ONE distribution at a time, properly spaced"""
        title = Text("Personality Trait Distributions", font_size=36, color=BLUE)
        title.to_edge(UP, buff=0.5)
        self.play(Write(title), run_time=1.5)
        self.wait(1)
        
        # Show each trait one by one
        traits = [
            {"name": "Achiever", "abbr": "AC", "color": "#FFD700"},
            {"name": "Free Spirit", "abbr": "FS", "color": "#00FF88"},
        ]
        
        for trait in traits:
            self.show_single_distribution(trait)
            self.wait(2)
        
        self.play(FadeOut(title), run_time=1)
        self.wait(1)
    
    
    def show_single_distribution(self, trait_info):
        """Show a single distribution, clean and centered"""
        # Clear label
        label = Text(f"{trait_info['name']} ({trait_info['abbr']})", 
                    font_size=32, color=trait_info['color'])
        label.shift(UP * 2.5)
        
        self.play(FadeIn(label), run_time=1)
        self.wait(1)
        
        # Create axes - centered, well-spaced
        axes = Axes(
            x_range=[0, 100, 25],
            y_range=[0, 25, 5],
            x_length=8,
            y_length=4,
            axis_config={
                "include_numbers": True,
                "font_size": 20,
                "stroke_width": 2
            },
            tips=False
        )
        axes.shift(DOWN * 0.5)
        
        # Axis labels
        x_label = Text("Score", font_size=24).next_to(axes.x_axis, DOWN, buff=0.4)
        y_label = Text("Count", font_size=24).next_to(axes.y_axis, LEFT, buff=0.4).rotate(PI/2)
        
        self.play(
            Create(axes),
            Write(x_label),
            Write(y_label),
            run_time=1.5
        )
        self.wait(1)
        
        # Generate data
        np.random.seed(hash(trait_info['abbr']) % 1000)
        if trait_info['abbr'] == "AC":
            data = np.random.weibull(1.5, 100) * 50
        else:  # FS
            data = np.random.beta(3, 2.5, 100) * 100
        data = np.clip(data, 0, 100)
        
        # Create histogram bars - ONE AT A TIME
        hist, bin_edges = np.histogram(data, bins=10, range=(0, 100))
        
        bars = []
        for i in range(len(hist)):
            x_pos = (bin_edges[i] + bin_edges[i+1]) / 2
            height = hist[i]
            
            bar = Rectangle(
                width=0.7,
                height=height * 0.15,
                fill_color=trait_info['color'],
                fill_opacity=0.7,
                stroke_color=trait_info['color'],
                stroke_width=2
            )
            bar.move_to(axes.c2p(x_pos, height / 2))
            bars.append(bar)
        
        # Animate bars appearing ONE BY ONE
        for bar in bars:
            self.play(GrowFromEdge(bar, DOWN), run_time=0.3)
        
        self.wait(1)
        
        # Fit curve
        if trait_info['abbr'] == "AC":
            curve = axes.plot(
                lambda x: 22 * 1.5/50 * ((x/50)**0.5) * np.exp(-((x/50)**1.5)),
                x_range=[0, 100],
                color=trait_info['color'],
                stroke_width=4
            )
        else:  # FS
            from scipy.stats import beta as beta_dist
            curve = axes.plot(
                lambda x: 22 * beta_dist.pdf(x/100, 3, 2.5),
                x_range=[0, 100],
                color=trait_info['color'],
                stroke_width=4
            )
        
        self.play(Create(curve), run_time=2)
        self.wait(2)
        
        # Clear this distribution completely before next
        self.play(
            FadeOut(VGroup(label, axes, x_label, y_label, curve, *bars)),
            run_time=1
        )
        self.wait(0.5)
    
    
    def show_learning(self):
        """Show learning effect - clean comparison"""
        title = Text("Learning Effects", font_size=36, color=BLUE)
        title.to_edge(UP, buff=0.5)
        self.play(Write(title), run_time=1.5)
        self.wait(1)
        
        # Show the concept first
        concept = MathTex(
            r"\text{Learning} = \text{Error}_{\text{Baseline}} - \text{Error}_{\text{After Training}}",
            font_size=28
        )
        concept.next_to(title, DOWN, buff=0.8)
        self.play(Write(concept), run_time=2.5)
        self.wait(2)
        self.play(FadeOut(concept), run_time=1)
        
        # Create clean bar chart
        axes = Axes(
            x_range=[0, 4, 1],
            y_range=[0, 0.6, 0.1],
            x_length=8,
            y_length=4,
            axis_config={
                "include_numbers": False,
                "stroke_width": 2
            },
            tips=False
        )
        axes.shift(DOWN * 0.5)
        
        y_label = Text("Average Error", font_size=24).next_to(axes.y_axis, LEFT, buff=0.4).rotate(PI/2)
        
        self.play(Create(axes), Write(y_label), run_time=1.5)
        self.wait(1)
        
        # Stage labels
        bl_label = Text("Baseline", font_size=20).move_to(axes.c2p(1, -0.08))
        str_label = Text("Training", font_size=20).move_to(axes.c2p(2, -0.08))
        ltr_label = Text("Retention", font_size=20).move_to(axes.c2p(3, -0.08))
        
        self.play(
            Write(bl_label),
            Write(str_label),
            Write(ltr_label),
            run_time=1.5
        )
        self.wait(1)
        
        # Show Control group bars first
        control_label = Text("Control Group", font_size=24, color="#FF6B6B")
        control_label.to_corner(UL, buff=0.8)
        self.play(Write(control_label), run_time=1)
        
        control_bars = [
            Rectangle(width=0.5, height=0.52*6, fill_color="#FF6B6B", fill_opacity=0.7, stroke_width=2).move_to(axes.c2p(1, 0.26)),
            Rectangle(width=0.5, height=0.36*6, fill_color="#FF6B6B", fill_opacity=0.7, stroke_width=2).move_to(axes.c2p(2, 0.18)),
            Rectangle(width=0.5, height=0.40*6, fill_color="#FF6B6B", fill_opacity=0.7, stroke_width=2).move_to(axes.c2p(3, 0.20))
        ]
        
        for bar in control_bars:
            self.play(GrowFromEdge(bar, DOWN), run_time=1)
            self.wait(0.5)
        
        self.wait(1)
        
        # Show Experimental group bars
        exp_label = Text("Experimental Group", font_size=24, color="#00FF88")
        exp_label.next_to(control_label, DOWN, buff=0.3)
        self.play(Write(exp_label), run_time=1)
        
        exp_bar1 = Rectangle(width=0.5, height=0.51*6, fill_color="#00FF88", fill_opacity=0.7, stroke_width=2)
        exp_bar1.move_to(axes.c2p(1, 0.255)).shift(RIGHT*0.3)
        
        exp_bar2 = Rectangle(width=0.5, height=0.25*6, fill_color="#00FF88", fill_opacity=0.7, stroke_width=2)
        exp_bar2.move_to(axes.c2p(2, 0.125)).shift(RIGHT*0.3)
        
        exp_bar3 = Rectangle(width=0.5, height=0.28*6, fill_color="#00FF88", fill_opacity=0.7, stroke_width=2)
        exp_bar3.move_to(axes.c2p(3, 0.14)).shift(RIGHT*0.3)
        
        exp_bars = [exp_bar1, exp_bar2, exp_bar3]
        
        for bar in exp_bars:
            self.play(GrowFromEdge(bar, DOWN), run_time=1)
            self.wait(0.5)
        
        self.wait(2)
        
        # Highlight the difference in Training stage
        arrow_start = axes.c2p(2, 0.36) + LEFT*0.25
        arrow_end = axes.c2p(2, 0.25) + RIGHT*0.3
        
        arrow = Arrow(
            start=arrow_start,
            end=arrow_end,
            color=YELLOW,
            stroke_width=4,
            buff=0.1
        )
        
        diff_text = Text("Robot assistance\nreduces error", font_size=20, color=YELLOW)
        diff_text.next_to(arrow, RIGHT, buff=0.5)
        
        self.play(GrowArrow(arrow), Write(diff_text), run_time=1.5)
        self.wait(3)
        
        # Clear everything
        self.play(
            FadeOut(VGroup(
                title, axes, y_label,
                bl_label, str_label, ltr_label,
                control_label, exp_label,
                *control_bars, *exp_bars,
                arrow, diff_text
            )),
            run_time=1.5
        )
        self.wait(1)
    
    
    def show_regression(self):
        """Show regression step by step"""
        title = Text("Regression Analysis", font_size=36, color=BLUE)
        title.to_edge(UP, buff=0.5)
        self.play(Write(title), run_time=1.5)
        self.wait(1)
        
        # Explain the model first
        model_text = Text("How do personality traits affect error?", font_size=28)
        model_text.next_to(title, DOWN, buff=0.8)
        self.play(Write(model_text), run_time=2)
        self.wait(2)
        
        model_eq = MathTex(
            r"\text{Error} = \beta_0 + \beta_{\text{FS}} \cdot \text{FreeSpirit} + ...",
            font_size=32
        )
        model_eq.next_to(model_text, DOWN, buff=0.6)
        self.play(Write(model_eq), run_time=2)
        self.wait(2)
        
        self.play(FadeOut(VGroup(model_text, model_eq)), run_time=1)
        
        # Focus on Free Spirit (strongest effect)
        focus_text = Text("Focus: Free Spirit trait", font_size=28, color="#00FF88")
        focus_text.next_to(title, DOWN, buff=0.8)
        self.play(Write(focus_text), run_time=1.5)
        self.wait(1.5)
        self.play(FadeOut(focus_text), run_time=1)
        
        # Create scatter plot
        axes = Axes(
            x_range=[20, 90, 10],
            y_range=[0.1, 0.7, 0.1],
            x_length=8,
            y_length=4.5,
            axis_config={
                "include_numbers": True,
                "font_size": 20,
                "stroke_width": 2
            },
            tips=False
        )
        axes.shift(DOWN * 0.3)
        
        x_label = Text("Free Spirit Score", font_size=24).next_to(axes.x_axis, DOWN, buff=0.4)
        y_label = Text("Error", font_size=24).next_to(axes.y_axis, LEFT, buff=0.4).rotate(PI/2)
        
        self.play(Create(axes), Write(x_label), Write(y_label), run_time=1.5)
        self.wait(1)
        
        # Add data points ONE BY ONE (not all at once)
        np.random.seed(42)
        n_points = 40  # Fewer points for cleaner look
        fs_values = np.random.uniform(25, 85, n_points)
        errors = 0.15 + 0.006 * fs_values + np.random.normal(0, 0.06, n_points)
        errors = np.clip(errors, 0.12, 0.65)
        
        dots = []
        for fs, err in zip(fs_values, errors):
            dot = Dot(
                axes.c2p(fs, err),
                radius=0.06,
                color="#00FF88",
                fill_opacity=0.8
            )
            dots.append(dot)
            self.play(FadeIn(dot, scale=0.5), run_time=0.15)
        
        self.wait(2)
        
        # Show regression line
        reg_line = axes.plot(
            lambda x: 0.15 + 0.006*x,
            x_range=[25, 85],
            color="#FF6B6B",
            stroke_width=4
        )
        
        line_label = Text("Best fit line", font_size=22, color="#FF6B6B")
        line_label.to_corner(UR, buff=0.8)
        
        self.play(Create(reg_line), Write(line_label), run_time=2)
        self.wait(2)
        
        # Show interpretation
        interp = Text("Higher Free Spirit → Higher Error", font_size=26, color=YELLOW)
        interp.to_edge(DOWN, buff=0.6)
        self.play(Write(interp), run_time=2)
        self.wait(3)
        
        # Clear everything
        self.play(
            FadeOut(VGroup(
                title, axes, x_label, y_label,
                *dots, reg_line, line_label, interp
            )),
            run_time=1.5
        )
        self.wait(1)
    
    
    def show_power(self):
        """Show power analysis clearly"""
        title = Text("Statistical Power Analysis", font_size=36, color=BLUE)
        title.to_edge(UP, buff=0.5)
        self.play(Write(title), run_time=1.5)
        self.wait(1)
        
        # Explain Cohen's d first
        concept = Text("Effect Size: Cohen's d", font_size=28)
        concept.next_to(title, DOWN, buff=0.8)
        self.play(Write(concept), run_time=1.5)
        self.wait(1.5)
        
        formula = MathTex(
            r"d = \frac{\text{Difference in means}}{\text{Standard deviation}}",
            font_size=30
        )
        formula.next_to(concept, DOWN, buff=0.6)
        self.play(Write(formula), run_time=2)
        self.wait(2)
        
        self.play(FadeOut(VGroup(concept, formula)), run_time=1)
        
        # Show distributions
        axes = Axes(
            x_range=[-3, 5, 1],
            y_range=[0, 0.5, 0.1],
            x_length=8,
            y_length=4,
            axis_config={"include_numbers": False, "stroke_width": 2},
            tips=False
        )
        axes.shift(DOWN * 0.5)
        
        self.play(Create(axes), run_time=1)
        self.wait(1)
        
        # Control distribution
        control_curve = axes.plot(
            lambda x: 0.4 * np.exp(-x**2/2),
            x_range=[-3, 3],
            color="#FF6B6B",
            stroke_width=4
        )
        control_label = Text("Control", font_size=24, color="#FF6B6B")
        control_label.move_to(axes.c2p(-1.5, 0.45))
        
        self.play(Create(control_curve), Write(control_label), run_time=2)
        self.wait(1.5)
        
        # Experimental distribution
        exp_curve = axes.plot(
            lambda x: 0.4 * np.exp(-(x-1.8)**2/2),
            x_range=[-1, 5],
            color="#00FF88",
            stroke_width=4
        )
        exp_label = Text("Experimental", font_size=24, color="#00FF88")
        exp_label.move_to(axes.c2p(3, 0.45))
        
        self.play(Create(exp_curve), Write(exp_label), run_time=2)
        self.wait(2)
        
        # Show the gap
        arrow = DoubleArrow(
            start=axes.c2p(0, 0.15),
            end=axes.c2p(1.8, 0.15),
            color=YELLOW,
            stroke_width=4,
            buff=0
        )
        gap_label = MathTex(r"d = 1.77", font_size=32, color=YELLOW)
        gap_label.next_to(arrow, DOWN, buff=0.3)
        
        self.play(GrowArrow(arrow), Write(gap_label), run_time=2)
        self.wait(3)
        
        # Clear
        self.play(
            FadeOut(VGroup(
                title, axes,
                control_curve, control_label,
                exp_curve, exp_label,
                arrow, gap_label
            )),
            run_time=1.5
        )
        self.wait(1)
    
    
    def show_correlations(self):
        """Show correlations simply"""
        title = Text("Trait Correlations", font_size=36, color=BLUE)
        title.to_edge(UP, buff=0.5)
        self.play(Write(title), run_time=1.5)
        self.wait(1)
        
        # Show one key correlation
        corr_text = Text("Achiever ↔ Free Spirit", font_size=32)
        corr_text.next_to(title, DOWN, buff=1)
        self.play(Write(corr_text), run_time=1.5)
        self.wait(1)
        
        corr_value = MathTex(r"r = 0.538", font_size=48, color="#00FF88")
        corr_value.next_to(corr_text, DOWN, buff=0.8)
        self.play(Write(corr_value), run_time=1.5)
        self.wait(1.5)
        
        interp = Text("Strong positive relationship", font_size=28, color=YELLOW)
        interp.next_to(corr_value, DOWN, buff=0.8)
        self.play(Write(interp), run_time=1.5)
        self.wait(3)
        
        # Clear
        self.play(
            FadeOut(VGroup(title, corr_text, corr_value, interp)),
            run_time=1.5
        )
        self.wait(1)
    
    
    def conclusion(self):
        """Simple conclusion"""
        title = Text("Key Findings", font_size=40, color=BLUE)
        self.play(Write(title), run_time=1.5)
        self.wait(1.5)
        
        findings = VGroup(
            Text("1. Robot assistance improves learning", font_size=28),
            Text("2. Personality traits affect performance", font_size=28),
            Text("3. Effects are statistically robust", font_size=28)
        ).arrange(DOWN, buff=0.6, aligned_edge=LEFT)
        findings.next_to(title, DOWN, buff=1)
        
        for finding in findings:
            self.play(FadeIn(finding, shift=UP*0.3), run_time=1.5)
            self.wait(2)
        
        self.wait(3)
        
        self.play(FadeOut(VGroup(title, findings)), run_time=2)
        self.wait(1)


# Render command:
# manim -pqh clean_animation.py CleanStatisticalAnalysis --format=mp4
