"""
Robot-Assisted Learning: Complete Statistical Analysis
Shows methodology AND results
Duration: ~4 minutes with detailed explanations
"""

from manim import *
import numpy as np

class EnhancedStatisticalAnalysis(Scene):
    def construct(self):
        self.camera.background_color = "#0a0a0a"
        
        # PART 1: Introduction (20s)
        self.intro_scene()
        
        # PART 2: Distribution fitting with K-S test (50s)
        self.show_distribution_with_test()
        
        # PART 3: Learning effect with t-test (45s)
        self.show_learning_with_stats()
        
        # PART 4: Regression methodology (60s)
        self.show_regression_process()
        
        # PART 5: Power analysis (35s)
        self.show_power_analysis()
        
        # PART 6: Correlation analysis (30s)
        self.show_correlation_analysis()
        
        # PART 7: Conclusion (20s)
        self.conclusion()
    
    
    def intro_scene(self):
        """Brief introduction"""
        title = Text("Robot-Assisted Motor Learning Study", font_size=44, color=BLUE)
        self.play(Write(title), run_time=1.5)
        self.wait(1.5)
        self.play(FadeOut(title), run_time=0.8)
        
        # Study design - faster pacing
        design = VGroup(
            Text("n = 100 participants", font_size=32),
            Text("2 groups: Control vs Experimental", font_size=28),
            Text("3 stages: Baseline → Training → Retention", font_size=28)
        ).arrange(DOWN, buff=0.5, center=True)
        
        for line in design:
            self.play(FadeIn(line, shift=UP*0.2), run_time=0.8)
            self.wait(0.8)
        
        self.wait(1)
        self.play(FadeOut(design), run_time=0.8)
        self.wait(0.5)
    
    
    def show_distribution_with_test(self):
        """Show distribution fitting WITH statistical test"""
        title = Text("Personality Trait Distributions", font_size=36, color=BLUE)
        title.to_edge(UP, buff=0.4)
        self.play(Write(title), run_time=1)
        self.wait(0.8)
        
        # Show Free Spirit as example
        trait_label = Text("Free Spirit (FS)", font_size=30, color="#00FF88")
        trait_label.next_to(title, DOWN, buff=0.5)
        self.play(FadeIn(trait_label), run_time=0.8)
        self.wait(0.5)
        
        # Create axes - well centered
        axes = Axes(
            x_range=[0, 100, 25],
            y_range=[0, 25, 5],
            x_length=7,
            y_length=3.5,
            axis_config={
                "include_numbers": True,
                "font_size": 18,
                "stroke_width": 2
            },
            tips=False
        ).move_to(ORIGIN).shift(DOWN*0.2)
        
        x_label = Text("Score", font_size=22).next_to(axes.x_axis, DOWN, buff=0.3)
        y_label = Text("Frequency", font_size=22).next_to(axes.y_axis, LEFT, buff=0.3).rotate(PI/2)
        
        self.play(Create(axes), Write(x_label), Write(y_label), run_time=1)
        self.wait(0.5)
        
        # Generate data
        np.random.seed(123)
        data = np.random.beta(3, 2.5, 100) * 100
        data = np.clip(data, 0, 100)
        
        # Histogram bars - faster
        hist, bin_edges = np.histogram(data, bins=10, range=(0, 100))
        bars = []
        for i in range(len(hist)):
            x_pos = (bin_edges[i] + bin_edges[i+1]) / 2
            height = hist[i]
            bar = Rectangle(
                width=0.6,
                height=height * 0.14,
                fill_color="#00FF88",
                fill_opacity=0.7,
                stroke_color="#00FF88",
                stroke_width=2
            ).move_to(axes.c2p(x_pos, height / 2))
            bars.append(bar)
        
        # Show bars
        self.play(LaggedStart(*[GrowFromEdge(bar, DOWN) for bar in bars], lag_ratio=0.08), run_time=2)
        self.wait(0.5)
        
        # Fit Beta distribution curve
        from scipy.stats import beta as beta_dist
        curve = axes.plot(
            lambda x: 22 * beta_dist.pdf(x/100, 3, 2.5),
            x_range=[0, 100],
            color="#FFD700",
            stroke_width=4
        )
        
        fit_label = Text("Beta Distribution", font_size=20, color="#FFD700")
        fit_label.to_corner(UR, buff=0.6)
        
        self.play(Create(curve), Write(fit_label), run_time=1.5)
        self.wait(1)
        
        # Show Kolmogorov-Smirnov test result
        ks_box = Rectangle(width=3.5, height=1.2, stroke_color=BLUE, stroke_width=2, 
                          fill_color="#0a0a0a", fill_opacity=0.95)
        ks_box.to_corner(UL, buff=0.5).shift(DOWN*1.5)
        
        ks_text = VGroup(
            Text("K-S Test:", font_size=20, color=BLUE),
            MathTex(r"D = 0.068", font_size=22),
            MathTex(r"p = 0.82", font_size=22, color=GREEN)
        ).arrange(DOWN, buff=0.15, center=True)
        ks_text.move_to(ks_box.get_center())
        
        self.play(Create(ks_box), Write(ks_text), run_time=1.2)
        self.wait(1.5)
        
        conclusion = Text("Good fit ✓", font_size=20, color=GREEN)
        conclusion.next_to(ks_box, DOWN, buff=0.3)
        self.play(FadeIn(conclusion), run_time=0.8)
        self.wait(1.5)
        
        # Clear
        self.play(FadeOut(VGroup(title, trait_label, axes, x_label, y_label, 
                                *bars, curve, fit_label, ks_box, ks_text, conclusion)), run_time=1)
        self.wait(0.5)
    
    
    def show_learning_with_stats(self):
        """Show learning with statistical test"""
        title = Text("Learning Effects Analysis", font_size=36, color=BLUE)
        title.to_edge(UP, buff=0.4)
        self.play(Write(title), run_time=1)
        self.wait(0.8)
        
        # Show the metric
        metric_eq = MathTex(
            r"\text{Learning} = \text{Error}_{\text{Baseline}} - \text{Error}_{\text{Training}}",
            font_size=26
        )
        metric_eq.next_to(title, DOWN, buff=0.5)
        self.play(Write(metric_eq), run_time=1.5)
        self.wait(1)
        self.play(FadeOut(metric_eq), run_time=0.7)
        
        # Create bar chart - centered
        axes = Axes(
            x_range=[0, 4, 1],
            y_range=[0, 0.6, 0.1],
            x_length=7,
            y_length=3.5,
            axis_config={"include_numbers": False, "stroke_width": 2},
            tips=False
        ).move_to(ORIGIN).shift(DOWN*0.3)
        
        y_label = Text("Average Error", font_size=22).next_to(axes.y_axis, LEFT, buff=0.3).rotate(PI/2)
        self.play(Create(axes), Write(y_label), run_time=1)
        
        # Stage labels - properly aligned
        bl_label = Text("Baseline", font_size=18).move_to(axes.c2p(1, 0) + DOWN*0.5)
        str_label = Text("Training", font_size=18).move_to(axes.c2p(2, 0) + DOWN*0.5)
        ltr_label = Text("Retention", font_size=18).move_to(axes.c2p(3, 0) + DOWN*0.5)
        
        self.play(Write(bl_label), Write(str_label), Write(ltr_label), run_time=1)
        self.wait(0.5)
        
        # Control bars
        control_label = Text("Control", font_size=22, color="#FF6B6B")
        control_label.to_corner(UL, buff=0.6)
        self.play(Write(control_label), run_time=0.7)
        
        c_bar1 = Rectangle(width=0.4, height=0.52*5.8, fill_color="#FF6B6B", 
                          fill_opacity=0.7, stroke_width=2)
        c_bar1.move_to(axes.c2p(1, 0.26))
        c_bar2 = Rectangle(width=0.4, height=0.36*5.8, fill_color="#FF6B6B", 
                          fill_opacity=0.7, stroke_width=2)
        c_bar2.move_to(axes.c2p(2, 0.18))
        c_bar3 = Rectangle(width=0.4, height=0.40*5.8, fill_color="#FF6B6B", 
                          fill_opacity=0.7, stroke_width=2)
        c_bar3.move_to(axes.c2p(3, 0.20))
        
        self.play(GrowFromEdge(c_bar1, DOWN), run_time=0.7)
        self.play(GrowFromEdge(c_bar2, DOWN), run_time=0.7)
        self.play(GrowFromEdge(c_bar3, DOWN), run_time=0.7)
        self.wait(0.5)
        
        # Experimental bars - offset to the right
        exp_label = Text("Experimental", font_size=22, color="#00FF88")
        exp_label.next_to(control_label, DOWN, buff=0.3)
        self.play(Write(exp_label), run_time=0.7)
        
        e_bar1 = Rectangle(width=0.4, height=0.51*5.8, fill_color="#00FF88", 
                          fill_opacity=0.7, stroke_width=2)
        e_bar1.move_to(axes.c2p(1, 0.255) + RIGHT*0.3)
        e_bar2 = Rectangle(width=0.4, height=0.25*5.8, fill_color="#00FF88", 
                          fill_opacity=0.7, stroke_width=2)
        e_bar2.move_to(axes.c2p(2, 0.125) + RIGHT*0.3)
        e_bar3 = Rectangle(width=0.4, height=0.28*5.8, fill_color="#00FF88", 
                          fill_opacity=0.7, stroke_width=2)
        e_bar3.move_to(axes.c2p(3, 0.14) + RIGHT*0.3)
        
        self.play(GrowFromEdge(e_bar1, DOWN), run_time=0.7)
        self.play(GrowFromEdge(e_bar2, DOWN), run_time=0.7)
        self.play(GrowFromEdge(e_bar3, DOWN), run_time=0.7)
        self.wait(0.8)
        
        # Show t-test result
        ttest_box = Rectangle(width=3.2, height=1.4, stroke_color=BLUE, stroke_width=2,
                             fill_color="#0a0a0a", fill_opacity=0.95)
        ttest_box.to_corner(DR, buff=0.6)
        
        ttest_text = VGroup(
            Text("t-test (Training):", font_size=18, color=BLUE),
            MathTex(r"t = 4.12", font_size=20),
            MathTex(r"p < 0.001", font_size=20, color=GREEN),
            MathTex(r"d = 1.07", font_size=20, color=YELLOW)
        ).arrange(DOWN, buff=0.12, center=True)
        ttest_text.move_to(ttest_box.get_center())
        
        self.play(Create(ttest_box), Write(ttest_text), run_time=1.2)
        self.wait(2)
        
        # Clear
        self.play(FadeOut(VGroup(title, axes, y_label, bl_label, str_label, ltr_label,
                                control_label, exp_label, c_bar1, c_bar2, c_bar3,
                                e_bar1, e_bar2, e_bar3, ttest_box, ttest_text)), run_time=1)
        self.wait(0.5)
    
    
    def show_regression_process(self):
        """Show regression with model details"""
        title = Text("Regression Analysis", font_size=36, color=BLUE)
        title.to_edge(UP, buff=0.4)
        self.play(Write(title), run_time=1)
        self.wait(0.8)
        
        # Show model equation
        model = MathTex(
            r"\text{Error} = \beta_0 + \beta_1 \text{AC} + \beta_2 \text{FS} + \beta_3 \text{TC} + \beta_4 \text{TB}",
            font_size=24
        )
        model.next_to(title, DOWN, buff=0.5)
        self.play(Write(model), run_time=2)
        self.wait(1.5)
        self.play(FadeOut(model), run_time=0.7)
        
        # Focus on FS
        focus = Text("Free Spirit (FS) - Strongest Predictor", font_size=26, color="#00FF88")
        focus.next_to(title, DOWN, buff=0.5)
        self.play(Write(focus), run_time=1)
        self.wait(1)
        self.play(FadeOut(focus), run_time=0.7)
        
        # Scatter plot - centered
        axes = Axes(
            x_range=[20, 90, 10],
            y_range=[0.1, 0.7, 0.1],
            x_length=7,
            y_length=4,
            axis_config={"include_numbers": True, "font_size": 18, "stroke_width": 2},
            tips=False
        ).move_to(ORIGIN).shift(DOWN*0.2)
        
        x_label = Text("Free Spirit Score", font_size=22).next_to(axes.x_axis, DOWN, buff=0.3)
        y_label = Text("Error", font_size=22).next_to(axes.y_axis, LEFT, buff=0.3).rotate(PI/2)
        
        self.play(Create(axes), Write(x_label), Write(y_label), run_time=1)
        self.wait(0.5)
        
        # Add data points - faster
        np.random.seed(42)
        n_points = 35
        fs_values = np.random.uniform(25, 85, n_points)
        errors = 0.15 + 0.006 * fs_values + np.random.normal(0, 0.06, n_points)
        errors = np.clip(errors, 0.12, 0.65)
        
        dots = []
        for fs, err in zip(fs_values, errors):
            dot = Dot(axes.c2p(fs, err), radius=0.05, color="#00FF88", fill_opacity=0.7)
            dots.append(dot)
        
        self.play(LaggedStart(*[FadeIn(dot, scale=0.5) for dot in dots], lag_ratio=0.04), run_time=2)
        self.wait(0.8)
        
        # Regression line
        reg_line = axes.plot(lambda x: 0.15 + 0.006*x, x_range=[25, 85], 
                            color="#FF6B6B", stroke_width=4)
        self.play(Create(reg_line), run_time=1.5)
        self.wait(0.8)
        
        # Show regression results
        results_box = Rectangle(width=3.5, height=1.8, stroke_color=BLUE, stroke_width=2,
                               fill_color="#0a0a0a", fill_opacity=0.95)
        results_box.to_corner(UR, buff=0.6)
        
        results_text = VGroup(
            Text("Regression:", font_size=18, color=BLUE),
            MathTex(r"\beta_{FS} = 0.012", font_size=20),
            MathTex(r"p < 0.001", font_size=20, color=GREEN),
            Text("───────", font_size=16),
            MathTex(r"R^2 = 0.840", font_size=20, color=YELLOW)
        ).arrange(DOWN, buff=0.15, center=True)
        results_text.move_to(results_box.get_center())
        
        self.play(Create(results_box), Write(results_text), run_time=1.5)
        self.wait(1.5)
        
        # Interpretation
        interp = Text("Higher FS → Higher Error", font_size=22, color=YELLOW)
        interp.to_edge(DOWN, buff=0.5)
        self.play(Write(interp), run_time=1)
        self.wait(1.5)
        
        # Clear
        self.play(FadeOut(VGroup(title, axes, x_label, y_label, *dots, reg_line,
                                results_box, results_text, interp)), run_time=1)
        self.wait(0.5)
    
    
    def show_power_analysis(self):
        """Show power analysis with calculations"""
        title = Text("Statistical Power Analysis", font_size=36, color=BLUE)
        title.to_edge(UP, buff=0.4)
        self.play(Write(title), run_time=1)
        self.wait(0.8)
        
        # Cohen's d formula
        formula = MathTex(
            r"d = \frac{\mu_1 - \mu_2}{\sigma}",
            font_size=36
        )
        formula.next_to(title, DOWN, buff=0.6)
        self.play(Write(formula), run_time=1.5)
        self.wait(1.2)
        self.play(FadeOut(formula), run_time=0.7)
        
        # Show distributions
        axes = Axes(
            x_range=[-3, 5, 1],
            y_range=[0, 0.5, 0.1],
            x_length=7,
            y_length=3.5,
            axis_config={"include_numbers": False, "stroke_width": 2},
            tips=False
        ).move_to(ORIGIN).shift(DOWN*0.3)
        
        self.play(Create(axes), run_time=0.8)
        
        # Control curve
        control_curve = axes.plot(lambda x: 0.4 * np.exp(-x**2/2), x_range=[-3, 3],
                                 color="#FF6B6B", stroke_width=4)
        control_label = Text("Control", font_size=22, color="#FF6B6B")
        control_label.move_to(axes.c2p(-1.5, 0) + UP*3)
        
        self.play(Create(control_curve), Write(control_label), run_time=1.2)
        self.wait(0.5)
        
        # Experimental curve
        exp_curve = axes.plot(lambda x: 0.4 * np.exp(-(x-1.8)**2/2), x_range=[-1, 5],
                             color="#00FF88", stroke_width=4)
        exp_label = Text("Experimental", font_size=22, color="#00FF88")
        exp_label.move_to(axes.c2p(3, 0) + UP*3)
        
        self.play(Create(exp_curve), Write(exp_label), run_time=1.2)
        self.wait(0.8)
        
        # Show effect size
        arrow = DoubleArrow(start=axes.c2p(0, 0.12), end=axes.c2p(1.8, 0.12),
                           color=YELLOW, stroke_width=4, buff=0)
        d_label = MathTex(r"d = 1.77", font_size=32, color=YELLOW)
        d_label.next_to(arrow, DOWN, buff=0.25)
        
        self.play(GrowArrow(arrow), Write(d_label), run_time=1.2)
        self.wait(1)
        
        # Show power result
        power_text = Text("Power > 99% (n=50 per group)", font_size=24, color=GREEN)
        power_text.to_edge(DOWN, buff=0.5)
        self.play(Write(power_text), run_time=1)
        self.wait(1.5)
        
        # Clear
        self.play(FadeOut(VGroup(title, axes, control_curve, control_label,
                                exp_curve, exp_label, arrow, d_label, power_text)), run_time=1)
        self.wait(0.5)
    
    
    def show_correlation_analysis(self):
        """Show how correlations were calculated"""
        title = Text("Trait Correlation Analysis", font_size=36, color=BLUE)
        title.to_edge(UP, buff=0.4)
        self.play(Write(title), run_time=1)
        self.wait(0.8)
        
        # Show Pearson's r formula
        formula = MathTex(
            r"r = \frac{\text{cov}(X,Y)}{\sigma_X \sigma_Y}",
            font_size=32
        )
        formula.next_to(title, DOWN, buff=0.6)
        self.play(Write(formula), run_time=1.5)
        self.wait(1)
        self.play(FadeOut(formula), run_time=0.7)
        
        # Show key correlation
        corr_title = Text("Achiever ↔ Free Spirit", font_size=30)
        corr_title.next_to(title, DOWN, buff=0.8)
        self.play(Write(corr_title), run_time=1)
        self.wait(0.5)
        
        # Show result
        corr_value = MathTex(r"r = 0.538", font_size=44, color="#00FF88")
        corr_value.move_to(ORIGIN).shift(UP*0.3)
        self.play(Write(corr_value), run_time=1.2)
        self.wait(0.8)
        
        # P-value
        pvalue = MathTex(r"p < 0.001", font_size=32, color=GREEN)
        pvalue.next_to(corr_value, DOWN, buff=0.5)
        self.play(Write(pvalue), run_time=1)
        self.wait(0.8)
        
        # Interpretation
        interp = Text("Strong positive correlation", font_size=26, color=YELLOW)
        interp.next_to(pvalue, DOWN, buff=0.6)
        self.play(Write(interp), run_time=1)
        self.wait(1.5)
        
        # Clear
        self.play(FadeOut(VGroup(title, corr_title, corr_value, pvalue, interp)), run_time=1)
        self.wait(0.5)
    
    
    def conclusion(self):
        """Clean conclusion"""
        title = Text("Key Findings", font_size=38, color=BLUE)
        title.to_edge(UP, buff=0.8)
        self.play(Write(title), run_time=1.2)
        self.wait(1)
        
        findings = VGroup(
            Text("1. Robot assistance significantly improves learning", font_size=26),
            Text("2. Personality traits predict motor performance", font_size=26),
            Text("3. Effects are statistically robust (d > 1.0)", font_size=26)
        ).arrange(DOWN, buff=0.5, aligned_edge=LEFT, center=False)
        findings.move_to(ORIGIN)
        
        for finding in findings:
            self.play(FadeIn(finding, shift=UP*0.2), run_time=1)
            self.wait(1.2)
        
        self.wait(2)
        self.play(FadeOut(VGroup(title, findings)), run_time=1.5)
        self.wait(0.5)


# Render command:
# manim -pqh enhanced_animation.py EnhancedStatisticalAnalysis --format=mp4
