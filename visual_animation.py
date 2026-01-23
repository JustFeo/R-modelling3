"""
Robot-Assisted Learning: Pure Visual Statistical Analysis
Minimal text, maximum mathematical visualizations
Duration: ~4 minutes with detailed visual explanations
"""

from manim import *
import numpy as np

class VisualStatisticalAnalysis(Scene):
    def construct(self):
        self.camera.background_color = "#0a0a0a"
        
        # PART 1: Data Introduction - Visual only (30s)
        self.visual_data_intro()
        
        # PART 2: Personality Distributions - Animated fitting (50s)
        self.animated_distribution_fitting()
        
        # PART 3: Learning Effects - Dynamic visualization (55s)
        self.learning_dynamics()
        
        # PART 4: Regression - Full scatter and fitting process (60s)
        self.regression_visual_fitting()
        
        # PART 5: Power Analysis - Visual distributions (35s)
        self.power_visual()
        
        # PART 6: Correlation Network (30s)
        self.correlation_network()
        
        # PART 7: Final visualization (20s)
        self.final_visual_summary()
    
    
    def visual_data_intro(self):
        """Pure visual data structure - no text walls"""
        # Title appears briefly
        title = Text("Robot-Assisted Motor Learning", font_size=40, color=BLUE)
        subtitle = MathTex(r"n=100, \quad 3 \text{ stages}, \quad 4 \text{ traits}", font_size=28)
        subtitle.next_to(title, DOWN, buff=0.3)
        
        self.play(Write(title), run_time=1)
        self.play(FadeIn(subtitle), run_time=0.7)
        self.wait(0.5)
        self.play(FadeOut(VGroup(title, subtitle)))
        
        # Visual representation: 100 dots representing participants
        dots = VGroup()
        for i in range(100):
            row = i // 10
            col = i % 10
            color = "#FF6B6B" if i < 50 else "#00FF88"  # Control vs Experimental
            dot = Dot(point=LEFT*4.5 + RIGHT*col*0.9 + UP*2.5 + DOWN*row*0.5, 
                     radius=0.15, color=color, fill_opacity=0.8)
            dots.add(dot)
        
        self.play(LaggedStart(*[GrowFromCenter(dot) for dot in dots], lag_ratio=0.01), run_time=2)
        self.wait(0.5)
        
        # Split into two groups with animation
        control_target = LEFT * 3
        experimental_target = RIGHT * 3
        
        animations = []
        for i, dot in enumerate(dots):
            if i < 50:
                animations.append(dot.animate.move_to(control_target + UP*(i//10)*0.4 + RIGHT*(i%10)*0.15))
            else:
                animations.append(dot.animate.move_to(experimental_target + UP*((i-50)//10)*0.4 + RIGHT*((i-50)%10)*0.15))
        
        self.play(*animations, run_time=1.5)
        
        # Label groups
        c_label = MathTex("C", font_size=48, color="#FF6B6B").next_to(control_target, DOWN, buff=1)
        e_label = MathTex("E", font_size=48, color="#00FF88").next_to(experimental_target, DOWN, buff=1)
        self.play(Write(c_label), Write(e_label), run_time=0.8)
        self.wait(1)
        
        # Show stages progression with arrows
        stages = MathTex(r"BL \rightarrow STR \rightarrow LTR", font_size=40).to_edge(DOWN, buff=1)
        self.play(Write(stages), run_time=1.5)
        self.wait(1)
        
        self.play(FadeOut(VGroup(dots, c_label, e_label, stages)))
        self.wait(0.3)
    
    
    def animated_distribution_fitting(self):
        """Show distribution fitting process visually"""
        title = Text("Personality Traits", font_size=32, color=BLUE).to_edge(UP, buff=0.3)
        self.play(Write(title), run_time=0.8)
        
        trait_data = {
            "AC": {"color": "#FFD700", "type": "Weibull", "k": 1.5, "scale": 50},
            "FS": {"color": "#00FF88", "type": "Beta", "alpha": 3, "beta": 2.5},
            "TC": {"color": "#4A9EFF", "type": "Beta", "alpha": 2.5, "beta": 2},
            "TB": {"color": "#FF6B9D", "type": "Normal", "mean": 50, "std": 20}
        }
        
        positions = [UL, UR, DL, DR]
        
        for (trait_name, trait_info), pos in zip(trait_data.items(), positions):
            self.show_distribution_fitting(trait_name, trait_info, pos)
        
        self.wait(1)
        self.play(FadeOut(title))
    
    
    def show_distribution_fitting(self, name, info, position):
        """Animate fitting a distribution to data"""
        axes = Axes(
            x_range=[0, 100, 25],
            y_range=[0, 30, 10],
            x_length=3,
            y_length=2,
            axis_config={"include_numbers": False, "stroke_width": 2},
            tips=False
        ).scale(0.7).move_to(position * 2.5)
        
        # Generate histogram data
        np.random.seed(hash(name) % 1000)
        if info["type"] == "Weibull":
            data = np.random.weibull(info["k"], 100) * info["scale"]
        elif info["type"] == "Beta":
            data = np.random.beta(info["alpha"], info["beta"], 100) * 100
        else:  # Normal
            data = np.random.normal(info["mean"], info["std"], 100)
        
        data = np.clip(data, 0, 100)
        
        # Create histogram bars
        hist, bin_edges = np.histogram(data, bins=8, range=(0, 100))
        bars = VGroup()
        for i, h in enumerate(hist):
            x_pos = bin_edges[i] + (bin_edges[i+1] - bin_edges[i])/2
            bar = Rectangle(
                width=0.3,
                height=h * 0.065,
                fill_color=info["color"],
                fill_opacity=0.6,
                stroke_width=1
            ).move_to(axes.c2p(x_pos, h/2))
            bars.add(bar)
        
        # Label
        label = Text(name, font_size=24, color=info["color"], weight=BOLD).next_to(axes, UP, buff=0.1)
        
        # Animate histogram appearing
        self.play(Create(axes), Write(label), run_time=0.5)
        self.play(LaggedStart(*[GrowFromEdge(bar, DOWN) for bar in bars], lag_ratio=0.05), run_time=0.8)
        
        # Fit curve
        if info["type"] == "Weibull":
            curve_func = lambda x: 25 * (info["k"]/info["scale"]) * ((x/info["scale"])**(info["k"]-1)) * np.exp(-((x/info["scale"])**info["k"]))
        elif info["type"] == "Beta":
            from scipy.stats import beta as beta_dist
            curve_func = lambda x: 25 * beta_dist.pdf(x/100, info["alpha"], info["beta"])
        else:  # Normal
            curve_func = lambda x: 25 * np.exp(-((x-info["mean"])**2)/(2*info["std"]**2)) / (info["std"]*np.sqrt(2*np.pi)) * info["std"]*2.5
        
        curve = axes.plot(curve_func, x_range=[0, 100], color=info["color"], stroke_width=3)
        
        # Distribution name
        dist_name = MathTex(info["type"], font_size=16, color="#888").next_to(label, DOWN, buff=0.05)
        
        self.play(Create(curve), Write(dist_name), run_time=1)
    
    
    def learning_dynamics(self):
        """Show learning as dynamic process with continuous error reduction"""
        title = Text("Learning Effects", font_size=32, color=BLUE).to_edge(UP, buff=0.3)
        self.play(Write(title), run_time=0.7)
        
        # Main formula
        formula = MathTex(
            r"\text{Error}_{i,s,t} = f(\text{Stage}, \text{Group}, \text{Trial})",
            font_size=28
        ).next_to(title, DOWN, buff=0.3)
        self.play(Write(formula), run_time=1.5)
        self.wait(0.5)
        
        # Create 3D-style plot showing error over time for both groups
        axes = Axes(
            x_range=[0, 120, 40],
            y_range=[0, 0.7, 0.1],
            x_length=8,
            y_length=4,
            axis_config={"include_numbers": True, "font_size": 16},
            tips=False
        ).shift(DOWN*0.5)
        
        x_label = Text("Trial Number", font_size=20).next_to(axes.x_axis, DOWN, buff=0.3)
        y_label = Text("Error", font_size=20).next_to(axes.y_axis, LEFT, buff=0.3).rotate(PI/2)
        
        self.play(FadeOut(formula))
        self.play(Create(axes), Write(x_label), Write(y_label), run_time=1)
        
        # Vertical lines marking stage boundaries
        bl_line = DashedLine(axes.c2p(40, 0), axes.c2p(40, 0.7), color=GREY, stroke_width=2)
        str_line = DashedLine(axes.c2p(80, 0), axes.c2p(80, 0.7), color=GREY, stroke_width=2)
        
        bl_label = Text("BL", font_size=16, color="#FF6B6B").next_to(axes.c2p(20, 0.65), UP, buff=0.1)
        str_label = Text("STR", font_size=16, color="#4ECDC4").next_to(axes.c2p(60, 0.65), UP, buff=0.1)
        ltr_label = Text("LTR", font_size=16, color="#95E1D3").next_to(axes.c2p(100, 0.65), UP, buff=0.1)
        
        self.play(Create(bl_line), Create(str_line), 
                 Write(bl_label), Write(str_label), Write(ltr_label), run_time=0.7)
        
        # Control group trajectory (slower learning)
        control_points = []
        for i in range(121):
            if i < 40:  # BL
                error = 0.52 + np.random.normal(0, 0.03)
            elif i < 80:  # STR
                progress = (i-40)/40
                error = 0.52 - progress * 0.16 + np.random.normal(0, 0.025)
            else:  # LTR
                progress = (i-80)/40
                error = 0.36 - progress * 0.04 + 0.04 + np.random.normal(0, 0.025)
            control_points.append(axes.c2p(i, max(0.1, error)))
        
        # Experimental group trajectory (faster learning)
        exp_points = []
        for i in range(121):
            if i < 40:  # BL
                error = 0.51 + np.random.normal(0, 0.03)
            elif i < 80:  # STR
                progress = (i-40)/40
                error = 0.51 - progress * 0.26 + np.random.normal(0, 0.02)
            else:  # LTR
                progress = (i-80)/40
                error = 0.25 - progress * 0.03 + 0.03 + np.random.normal(0, 0.02)
            exp_points.append(axes.c2p(i, max(0.1, error)))
        
        # Create lines
        control_line = VMobject(color="#FF6B6B", stroke_width=3)
        control_line.set_points_smoothly(control_points)
        
        exp_line = VMobject(color="#00FF88", stroke_width=3)
        exp_line.set_points_smoothly(exp_points)
        
        # Animate drawing the trajectories
        self.play(Create(control_line), run_time=3)
        self.play(Create(exp_line), run_time=3)
        self.wait(0.5)
        
        # Highlight the gap in STR and LTR
        gap_str = DoubleArrow(
            start=axes.c2p(60, 0.36),
            end=axes.c2p(60, 0.25),
            color=YELLOW,
            stroke_width=3,
            buff=0
        )
        gap_ltr = DoubleArrow(
            start=axes.c2p(100, 0.40),
            end=axes.c2p(100, 0.28),
            color=YELLOW,
            stroke_width=3,
            buff=0
        )
        
        self.play(GrowArrow(gap_str), run_time=0.7)
        self.wait(0.3)
        self.play(GrowArrow(gap_ltr), run_time=0.7)
        self.wait(1)
        
        self.play(FadeOut(VGroup(title, axes, x_label, y_label, bl_line, str_line,
                                bl_label, str_label, ltr_label, control_line, exp_line,
                                gap_str, gap_ltr)))
        self.wait(0.3)
    
    
    def regression_visual_fitting(self):
        """Show regression fitting process visually with residuals"""
        title = Text("Regression Analysis", font_size=32, color=BLUE).to_edge(UP, buff=0.3)
        self.play(Write(title), run_time=0.7)
        
        # Model equation
        model = MathTex(
            r"\text{Error} = \beta_0 + \beta_1 AC + \beta_2 FS + \beta_3 TC + \beta_4 TB + \beta_5 Group + \ldots",
            font_size=24
        ).next_to(title, DOWN, buff=0.3)
        self.play(Write(model), run_time=1.5)
        self.wait(0.5)
        
        # Focus on FS (strongest effect)
        focus = MathTex(r"\text{Focus: Free Spirit (FS)}", font_size=24, color="#00FF88").next_to(model, DOWN, buff=0.3)
        self.play(Write(focus), run_time=0.7)
        self.wait(0.5)
        
        # Create larger scatter plot
        axes = Axes(
            x_range=[20, 90, 10],
            y_range=[0.1, 0.7, 0.1],
            x_length=7,
            y_length=5,
            axis_config={"include_numbers": True, "font_size": 18},
            tips=False
        ).shift(DOWN*0.3)
        
        x_label = Text("Free Spirit Score", font_size=22).next_to(axes.x_axis, DOWN, buff=0.4)
        y_label = Text("Average Error", font_size=22).next_to(axes.y_axis, LEFT, buff=0.4).rotate(PI/2)
        
        self.play(FadeOut(VGroup(model, focus)))
        self.play(Create(axes), Write(x_label), Write(y_label), run_time=1)
        
        # Generate scatter data with realistic correlation
        np.random.seed(42)
        n_points = 60
        fs_values = np.random.uniform(25, 85, n_points)
        errors = 0.15 + 0.006 * fs_values + np.random.normal(0, 0.06, n_points)
        errors = np.clip(errors, 0.12, 0.65)
        
        # Create dots
        dots = VGroup()
        for fs, err in zip(fs_values, errors):
            dot = Dot(axes.c2p(fs, err), radius=0.05, color="#00FF88", fill_opacity=0.6)
            dots.add(dot)
        
        # Animate dots appearing with slight delay
        self.play(LaggedStart(*[FadeIn(dot, scale=0.5) for dot in dots], lag_ratio=0.02), run_time=2)
        self.wait(0.5)
        
        # Show regression fitting process
        # Start with a bad fit line
        bad_line = axes.plot(lambda x: 0.35, x_range=[25, 85], color=RED, stroke_width=2)
        self.play(Create(bad_line), run_time=0.7)
        self.wait(0.3)
        
        # Show it improving
        better_line = axes.plot(lambda x: 0.25 + 0.003*x, x_range=[25, 85], color=YELLOW, stroke_width=2)
        self.play(Transform(bad_line, better_line), run_time=1)
        self.wait(0.3)
        
        # Final best fit
        best_line = axes.plot(lambda x: 0.15 + 0.006*x, x_range=[25, 85], color="#FF6B6B", stroke_width=3)
        self.play(Transform(bad_line, best_line), run_time=1)
        self.wait(0.5)
        
        # Show some residuals
        residual_lines = VGroup()
        for i in range(0, len(dots), 3):  # Show every 3rd residual
            fs = fs_values[i]
            err = errors[i]
            predicted = 0.15 + 0.006*fs
            residual = Line(
                axes.c2p(fs, err),
                axes.c2p(fs, predicted),
                color=GREY,
                stroke_width=1.5
            )
            residual_lines.add(residual)
        
        self.play(LaggedStart(*[Create(line) for line in residual_lines], lag_ratio=0.05), run_time=1.5)
        self.wait(0.5)
        
        # Show R² value
        r2_box = Rectangle(width=2.5, height=0.8, stroke_color="#00FF88", stroke_width=2,
                          fill_color="#0a0a0a", fill_opacity=0.9).to_corner(UR, buff=0.5)
        r2_text = MathTex(r"R^2 = 0.84", font_size=28, color=GREEN).move_to(r2_box)
        
        self.play(Create(r2_box), Write(r2_text), run_time=1)
        self.wait(1)
        
        self.play(FadeOut(VGroup(title, axes, x_label, y_label, dots, bad_line, 
                                residual_lines, r2_box, r2_text)))
        self.wait(0.3)
    
    
    def power_visual(self):
        """Visual representation of statistical power"""
        title = Text("Statistical Power", font_size=32, color=BLUE).to_edge(UP, buff=0.3)
        self.play(Write(title), run_time=0.7)
        
        # Cohen's d formula
        formula = MathTex(
            r"d = \frac{\mu_1 - \mu_2}{\sigma_{pooled}}",
            font_size=40
        ).next_to(title, DOWN, buff=0.4)
        self.play(Write(formula), run_time=1.3)
        self.wait(0.5)
        
        # Show two overlapping distributions
        axes = Axes(
            x_range=[-4, 5, 1],
            y_range=[0, 0.45, 0.1],
            x_length=8,
            y_length=3.5,
            axis_config={"include_numbers": False},
            tips=False
        ).shift(DOWN*0.2)
        
        self.play(FadeOut(formula))
        self.play(Create(axes), run_time=0.7)
        
        # Control distribution
        control_dist = axes.plot(
            lambda x: 0.4 * np.exp(-x**2/2),
            x_range=[-4, 4],
            color="#FF6B6B",
            stroke_width=3
        )
        control_area = axes.get_area(
            control_dist,
            x_range=[-4, 4],
            color="#FF6B6B",
            opacity=0.3
        )
        control_label = MathTex(r"\mu_C", font_size=28, color="#FF6B6B").next_to(axes.c2p(0, 0.35), UP)
        
        self.play(Create(control_dist), FadeIn(control_area), Write(control_label), run_time=1)
        self.wait(0.3)
        
        # Experimental distribution (shifted)
        exp_dist = axes.plot(
            lambda x: 0.4 * np.exp(-(x-1.8)**2/2),
            x_range=[-2, 5],
            color="#00FF88",
            stroke_width=3
        )
        exp_area = axes.get_area(
            exp_dist,
            x_range=[-2, 5],
            color="#00FF88",
            opacity=0.3
        )
        exp_label = MathTex(r"\mu_E", font_size=28, color="#00FF88").next_to(axes.c2p(1.8, 0.35), UP)
        
        self.play(Create(exp_dist), FadeIn(exp_area), Write(exp_label), run_time=1)
        self.wait(0.5)
        
        # Show Cohen's d with arrow
        d_arrow = DoubleArrow(
            start=axes.c2p(0, 0.2),
            end=axes.c2p(1.8, 0.2),
            color=YELLOW,
            stroke_width=3,
            buff=0
        )
        d_value = MathTex("d = 1.8", font_size=32, color=YELLOW).next_to(d_arrow, DOWN, buff=0.2)
        
        self.play(GrowArrow(d_arrow), Write(d_value), run_time=1)
        self.wait(0.5)
        
        # Show smaller overlap (high power)
        overlap_region = axes.get_area(
            control_dist,
            x_range=[0.9, 4],
            color=YELLOW,
            opacity=0.5
        )
        power_label = Text("Overlap = Low\nPower = High", font_size=22, color=GREEN).to_corner(DR, buff=0.5)
        
        self.play(FadeIn(overlap_region), Write(power_label), run_time=1)
        self.wait(0.7)
        
        # Show actual d values
        self.play(FadeOut(VGroup(axes, control_dist, control_area, exp_dist, exp_area,
                                control_label, exp_label, d_arrow, d_value, overlap_region, power_label)))
        
        d_values = VGroup(
            MathTex(r"d_{\text{STR}} = 1.07", font_size=36, color="#4ECDC4"),
            MathTex(r"d_{\text{LTR}} = 1.77", font_size=36, color="#95E1D3"),
            MathTex(r"d_{\text{Training}} = 4.98", font_size=36, color="#FFD700")
        ).arrange(DOWN, buff=0.5).move_to(ORIGIN)
        
        for d_val in d_values:
            self.play(Write(d_val), run_time=0.8)
            self.wait(0.3)
        
        self.wait(1)
        
        self.play(FadeOut(VGroup(title, d_values)))
        self.wait(0.3)
    
    
    def correlation_network(self):
        """Network graph showing trait correlations"""
        title = Text("Trait Correlations", font_size=32, color=BLUE).to_edge(UP, buff=0.3)
        self.play(Write(title), run_time=0.7)
        
        # Create nodes for each trait
        node_positions = {
            "AC": LEFT*2.5 + UP*1.5,
            "FS": RIGHT*2.5 + UP*1.5,
            "TC": LEFT*2.5 + DOWN*1.5,
            "TB": RIGHT*2.5 + DOWN*1.5
        }
        
        node_colors = {
            "AC": "#FFD700",
            "FS": "#00FF88",
            "TC": "#4A9EFF",
            "TB": "#FF6B9D"
        }
        
        # Correlation values (from your analysis)
        correlations = {
            ("AC", "FS"): 0.538,
            ("AC", "TC"): -0.139,
            ("AC", "TB"): -0.024,
            ("FS", "TC"): -0.073,
            ("FS", "TB"): -0.334,
            ("TC", "TB"): -0.032
        }
        
        # Create nodes
        nodes = {}
        for trait, pos in node_positions.items():
            circle = Circle(radius=0.5, color=node_colors[trait], fill_opacity=0.8, stroke_width=3)
            label = Text(trait, font_size=28, color=BLACK, weight=BOLD)
            node = VGroup(circle, label).move_to(pos)
            nodes[trait] = node
            self.play(GrowFromCenter(node), run_time=0.5)
        
        self.wait(0.3)
        
        # Create edges with thickness based on correlation strength
        edges = VGroup()
        for (trait1, trait2), corr in correlations.items():
            start = node_positions[trait1]
            end = node_positions[trait2]
            
            # Only show significant correlations
            if abs(corr) > 0.25:
                # Color based on sign
                color = GREEN if corr > 0 else RED
                # Thickness based on magnitude
                width = abs(corr) * 8
                
                line = Line(start, end, color=color, stroke_width=width, stroke_opacity=0.7)
                
                # Add correlation value
                mid_point = (start + end) / 2
                corr_label = Text(f"{corr:.2f}", font_size=16, color=color).move_to(mid_point)
                
                edges.add(line, corr_label)
        
        self.play(LaggedStart(*[Create(obj) if isinstance(obj, Line) else FadeIn(obj) 
                               for obj in edges], lag_ratio=0.2), run_time=2)
        self.wait(1)
        
        # Highlight strongest correlation (AC <-> FS)
        highlight1 = Circle(radius=0.6, color=YELLOW, stroke_width=4).move_to(node_positions["AC"])
        highlight2 = Circle(radius=0.6, color=YELLOW, stroke_width=4).move_to(node_positions["FS"])
        
        self.play(Create(highlight1), Create(highlight2), run_time=0.7)
        self.wait(0.7)
        
        self.play(FadeOut(VGroup(title, *nodes.values(), edges, highlight1, highlight2)))
        self.wait(0.3)
    
    
    def final_visual_summary(self):
        """Final summary with minimal text, maximum visuals"""
        # Show three key visualizations side by side
        
        # 1. Learning effect (bar comparison)
        axes1 = Axes(
            x_range=[0, 3, 1],
            y_range=[0, 0.6, 0.2],
            x_length=2.5,
            y_length=2,
            axis_config={"include_numbers": False},
            tips=False
        ).shift(LEFT*4 + UP*0.5)
        
        bars1 = VGroup(
            Rectangle(width=0.4, height=1.5, fill_color="#FF6B6B", fill_opacity=0.8).move_to(axes1.c2p(0.8, 0.26)),
            Rectangle(width=0.4, height=1.9, fill_color="#00FF88", fill_opacity=0.8).move_to(axes1.c2p(1.8, 0.32))
        )
        label1 = Text("Learning", font_size=18, color=YELLOW).next_to(axes1, DOWN, buff=0.2)
        
        # 2. Regression fit
        axes2 = Axes(
            x_range=[0, 100, 50],
            y_range=[0, 0.7, 0.35],
            x_length=2.5,
            y_length=2,
            axis_config={"include_numbers": False},
            tips=False
        ).move_to(ORIGIN + UP*0.5)
        
        np.random.seed(10)
        dots2 = VGroup(*[
            Dot(axes2.c2p(np.random.uniform(20, 80), 0.2 + 0.005*np.random.uniform(20, 80) + np.random.normal(0, 0.08)),
                radius=0.03, color="#00FF88", fill_opacity=0.5)
            for _ in range(20)
        ])
        line2 = axes2.plot(lambda x: 0.2 + 0.005*x, x_range=[20, 80], color="#FF6B6B", stroke_width=2)
        label2 = Text("Regression", font_size=18, color=YELLOW).next_to(axes2, DOWN, buff=0.2)
        
        # 3. Power (two distributions)
        axes3 = Axes(
            x_range=[-3, 4, 2],
            y_range=[0, 0.4, 0.2],
            x_length=2.5,
            y_length=2,
            axis_config={"include_numbers": False},
            tips=False
        ).shift(RIGHT*4 + UP*0.5)
        
        dist1 = axes3.plot(lambda x: 0.35*np.exp(-x**2/2), x_range=[-3, 3], color="#FF6B6B", stroke_width=2)
        dist2 = axes3.plot(lambda x: 0.35*np.exp(-(x-1.5)**2/2), x_range=[-1.5, 4], color="#00FF88", stroke_width=2)
        label3 = Text("Power", font_size=18, color=YELLOW).next_to(axes3, DOWN, buff=0.2)
        
        # Animate all three
        self.play(
            Create(axes1), Create(axes2), Create(axes3),
            run_time=1
        )
        self.play(
            LaggedStart(*[GrowFromEdge(bar, DOWN) for bar in bars1], lag_ratio=0.2),
            LaggedStart(*[FadeIn(dot, scale=0.5) for dot in dots2], lag_ratio=0.02),
            Create(dist1), Create(dist2),
            run_time=1.5
        )
        self.play(
            Create(line2),
            Write(label1), Write(label2), Write(label3),
            run_time=1
        )
        self.wait(1)
        
        # Final equation
        final_eq = MathTex(
            r"\text{Error} = f(\text{Personality}, \text{Training}, \text{Guidance})",
            font_size=32,
            color=BLUE
        ).to_edge(DOWN, buff=0.8)
        
        self.play(Write(final_eq), run_time=1.5)
        self.wait(2)
        
        # Fade all
        self.play(FadeOut(VGroup(axes1, axes2, axes3, bars1, dots2, line2, dist1, dist2,
                                label1, label2, label3, final_eq)), run_time=1.5)
        self.wait(0.5)


# Render command:
# manim -pqh visual_animation.py VisualStatisticalAnalysis --format=mp4
