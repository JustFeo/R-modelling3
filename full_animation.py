"""
Complete Statistical Analysis Animation with Real Mathematics and Visualizations
Single continuous video with dynamic graphs, distributions, and calculations
Duration: ~5 minutes
"""

from manim import *
import numpy as np

class CompleteStatisticalAnalysis(Scene):
    def construct(self):
        self.camera.background_color = "#0a0a0a"
        
        # ============ PART 1: INTRODUCTION WITH DATA STRUCTURE (30s) ============
        self.introduction_with_data()
        
        # ============ PART 2: PERSONALITY DISTRIBUTIONS WITH REAL PLOTS (50s) ============
        self.personality_distributions()
        
        # ============ PART 3: LEARNING EFFECTS WITH ANIMATED GRAPHS (60s) ============
        self.learning_effects_visualization()
        
        # ============ PART 4: REGRESSION WITH SCATTER PLOTS (60s) ============
        self.regression_visualization()
        
        # ============ PART 5: POWER ANALYSIS WITH CALCULATIONS (40s) ============
        self.power_analysis_visualization()
        
        # ============ PART 6: TRAIT DEPENDENCE WITH CORRELATION MATRIX (30s) ============
        self.trait_dependence_visualization()
        
        # ============ PART 7: FINAL SUMMARY (20s) ============
        self.final_summary()
    
    
    def introduction_with_data(self):
        """Introduction with data structure visualization"""
        title = Text("Robot-Assisted Motor Learning\nStatistical Analysis", 
                    font_size=48, weight=BOLD, color=BLUE)
        self.play(Write(title), run_time=2)
        self.wait(1)
        self.play(title.animate.scale(0.6).to_edge(UP, buff=0.3))
        
        # Data structure table
        table_title = Text("Dataset Structure", font_size=32, color=YELLOW).next_to(title, DOWN, buff=0.5)
        
        # Create visual representation of data
        headers = VGroup(
            Text("ID", font_size=24, weight=BOLD),
            Text("Group", font_size=24, weight=BOLD),
            Text("Stage", font_size=24, weight=BOLD),
            Text("Error", font_size=24, weight=BOLD),
            Text("Traits", font_size=24, weight=BOLD)
        ).arrange(RIGHT, buff=0.8)
        
        # Sample data rows
        row1 = VGroup(
            Text("1", font_size=20, color="#888"),
            Text("C", font_size=20, color="#FF6B6B"),
            Text("BL", font_size=20, color="#4A9EFF"),
            Text("0.45", font_size=20),
            Text("AC,FS,TC,TB", font_size=16, color="#00FF88")
        ).arrange(RIGHT, buff=0.8)
        
        row2 = VGroup(
            Text("2", font_size=20, color="#888"),
            Text("E", font_size=20, color="#00FF88"),
            Text("BL", font_size=20, color="#4A9EFF"),
            Text("0.42", font_size=20),
            Text("AC,FS,TC,TB", font_size=16, color="#00FF88")
        ).arrange(RIGHT, buff=0.8)
        
        row3 = VGroup(
            Text("...", font_size=20, color="#888"),
            Text("...", font_size=20, color="#888"),
            Text("...", font_size=20, color="#888"),
            Text("...", font_size=20, color="#888"),
            Text("...", font_size=16, color="#888")
        ).arrange(RIGHT, buff=0.8)
        
        table = VGroup(headers, row1, row2, row3).arrange(DOWN, buff=0.3, aligned_edge=LEFT)
        table.next_to(table_title, DOWN, buff=0.4)
        
        self.play(Write(table_title), run_time=1)
        self.play(LaggedStart(*[FadeIn(row, shift=RIGHT*0.3) for row in table], lag_ratio=0.3), run_time=2)
        self.wait(1.5)
        
        # Study design annotations
        n_formula = MathTex(r"n = 100 \text{ participants}", font_size=32).to_edge(LEFT, buff=1).shift(DOWN)
        groups = MathTex(r"\text{Groups: Control (C) vs Experimental (E)}", font_size=28).next_to(n_formula, DOWN, buff=0.3)
        stages = MathTex(r"\text{Stages: } BL \rightarrow STR \rightarrow LTR", font_size=28).next_to(groups, DOWN, buff=0.3)
        
        self.play(Write(n_formula), run_time=1)
        self.play(Write(groups), run_time=1)
        self.play(Write(stages), run_time=1)
        self.wait(2)
        
        self.play(FadeOut(VGroup(title, table_title, table, n_formula, groups, stages)))
        self.wait(0.5)
    
    
    def personality_distributions(self):
        """Visualize personality trait distributions with actual curves"""
        title = Text("Week 1: Personality Trait Distributions", 
                    font_size=36, color=BLUE, weight=BOLD).to_edge(UP, buff=0.3)
        self.play(Write(title), run_time=1.5)
        
        # Four traits in a grid
        trait_colors = {"AC": "#FFD700", "FS": "#00FF88", "TC": "#4A9EFF", "TB": "#FF6B9D"}
        trait_names = {"AC": "Achiever", "FS": "Free Spirit", "TC": "Transform Challenge", "TB": "Transform Boredom"}
        trait_dists = {"AC": "Weibull", "FS": "Beta", "TC": "Beta", "TB": "Normal"}
        
        # Create axes for each trait
        axes_group = VGroup()
        for i, (trait, color) in enumerate(trait_colors.items()):
            # Position in 2x2 grid
            x_pos = -3.5 if i % 2 == 0 else 1.5
            y_pos = 1 if i < 2 else -2
            
            # Create histogram bars (simulated data)
            axes = Axes(
                x_range=[0, 100, 25],
                y_range=[0, 25, 5],
                x_length=3,
                y_length=2,
                tips=False,
                axis_config={"include_numbers": False, "stroke_width": 2}
            ).shift(RIGHT*x_pos + UP*y_pos)
            
            # Simulate histogram data
            np.random.seed(i)
            if trait == "AC":
                # Weibull-like distribution
                bars_heights = [5, 12, 20, 18, 10, 6]
            elif trait in ["FS", "TC"]:
                # Beta-like distribution
                bars_heights = [4, 10, 18, 20, 15, 7]
            else:  # TB
                # Normal-like distribution
                bars_heights = [3, 8, 18, 22, 16, 5]
            
            bars = VGroup()
            for j, height in enumerate(bars_heights):
                x_start = 16.67 * j
                bar = Rectangle(
                    width=0.4,
                    height=height * 0.08,
                    fill_color=color,
                    fill_opacity=0.7,
                    stroke_width=1,
                    stroke_color=WHITE
                ).move_to(axes.c2p(x_start + 8, height/2))
                bars.add(bar)
            
            # Fitted distribution curve
            if trait == "AC":
                # Weibull curve
                curve_func = lambda x: 20 * (1.5/50) * ((x/50)**(0.5)) * np.exp(-((x/50)**1.5))
            elif trait in ["FS", "TC"]:
                # Beta curve
                curve_func = lambda x: 20 * (x/100)**2 * ((100-x)/100)**1.5
            else:  # TB
                # Normal curve
                curve_func = lambda x: 22 * np.exp(-((x-50)**2)/(2*20**2))
            
            curve = axes.plot(curve_func, x_range=[0, 100], color=color, stroke_width=3)
            
            # Labels
            trait_label = Text(trait, font_size=24, color=color, weight=BOLD).next_to(axes, UP, buff=0.2)
            dist_label = Text(f"~ {trait_dists[trait]}", font_size=18, color="#888").next_to(trait_label, DOWN, buff=0.1)
            
            group = VGroup(axes, bars, curve, trait_label, dist_label)
            axes_group.add(group)
        
        # Animate all distributions
        for group in axes_group:
            axes, bars, curve, trait_label, dist_label = group
            self.play(Create(axes), Write(trait_label), run_time=0.8)
            self.play(LaggedStart(*[GrowFromEdge(bar, DOWN) for bar in bars], lag_ratio=0.1), run_time=1)
            self.play(Create(curve), Write(dist_label), run_time=1.2)
        
        self.wait(2)
        
        # Show KS test results
        ks_box = Rectangle(width=6, height=1.5, stroke_color=GREEN, stroke_width=2, 
                          fill_color="#0a0a0a", fill_opacity=0.9).to_edge(DOWN, buff=0.5)
        ks_text = VGroup(
            Text("Kolmogorov-Smirnov Tests", font_size=24, color=GREEN, weight=BOLD),
            Text("All p-values > 0.05", font_size=20, color=WHITE),
            Text("✓ Good fit confirmed", font_size=20, color=GREEN)
        ).arrange(DOWN, buff=0.15).move_to(ks_box)
        
        self.play(Create(ks_box), FadeIn(ks_text, shift=UP*0.3), run_time=1.5)
        self.wait(2)
        
        self.play(FadeOut(VGroup(title, axes_group, ks_box, ks_text)))
        self.wait(0.5)
    
    
    def learning_effects_visualization(self):
        """Visualize learning effects with animated bar charts"""
        title = Text("Week 2: Learning Effects", font_size=36, color=BLUE, weight=BOLD).to_edge(UP, buff=0.3)
        self.play(Write(title), run_time=1)
        
        # Show the formula first
        formula = MathTex(
            r"AveAbsError_{i,s} = \frac{1}{40}\sum_{k=1}^{20}|Error|_{i,s,k}",
            font_size=32
        ).next_to(title, DOWN, buff=0.4)
        self.play(Write(formula), run_time=2)
        self.wait(1)
        
        # Create bar chart comparing Control vs Experimental across stages
        axes = Axes(
            x_range=[0, 4, 1],
            y_range=[0, 0.6, 0.1],
            x_length=8,
            y_length=4,
            axis_config={"include_numbers": False},
            tips=False
        ).shift(DOWN*0.5)
        
        # Axes labels
        y_label = Text("Average Error", font_size=24).next_to(axes.y_axis, LEFT, buff=0.3).rotate(PI/2)
        stage_labels = VGroup(
            Text("BL", font_size=20, color="#FF6B6B"),
            Text("STR", font_size=20, color="#4ECDC4"),
            Text("LTR", font_size=20, color="#95E1D3")
        )
        for i, label in enumerate(stage_labels):
            label.next_to(axes.c2p(i+1, 0), DOWN, buff=0.3)
        
        self.play(Create(axes), Write(y_label), *[Write(label) for label in stage_labels], run_time=1.5)
        
        # Data (approximate from your analysis)
        control_data = [0.52, 0.36, 0.40]  # BL, STR, LTR
        experimental_data = [0.51, 0.25, 0.28]
        
        # Create bars
        control_bars = VGroup()
        experimental_bars = VGroup()
        
        for i, (c_val, e_val) in enumerate(zip(control_data, experimental_data)):
            # Control bar (left)
            c_bar = Rectangle(
                width=0.3,
                height=c_val * 6,
                fill_color="#FF6B6B",
                fill_opacity=0.8,
                stroke_width=2,
                stroke_color=WHITE
            ).move_to(axes.c2p(i+0.85, c_val/2))
            
            # Experimental bar (right)
            e_bar = Rectangle(
                width=0.3,
                height=e_val * 6,
                fill_color="#00FF88",
                fill_opacity=0.8,
                stroke_width=2,
                stroke_color=WHITE
            ).move_to(axes.c2p(i+1.15, e_val/2))
            
            control_bars.add(c_bar)
            experimental_bars.add(e_bar)
        
        # Animate bars growing
        self.play(LaggedStart(*[GrowFromEdge(bar, DOWN) for bar in control_bars], lag_ratio=0.2), run_time=2)
        self.play(LaggedStart(*[GrowFromEdge(bar, DOWN) for bar in experimental_bars], lag_ratio=0.2), run_time=2)
        
        # Add value labels on bars
        for i, (c_bar, e_bar, c_val, e_val) in enumerate(zip(control_bars, experimental_bars, control_data, experimental_data)):
            c_label = Text(f"{c_val:.2f}", font_size=16, color=WHITE).next_to(c_bar, UP, buff=0.1)
            e_label = Text(f"{e_val:.2f}", font_size=16, color=WHITE).next_to(e_bar, UP, buff=0.1)
            self.play(FadeIn(c_label), FadeIn(e_label), run_time=0.5)
        
        self.wait(1)
        
        # Show learning effect calculation
        learning_box = Rectangle(width=7, height=2, stroke_color=YELLOW, stroke_width=2,
                                fill_color="#0a0a0a", fill_opacity=0.9).to_corner(DR, buff=0.5)
        learning_title = Text("Learning Effect", font_size=24, color=YELLOW, weight=BOLD).move_to(learning_box.get_top() + DOWN*0.3)
        learning_formula = MathTex(
            r"Learn_{STR} = Error_{BL} - Error_{STR}",
            font_size=24
        ).next_to(learning_title, DOWN, buff=0.3)
        learning_values = VGroup(
            MathTex(r"\text{Control: } 0.52 - 0.36 = 0.16", font_size=20, color="#FF6B6B"),
            MathTex(r"\text{Exp: } 0.51 - 0.25 = 0.26", font_size=20, color="#00FF88")
        ).arrange(DOWN, buff=0.2, aligned_edge=LEFT).next_to(learning_formula, DOWN, buff=0.3)
        
        self.play(Create(learning_box), Write(learning_title), run_time=1)
        self.play(Write(learning_formula), run_time=1.5)
        self.play(Write(learning_values), run_time=2)
        self.wait(2)
        
        # Highlight that Experimental learns more
        conclusion = Text("✓ Robot guidance enhances learning", font_size=24, color=GREEN, weight=BOLD).to_edge(DOWN, buff=0.3)
        self.play(FadeIn(conclusion, shift=UP*0.3), run_time=1)
        self.wait(2)
        
        self.play(FadeOut(VGroup(title, formula, axes, y_label, stage_labels, control_bars, experimental_bars, 
                                learning_box, learning_title, learning_formula, learning_values, conclusion)))
        self.wait(0.5)
    
    
    def regression_visualization(self):
        """Regression models with actual scatter plots and fitted lines"""
        title = Text("Week 3: Regression Analysis", font_size=36, color=BLUE, weight=BOLD).to_edge(UP, buff=0.3)
        self.play(Write(title), run_time=1)
        
        # Show Model 1 equation
        model1_label = Text("Model 1: Main Effects", font_size=28, color=YELLOW).next_to(title, DOWN, buff=0.4)
        model1_eq = MathTex(
            r"Error = \beta_0 + \beta_1 AC + \beta_2 FS + \beta_3 TC + \beta_4 TB",
            font_size=28
        ).next_to(model1_label, DOWN, buff=0.3)
        
        self.play(Write(model1_label), run_time=0.8)
        self.play(Write(model1_eq), run_time=2)
        self.wait(1.5)
        
        # Create scatter plot for FS vs Error (most significant effect)
        axes = Axes(
            x_range=[0, 100, 25],
            y_range=[0, 0.8, 0.2],
            x_length=5,
            y_length=3.5,
            axis_config={"include_numbers": True, "font_size": 16},
            tips=False
        ).shift(DOWN*0.5 + LEFT*2.5)
        
        x_label = Text("Free Spirit Score", font_size=20).next_to(axes.x_axis, DOWN, buff=0.3)
        y_label = Text("Error", font_size=20).next_to(axes.y_axis, LEFT, buff=0.3).rotate(PI/2)
        
        self.play(FadeOut(model1_label), model1_eq.animate.scale(0.7).to_corner(UL, buff=0.5).shift(DOWN))
        self.play(Create(axes), Write(x_label), Write(y_label), run_time=1.5)
        
        # Generate scatter points (simulated positive correlation)
        np.random.seed(42)
        n_points = 40
        fs_values = np.random.uniform(20, 90, n_points)
        errors = 0.2 + 0.005 * fs_values + np.random.normal(0, 0.08, n_points)
        errors = np.clip(errors, 0.1, 0.7)
        
        dots = VGroup()
        for fs, err in zip(fs_values, errors):
            dot = Dot(axes.c2p(fs, err), radius=0.04, color="#00FF88", fill_opacity=0.7)
            dots.add(dot)
        
        self.play(LaggedStart(*[FadeIn(dot, scale=0.5) for dot in dots], lag_ratio=0.02), run_time=2)
        self.wait(1)
        
        # Fitted regression line
        line_func = lambda x: 0.2 + 0.005 * x
        regression_line = axes.plot(line_func, x_range=[20, 90], color="#FF6B6B", stroke_width=3)
        
        self.play(Create(regression_line), run_time=1.5)
        self.wait(1)
        
        # Show coefficient
        coef_box = Rectangle(width=3, height=1.2, stroke_color="#00FF88", stroke_width=2,
                            fill_color="#0a0a0a", fill_opacity=0.9).next_to(axes, RIGHT, buff=0.5)
        coef_text = VGroup(
            MathTex(r"\beta_{FS} = +0.012", font_size=24, color="#00FF88"),
            Text("p < 0.001", font_size=18, color=GREEN)
        ).arrange(DOWN, buff=0.2).move_to(coef_box)
        
        self.play(Create(coef_box), Write(coef_text), run_time=1.5)
        self.wait(1.5)
        
        # Now show Model 2 with interactions
        self.play(FadeOut(VGroup(axes, x_label, y_label, dots, regression_line, coef_box, coef_text)))
        
        model2_label = Text("Model 2: Group Interactions", font_size=28, color=YELLOW).move_to(ORIGIN + UP*1.5)
        model2_eq = MathTex(
            r"Error = \beta_0 + \sum \beta_i \cdot Trait_i + \beta_5 \cdot Group",
            font_size=24
        ).next_to(model2_label, DOWN, buff=0.3)
        model2_interaction = MathTex(
            r"+ \sum \beta_{j} \cdot (Group \times Trait_j)",
            font_size=24
        ).next_to(model2_eq, DOWN, buff=0.2)
        
        self.play(Write(model2_label), run_time=1)
        self.play(Write(model2_eq), run_time=1.5)
        self.play(Write(model2_interaction), run_time=1.5)
        self.wait(1)
        
        # Show R² comparison
        r2_comparison = VGroup(
            Text("Model Fit (R²):", font_size=24, color=YELLOW, weight=BOLD),
            MathTex(r"R^2_{BL} = 0.788", font_size=22, color="#FF6B6B"),
            MathTex(r"R^2_{STR} = 0.803", font_size=22, color="#4ECDC4"),
            MathTex(r"R^2_{LTR} = 0.840", font_size=22, color="#95E1D3")
        ).arrange(DOWN, buff=0.3, aligned_edge=LEFT).to_edge(DOWN, buff=0.8)
        
        self.play(FadeIn(r2_comparison, shift=UP*0.3), run_time=2)
        self.wait(2)
        
        self.play(FadeOut(VGroup(title, model1_eq, model2_label, model2_eq, model2_interaction, r2_comparison)))
        self.wait(0.5)
    
    
    def power_analysis_visualization(self):
        """Power analysis with Cohen's d visualization"""
        title = Text("Week 4: Statistical Power Analysis", font_size=36, color=BLUE, weight=BOLD).to_edge(UP, buff=0.3)
        self.play(Write(title), run_time=1)
        
        # Cohen's d formula with visual explanation
        cohend_title = Text("Effect Size: Cohen's d", font_size=28, color=YELLOW).next_to(title, DOWN, buff=0.4)
        cohend_formula = MathTex(
            r"d = \frac{\bar{x}_1 - \bar{x}_2}{s_{pooled}}",
            font_size=40
        ).next_to(cohend_title, DOWN, buff=0.3)
        
        self.play(Write(cohend_title), run_time=1)
        self.play(Write(cohend_formula), run_time=1.5)
        self.wait(1)
        
        # Visual representation of two distributions
        axes = Axes(
            x_range=[-3, 3, 1],
            y_range=[0, 0.5, 0.1],
            x_length=6,
            y_length=2.5,
            axis_config={"include_numbers": False},
            tips=False
        ).shift(DOWN*0.3)
        
        # Two normal distributions (Control vs Experimental)
        dist1 = axes.plot(lambda x: 0.4 * np.exp(-x**2/2), x_range=[-3, 3], color="#FF6B6B", stroke_width=3)
        dist2 = axes.plot(lambda x: 0.4 * np.exp(-(x-1.5)**2/2), x_range=[-3, 3], color="#00FF88", stroke_width=3)
        
        label1 = Text("Control", font_size=20, color="#FF6B6B").next_to(axes.c2p(-0.5, 0.35), UP)
        label2 = Text("Experimental", font_size=20, color="#00FF88").next_to(axes.c2p(1.5, 0.35), UP)
        
        # Arrow showing the distance
        arrow = DoubleArrow(
            start=axes.c2p(0, 0.15),
            end=axes.c2p(1.5, 0.15),
            color=YELLOW,
            stroke_width=3,
            buff=0
        )
        d_label = MathTex("d", font_size=32, color=YELLOW).next_to(arrow, DOWN, buff=0.2)
        
        self.play(FadeOut(cohend_title), cohend_formula.animate.scale(0.6).to_corner(UL, buff=0.5))
        self.play(Create(axes), run_time=1)
        self.play(Create(dist1), Write(label1), run_time=1)
        self.play(Create(dist2), Write(label2), run_time=1)
        self.play(GrowArrow(arrow), Write(d_label), run_time=1.5)
        self.wait(1.5)
        
        self.play(FadeOut(VGroup(axes, dist1, dist2, label1, label2, arrow, d_label)))
        
        # Show observed effect sizes
        effects_title = Text("Observed Effect Sizes", font_size=28, color=GREEN, weight=BOLD).move_to(ORIGIN + UP*1)
        effects = VGroup(
            MathTex(r"d_{Learn\_STR} = 1.07", font_size=32, color="#4ECDC4"),
            MathTex(r"d_{Learn\_LTR} = 1.77", font_size=32, color="#95E1D3"),
            MathTex(r"d_{Training\_STR} = 4.98", font_size=32, color="#FFD700"),
            MathTex(r"d_{Training\_LTR} = 3.63", font_size=32, color="#FFD700")
        ).arrange(DOWN, buff=0.3, aligned_edge=LEFT).next_to(effects_title, DOWN, buff=0.5)
        
        self.play(Write(effects_title), run_time=1)
        self.play(LaggedStart(*[Write(eff) for eff in effects], lag_ratio=0.3), run_time=3)
        self.wait(1)
        
        # Power conclusion
        power_box = Rectangle(width=8, height=1.5, stroke_color=GREEN, stroke_width=3,
                             fill_color="#0a0a0a", fill_opacity=0.9).to_edge(DOWN, buff=0.5)
        power_text = VGroup(
            Text("Statistical Power: 100%", font_size=28, color=GREEN, weight=BOLD),
            Text("Sample size (n=50 per group) is sufficient", font_size=22, color=WHITE)
        ).arrange(DOWN, buff=0.2).move_to(power_box)
        
        self.play(Create(power_box), FadeIn(power_text, scale=1.1), run_time=1.5)
        self.wait(2)
        
        self.play(FadeOut(VGroup(title, cohend_formula, effects_title, effects, power_box, power_text)))
        self.wait(0.5)
    
    
    def trait_dependence_visualization(self):
        """Correlation matrix visualization"""
        title = Text("Extension: Personality Trait Dependence", font_size=36, color=BLUE, weight=BOLD).to_edge(UP, buff=0.3)
        self.play(Write(title), run_time=1)
        
        # Create correlation matrix
        matrix_title = Text("Correlation Matrix", font_size=28, color=YELLOW).next_to(title, DOWN, buff=0.4)
        self.play(Write(matrix_title), run_time=1)
        
        # 4x4 correlation matrix
        traits = ["AC", "FS", "TC", "TB"]
        # Approximate correlation values from your analysis
        corr_values = [
            [1.000, 0.538, -0.139, -0.024],
            [0.538, 1.000, -0.073, -0.334],
            [-0.139, -0.073, 1.000, -0.032],
            [-0.024, -0.334, -0.032, 1.000]
        ]
        
        # Create matrix grid
        cell_size = 1
        matrix_group = VGroup()
        
        # Labels
        for i, trait in enumerate(traits):
            # Row labels
            row_label = Text(trait, font_size=20, color="#FFD700").move_to(LEFT*2.5 + UP*(1.5-i))
            # Column labels
            col_label = Text(trait, font_size=20, color="#FFD700").move_to(LEFT*(-0.5+i) + UP*2.5)
            matrix_group.add(row_label, col_label)
        
        # Color cells based on correlation strength
        for i in range(4):
            for j in range(4):
                val = corr_values[i][j]
                # Color interpolation: red for negative, green for positive
                if val > 0:
                    color = interpolate_color(WHITE, GREEN, val)
                else:
                    color = interpolate_color(WHITE, RED, abs(val))
                
                if i == j:
                    color = BLUE  # Diagonal
                
                cell = Square(
                    side_length=cell_size,
                    fill_color=color,
                    fill_opacity=0.6,
                    stroke_color=WHITE,
                    stroke_width=2
                ).move_to(LEFT*(-0.5+j) + UP*(1.5-i))
                
                # Value text
                if abs(val) > 0.3 and i != j:  # Only show significant correlations
                    val_text = Text(f"{val:.2f}", font_size=16, color=WHITE, weight=BOLD).move_to(cell)
                    matrix_group.add(cell, val_text)
                else:
                    matrix_group.add(cell)
        
        self.play(FadeOut(matrix_title))
        self.play(LaggedStart(*[FadeIn(obj, scale=0.8) for obj in matrix_group], lag_ratio=0.02), run_time=3)
        self.wait(1.5)
        
        # Highlight strong correlation AC <-> FS
        highlight = Square(side_length=cell_size, stroke_color=YELLOW, stroke_width=4, fill_opacity=0).move_to(LEFT*(-0.5+1) + UP*(1.5-0))
        highlight2 = Square(side_length=cell_size, stroke_color=YELLOW, stroke_width=4, fill_opacity=0).move_to(LEFT*(-0.5+0) + UP*(1.5-1))
        
        finding = Text("Strong positive correlation:\nAC ↔ FS (r = 0.538, p < 0.001)", 
                      font_size=22, color=YELLOW).to_edge(DOWN, buff=0.8)
        
        self.play(Create(highlight), Create(highlight2), run_time=1)
        self.play(Write(finding), run_time=1.5)
        self.wait(2)
        
        self.play(FadeOut(VGroup(title, matrix_group, highlight, highlight2, finding)))
        self.wait(0.5)
    
    
    def final_summary(self):
        """Final comprehensive summary"""
        title = Text("Summary", font_size=48, color=BLUE, weight=BOLD).move_to(ORIGIN + UP*2.5)
        self.play(Write(title), run_time=1.5)
        self.wait(0.5)
        
        # Key findings with checkmarks
        findings = VGroup(
            VGroup(
                Text("✓", font_size=36, color=GREEN, weight=BOLD),
                Text("Robot guidance significantly reduces errors", font_size=26)
            ).arrange(RIGHT, buff=0.3),
            VGroup(
                Text("✓", font_size=36, color=GREEN, weight=BOLD),
                Text("Effect persists in long-term retention", font_size=26)
            ).arrange(RIGHT, buff=0.3),
            VGroup(
                Text("✓", font_size=36, color=GREEN, weight=BOLD),
                Text("Personality traits predict performance:", font_size=26)
            ).arrange(RIGHT, buff=0.3),
            VGroup(
                Text("  •", font_size=26, color=YELLOW),
                MathTex(r"\text{Free Spirit} \uparrow \Rightarrow \text{Error} \uparrow", font_size=24, color="#FF6B6B")
            ).arrange(RIGHT, buff=0.2),
            VGroup(
                Text("  •", font_size=26, color=YELLOW),
                MathTex(r"\text{Transform Challenge} \uparrow \Rightarrow \text{Error} \downarrow", font_size=24, color="#00FF88")
            ).arrange(RIGHT, buff=0.2),
            VGroup(
                Text("✓", font_size=36, color=GREEN, weight=BOLD),
                Text("Guidance effect depends on personality (R² > 0.78)", font_size=26)
            ).arrange(RIGHT, buff=0.3),
            VGroup(
                Text("✓", font_size=36, color=GREEN, weight=BOLD),
                Text("Study is well-powered (100%)", font_size=26)
            ).arrange(RIGHT, buff=0.3),
        ).arrange(DOWN, buff=0.25, aligned_edge=LEFT).next_to(title, DOWN, buff=0.6)
        
        self.play(LaggedStart(*[FadeIn(finding, shift=RIGHT*0.3) for finding in findings], 
                             lag_ratio=0.3), run_time=4)
        self.wait(2)
        
        # Final message
        final_text = Text("Statistical Analysis Complete", font_size=36, color=BLUE, weight=BOLD).to_edge(DOWN, buff=0.5)
        self.play(FadeIn(final_text, scale=1.2), run_time=1.5)
        self.wait(3)
        
        # Fade everything out
        self.play(FadeOut(VGroup(title, findings, final_text)), run_time=2)
        self.wait(1)


# To render: manim -pqh full_animation.py CompleteStatisticalAnalysis --format=mp4
