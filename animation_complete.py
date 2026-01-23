"""
Complete animation for Robot-Assisted Learning Statistical Analysis
3-5 minute video with 7 scenes covering the entire project
Dark theme mathematics-style animation designed for voice-over
"""

from manim import *
import numpy as np

# ==================== SCENE 1: INTRODUCTION (30s) ====================
class Scene1_Introduction(Scene):
    def construct(self):
        self.camera.background_color = "#0a0a0a"
        
        # Title
        title = Text("Robot-Assisted Learning\nStatistical Analysis", 
                    font_size=56, weight=BOLD, color=WHITE)
        title.move_to(ORIGIN)
        
        subtitle = Text("100 Patients • 3 Stages • 4 Personality Traits", 
                       font_size=32, color="#888888")
        subtitle.next_to(title, DOWN, buff=0.8)
        
        self.play(Write(title), run_time=2)
        self.play(FadeIn(subtitle), run_time=1)
        self.wait(2)
        self.play(FadeOut(title), FadeOut(subtitle))
        
        # Research question
        question = Text("Research Question:", font_size=40, color=BLUE, weight=BOLD)
        question.to_edge(UP, buff=0.8)
        
        question_text = Text(
            "Can robot-assisted learning help patients with\n"
            "different personality traits reduce errors?",
            font_size=32, color=WHITE
        )
        question_text.next_to(question, DOWN, buff=0.6)
        
        self.play(Write(question), run_time=1)
        self.play(Write(question_text), run_time=2)
        self.wait(2)
        
        # Study design
        design_title = Text("Study Design", font_size=36, color=YELLOW, weight=BOLD)
        design_title.next_to(question_text, DOWN, buff=0.8)
        
        design_items = VGroup(
            Text("• 100 participants (50 Control, 50 Experimental)", font_size=28),
            Text("• 3 stages: Baseline, Short-term, Long-term", font_size=28),
            Text("• 4 personality traits: AC, FS, TC, TB", font_size=28),
            Text("• Experimental group receives robot guidance", font_size=28)
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.3)
        design_items.next_to(design_title, DOWN, buff=0.4)
        
        self.play(Write(design_title), run_time=1)
        self.play(LaggedStart(*[FadeIn(item, shift=RIGHT*0.3) for item in design_items], 
                             lag_ratio=0.3), run_time=3)
        self.wait(2)
        self.play(FadeOut(VGroup(question, question_text, design_title, design_items)))


# ==================== SCENE 2: WEEK 1 - PERSONALITY DISTRIBUTIONS (40s) ====================
class Scene2_PersonalityDistributions(Scene):
    def construct(self):
        self.camera.background_color = "#0a0a0a"
        
        # Title
        title = Text("Week 1: Personality Trait Distributions", 
                    font_size=44, color=BLUE, weight=BOLD)
        title.to_edge(UP, buff=0.5)
        self.play(Write(title), run_time=1.5)
        self.wait(1)
        
        # Four traits displayed
        traits = VGroup(
            VGroup(
                Text("AC", font_size=36, color="#FFD700", weight=BOLD),
                Text("Achiever", font_size=24, color="#888888")
            ).arrange(DOWN, buff=0.2),
            VGroup(
                Text("FS", font_size=36, color="#00FF88", weight=BOLD),
                Text("Free Spirit", font_size=24, color="#888888")
            ).arrange(DOWN, buff=0.2),
            VGroup(
                Text("TC", font_size=36, color="#4A9EFF", weight=BOLD),
                Text("Transform of\nChallenge", font_size=24, color="#888888")
            ).arrange(DOWN, buff=0.2),
            VGroup(
                Text("TB", font_size=36, color="#FF6B9D", weight=BOLD),
                Text("Transform of\nBoredom", font_size=24, color="#888888")
            ).arrange(DOWN, buff=0.2)
        ).arrange(RIGHT, buff=1)
        traits.next_to(title, DOWN, buff=0.8)
        
        self.play(LaggedStart(*[FadeIn(trait, scale=1.2) for trait in traits], 
                             lag_ratio=0.2), run_time=2)
        self.wait(1.5)
        
        # Distribution fitting
        dist_title = Text("Distribution Fitting:", font_size=32, color=YELLOW)
        dist_title.next_to(traits, DOWN, buff=0.8)
        
        distributions = VGroup(
            MathTex(r"\text{AC} \sim \text{Weibull}", color="#FFD700", font_size=36),
            MathTex(r"\text{FS} \sim \text{Beta}", color="#00FF88", font_size=36),
            MathTex(r"\text{TC} \sim \text{Beta}", color="#4A9EFF", font_size=36),
            MathTex(r"\text{TB} \sim \text{Normal}", color="#FF6B9D", font_size=36)
        ).arrange(DOWN, buff=0.3, aligned_edge=LEFT)
        distributions.next_to(dist_title, DOWN, buff=0.4)
        
        self.play(Write(dist_title), run_time=1)
        self.play(LaggedStart(*[Write(dist) for dist in distributions], 
                             lag_ratio=0.3), run_time=2.5)
        self.wait(1)
        
        # Goodness of fit
        gof = Text("✓ Kolmogorov-Smirnov tests confirm good fit", 
                  font_size=28, color=GREEN)
        gof.to_edge(DOWN, buff=0.8)
        self.play(FadeIn(gof, shift=UP*0.3), run_time=1)
        self.wait(2)
        
        self.play(FadeOut(VGroup(title, traits, dist_title, distributions, gof)))


# ==================== SCENE 3: WEEK 2 - LEARNING EFFECTS (50s) ====================
class Scene3_LearningEffects(Scene):
    def construct(self):
        self.camera.background_color = "#0a0a0a"
        
        # Title
        title = Text("Week 2: Learning Effects & Group Comparisons", 
                    font_size=40, color=BLUE, weight=BOLD)
        title.to_edge(UP, buff=0.5)
        self.play(Write(title), run_time=1.5)
        
        # Formula for average error
        formula_label = Text("Average Absolute Error:", font_size=32, color=YELLOW)
        formula_label.next_to(title, DOWN, buff=0.6)
        
        formula = MathTex(
            r"AveAbsError_{i,s} = \frac{1}{40}\sum_{k=1}^{20}(|Error|_{i,s,1,k}+|Error|_{i,s,2,k})",
            font_size=34
        )
        formula.next_to(formula_label, DOWN, buff=0.4)
        
        self.play(Write(formula_label), run_time=1)
        self.play(Write(formula), run_time=2)
        self.wait(1.5)
        
        # Three stages
        stages = VGroup(
            VGroup(
                Text("BL", font_size=36, color="#FF6B6B", weight=BOLD),
                Text("Baseline", font_size=24, color="#888888")
            ).arrange(DOWN, buff=0.2),
            VGroup(
                Text("STR", font_size=36, color="#4ECDC4", weight=BOLD),
                Text("Short-term", font_size=24, color="#888888")
            ).arrange(DOWN, buff=0.2),
            VGroup(
                Text("LTR", font_size=36, color="#95E1D3", weight=BOLD),
                Text("Long-term", font_size=24, color="#888888")
            ).arrange(DOWN, buff=0.2)
        ).arrange(RIGHT, buff=1.2)
        stages.next_to(formula, DOWN, buff=0.8)
        
        self.play(LaggedStart(*[FadeIn(stage, scale=1.2) for stage in stages], 
                             lag_ratio=0.3), run_time=2)
        self.wait(1)
        
        # Key findings
        findings_title = Text("Key Findings:", font_size=32, color=GREEN, weight=BOLD)
        findings_title.next_to(stages, DOWN, buff=0.7)
        
        findings = VGroup(
            Text("✓ Error reduces significantly after training", font_size=28, color=WHITE),
            Text("✓ No difference at baseline (randomization worked)", font_size=28, color=WHITE),
            Text("✓ Experimental group shows lower errors at STR & LTR", font_size=28, color=WHITE),
            Text("✓ Robot guidance enhances learning", font_size=28, color=GREEN)
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.25)
        findings.next_to(findings_title, DOWN, buff=0.4)
        
        self.play(Write(findings_title), run_time=1)
        self.play(LaggedStart(*[FadeIn(f, shift=RIGHT*0.3) for f in findings], 
                             lag_ratio=0.4), run_time=3)
        self.wait(2)
        
        self.play(FadeOut(VGroup(title, formula_label, formula, stages, findings_title, findings)))


# ==================== SCENE 4: WEEK 3 - REGRESSION MODELS (50s) ====================
class Scene4_RegressionModels(Scene):
    def construct(self):
        self.camera.background_color = "#0a0a0a"
        
        # Title
        title = Text("Week 3: Regression Models", 
                    font_size=44, color=BLUE, weight=BOLD)
        title.to_edge(UP, buff=0.5)
        self.play(Write(title), run_time=1.5)
        
        # Model 1
        model1_title = Text("Model 1: Personality Effects", font_size=32, color=YELLOW)
        model1_title.next_to(title, DOWN, buff=0.6)
        
        model1 = MathTex(
            r"Error = \beta_0 + \beta_1 AC + \beta_2 FS + \beta_3 TC + \beta_4 TB",
            font_size=32
        )
        model1.next_to(model1_title, DOWN, buff=0.3)
        
        self.play(Write(model1_title), run_time=1)
        self.play(Write(model1), run_time=2)
        self.wait(1.5)
        
        # Model 2
        model2_title = Text("Model 2: Group Interactions", font_size=32, color=YELLOW)
        model2_title.next_to(model1, DOWN, buff=0.6)
        
        model2 = MathTex(
            r"Error = ", r"\beta_0 + \beta_1 AC + \beta_2 FS + \beta_3 TC + \beta_4 TB\\",
            r"+ (\beta_5 + \beta_6 AC + \beta_7 FS + \beta_8 TC + \beta_9 TB) \cdot Group_E",
            font_size=28
        )
        model2.next_to(model2_title, DOWN, buff=0.3)
        
        self.play(Write(model2_title), run_time=1)
        self.play(Write(model2), run_time=2.5)
        self.wait(1.5)
        
        # Results
        results_box = Rectangle(
            width=11, height=2.5, 
            stroke_color=GREEN, stroke_width=3, fill_color="#0a0a0a", fill_opacity=0.8
        )
        results_box.to_edge(DOWN, buff=0.5)
        
        results_title = Text("Results (all stages):", font_size=28, color=GREEN, weight=BOLD)
        results_title.move_to(results_box.get_top() + DOWN*0.4)
        
        results = VGroup(
            MathTex(r"\text{FS} \uparrow \Rightarrow \text{Error} \uparrow", 
                   color="#FF6B6B", font_size=28),
            MathTex(r"\text{TC} \uparrow \Rightarrow \text{Error} \downarrow", 
                   color="#00FF88", font_size=28),
            Text("• Guidance effect depends on personality traits", 
                font_size=26, color=WHITE),
            Text("• R² > 0.78 in all models (good fit)", 
                font_size=26, color=WHITE)
        ).arrange(DOWN, buff=0.2, aligned_edge=LEFT)
        results.next_to(results_title, DOWN, buff=0.3)
        
        self.play(Create(results_box), run_time=1)
        self.play(Write(results_title), run_time=0.8)
        self.play(LaggedStart(*[FadeIn(r, shift=RIGHT*0.2) for r in results], 
                             lag_ratio=0.3), run_time=2.5)
        self.wait(2)
        
        self.play(FadeOut(VGroup(title, model1_title, model1, model2_title, model2, 
                                results_box, results_title, results)))


# ==================== SCENE 5: WEEK 4 - POWER ANALYSIS (30s) ====================
class Scene5_PowerAnalysis(Scene):
    def construct(self):
        self.camera.background_color = "#0a0a0a"
        
        # Title
        title = Text("Week 4: Power Analysis", 
                    font_size=44, color=BLUE, weight=BOLD)
        title.to_edge(UP, buff=0.5)
        self.play(Write(title), run_time=1.5)
        
        # Cohen's d definition
        cohend_label = Text("Effect Size (Cohen's d):", font_size=32, color=YELLOW)
        cohend_label.next_to(title, DOWN, buff=0.6)
        
        cohend_formula = MathTex(
            r"d = \frac{\bar{x}_1 - \bar{x}_2}{s_{pooled}}",
            font_size=40
        )
        cohend_formula.next_to(cohend_label, DOWN, buff=0.4)
        
        self.play(Write(cohend_label), Write(cohend_formula), run_time=2)
        self.wait(1)
        
        # Observed effect sizes
        effects_title = Text("Observed Effect Sizes:", font_size=32, color=GREEN, weight=BOLD)
        effects_title.next_to(cohend_formula, DOWN, buff=0.7)
        
        effects = VGroup(
            MathTex(r"d_{STR} = 1.07", font_size=34, color="#4ECDC4"),
            MathTex(r"d_{LTR} = 1.77", font_size=34, color="#95E1D3"),
            MathTex(r"d_{training} > 3.6", font_size=34, color="#FFD700")
        ).arrange(DOWN, buff=0.3, aligned_edge=LEFT)
        effects.next_to(effects_title, DOWN, buff=0.4)
        
        self.play(Write(effects_title), run_time=1)
        self.play(LaggedStart(*[Write(eff) for eff in effects], lag_ratio=0.3), run_time=2)
        self.wait(1)
        
        # Power conclusion
        power_box = Rectangle(
            width=10, height=1.5, 
            stroke_color=GREEN, stroke_width=3, fill_color="#0a0a0a", fill_opacity=0.9
        )
        power_box.to_edge(DOWN, buff=0.7)
        
        power_text = Text(
            "Statistical Power: 100%\nSample size is sufficient for all conclusions",
            font_size=28, color=GREEN, weight=BOLD
        )
        power_text.move_to(power_box)
        
        self.play(Create(power_box), run_time=1)
        self.play(FadeIn(power_text, scale=1.1), run_time=1.5)
        self.wait(2)
        
        self.play(FadeOut(VGroup(title, cohend_label, cohend_formula, effects_title, 
                                effects, power_box, power_text)))


# ==================== SCENE 6: EXTENSION - TRAIT DEPENDENCE (30s) ====================
class Scene6_TraitDependence(Scene):
    def construct(self):
        self.camera.background_color = "#0a0a0a"
        
        # Title
        title = Text("Extension: Personality Trait Dependence", 
                    font_size=40, color=BLUE, weight=BOLD)
        title.to_edge(UP, buff=0.5)
        self.play(Write(title), run_time=1.5)
        
        # Two approaches
        approach1 = Text("1. Correlation Analysis", font_size=32, color=YELLOW, weight=BOLD)
        approach1.next_to(title, DOWN, buff=0.7)
        
        corr_finding = Text(
            "Strong positive correlation: AC ↔ FS (r = 0.538, p < 0.001)",
            font_size=26, color=WHITE
        )
        corr_finding.next_to(approach1, DOWN, buff=0.3)
        
        self.play(Write(approach1), run_time=1)
        self.play(FadeIn(corr_finding, shift=RIGHT*0.3), run_time=1.5)
        self.wait(1.5)
        
        # BMA
        approach2 = Text("2. Bayesian Model Averaging (BMA)", 
                        font_size=32, color=YELLOW, weight=BOLD)
        approach2.next_to(corr_finding, DOWN, buff=0.7)
        
        bma_findings = VGroup(
            Text("• FS best predicted by AC and TB", font_size=26, color=WHITE),
            Text("• TC shows weak dependence on other traits", font_size=26, color=WHITE),
            Text("• TB negatively associated with FS", font_size=26, color=WHITE)
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.25)
        bma_findings.next_to(approach2, DOWN, buff=0.3)
        
        self.play(Write(approach2), run_time=1)
        self.play(LaggedStart(*[FadeIn(f, shift=RIGHT*0.2) for f in bma_findings], 
                             lag_ratio=0.3), run_time=2)
        self.wait(2)
        
        # Conclusion
        conclusion = Text(
            "Personality traits are interconnected, not independent",
            font_size=28, color=GREEN, weight=BOLD
        )
        conclusion.to_edge(DOWN, buff=0.8)
        self.play(FadeIn(conclusion, shift=UP*0.3), run_time=1.5)
        self.wait(1.5)
        
        self.play(FadeOut(VGroup(title, approach1, corr_finding, approach2, 
                                bma_findings, conclusion)))


# ==================== SCENE 7: CONCLUSION (20s) ====================
class Scene7_Conclusion(Scene):
    def construct(self):
        self.camera.background_color = "#0a0a0a"
        
        # Title
        title = Text("Conclusions", font_size=52, color=BLUE, weight=BOLD)
        title.move_to(ORIGIN + UP*2)
        self.play(Write(title), run_time=1.5)
        self.wait(0.5)
        
        # Main findings
        findings = VGroup(
            Text("✓ Robot-assisted learning significantly reduces errors", 
                font_size=32, color=GREEN),
            Text("✓ Effect persists in long-term retention", 
                font_size=32, color=GREEN),
            Text("✓ Personality traits influence performance:", 
                font_size=32, color=YELLOW),
            Text("    • Free Spirit → higher errors", 
                font_size=28, color="#FF6B6B"),
            Text("    • Transform of Challenge → lower errors", 
                font_size=28, color="#00FF88"),
            Text("✓ Guidance effect depends on personality", 
                font_size=32, color=GREEN),
            Text("✓ Study is well-powered (100%)", 
                font_size=32, color=GREEN)
        ).arrange(DOWN, aligned_edge=LEFT, buff=0.3)
        findings.next_to(title, DOWN, buff=0.7)
        
        self.play(LaggedStart(*[FadeIn(f, shift=RIGHT*0.3) for f in findings], 
                             lag_ratio=0.2), run_time=4)
        self.wait(2)
        
        # Final message
        final = Text(
            "Statistical Analysis Complete",
            font_size=40, color=BLUE, weight=BOLD
        )
        final.to_edge(DOWN, buff=0.8)
        self.play(FadeIn(final, scale=1.2), run_time=1.5)
        self.wait(2)
        
        self.play(FadeOut(VGroup(title, findings, final)), run_time=1.5)


# ==================== RENDER ALL SCENES ====================
if __name__ == "__main__":
    # To render all scenes:
    # manim -pqh animation_complete.py Scene1_Introduction Scene2_PersonalityDistributions Scene3_LearningEffects Scene4_RegressionModels Scene5_PowerAnalysis Scene6_TraitDependence Scene7_Conclusion
    pass
