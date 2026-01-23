# Robot-Assisted Learning Animation

This folder contains a complete 3-5 minute animation explaining the entire statistical analysis project using dark-theme mathematics-style visualizations designed for voice-over.

## Project Overview

The animation covers:
1. **Introduction** (30s) - Research question and study design
2. **Week 1** (40s) - Personality trait distributions (AC, FS, TC, TB)
3. **Week 2** (50s) - Learning effects and group comparisons
4. **Week 3** (50s) - Regression models with personality traits
5. **Week 4** (30s) - Power analysis
6. **Extension** (30s) - Trait dependence analysis
7. **Conclusion** (20s) - Key findings summary

**Total Duration:** ~4 minutes

## Files

- `animation_complete.py` - Main animation file with all 7 scenes
- `render_animation.sh` - Shell script to render all scenes
- `animation_scene1.py` - Old test scene (can be deleted)

## How to Render

### Option 1: Render All Scenes at Once

```bash
chmod +x render_animation.sh
./render_animation.sh
```

### Option 2: Render Individual Scenes

Low quality (fast, for testing):
```bash
manim -pql animation_complete.py Scene1_Introduction
```

High quality (1080p60, for final version):
```bash
manim -pqh animation_complete.py Scene1_Introduction
manim -pqh animation_complete.py Scene2_PersonalityDistributions
manim -pqh animation_complete.py Scene3_LearningEffects
manim -pqh animation_complete.py Scene4_RegressionModels
manim -pqh animation_complete.py Scene5_PowerAnalysis
manim -pqh animation_complete.py Scene6_TraitDependence
manim -pqh animation_complete.py Scene7_Conclusion
```

4K quality (slower, highest quality):
```bash
manim -pqk animation_complete.py Scene1_Introduction
```

## Combining Scenes into One Video

After rendering all scenes, create `scenes_list.txt`:

```
file 'media/videos/animation_complete/1080p60/Scene1_Introduction.mp4'
file 'media/videos/animation_complete/1080p60/Scene2_PersonalityDistributions.mp4'
file 'media/videos/animation_complete/1080p60/Scene3_LearningEffects.mp4'
file 'media/videos/animation_complete/1080p60/Scene4_RegressionModels.mp4'
file 'media/videos/animation_complete/1080p60/Scene5_PowerAnalysis.mp4'
file 'media/videos/animation_complete/1080p60/Scene6_TraitDependence.mp4'
file 'media/videos/animation_complete/1080p60/Scene7_Conclusion.mp4'
```

Then concatenate:
```bash
ffmpeg -f concat -safe 0 -i scenes_list.txt -c copy final_animation.mp4
```

## Adding Voice-Over

The animation is designed with slow pacing and pauses for voice-over. To add voice-over:

1. Record audio narration following the on-screen text
2. Use video editing software (iMovie, DaVinci Resolve, Premiere Pro) to:
   - Import `final_animation.mp4`
   - Import your audio file
   - Sync audio with visuals
   - Adjust timing if needed

**Suggested narration script:**

### Scene 1: Introduction
"This project investigates whether robot-assisted learning can help patients with different personality traits reduce errors in a rehabilitation game. Our study involves 100 participants, split into control and experimental groups, measured across three stages: baseline, short-term retention, and long-term retention."

### Scene 2: Personality Distributions
"We measured four personality traits for each participant: Achiever, Free Spirit, Transform of Challenge, and Transform of Boredom. Statistical analysis revealed that each trait follows a specific distribution: Achiever fits a Weibull distribution, Free Spirit and Transform of Challenge follow Beta distributions, and Transform of Boredom is normally distributed. Kolmogorov-Smirnov tests confirmed good fit for all distributions."

### Scene 3: Learning Effects
"For each participant and stage, we computed the average absolute error across 40 target attempts. Our analysis revealed several key findings: errors reduce significantly after training, there was no baseline difference between groups confirming proper randomization, and the experimental group with robot guidance showed significantly lower errors in both short and long-term retention stages."

### Scene 4: Regression Models
"We built regression models to understand how personality traits influence error. Model 1 examines direct effects of each trait. Model 2 adds group interactions to see how guidance effects depend on personality. Results show that Free Spirit increases error, Transform of Challenge decreases error, and the effect of guidance varies by personality profile. All models achieved R-squared values above 0.78, indicating good fit."

### Scene 5: Power Analysis
"To validate our statistical conclusions, we performed power analysis using Cohen's d effect sizes. The observed effects are large: d equals 1.07 for short-term and 1.77 for long-term robot assistance, with training effects exceeding 3.6. With our sample of 50 participants per group, we achieve 100% statistical power, confirming our study is adequately sized for all conclusions."

### Scene 6: Extension
"As an extension, we analyzed dependence between personality traits. Correlation analysis revealed a strong positive relationship between Achiever and Free Spirit traits. Bayesian Model Averaging showed that Free Spirit is best predicted by Achiever and Transform of Boredom, while Transform of Challenge shows weak dependence on other traits. This indicates personality traits are interconnected rather than independent."

### Scene 7: Conclusion
"In conclusion, robot-assisted learning significantly reduces errors with effects persisting in long-term retention. Personality traits play a crucial role: Free Spirit predicts higher errors while Transform of Challenge predicts lower errors. The effectiveness of guidance depends on individual personality profiles. Our study is well-powered at 100%, giving us confidence in these findings. These results suggest personalized robot-assisted rehabilitation strategies based on personality assessment could optimize patient outcomes."

## Scene Timings

- Scene 1: ~30 seconds (Introduction)
- Scene 2: ~40 seconds (Personality Distributions)
- Scene 3: ~50 seconds (Learning Effects)
- Scene 4: ~50 seconds (Regression Models)
- Scene 5: ~30 seconds (Power Analysis)
- Scene 6: ~30 seconds (Trait Dependence)
- Scene 7: ~20 seconds (Conclusion)

**Total: ~4 minutes**

## Customization

To modify the animation:

1. Open `animation_complete.py`
2. Edit the `construct()` method of any scene class
3. Adjust `run_time` parameters to change animation speed
4. Modify `wait()` durations for longer/shorter pauses
5. Change colors by editing hex codes (e.g., `"#FFD700"` for gold)
6. Adjust font sizes for readability

## Color Scheme

- Background: `#0a0a0a` (near black)
- Title text: Blue
- Emphasis: Yellow, Green
- Trait colors:
  - AC (Achiever): Gold `#FFD700`
  - FS (Free Spirit): Mint `#00FF88`
  - TC (Transform of Challenge): Blue `#4A9EFF`
  - TB (Transform of Boredom): Pink `#FF6B9D`

## Requirements

- Python 3.9+
- Manim Community Edition
- FFmpeg (for combining videos)
- LaTeX (with packages: standalone, preview, amsmath, amssymb)

## Troubleshooting

**LaTeX errors:**
```bash
sudo tlmgr update --self
sudo tlmgr install standalone preview amsmath amssymb dvisvgm
```

**Slow rendering:**
Use `-pql` for low quality preview renders during development

**Memory issues:**
Render scenes individually rather than all at once

## Output Location

Rendered videos are saved in:
```
media/videos/animation_complete/1080p60/Scene*.mp4
```

## License

This animation is part of the TW2-21 Modelling 3 final project, Group 12.
Authors: Ansh Jakhari, Feodor Romanov, Markus Stenberg, Thijs Vissers
