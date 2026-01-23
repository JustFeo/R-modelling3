#!/bin/bash
# Script to render all 7 scenes of the animation

echo "Rendering Robot-Assisted Learning Animation..."
echo "This will take several minutes. Each scene will be saved separately."
echo ""

cd "$(dirname "$0")"

# Quality: -pql (low), -pqm (medium), -pqh (high), -pqk (4K)
QUALITY="-pqh"

echo "Scene 1: Introduction..."
manim $QUALITY animation_complete.py Scene1_Introduction

echo "Scene 2: Personality Distributions..."
manim $QUALITY animation_complete.py Scene2_PersonalityDistributions

echo "Scene 3: Learning Effects..."
manim $QUALITY animation_complete.py Scene3_LearningEffects

echo "Scene 4: Regression Models..."
manim $QUALITY animation_complete.py Scene4_RegressionModels

echo "Scene 5: Power Analysis..."
manim $QUALITY animation_complete.py Scene5_PowerAnalysis

echo "Scene 6: Trait Dependence..."
manim $QUALITY animation_complete.py Scene6_TraitDependence

echo "Scene 7: Conclusion..."
manim $QUALITY animation_complete.py Scene7_Conclusion

echo ""
echo "✓ All scenes rendered!"
echo "Videos saved in: media/videos/animation_complete/1080p60/"
echo ""
echo "To combine all scenes into one video, use:"
echo "ffmpeg -f concat -safe 0 -i scenes_list.txt -c copy final_animation.mp4"
