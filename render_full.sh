#!/bin/bash
# Render the complete statistical analysis animation as a single MP4

echo "========================================="
echo "  Robot-Assisted Learning Animation"
echo "  Full Version with Real Mathematics"
echo "========================================="
echo ""
echo "This will render a ~5 minute video with:"
echo "  • Real distribution plots and fitted curves"
echo "  • Animated bar charts and scatter plots"
echo "  • Regression visualizations with data points"
echo "  • Cohen's d calculations with visuals"
echo "  • Correlation matrix heatmap"
echo ""
echo "Rendering in HIGH QUALITY (1080p60)..."
echo "This may take 10-15 minutes depending on your system."
echo ""

cd "$(dirname "$0")"

# Render the complete animation
manim -pqh full_animation.py CompleteStatisticalAnalysis --format=mp4

echo ""
echo "========================================="
echo "✓ RENDERING COMPLETE!"
echo "========================================="
echo ""
echo "Your video is saved at:"
echo "media/videos/full_animation/1080p60/CompleteStatisticalAnalysis.mp4"
echo ""
echo "To render in 4K (slower but highest quality):"
echo "  manim -pqk full_animation.py CompleteStatisticalAnalysis --format=mp4"
echo ""
echo "To render quickly for preview (480p):"
echo "  manim -pql full_animation.py CompleteStatisticalAnalysis --format=mp4"
echo ""
