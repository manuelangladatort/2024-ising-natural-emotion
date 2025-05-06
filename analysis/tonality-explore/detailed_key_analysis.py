import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from collections import Counter
import numpy as np

# Load the results
df = pd.read_csv("tonality-explore/key_estimation_results.csv")

# 1. Confidence Analysis
print("\n=== Confidence Analysis ===")
confidence_stats = df['m21_confidence'].describe()
print("Confidence Score Statistics:")
print(confidence_stats)

# Create confidence distribution plot with more bins
plt.figure(figsize=(12, 6))
sns.histplot(df['m21_confidence'], bins=50, kde=True)
plt.title("Detailed Distribution of Key Estimation Confidence")
plt.xlabel("Confidence Score")
plt.ylabel("Frequency")
plt.grid(True, alpha=0.3)
plt.savefig('plots/detailed_confidence_distribution.png', dpi=300, bbox_inches='tight')
plt.close()

# 2. Key Distribution Analysis
print("\n=== Key Distribution Analysis ===")
# Separate major and minor keys
df['is_major'] = df['m21_mode'] == 'major'
major_minor_counts = df['is_major'].value_counts()
print("\nMajor vs Minor Distribution:")
print(major_minor_counts)
print(f"Major/Minor Ratio: {major_minor_counts[True]/major_minor_counts[False]:.2f}")

# Create pie chart for major/minor distribution
plt.figure(figsize=(8, 8))
plt.pie(major_minor_counts, labels=['Major', 'Minor'], autopct='%1.1f%%', 
        colors=['#ff9999','#66b3ff'])
plt.title("Distribution of Major vs Minor Keys")
plt.savefig('plots/major_minor_distribution.png', dpi=300, bbox_inches='tight')
plt.close()

# 3. Tonic Note Analysis
print("\n=== Tonic Note Analysis ===")
tonic_counts = df['m21_key'].value_counts()
print("\nDistribution of Tonic Notes:")
print(tonic_counts)

# Create circular plot for tonic distribution
plt.figure(figsize=(10, 10))
angles = np.linspace(0, 2*np.pi, len(tonic_counts), endpoint=False)
radii = tonic_counts.values
plt.polar(angles, radii)
plt.xticks(angles, tonic_counts.index)
plt.title("Distribution of Tonic Notes (Circular Plot)")
plt.savefig('plots/tonic_distribution_circular.png', dpi=300, bbox_inches='tight')
plt.close()

# 4. Confidence by Key Analysis
print("\n=== Confidence by Key Analysis ===")
confidence_by_key = df.groupby('m21_key_full')['m21_confidence'].mean().sort_values(ascending=False)
print("\nAverage Confidence by Key:")
print(confidence_by_key.head(10))

# Create heatmap of confidence by key
plt.figure(figsize=(12, 8))
sns.boxplot(x='m21_key_full', y='m21_confidence', data=df)
plt.xticks(rotation=45, ha='right')
plt.title("Confidence Distribution by Key")
plt.tight_layout()
plt.savefig('plots/confidence_by_key.png', dpi=300, bbox_inches='tight')
plt.close()

# 5. Correlation Analysis
print("\n=== Correlation Analysis ===")
# Create a correlation matrix between key features
key_features = pd.get_dummies(df[['m21_key', 'm21_mode']])
correlation_matrix = key_features.corr()
print("\nFeature Correlation Matrix:")
print(correlation_matrix)

# 6. Statistical Tests
print("\n=== Statistical Tests ===")
from scipy import stats

# Test if major and minor keys have different confidence distributions
major_conf = df[df['is_major']]['m21_confidence']
minor_conf = df[~df['is_major']]['m21_confidence']
t_stat, p_value = stats.ttest_ind(major_conf, minor_conf)
print(f"\nT-test between major and minor key confidence:")
print(f"t-statistic: {t_stat:.3f}")
print(f"p-value: {p_value:.3f}")

# Save detailed analysis results
with open('tonality-explore/detailed_analysis_results.txt', 'w') as f:
    f.write("=== Detailed Key Analysis Results ===\n\n")
    f.write("Confidence Analysis:\n")
    f.write(str(confidence_stats))
    f.write("\n\nMajor vs Minor Distribution:\n")
    f.write(str(major_minor_counts))
    f.write("\n\nTonic Note Distribution:\n")
    f.write(str(tonic_counts))
    f.write("\n\nConfidence by Key:\n")
    f.write(str(confidence_by_key))
    f.write("\n\nStatistical Tests:\n")
    f.write(f"T-test between major and minor key confidence:\n")
    f.write(f"t-statistic: {t_stat:.3f}\n")
    f.write(f"p-value: {p_value:.3f}\n")

print("\nDetailed analysis complete. Results saved to 'tonality-explore/detailed_analysis_results.txt'") 