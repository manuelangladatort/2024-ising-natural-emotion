import pandas as pd
import matplotlib.pyplot as plt
from music21 import stream, note
import os
import numpy as np

# Validate input file
input_file = "data-random_melodies-estiamted-key.csv"
if not os.path.exists(input_file):
    raise FileNotFoundError(f"Input file {input_file} not found")

# Load and validate dataset
df = pd.read_csv(input_file)
required_columns = ['sung_pitch1', 'sung_pitch2', 'sung_pitch3', 'sung_pitch4', 'sung_pitch5']
if not all(col in df.columns for col in required_columns):
    raise ValueError("Missing required columns in the dataset")

# Function to estimate key using music21
def estimate_key_m21(pitches):
    # Check if all pitches are NaN
    if all(pd.isna(p) for p in pitches):
        return None, None, None
        
    s = stream.Stream()
    for p in pitches:
        if pd.notna(p):
            # Validate MIDI pitch range
            midi_pitch = int(round(p))
            if not (0 <= midi_pitch <= 127):
                print(f"Warning: Invalid MIDI pitch {midi_pitch} found")
                continue
            s.append(note.Note(midi=midi_pitch))
    
    if len(s.notes) == 0:
        return None, None, None
        
    try:
        k = s.analyze('key')
        return k.tonic.name, k.mode, k.correlationCoefficient
    except Exception as e:
        print(f"Error analyzing key: {str(e)}")
        return None, None, None

# Apply to all melodies
results = df.apply(
    lambda row: estimate_key_m21([
        row['sung_pitch1'], row['sung_pitch2'],
        row['sung_pitch3'], row['sung_pitch4'], row['sung_pitch5']
    ]), axis=1)

# Add results to DataFrame
df['m21_key'], df['m21_mode'], df['m21_confidence'] = zip(*results)

# Create output directory for plots
os.makedirs('plots', exist_ok=True)

# Plot: Distribution of confidence scores
plt.figure(figsize=(10, 5))
plt.hist(df['m21_confidence'].dropna(), bins=25, color='skyblue', edgecolor='black')
plt.title("Distribution of Key Estimation Confidence Scores (music21)")
plt.xlabel("Correlation Coefficient")
plt.ylabel("Number of Melodies")
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('plots/confidence_distribution.png', dpi=300, bbox_inches='tight')
plt.close()

# Plot key frequency with improved visualisation
df['m21_key_full'] = df['m21_key'] + " " + df['m21_mode']
key_counts = df['m21_key_full'].value_counts()
plt.figure(figsize=(12, 6))
bars = plt.bar(key_counts.index, key_counts.values, color='coral', edgecolor='black')

# Add value labels on top of bars
for bar in bars:
    height = bar.get_height()
    plt.text(bar.get_x() + bar.get_width()/2., height,
             f'{int(height)}',
             ha='center', va='bottom')

plt.title("Distribution of Estimated Keys for Random Melodies (music21)")
plt.ylabel("Number of Melodies")
plt.xticks(rotation=45, ha='right')
plt.tight_layout()
plt.savefig('plots/key_distribution.png', dpi=300, bbox_inches='tight')
plt.close()

# Print summary statistics
print("\nKey Estimation Summary:")
print(f"Total melodies analyzed: {len(df)}")
print(f"Successfully analyzed: {df['m21_confidence'].notna().sum()}")
print(f"Average confidence: {df['m21_confidence'].mean():.3f}")
print("\nTop 5 most common keys:")
print(key_counts.head()) 