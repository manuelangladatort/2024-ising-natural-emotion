# Imports
import pandas as pd  # For data manipulation and analysis
import matplotlib.pyplot as plt  # For creating visualisations
from music21 import stream, note  # For music theory analysis
import os  # For file and directory operations
import numpy as np  # For numerical operations

# Load and validate the dataset containing random melodies
df = pd.read_csv("tonality-explore/data-sing-scales-v2_full-tonality.csv")

# Validate input file
input_file = "tonality-explore/data-sing-scales-v2_full-tonality.csv"
if not os.path.exists(input_file):
    raise FileNotFoundError(f"Input file {input_file} not found")

# Check if all required columns are present in the dataset
required_columns = ['sung_pitch1', 'sung_pitch2', 'sung_pitch3', 'sung_pitch4', 'sung_pitch5']
if not all(col in df.columns for col in required_columns):
    raise ValueError("Missing required columns in the dataset")

# Function to estimate the musical key of a sequence of pitches using music21
def estimate_key_m21(pitches):
    # Handle case where all pitches are missing (NaN)
    if all(pd.isna(p) for p in pitches):
        return None, None, None
        
    # Create a music21 stream to hold the notes
    s = stream.Stream()
    for p in pitches:
        if pd.notna(p):
            # Convert pitch to MIDI number and validate range
            midi_pitch = int(round(p))
            if not (0 <= midi_pitch <= 127):
                print(f"Warning: Invalid MIDI pitch {midi_pitch} found")
                continue
            s.append(note.Note(midi=midi_pitch))
    
    # Return None if no valid notes were added
    if len(s.notes) == 0:
        return None, None, None
        
    try:
        # Use music21's key analysis to determine the key
        k = s.analyze('key')
        # Return the tonic note, mode (major/minor), and confidence score
        return k.tonic.name, k.mode, k.correlationCoefficient
    except Exception as e:
        print(f"Error analyzing key: {str(e)}")
        return None, None, None

# Apply key estimation to each melody in the dataset
results = df.apply(
    lambda row: estimate_key_m21([
        row['sung_pitch1'], row['sung_pitch2'],
        row['sung_pitch3'], row['sung_pitch4'], row['sung_pitch5']
    ]), axis=1)

# Add the analysis results to the DataFrame
df['m21_key'], df['m21_mode'], df['m21_confidence'] = zip(*results)

# Create directory for saving plots if it doesn't exist
os.makedirs('plots', exist_ok=True)

# Create histogram of confidence scores
plt.figure(figsize=(10, 5))
plt.hist(df['m21_confidence'].dropna(), bins=25, color='skyblue', edgecolor='black')
plt.title("Distribution of Key Estimation Confidence Scores (music21)")
plt.xlabel("Correlation Coefficient")
plt.ylabel("Number of Melodies")
plt.grid(True, alpha=0.3)
plt.tight_layout()
plt.savefig('plots/confidence_distribution.png', dpi=300, bbox_inches='tight')
plt.close()

# Create bar chart of key distribution
df['m21_key_full'] = df['m21_key'] + " " + df['m21_mode']
key_counts = df['m21_key_full'].value_counts()
plt.figure(figsize=(12, 6))
bars = plt.bar(key_counts.index, key_counts.values, color='coral', edgecolor='black')

# Add count labels to each bar
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

# Save the complete results to a CSV file
output_file = "tonality-explore/key_estimation_results.csv"
df.to_csv(output_file, index=False)
print(f"\nResults exported to {output_file}")

# Print summary statistics of the analysis
print("\nKey Estimation Summary:")
print(f"Total melodies analyzed: {len(df)}")
print(f"Successfully analyzed: {df['m21_confidence'].notna().sum()}")
print(f"Average confidence: {df['m21_confidence'].mean():.3f}")
print("\nTop 5 most common keys:")
print(key_counts.head())