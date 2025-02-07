import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from tqdm.notebook import tqdm  # For Jupyter notebook progress bars

class TinderSimulation:
    def __init__(self, n_males=1650, n_females=1350, des_matches_mean=7, 
                 des_matches_sd=2, expected_success_rate=0.1, max_likes=700):
        self.n_males = n_males
        self.n_females = n_females
        self.max_likes = max_likes
        
        # Initialize population dataframe
        n_total = n_males + n_females
        self.df = pd.DataFrame({
            'id': range(1, n_total + 1),
            'sex': ['M'] * n_males + ['F'] * n_females,
            'des_matches': np.maximum(
                np.random.normal(des_matches_mean, des_matches_sd, n_total), 
                1
            )
        })
        
        # Calculate initial number of likes based on expected success rate
        self.df['n_given_likes'] = np.round(
            self.df['des_matches'] / expected_success_rate
        ).astype(int)
        self.df['n_matches'] = np.nan
        
        # Store IDs by sex for easy access
        self.male_ids = self.df[self.df['sex'] == 'M']['id'].values
        self.female_ids = self.df[self.df['sex'] == 'F']['id'].values
        
        # Store results
        self.results = []

    def simulate_week(self):
        """Simulate one week of Tinder activity"""
        # Males give likes to females
        likes_men_to_women = {
            mid: np.random.choice(
                self.female_ids, 
                size=min(self.df.loc[mid-1, 'n_given_likes'], len(self.female_ids)),
                replace=False
            ) for mid in self.male_ids
        }
        
        # Females give likes to males
        likes_women_to_men = {
            fid: np.random.choice(
                self.male_ids,
                size=min(self.df.loc[fid-1, 'n_given_likes'], len(self.male_ids)),
                replace=False
            ) for fid in self.female_ids
        }
        
        # Calculate matches
        matches = []
        for mid in self.male_ids:
            # Women who liked this male
            women_likes = [fid for fid, male_likes in likes_women_to_men.items() 
                         if mid in male_likes]
            # Check if male also liked these women
            for fid in women_likes:
                if fid in likes_men_to_women[mid]:
                    matches.append((mid, fid))
        
        # Count matches per person
        match_counts = pd.DataFrame(matches).value_counts()
        self.df['n_matches'] = 0
        for idx in match_counts.index:
            self.df.loc[idx[0]-1, 'n_matches'] += 1
            self.df.loc[idx[1]-1, 'n_matches'] += 1
        
        # Update strategy for next week
        self.update_strategy()
        
        # Store current state
        self.results.append(self.df.copy())
    
    def update_strategy(self):
        """Update number of likes for next week based on match results"""
        # No matches: double likes
        mask_no_matches = self.df['n_matches'] == 0
        self.df.loc[mask_no_matches, 'n_given_likes'] *= 2
        
        # Some matches: adjust proportionally
        mask_some_matches = ~mask_no_matches
        self.df.loc[mask_some_matches, 'n_given_likes'] = np.minimum(
            np.round(
                self.df.loc[mask_some_matches, 'n_given_likes'] / 
                self.df.loc[mask_some_matches, 'n_matches'] * 
                self.df.loc[mask_some_matches, 'des_matches']
            ),
            self.df.loc[mask_some_matches, 'n_given_likes'] * 2
        )
        
        # Ensure we don't exceed maximum likes
        self.df['n_given_likes'] = np.minimum(
            self.df['n_given_likes'], 
            self.max_likes
        ).astype(int)
    
    def run_simulation(self, n_weeks=24):
        """Run the simulation for specified number of weeks"""
        for _ in tqdm(range(n_weeks), desc='Simulating weeks'):
            self.simulate_week()
        return self.get_results()
    
    def get_results(self):
        """Combine results from all weeks into a single DataFrame"""
        result_df = pd.concat(
            [df.assign(week=i+1) for i, df in enumerate(self.results)],
            ignore_index=True
        )
        return result_df
    
    def plot_results(self):
        """Plot boxplots of given likes by sex and week"""
        results = self.get_results()
        
        # Create subplot for each week
        n_weeks = results['week'].nunique()
        fig, axes = plt.subplots(n_weeks, 1, figsize=(10, 4*n_weeks))
        
        for week, ax in zip(sorted(results['week'].unique()), axes):
            week_data = results[results['week'] == week]
            sns.boxplot(data=week_data, x='sex', y='n_given_likes', ax=ax)
            ax.set_title(f'Week {week}')
        
        plt.tight_layout()
        plt.show()

# Example usage
if __name__ == "__main__":
    sim = TinderSimulation()
    results = sim.run_simulation(n_weeks=24)
    sim.plot_results()
