# Best Learned Weight Vectors

### How to Use Trained Weights

#### Step 1: Replace the weight

Open any `weights_int100_*.txt` file in the `save` folder, you will see the weight vector was stored as following:
```
	Score_Pair(20808,27737),
	Score_Pair(62983,63590),
	Score_Pair(75063,74186),
	...
	Score_Pair(6489,3939),
```
In the Senpai 2.0 source file `eval.cpp`, from line 76 to 836 we will see a similar hard-code weight vector. Replace that original weights with your the learned weights and recompile Senpai.

#### Step 2: Change the `Scale` to `200`

In the source file `eval.cpp`, at line 30 you will see 
```
const int  Scale { 100 }; // units per cp
```
We need to change this line to
```
const int  Scale { 200 }; // units per cp
```

This is because the training data is generated from original Stockfish, which has a range of [-32000, +32000], while Senpai evaluation use a range of [-10000, +10000]. As a result, the computed evaluation score with our new learned weight is usually at least two times larger than the original Senpai score. To ensure that all other components involving the actual score values (e.g., aspiration window, futility pruning, delta pruning, etc.) work correctly, we need to shrink the computed score for its 0.5 scale to avoid unexpected ELO drops.

#### Step 3: Change the `Inf` to `32000`

In the source file `score.hpp`, at line 14 you will see
```
const Score Inf = Score(10000);
```
 then change it to 
```
const Score Inf = Score(32000);
```
due to the same reason as Step 2.

After doing the 3 steps above, the new weight is ready to use. According to my test with 40moves/5mins time control, the current best-performed learned weight (You can find them in the `best-learned-weights` folder) has a comparable strength against the original weight of Senpai. Please see more details in 

### Strengths

#### Under 40moves/5mins Time Control

| Senpai weights           | ELO  | Error   |
| :-------                 | :--- | :----:  |
| original-senpai2-weights | 0    |   -     |
| weights_int100_20220106  | -20  | $\pm$ 8 |
| weights_int100_20220123  | -4   | $\pm$ 7 |
