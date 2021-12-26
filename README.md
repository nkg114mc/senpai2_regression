# senpai2-regression

[Senpai v2.0](http://www.amateurschach.de/main/_senpai.htm) is an open source chess engine released in 2017. Different from the v1.0, in v2.0 the evaluation function  has been completely parameterized. The current evaluation score is a simple linear model: each evaluation feature has a corresponding weight and then final evaluation will be this weighed sum over all the features. The original author of Senpai, Fabien, mentioned that the current weight hard-coded in Senpai code is trained from the logistic regression, but I am not exactly clear about the details of how Fabien performs this training. This project tries to provide a brute force training approach: directly run a linear regression with the training data automatically generated with the NNUE data-generation algorithm. 

### Compile

Current code are only developed and tested under Linux/Unix environment. To compile the code, ensure that the latest GCC has been installed and then run `make` under the project root folder. The exe file is still named `senpai`.

### Get Training Data

**NNUE sfen file as input**
senpai2-regression use the [NNUE](https://www.chessprogramming.org/Stockfish_NNUE) (Efficiently Updatable Neural Networks) training data as input. See [here](https://github.com/joergoster/Stockfish-NNUE) for more details about the training data generation approach in NNUE. 

**Pre-computed feature files as input**
Obviously, repeatedly decoding the chess position and re-computing the feature vectors in each epoch is purely a waste of time, because the feature vector will remain unchanged across epochs. Storing the featurization result in memory is impossible neither due to the huge number of training examples. In solve this problem, I designed a file format to store the feature vectors, so that the trainer will directly read the featurization results from disk rather than re-computed them again and again. 

You can run the following command to convert the original sfen file to feature files:
```
./senpai featurefilegen inputsfen /path/to/nnue-generated-sfen.bin multichunks multichunksfolder /path/to/my-feature-file-dir multichunksname my-feature-name chucksize 10000
```
In the command above, besides creating the feature files, this command will also split the total training set to several chunks (each chunk contains 10000 examples), so that the data shuffling can be approximately done by shuffling the data chunks.


### Run Linear Regression Training

To run training with the original NNUE sfen file as input training data, use the following style command:
```
./senpai train inputsfen trainsfen /path/to/nnue-generated-sfen.bin validsfen /path/to/my-validation-sfen.bin epoch 10 minbatch 50000 learningrate 0.0025 regularizer 0.005 helpers 4 doshuffle true

```
To run training with the pre-computed feature files as input, use the following style command:
```
./senpai train inputfeatbin trainlist /path/to/my-chunk-files-listfile.txt validsfen /path/to/my-validation-sfen.bin epoch 10 minbatch 50000 learningrate 0.0025 regularizer 0.005 helpers 4 doshuffle true
```
Please see more details about descriptions of options in the Option section. During training, the trainer will create a folder named `save`, and store the snapshot of weight vector after each epoch. There will be two files created after the X epoch: `weights_X.txt` which contains the weights in float type and `weights_int100_X.txt` which times the original weight with 100 and rounded to integer.

### How to Use Trained Weights

Open any `weights_int100_*.txt` file in the `save` folder, you will see the weight vector was stored as following:
```
	Score_Pair(20808,27737),
	Score_Pair(62983,63590),
	Score_Pair(75063,74186),
	...
	Score_Pair(6489,3939),
```
In the Senpai 2.0 source file `eval.cpp`, from line 76 to 836 we will see a similar hard-code weight vector. Replace that original weights with your the learned weights and recompile Senpai, then it is ready to use.

According to my test with 40moves/5mins time control, the current best-performed learned weight is still 60 ELO weaker than the original weight of Senpai. I will provide a more detailed report including all findings later.


### Options
