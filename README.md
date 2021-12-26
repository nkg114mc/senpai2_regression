# senpai2-regression

[Senpai v2.0](http://www.amateurschach.de/main/_senpai.htm) is an open source chess engine released in 2017. Different from the v1.0, in v2.0 the evaluation function  has been completely parameterized. The current evaluation score is a simple linear model: each evaluation feature has a corresponding weight and then final evaluation will be this weighed sum over all the features. The original author of Senpai, Fabien, mentioned that the current weight hard-coded in Senpai code is trained from the logistic regression, but I am not exactly clear about the details of how Fabien performs this training. This project tries to provide a brute force training approach: directly run a linear regression with the training data automatically generated with the NNUE data-generation algorithm. 

### Compile

Current code are only developed and tested under Linux/Unix environment. To compile the code, ensure that the latest GCC has been installed and then run `make` under the project root folder. The exe file is still named `senpai`.

### Get Training Data

#### NNUE sfen file as input

senpai2-regression use the [NNUE](https://www.chessprogramming.org/Stockfish_NNUE) (Efficiently Updatable Neural Networks) training data as input. See [here](https://github.com/joergoster/Stockfish-NNUE) for more details about the training data generation approach in NNUE. 

#### Pre-computed feature files as input

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

According to my test with 40moves/5mins time control, the current best-performed learned weight (You can find them in the `best-learned-weights` folder) is still 60 ELO weaker than the original weight of Senpai. I will provide a more detailed report including all findings later.


### Options

#### Training Options
*  `train`: Run the training task.
*  `inputsfen SFEN_PATH`: The input sfen file `SFEN_PATH` as the training set.
*  `trainlist LIST_FILE`: The list file path `LIST_FILE` as the training set. The list file is a file that contains a list full path of chunk feature files, one path per line. The following is an example:
```
/full/path/to/feature-chunk-0.bin
/full/path/to/feature-chunk-1.bin
/full/path/to/feature-chunk-2.bin
...
/full/path/to/feature-chunk-320.bin
```
Note that you can choose only one of input mode between `inputsfen` and `trainlist`, but not both.
*  `validsfen SFEN_PATH`: The sfen file `SFEN_PATH` as the validation set.
*  `epoch MAX_EPOCHS`: The maximum number of epochs to run linear regression. Default `MAX_EPOCHS=200`.
*  `minbatch BATCH_SIZE`: The mini-batch size of linear regression. Default `BATCH_SIZE=40000`.
*  `learningrate LEARNING_RATE`: The learning rate of linear regression. Default `LEARNING_RATE=0.002`.
*  `regularizer REG_LAMDBA`: The L1 regularizer of linear regression. Default `REG_LAMDBA=0.001`.
*  `helpers N_THREADS`: The number of threads to run the inference in parallel. Maximum supported `N_THREADS` is 16.
*  `doshuffle BOOL`: Do data shuffle before each epoch or not. `BOOL` can be `true` /`false`. Default `true`.
*  `dofeaturenorm BOOL`: Do feature normalization or not. `BOOL` can be `true`/`false`. Default `false`.

#### Feature Converting Options

*  `featurefilegen`: Run feature converting task: converting a sfen file to one or multiple pre-computed feature files.
*  `inputsfen SFEN_PATH`: The input sfen file `SFEN_PATH` that need to be converted.

**Converting to a single feature file:**
*  `singlebin`: Convert the input sfen file to a single feature file.
*  `singlefilename SINGLE_FILE_PATH`: The single output file path `SINGLE_FILE_PATH`.

**Converting to multiple feature files:**
*  `multichunks`: Convert the input sfen file to multiple chunk feature files.
*  `multichunksfolder OUTPUT_DIR_PATH`: The output folder to store the chunk feature files.
*  `multichunksname FILE_NAME`: The file name of each chunk file. The final file name will be `FILE_NAME-N.bin`, where `N` is the chunk index.
*  `chucksize CHUNK_SIZE`: The number of examples in each chunk file.

#### Validation Options

*  `validationonly`: Run validation task: takes a sfen validation set and a weight file, compute and print the loss.
*  `validsfenfile SFEN_PATH`: The sfen file `SFEN_PATH` as the validation set.
*  `weightfile WEIGHT_PATH`: The weight file `WEIGHT_PATH` with name pattern "weights_int100_*.txt" (scaled and rounded weight). Do not use the float type weight file here.
*  `batchsize BATCH_SIZE`: Set the batch size of running validation to `BATCH_SIZE` (this option will not affects the loss value, so usually the default value is enough).
