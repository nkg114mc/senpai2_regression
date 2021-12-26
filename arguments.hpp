#include <string>
#include <vector>

#ifndef ARGUEMENTS_HPP
#define ARGUEMENTS_HPP


struct TrainingArguments {

    std::vector<std::string> trnList;
    std::string trnFn; // for old trainer only
    std::string valFn;

    int epochs;
    int miniBatchSize;
    double learningRate;
    double regLambda; // regularizer
    double nnueLambda; // nnue loss iterpolation
    bool doShuffle;
    int n_helpers;
    bool doFeatureNormalization;
};

#endif // #ifndef ARGUEMENTS_HPP
