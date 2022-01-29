#include <fstream>
#include <vector>
#include <string>


#ifndef PERF_HPP
#define PERF_HPP

void perftest_read_feature_single_bin(std::string fileName, int miniBatchSize);
void perftest_read_feature_block_bin(std::vector<std::string> trnList, int miniBatchSize);
void perftest_read_multifeature_single_bin(std::string fileName, int miniBatchSize);
void perftest_parallel_read_feature_block_bin(std::vector<std::string> trnList, 
											  int miniBatchSize,
											  int n_helpers);
void perftest_read_feature_sfen_bin(std::vector<std::string> trnList, int miniBatchSize);
void check_position_qsearch(std::string trainFile, int miniBatchSize);

#endif // #ifndef PERF_HPP

