#include <iostream>
#include <fstream>
#include <vector>
#include <cassert>
#include <cstring>
#include <algorithm>
#include <random>

#include "readsfen.hpp"


#ifndef SFEN_BUFFER_HPP
#define SFEN_BUFFER_HPP

class SfenBufferedReader {
public:

  static const int BUFF_SIZE = 1000000;
  static const int SFEN_SIZE = (int)(sizeof(PackedSfenValue));
  std::ifstream inputStream;
  PackedSfenValue* data;
  int head, tail;

  // for multi-file mode
  int curFileIdx;
  std::vector<std::string> fileList;
  bool doBufferShuffle;

  void openCurrentFile();
  bool readOneSfen(PackedSfenValue **ptr);
  bool notReachingEOF();
  int remainingCnt();

  void initBuffer();
  void fillBuffer();
  void shuffleBuffer();
  uint32_t getRndIndex(uint32_t start, uint32_t len);

public:

  SfenBufferedReader();
  SfenBufferedReader(bool bufShuffle);
  ~SfenBufferedReader();

  void initFileList(std::vector<std::string> &srcList);
  void initSingleFile(std::string fileName);
  void insertFileToList(std::string fn);
  void shuffleFileList();
  void clear();

  // return true if it reach the eof of train_fstrean
  bool read_from_one_file(int requestSize, 
                          std::vector<PackedSfenValue> &examples);
  int read_one_mini_batch(int requestSize, 
                          std::vector<PackedSfenValue> &examples);

};

#endif // SFEN_BUFFER_HPP