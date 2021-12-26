#include <iostream>
#include <fstream>
#include <vector>
#include <cassert>
#include <cstring>

#include "sfen_buffer.hpp"


//// Member functions

void SfenBufferedReader::initFileList(std::vector<std::string> &srcList) {
  fileList.clear();
  for (int i = 0; i < srcList.size(); i++) {
    fileList.push_back(std::string(srcList[i]));
  }
  clear();
}

void SfenBufferedReader::insertFileToList(std::string fileName) {
  fileList.push_back(fileName);
}

void SfenBufferedReader::initSingleFile(std::string fileName) {
  fileList.clear();
  fileList.push_back(fileName);
  clear();
}

void SfenBufferedReader::shuffleFileList() {

}

void SfenBufferedReader::shuffleBuffer() {
  int dataSize = remainingCnt();
  for (unsigned int i = 0; i < dataSize; i++) {
    uint32_t j = getRndIndex(i, dataSize - i);
    PackedSfenValue cur = data[j];
    data[j] = data[i];
  }
}

uint32_t SfenBufferedReader::getRndIndex(uint32_t start, uint32_t len) {
  if (len == 0) {
    return start;
  }
  uint64_t rnd = rand();
  rnd = (rnd << 15) | rand();
  rnd = (rnd << 15) | rand();
  rnd = (rnd << 15) | rand();
  rnd = (rnd % len) + start;
  return (uint32_t)rnd;
}


//// public members

void SfenBufferedReader::initBuffer() {
  data = new PackedSfenValue[BUFF_SIZE];
  head = 0;
  tail = 0;
}

SfenBufferedReader::SfenBufferedReader() {
  initBuffer(); // init buffer
  doBufferShuffle = false;
}

SfenBufferedReader::SfenBufferedReader(bool bufShuffle) {
  initBuffer(); // init buffer
  doBufferShuffle = bufShuffle;
}

SfenBufferedReader::~SfenBufferedReader() {
  free(data);
}


void SfenBufferedReader::clear() {
  curFileIdx = 0;
  inputStream.close();
  inputStream.clear();
  assert(fileList.size() > 0); // ensure that there are files to read
  openCurrentFile(); // open the first file to be ready
}

void SfenBufferedReader::openCurrentFile() {
  assert(curFileIdx < fileList.size());
  std::string fn = fileList[curFileIdx];
  inputStream.open(fn, std::ios::binary);
  //std::cout << "Open file " << fn << std::endl;
}

// return true if it reach the eof of train_fstrean
bool SfenBufferedReader::read_from_one_file(int requestSize, 
                                            std::vector<PackedSfenValue> &examples) {
  int doneCnt = 0;
  while (notReachingEOF()) {
    PackedSfenValue *p;
    if (readOneSfen(&p)) {
      examples.push_back(*p);
      doneCnt++;
	  } else {
      return true; // is eof
	  }
    
    if (doneCnt >= requestSize) {
      return false;
    }
  }
  return true; // if you reach here, it means while (train_fstrean) is false, then you reach eof
}

int SfenBufferedReader::read_one_mini_batch(int requestSize, 
                                            std::vector<PackedSfenValue> &examples) {

  examples.clear();

  while (curFileIdx < fileList.size()) {
    bool reachEof = read_from_one_file(requestSize - examples.size(), examples);
    if (reachEof) {
      // close current one
      inputStream.close();
      inputStream.clear();

      // prepare for next file
      curFileIdx++;
      if (curFileIdx < fileList.size()) {
        openCurrentFile();
      }
    }

    if (examples.size() >= requestSize) {
      break;
    }
  }

  //std::cout << "Reading " << examples.size() << " examples." << std::endl;
  return examples.size();
}

void SfenBufferedReader::fillBuffer() {
  assert(head >= tail);

  head = 0;
  tail = 0;
  while (!inputStream.eof()) {
    PackedSfenValue *ptr;
    ptr = data + tail;
    if (inputStream.read((char*)ptr, sizeof(PackedSfenValue))) {
      tail++;
      if (remainingCnt() >= BUFF_SIZE) {
        break;
      }
    } else {
      break;
    }
  }

  if (doBufferShuffle) {
    if (remainingCnt() > 0) {
      shuffleBuffer();
    }
  }

  //std::cout << "refilled buffer to size " << remainingCnt() << std::endl;
}

bool SfenBufferedReader::readOneSfen(PackedSfenValue **ptr) {
  if (head >= tail) {
    fillBuffer();
    if (head >= tail) {
      // nothing was read actually
      return false;
    }
  }

  *ptr = (data + head);
  head++;
  return true;
}

int SfenBufferedReader::remainingCnt() {
  assert(tail >= head);
  return (tail - head);
}

bool SfenBufferedReader::notReachingEOF() {
  if (!inputStream.eof()) {
    return true;
  } else { // already reached eof
    if (remainingCnt() > 0) {
      return true;
    }
    return false; // nothing to read anymore
  }
}
