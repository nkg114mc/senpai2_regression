#include <iostream>
#include <fstream>
#include <vector>
#include <cassert>
#include <cstring>

#include "feat_read.hpp"


bool BinFeatureReader::read_single_binary_feat(FeatureAndFactor &example) {

    const uint16_t mask = 0x3FFF;
    
    bool ok = false;
    example.sv.clear();

    while (notReachingEOF()) {
        uint16_t flagAndIdx;
        ok = readBytes((char*)&flagAndIdx, sizeof(uint16_t));
        if (!ok) {
            break;
        }

        uint16_t flag = (flagAndIdx) >> 14;
        uint16_t idx = (mask & flagAndIdx);

        if (idx >= WEIGHT_DIM) {
            //assert(flag == 2);
            
            // flag is game_result
            assert(flag == 0 || flag == 1 || flag == 2);
            int game_result = flag - 1;
            
            int16_t score;
            uint8_t mg_fac;
            uint8_t eg_fac;

            ok = readBytes((char*)&score, sizeof(int16_t));
            ok = readBytes((char*)&mg_fac, sizeof(uint8_t));
            ok = readBytes((char*)&eg_fac, sizeof(uint8_t));
            if (!ok) {
                break;
            }

            example.mg_factor = mg_fac;
            example.eg_factor = eg_fac;
            example.score = score;
            example.result = game_result;

            ok = true;
            break;

        } else {
            double value;
            if (flag == 0) {
                float fVal;
                ok = readBytes((char*)&fVal, sizeof(float));
                if (!ok) {
                    break;
                }
                value = (double) fVal;
            } else if (flag == 1) {
                int8_t b8;
                ok = readBytes((char*)&b8, sizeof(int8_t));
                if (!ok) {
                    break;
                }
                value = (double) b8;
            } else if (flag == 2) {
                int16_t b16;
                ok = readBytes((char*)&b16, sizeof(int16_t));
                if (!ok) {
                    break;
                }
                value = (double) b16;
            } else if (flag == 3) {
                int32_t b32;
                ok = readBytes((char*)&b32, sizeof(int32_t));
                if (!ok) {
                    break;
                }
                value = (double) b32;
            } else {
                std::cerr << "Error: unknown flag = " << flag << std::endl;
                return false;
            }
            example.sv.insert(idx, value);
        }
    }

    return ok;
}
/*
int BinFeatureReader::read_binary_mini_batch(std::ifstream &train_fstream, 
                                             std::vector<FeatureAndFactor> &examples,
                                             int miniBatchSize) {
    examples.clear();

	while (train_fstream) {
        FeatureAndFactor feat;
	    if (read_single_binary_feat(train_fstream, feat)) {
            examples.push_back(feat);
		} else {
			break;
		}

        if (examples.size() >= miniBatchSize) {
            break;
        }
    }

    //std::cout << "Reading " << examples.size() << " examples." << std::endl;
    return examples.size();
}
*/

//// Member functions

void BinFeatureReader::initFileList(std::vector<std::string> &srcList) {
    fileList.clear();
    for (int i = 0; i < srcList.size(); i++) {
        fileList.push_back(std::string(srcList[i]));
    }
    clear();
}

void BinFeatureReader::insertFileToList(std::string fileName) {
    fileList.push_back(fileName);
}

void BinFeatureReader::initSingleFile(std::string fileName) {
    fileList.clear();
    fileList.push_back(fileName);
    clear();
}

void BinFeatureReader::shuffleFileList() {

}

void BinFeatureReader::clear() {
    curFileIdx = 0;
    inputStream.close();
    inputStream.clear();
    assert(fileList.size() > 0); // ensure that there are files to read
    openCurrentFile(); // open the first file to be ready
}

void BinFeatureReader::openCurrentFile() {
    assert(curFileIdx < fileList.size());
    std::string fn = fileList[curFileIdx];
    inputStream.open(fn, std::ios::binary);
    //std::cout << "Open file " << fn << std::endl;

	std::filebuf* buf_ptr = inputStream.rdbuf();
    buf_ptr->pubsetbuf(myBuff, 65536);

    // for our own reader buffer
    buffer.setFile(&inputStream);
}

// return true if it reach the eof of train_fstrean
bool BinFeatureReader::read_from_one_file(int requestSize, 
                                          std::vector<FeatureAndFactor> &examples) {
    int doneCnt = 0;

    //while (train_fstream) {
    while (notReachingEOF()) {
        FeatureAndFactor feat;
	    //if (BinFeatureReader::read_single_binary_feat(train_fstream, feat)) {
        if (read_single_binary_feat(feat)) {
            examples.push_back(feat);
            //assert(buffer.remainingByteCnt() == 0);
            //std::cout << feat.result << " " << feat.score << std::endl;
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

int BinFeatureReader::read_one_mini_batch(int requestSize, 
                                          std::vector<FeatureAndFactor> &examples) {

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



//// FileReadBuffer

void FileReadBuffer::init() {
    head = data;
    tail = data;
    maxBlockSize = 0;
}

void FileReadBuffer::fillBuffer() {
    // load a memory block
    uint16_t byteCnt = 0;
    bool sizeOk = (bool)infs_ptr->read((char*)&byteCnt, sizeof(uint16_t));
    if (sizeOk && byteCnt > 0) {
        infs_ptr->read(data, byteCnt); // load a block of data
        head = data;
        tail = head + byteCnt;
        /*
        if (maxBlockSize < byteCnt) {
            maxBlockSize = byteCnt;
            std::cout << "max loaded buffer block size = " << maxBlockSize << std::endl;
        }
        */
    }
}

void FileReadBuffer::setFile(std::ifstream *ptr) {
    assert(ptr != nullptr);
    infs_ptr = ptr;
    init();
}

bool FileReadBuffer::readBytes(char* dest, int nBytes) {
    // in case buffer is empty
    //assert(head <= tail);
    if (head >= tail) {
        fillBuffer();
    }

    if ((head + nBytes) > tail) {
        nBytes = tail - head;
        memcpy(dest, head, nBytes);
        head += nBytes;
        return false;
    }
    memcpy(dest, head, nBytes);
	head += nBytes;
    return true;
}

int FileReadBuffer::remainingByteCnt() {
    //assert(tail >= head);
    return (tail - head);
}

bool FileReadBuffer::notReachingEOF() {
    if (!infs_ptr->eof()) {
        return true;
    } else { // already reached eof
        if (remainingByteCnt() > 0) {
            return true;
        }
        return false; // nothing to read anymore
    }
}
