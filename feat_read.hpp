#include <fstream>

#include "readsfen.hpp"

#ifndef FEAT_READ_HPP
#define FEAT_READ_HPP


class FileReadBuffer {
private:

    static const int BUFFER_SIZE = 65536;
    std::ifstream* infs_ptr;
    char data[BUFFER_SIZE];
    char* head;
    char* tail;
    int maxBlockSize;

    void init();

public:

    void fillBuffer();
    void setFile(std::ifstream *ptr);
    bool readBytes(char * dest, int nBytes);
    bool notReachingEOF();
    int remainingByteCnt();
};


#define USE_FILE_BUFFER

class BinFeatureReader {
public:
    void initFileList(std::vector<std::string> &srcList);
    void initSingleFile(std::string fileName);
    void insertFileToList(std::string fn);
    void shuffleFileList();
    void clear();

    //bool readBytes(char * dest, int nBytes);
    //bool notReachingEOF();

    inline bool readBytes(char * dest, int nBytes) {
    #ifdef USE_FILE_BUFFER
        return buffer.readBytes(dest, nBytes);
    #else
        return (bool)inputStream.read(dest, nBytes);
    #endif
    }

    inline bool notReachingEOF() {
    #ifdef USE_FILE_BUFFER
        return buffer.notReachingEOF();
    #else
        return (bool)(!inputStream.eof());
    #endif
    }

    // return true if it reach the eof of train_fstrean
    bool read_from_one_file(int requestSize, 
                            std::vector<FeatureAndFactor> &examples);
    int read_one_mini_batch(int requestSize, 
                            std::vector<FeatureAndFactor> &examples);
    

    // statics
    bool read_single_binary_feat(FeatureAndFactor &example);

private:

    int curFileIdx;
    std::ifstream inputStream;
    std::vector<std::string> fileList;

    FileReadBuffer buffer;

    char myBuff[65536];
    void openCurrentFile();
};

#endif // #ifndef FEAT_READ_HPP