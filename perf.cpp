#include <iostream>
#include <sstream>
#include <string>
#include <cctype>
#include <fstream>
#include <cstring>
#include <vector>
#include <condition_variable>
#include <mutex>
#include <thread>
#include <atomic>
#include <algorithm>

#include "readsfen.hpp"
#include "feat_read.hpp"
#include "sfen_buffer.hpp"


struct DummyCache {
	char data[2000];
	char *head;

	int read32(char *ptr) {
		memcpy(ptr, head, 4);
		head += 4;
	}
	int read16(char *ptr) {
		memcpy(ptr, head, 2);
		head += 2;
	}

	bool readBytes(char *ptr, int n) {
		memcpy(ptr, head, n);
		head += n;
		return true;
	}

	bool read_single_binary_feat(FeatureAndFactor &example) {

		const uint16_t mask = 0x3FFF;
		
		bool ok = false;
		example.sv.clear();

		while (true) {
			uint16_t flagAndIdx;
			ok = readBytes((char*)&flagAndIdx, sizeof(uint16_t));
			if (!ok) {
				break;
			}

			uint16_t flag = (flagAndIdx) >> 14;
			uint16_t idx = (mask & flagAndIdx);

			if (idx >= WEIGHT_DIM) {
				assert(flag == 2);
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

				ok = true;
				break;

			} else {
				double value;
				switch (flag) {
					case 0: {
						float fVal;
						ok = readBytes((char*)&fVal, sizeof(float));
						if (!ok) {
							break;
						}
						value = (double) fVal;
						break;
					} 
					case 1: {
						int8_t b8;
						ok = readBytes((char*)&b8, sizeof(int8_t));
						if (!ok) {
							break;
						}
						value = (double) b8;
						break;
					} 
					case 2: {
						int16_t b16;
						ok = readBytes((char*)&b16, sizeof(int16_t));
						if (!ok) {
							break;
						}
						value = (double) b16;
						break;
					} 
					case 3: {
						int32_t b32;
						ok = readBytes((char*)&b32, sizeof(int32_t));
						if (!ok) {
							break;
						}
						value = (double) b32;
						break;
					} 
					default: {
						std::cerr << "Error: unknown flag = " << flag << std::endl;
						return false;
					}
				}
				example.sv.insert(idx, value);
			}
		}

		return ok;
	}
};

void perftest_read_feature_block_bin(std::vector<std::string> trnList, int miniBatchSize) {

	BinFeatureReader reader;
	reader.initFileList(trnList);

	UtilTimer timer;

	int cnt = 0;
	std::vector<FeatureAndFactor> examples;
	
	int t1 = 0;
    UtilTimer::time_t start_iter = UtilTimer::now();


	examples.clear();
	int readCnt = 0;
	do {
		readCnt = reader.read_one_mini_batch(miniBatchSize, examples);
		cnt += readCnt;
        if (examples.size() >= miniBatchSize) {
            examples.clear();
        }
    } while (readCnt > 0);

	t1 += (timer.time(start_iter));
    std::cout << "Reading " << cnt << " examples." << std::endl;
	std::cout << "Time spend: " << t1 << std::endl;
}


void perftest_read_feature_single_bin(std::string fileName, int miniBatchSize) {
	
	UtilTimer timer;
	BinFeatureReader reader;
	reader.initSingleFile(fileName);

	int cnt = 0;
	std::vector<FeatureAndFactor> examples;

	int t1 = 0;
    UtilTimer::time_t start_iter = UtilTimer::now();


	examples.clear();
	
	while (reader.notReachingEOF()) {
        FeatureAndFactor feat;
	    if (reader.read_single_binary_feat(feat)) {
            examples.push_back(feat);
			cnt++;
		} else {
			break;
		}

        if (examples.size() >= miniBatchSize) {
            examples.clear();
        }
    }

	t1 += (timer.time(start_iter));
    std::cout << "Reading " << cnt << " examples." << std::endl;
	std::cout << "Time spend: " << t1 << std::endl;
}

void perftest_read_multifeature_single_bin(std::string fileName, int miniBatchSize) {
	
	DummyCache dummy;
	UtilTimer timer;
	std::ifstream inf;
	inf.open(fileName, std::ios::binary);

	int maxByteCnt = 0;
	int cnt = 0;
	std::vector<FeatureAndFactor> examples;

	int t1 = 0;
    UtilTimer::time_t start_iter = UtilTimer::now();


	examples.clear();
	
	while (!inf.eof()) {
        FeatureAndFactor feat;

		uint16_t byteCnt = 0;
		if (!inf.read((char*)&byteCnt, sizeof(uint16_t))) {
			break;
		}

		if (maxByteCnt < byteCnt) {
			maxByteCnt = byteCnt;
		}

		if (inf.read((char*)&dummy, byteCnt)) {
			dummy.head = dummy.data;
			/*
			for (int i = 0; i < 25; i++) {
				int a, b1, b2;
				dummy.read32((char*)&a);
				dummy.read16((char*)&b1);
				dummy.read16((char*)&b2);
			}*/
			bool ok = dummy.read_single_binary_feat(feat);
			assert(ok);
            examples.push_back(feat);
			cnt++;
		} else {
			break;
		}

        if (examples.size() >= miniBatchSize) {
            examples.clear();
        }
    }

	inf.close();


	t1 += (timer.time(start_iter));
    std::cout << "Reading " << cnt << " examples." << std::endl;
	std::cout << "Time spend: " << t1 << std::endl;

	std::cout << "Max byte count: " << maxByteCnt << std::endl;
}


struct PerfTask {
	// input
	int idx;
	int requestBatchSize;
	std::vector<std::string> fileList;

	// output
	uint64_t cnt;
	int t1;
};

// main work function
void perf_thread_working(PerfTask * task) {
    
	UtilTimer timer;
    uint64_t cnt = 0;

	int t1;
    UtilTimer::time_t start_t1 = UtilTimer::now();

	BinFeatureReader reader;
	reader.initFileList(task->fileList);
    
	// read examples
    std::vector<FeatureAndFactor> examples;
	examples.clear();

	int readCnt = 0;
	do {
		readCnt = reader.read_one_mini_batch(task->requestBatchSize, examples);
		cnt += readCnt;
        if (examples.size() >= task->requestBatchSize) {
            examples.clear();
        }
    } while (readCnt > 0);

	t1 += (timer.time(start_t1));

	task->cnt = cnt;
	task->t1 = t1;
}


void perftest_parallel_read_feature_block_bin(std::vector<std::string> trnList, 
											  int miniBatchSize,
											  int n_helpers) {
	assert(n_helpers >= 1 && n_helpers <= 16);

	UtilTimer timer;

	std::thread all_threads[16];
	PerfTask tasks[16];

	
	int t1 = 0;
    UtilTimer::time_t start_iter = UtilTimer::now();

	// distribute files
    for (int i = 0; i < trnList.size(); i++) {
      int j = i % n_helpers;
      tasks[j].fileList.push_back(trnList[i]);
    }

    for (int j = 0; j < n_helpers; j++) {
    	std::cout << "Init helper[" << j << "] file: " << std::endl;
    	for (int i = 0; i < tasks[j].fileList.size(); i++) {
    		std::cout << "\t" << (tasks[j].fileList[i]) << std::endl;
    	}
    }

	// distribute task
    int eachCnt = miniBatchSize / n_helpers;
    int totalCnt = 0;
    for (int i = 0; i < n_helpers; i++) {
		int requestCnt = eachCnt; // the number of examples/feature_rows need to be read from file
        if (i == (n_helpers - 1)) { // last one
        	requestCnt = miniBatchSize - totalCnt;
        }
		tasks[i].idx = i;
		tasks[i].requestBatchSize = requestCnt;
		tasks[i].cnt = 0;
		totalCnt += requestCnt;
	}

	// start threads
    for (int i = 0; i < n_helpers; i++) {
		all_threads[i] = std::thread(perf_thread_working, &(tasks[i]));
    }

	// wait all threads stop
	for (int i = 0; i < n_helpers; i++) {
		std::cout << "Worker[" << i << "] is waiting." << std::endl;
		all_threads[i].join();
		std::cout << "Worker[" << i << "] done." << std::endl;
    }

	// count examples
	uint64_t cnt = 0;
	for (int i = 0; i < n_helpers; i++) {
		std::cout << "Worker[" << i << "] reads " << tasks[i].cnt << " examples." << std::endl;
		std::cout << "Worker[" << i << "] time " << tasks[i].t1 << std::endl;
		cnt += tasks[i].cnt;
	}


	t1 += (timer.time(start_iter));
    std::cout << "Reading " << cnt << " examples." << std::endl;
	std::cout << "Time spend: " << t1 << std::endl;
}


void perftest_read_feature_sfen_bin(std::vector<std::string> trnList, 
																		int miniBatchSize) {

	UtilTimer timer;
	SfenBufferedReader reader(true);
	reader.initFileList(trnList);

	int cnt = 0;
	std::vector<PackedSfenValue> examples;

	int t1 = 0;
  UtilTimer::time_t start_iter = UtilTimer::now();

	examples.clear();

	int readCnt = 0;
	do {
		readCnt = reader.read_one_mini_batch(miniBatchSize, examples);
		cnt += readCnt;
    if (examples.size() >= miniBatchSize) {
      examples.clear();
    }
  } while (readCnt > 0);

	t1 += (timer.time(start_iter));
  std::cout << "Reading " << cnt << " sfen examples." << std::endl;
	std::cout << "Time spend: " << t1 << std::endl;
}


int EvalTest(char *filename)
{
	std::ifstream fenfile;
	int err = 0, val, i;
	std::string fenstr;

   clear_pawn_table();
   Pos board;

	fenfile.open(filename);
	if (err != 0) {
		printf("Fen file not found!\n");
		return -1;
	}

	FILE *logf;
	logf = fopen("senp2evtest.log", "w");
	while (!fenfile.eof()) {
		getline(fenfile, fenstr);
      if (fenstr != "") {
         board = pos_from_fen(fenstr);
         val = (int)eval(board, board.turn());

         for (i = 0; i < fenstr.length(); i++) {
            if (fenstr[i] == '\n') fenstr[i] = '\0';
         }

         //std::cout << fenstr << " " << val << std::endl;
         fprintf(logf, "%s %d\n", fenstr.c_str(), val);
      }
	}
	printf("Test finished.\n");
	fclose(logf);
	fenfile.close();
	return 0;
}

void test_read_sfen(std::string input_filename) {

   read_sfen_file(input_filename);

}

void test_multifile_reader() {
   std::vector<std::string> trnList{
      "data_folder/train100m_split16/feat3-0.bin",
      "data_folder/train100m_split16/feat3-10.bin",
      "data_folder/train100m_split16/feat3-11.bin",
      "data_folder/train100m_split16/feat3-12.bin",
      "data_folder/train100m_split16/feat3-13.bin",
      "data_folder/train100m_split16/feat3-14.bin",
      "data_folder/train100m_split16/feat3-15.bin",
      "data_folder/train100m_split16/feat3-1.bin",
      "data_folder/train100m_split16/feat3-2.bin",
      "data_folder/train100m_split16/feat3-3.bin",
      "data_folder/train100m_split16/feat3-4.bin",
      "data_folder/train100m_split16/feat3-5.bin",
      "data_folder/train100m_split16/feat3-6.bin",
      "data_folder/train100m_split16/feat3-7.bin",
      "data_folder/train100m_split16/feat3-8.bin",
      "data_folder/train100m_split16/feat3-9.bin"
   };

   std::vector<FeatureAndFactor> examples;

   BinFeatureReader reader;
   reader.initFileList(trnList);
   reader.clear();
   int cnt = reader.read_one_mini_batch(200000, examples); 
   std::cout << cnt << std::endl;
}

void check_position_qsearch(std::string trainFile, int miniBatchSize) {

    SfenBufferedReader reader(false); // reader only do shuffle in each buffer
    reader.initSingleFile(trainFile);

    int64_t totalCnt = 0;
    int actualBatchSize = 0;
	int64_t quietCnt = 0;
	int64_t nonCheckCnt = 0;
	int64_t bothCnt = 0;
    Pos pos;

    do {

		std::vector<PackedSfenValue> trainExamples;
		actualBatchSize = reader.read_one_mini_batch(miniBatchSize, trainExamples);
        totalCnt += actualBatchSize;

        if (actualBatchSize > 0) {
			for (int i = 0; i < trainExamples.size(); i++) {
				pos_from_sfen(pos, trainExamples[i].sfen, false);
				Move bestMove = sfen_move_to_senp_move(trainExamples[i].move);
				bool nonCapture = false;
				bool nonCheck = false;
				
				if (!move_is_capture_or_promote(bestMove, pos)) {
					quietCnt++;
					nonCapture = true;
				}/* else {
					std::string fenStr = pos_to_fen(pos);
					std::string moveStr = move::to_uci(bestMove, pos);

					std::cout << fenStr << " " << moveStr << std::endl;
				}*/
				if (!in_check(pos, pos.turn())) {
					nonCheckCnt++;
					nonCheck = true;
				}/* else {
					std::string fenStr = pos_to_fen(pos);
					std::cout << fenStr << std::endl;
				}*/

				if (nonCapture && nonCheck) {
					bothCnt++;
				}
			}
			trainExamples.clear();
        }

    } while (actualBatchSize > 0);

	std::cout << "Example count = " << totalCnt << std::endl;
	std::cout << "NonCapture count = " << quietCnt << std::endl;
	std::cout << "NonCheck count = " << nonCheckCnt << std::endl;
	std::cout << "Both count = " << bothCnt << std::endl;

	double ratio = ((double)quietCnt) / ((double)totalCnt);
	std::cout << "NonCapture percent = " << ratio << std::endl;
	std::cout << "NonCheck percent = " << (((double)nonCheckCnt) / ((double)totalCnt)) << std::endl;
	std::cout << "Both percent = " << (((double)bothCnt) / ((double)totalCnt)) << std::endl;
}
