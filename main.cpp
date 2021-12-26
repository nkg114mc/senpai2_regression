// includes

#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

/*
#include "bit.hpp"
#include "common.hpp"
#include "eval.hpp"
#include "pawn.hpp"
#include "pos.hpp"
*/
#include "senp2_eval.hpp"
#include "readsfen.hpp"
#include "feat_read.hpp"
#include "perf.hpp"
#include "arguments.hpp"


void load_file_names_from_list_file(std::string listFn, std::vector<std::string> &list) {
   list.clear();

   std::ifstream inf;
   inf.open(listFn);

   while (inf) {
      std::string line;
      std::getline(inf, line);
      if (line != "") {
         //std::cout << line << std::endl;
         list.push_back(line);
      }
   }

   inf.close();
   std::cout << "Find " << list.size() << " paths from listfile" << std::endl;
}

void parse_sfen_input_training_args(std::string cmd, TrainingArguments &trnArgs) {
   std::istringstream iss(cmd);

   // assign default arguments 
   trnArgs.epochs = 200;
   trnArgs.miniBatchSize = 40000;
   trnArgs.learningRate = 1;
   trnArgs.regLambda = 0.00001;
   trnArgs.n_helpers = 16;
   trnArgs.doShuffle = false;
   trnArgs.nnueLambda = 1;
   trnArgs.doFeatureNormalization = false;

	while (iss) {
		std::string option;
		iss >> option;

		if (option == "") {
			break;
      } else if (option == "train") {
         // ignore argument
      } else if (option == "inputsfen") {
         // ignore argument
      } else if (option == "trainlist") {
         std::string trainListFile;
         iss >> trainListFile;

         trnArgs.trnList.clear();
         load_file_names_from_list_file(trainListFile, trnArgs.trnList);
      } else if (option == "trainsfen") { // not used any more
         iss >> trnArgs.trnFn;
      } else if (option == "validsfen") {
         iss >> trnArgs.valFn;
      } else if (option == "epoch") {
         iss >> trnArgs.epochs;
      } else if (option == "minbatch") {
         iss >> trnArgs.miniBatchSize;
      } else if (option == "learningrate") {
         iss >> trnArgs.learningRate;
      } else if (option == "regularizer") {
         iss >> trnArgs.regLambda;
      } else if (option == "helpers") {
         iss >> trnArgs.n_helpers;
      } else if (option == "doshuffle") {
         iss >> trnArgs.doShuffle;
      } else if (option == "dofeaturenorm") {
         iss >> trnArgs.doFeatureNormalization;
      } else {
         std::cerr << "unknown argument: " << option << std::endl;
      }
   }
}

void parse_feature_bin_input_training_args(std::string cmd, TrainingArguments &trnArgs) {
   std::istringstream iss(cmd);

   // default arguments
   trnArgs.epochs = 200;
   trnArgs.miniBatchSize = 100000;
   trnArgs.learningRate = 0.002;
   trnArgs.regLambda = 0.001;
   trnArgs.n_helpers = 16;
   trnArgs.doShuffle = true;
   trnArgs.doFeatureNormalization = false;

	while (iss) {
		std::string option;
		iss >> option;

		if (option == "") {
			break;
      } else if (option == "train") {
         // ignore argument
      } else if (option == "inputfeatbin") {
         // ignore argument
      } else if (option == "trainlist") {
         std::string trainListFile;
         iss >> trainListFile;

         trnArgs.trnList.clear();
         load_file_names_from_list_file(trainListFile, trnArgs.trnList);
      } else if (option == "validsfen") {
         iss >> trnArgs.valFn;
      } else if (option == "epoch") {
         iss >> trnArgs.epochs; 
      } else if (option == "minbatch") {
         iss >> trnArgs.miniBatchSize;
      } else if (option == "learningrate") {
         iss >> trnArgs.learningRate;
      } else if (option == "regularizer") {
         iss >> trnArgs.regLambda;
      } else if (option == "helpers") {
         iss >> trnArgs.n_helpers;
      } else if (option == "doshuffle") {
         iss >> trnArgs.doShuffle;
      } else if (option == "dofeaturenorm") {
         iss >> trnArgs.doFeatureNormalization;
      } else {
         std::cerr << "unknown argument: " << option << std::endl;
      }
   }
}

void parse_featgen_singlefile_args(std::string cmd, std::string &outputFn) {
   std::istringstream iss(cmd);

	while (iss) {
		std::string option;
		iss >> option;

		if (option == "") {
			break;
      } else if (option == "featurefilegen") {
         // ignore argument
      } else if (option == "inputsfen") {
         // ignore argument
         std::string inputfn;
         iss >> inputfn;
      } else if (option == "singlebin") {
         // ignore argument
      
      } else if (option == "singlefilename") { // only thing we need
         iss >> outputFn;
      } else {
         std::cerr << "unknown argument: " << option << std::endl;
      }
   }
}

void parse_featgen_multichunks_args(std::string cmd, 
                                    std::string &outputDir,
                                    std::string &outputName,
                                    int &batchSize) {
   std::istringstream iss(cmd);

	while (iss) {
		std::string option;
		iss >> option;

		if (option == "") {
			break;
      } else if (option == "featurefilegen") {
         // ignore argument
      } else if (option == "inputsfen") {
         // ignore argument
         std::string inputfn;
         iss >> inputfn;
      } else if (option == "multichunks") {
         // ignore argument
      
      } else if (option == "multichunksfolder") { // things we need
         iss >> outputDir;
      } else if (option == "multichunksname") { // things we need
         iss >> outputName;
      } else if (option == "chucksize") { // things we need
         iss >> batchSize;
      } else {
         std::cerr << "unknown argument: " << option << std::endl;
      }
   }
}

int main(int argc, char * argv[]) {

   math::init();
   bit::init();
   hash::init();
   pawn::init();
   pos::init();
   clear_pawn_table();


   std::string cmd = "";
   for (int i = 1; i < argc; ++i) {
      cmd += std::string(argv[i]) + " ";
   }

   std::istringstream is(cmd);
	// Assume the filenames are staggered.
	while (is)
	{
		std::string option = "";
		is >> option;

		if (option == "") {
			break;
      }

      // training options
      else if (option == "train") {
         std::cout << "Run training task." << std::endl;

         /*
         //run_training("./test_gensfen4000.bin", "./test_gensfen4000_copy.bin");
         //std::string trnFn1b = "/home/mc/sidework/nnchess/Stockfish-NNUE/src/trainingdata1b/trn_1b_d10.bin";
         //std::string trnFn = "/home/mc/sidework/nnchess/Stockfish-NNUE/src/trainingdata100m/trn_100m_d10.bin";
         //std::string vadFn = "/home/mc/sidework/nnchess/Stockfish-NNUE/src/validate1m/val_1m_d14.bin";
         std::string trnFn1b = "/media/mc/Fastdata/Stockfish-NNUE/trainingdata1b/trn_1b_d10.bin";
         std::string trnFn = "/media/mc/Fastdata/Stockfish-NNUE/trainingdata100m/trn_100m_d10.bin";
         std::string vadFn = "/media/mc/Fastdata/Stockfish-NNUE/validate1m/val_1m_d14.bin";

         std::string trnFnSenp2 = "/media/mc/Fastdata/Stockfish-NNUE/senp2/trn_100m_senp2.bin";
         std::string vadFnSenp2 = "/media/mc/Fastdata/Stockfish-NNUE/senp2/val_1m_senp2.bin";


         //run_training(trnFn, vadFn);

         //run_training("./feat2.bin", vadFn);
         std::vector<std::string> trnList;
         //load_file_names_from_list_file("/media/mc/Fastdata/senpai2_regression/data_folder/train100m_split320/filelist.txt", trnList);
         load_file_names_from_list_file("/media/mc/Fastdata/senpai2_regression/data_folder/train1b_split320/filelist.txt", trnList);
         //load_file_names_from_list_file("/media/mc/Fastdata/senpai2_regression/data_folder/senp2_train100m_split320/filelist.txt", trnList);
         std::vector<std::string> tList{
            "/media/mc/Fastdata/senpai2_regression/data_folder/train100m_split320/feat3-0.bin",
            //"/media/mc/Fastdata/senpai2_regression/data_folder/train100m_split320/feat3-1.bin"
         };

         run_training_with_reader(trnList, vadFn);
         */

         int trainMode = -1; // 0, 1
         TrainingArguments trainingArgs;

         while (is) {
		      is >> option;

            if (option == "") {
               break;
            } else if (option == "inputsfen") {
               trainMode = 1;
               parse_sfen_input_training_args(cmd, trainingArgs);
            } else if (option == "inputfeatbin") {
               trainMode = 2;
               parse_feature_bin_input_training_args(cmd, trainingArgs);
            }
         }
         
         if (trainMode == 1) {
            std::cout << "Train from original sfen file mode." << std::endl;
            run_training_sfen(trainingArgs);
         } else if (trainMode == 2) {
            std::cout << "Train from pre-generated feature file chunks mode." << std::endl;
            run_training_feature_bin_chunks(trainingArgs);
         } else {
            std::cerr << "No input format was specified" << std::endl;
         }

         break;
      } 
      
      else if (option == "featurefilegen") {
         std::cout << "Run feature file generation task." << std::endl;
         
         uint8_t convertMode = -1; // 1, 2

         // single file
         std::string inputSfenFn = "";
         std::string singleFn = "";

         // multi file
         std::string folder = "";
         std::string chunkName = "";
         int batchSize = 312500;

         while (is) {
		      is >> option;

            if (option == "") {
               break;

            } else if (option == "inputsfen") {
               is >> inputSfenFn;
            
            // single file
            } else if (option == "singlebin") {
               convertMode = 1;
               parse_featgen_singlefile_args(cmd, singleFn);

            // multiple chunks
            } else if (option == "multichunks") {
               convertMode = 2;
               parse_featgen_multichunks_args(cmd, folder, chunkName, batchSize);
            }/* else {
               std::cerr << "unknown con: " << option << std::endl;
            }*/
         }

         if (convertMode == 1) {
            std::cout << "Generate singe feature file mode." << std::endl;
            convert_sfen_to_bin(inputSfenFn, singleFn);
         } else if (convertMode == 2) {
            std::cout << "Generate multiple feature file chunks mode." << std::endl;
            convert_sfen_to_bin_blocks(inputSfenFn, folder, chunkName, batchSize);
         } else {
            std::cerr << "unknown convert mode..." << std::endl;
         }
         break;
      }

      else if (option == "validationonly") {
         std::cout << "Run validation task." << std::endl;

         std::string vadSfenFn = "";
         std::string weightFn = "";
         int batchSize = 50000;

         while (is) {
		      is >> option;
            if (option == "") {
               break;
            } else if (option == "validsfenfile") {
               is >> vadSfenFn;
            } else if (option == "weightfile") {
               is >> weightFn;
            } else if (option == "batchsize") {
               is >> batchSize;
            } else {
               std::cerr << "unknown argument: " << option << std::endl;
            }
         }

         run_validate(vadSfenFn, weightFn);
         break;
      }

      //// The following is for debug only

      else if (option == "speedtest") {
         std::cout << "Run speedtest task." << std::endl;

         //perftest_read_feature_block_bin(trnList, 40000);
         //perftest_read_feature_single_bin("/home/mc/sidework/nnchess/senpai2_regression/data_folder/train100m_split320/feat3-319.bin", 40000);
         //perftest_read_multifeature_single_bin("/home/mc/sidework/nnchess/senpai2_regression/feat4.bin", 40000);
         //perftest_parallel_read_feature_block_bin(trnList, 40000, 16);
         //perftest_read_feature_sfen_bin(sfenList, 40000);
         break;
      }

      else if (option == "debug") {
         std::cout << "Run debug task." << std::endl;

         //load_training_data_to_mem(trnFn);
         //test_shuffle();
         //generate_senpai_sfen_bin(trnFn, vadFnSenp2);
         //check_feature_scale_bin(trnFn1b);
         //EvalTest("./crafty_openings.fen");
         //test_read_sfen("/home/mc/sidework/nnchess/Stockfish-NNUE/src/validate1m/val_1m_d14.bin");
         break;
      }
   }

   return EXIT_SUCCESS;
}