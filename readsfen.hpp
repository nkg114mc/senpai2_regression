#include <vector>

#include "senp2_eval.hpp"
#include "arguments.hpp"

#ifndef READSFEN_HPP
#define READSFEN_HPP

extern const int WEIGHT_DIM;


class UtilTimer {
private:
  typedef std::chrono::duration<int, std::ratio<1, 1000>> millisecond_t;

public:
  typedef std::chrono::time_point<std::chrono::system_clock> time_t;
  static time_t now() {
    return std::chrono::system_clock::now();
  }
  int time(time_t start_time) const {
    return std::chrono::duration_cast<millisecond_t>(now() - start_time).count();
  }
};

class DenseVector;

class SparseVector {
private:
  std::vector<int> indices;
  std::vector<double> values;

public:
  void clear();
  void insert(int idx, double val);
  double dotProduct(DenseVector & dv);

  int size();
  int getIndex(int i);
  double getValue(int i);
};

class DenseVector {
private:
  static const int MAX_LEN = 1600;
  int length;
  double values[MAX_LEN];

public:
  DenseVector();
  DenseVector(int sz);
  DenseVector(DenseVector &src_dv);
  DenseVector(SparseVector &src_sv, int len);
  void set(int idx, double val);
  int size();
  void setSize(int sz);
  double get(int idx);
  void clear();

  void addSparse(SparseVector & sv);
  void addSparseAndMulti(SparseVector & sv, double factor);
  void addDense(DenseVector & dv);
  void divide(double deno);
  void multiply(double mult);

};

struct FeatureAndFactor {
  SparseVector sv;
  int mg_factor, eg_factor;
  int score;
  int result; // -1, 0, 1
};


struct PackedSfen { 
  uint8_t data[32]; 
};

struct PackedSfenValue
{
	// phase
	PackedSfen sfen;

	// Evaluation value returned from Learner::search()
	int16_t score;

	// PV first move
	// Used when finding the match rate with the teacher
	uint16_t move;

	// Trouble of the phase from the initial phase.
	uint16_t gamePly;

	// 1 if the player on this side ultimately wins the game. -1 if you are losing.
	// 0 if a draw is reached.
	// The draw is in the teacher position generation command gensfen,
	// Only write if LEARN_GENSFEN_DRAW_RESULT is enabled.
	int8_t game_result;

	// When exchanging the file that wrote the teacher aspect with other people
	//Because this structure size is not fixed, pad it so that it is 40 bytes in any environment.
	uint8_t padding;

	// 32 + 2 + 2 + 2 + 1 + 1 = 40bytes
};

// featurizer
int eval_nonreduce(const Pos & pos);
int eval_featurize(const Pos & pos, SparseVector &sv);
int eval_featurize_with_factor(const Pos & pos, FeatureAndFactor &fv);
//void run_training(std::string trnFn, std::string valFn);
//void run_training_with_reader(std::vector<std::string> trnList, std::string valFn);
void run_training_sfen(TrainingArguments &args);
void run_training_feature_bin_chunks(TrainingArguments &args);
void run_validate(std::string valFn, std::string weightFn);
void load_training_data_to_mem(std::string trnFn);
void convert_sfen_to_bin(std::string trnFn, std::string outputFn);
void convert_sfen_to_bin_blocks(std::string trnFn, 
                                std::string outputFldr, 
                                std::string baseOutputName, 
                                uint64_t chunkSize);
void test_shuffle();
void generate_senpai_sfen_bin(std::string inputFn, std::string outputFn);
void check_feature_scale_bin(std::string trnFn);

#endif // #ifndef READSFEN_HPP
