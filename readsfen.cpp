#include <iostream>
#include <sstream>
#include <string>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <string>
#include <cmath>
#include <cstdint>
#include <random>
#include <cstring>
#include <vector>
#include <condition_variable>
#include <mutex>
#include <thread>
#include <atomic>
#include <unistd.h>
#include <cstdarg>
#include <algorithm>

#include "senp2_eval.hpp"
#include "readsfen.hpp"
#include "feat_read.hpp"
#include "sfen_buffer.hpp"
#include "arguments.hpp"

using std::string;

const int HALF_WEIGHT_DIM = 759;
const int WEIGHT_DIM = HALF_WEIGHT_DIM * 2;


const Square H1 = square_make(File_H, Rank_1);
const Square A1 = square_make(File_A, Rank_1);
const Square H8 = square_make(File_H, Rank_8);
const Square A8 = square_make(File_A, Rank_8);

const Piece_Side w_King = piece_side_make(King, White);
const Piece_Side b_King = piece_side_make(King, Black);
const Piece_Side Side_NoPc = Piece_Side(12);


/*
// See in readsfen.hpp
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
*/


// Class that handles bitstream
// useful when doing aspect encoding
struct BitStream
{
  // Set the memory to store the data in advance.
  // Assume that memory is cleared to 0.
  void  set_data(uint8_t* data_) { data = data_; reset(); }

  // Get the pointer passed in set_data().
  uint8_t* get_data() const { return data; }

  // Get the cursor.
  int get_cursor() const { return bit_cursor; }

  // reset the cursor
  void reset() { bit_cursor = 0; }

  // Write 1bit to the stream.
  // If b is non-zero, write out 1. If 0, write 0.
  void write_one_bit(int b)
  {
    if (b)
      data[bit_cursor / 8] |= 1 << (bit_cursor & 7);

    ++bit_cursor;
  }

  // Get 1 bit from the stream.
  int read_one_bit()
  {
    int b = (data[bit_cursor / 8] >> (bit_cursor & 7)) & 1;
    ++bit_cursor;

    return b;
  }

  // write n bits of data
  // Data shall be written out from the lower order of d.
  void write_n_bit(int d, int n)
  {
    for (int i = 0; i <n; ++i)
      write_one_bit(d & (1 << i));
  }

  // read n bits of data
  // Reverse conversion of write_n_bit().
  int read_n_bit(int n)
  {
    int result = 0;
    for (int i = 0; i < n; ++i)
      result |= read_one_bit() ? (1 << i) : 0;

    return result;
  }

private:
  // Next bit position to read/write.
  int bit_cursor;

  // data entity
  uint8_t* data;
};


// Huffman coding
// * is simplified from mini encoding to make conversion easier.
//
// 1 box on the board (other than NO_PIECE) = 2 to 6 bits (+ 1-bit flag + 1-bit forward and backward)
// 1 piece of hand piece = 1-5bit (+ 1-bit flag + 1bit ahead and behind)
//
// empty xxxxx0 + 0 (none)
// step xxxx01 + 2 xxxx0 + 2
// incense xx0011 + 2 xx001 + 2
// Katsura xx1011 + 2 xx101 + 2
// silver xx0111 + 2 xx011 + 2
// Gold x01111 + 1 x0111 + 1 // Gold is valid and has no flags.
// corner 011111 + 2 01111 + 2
// Fly 111111 + 2 11111 + 2
//
// Assuming all pieces are on the board,
// Sky 81-40 pieces = 41 boxes = 41bit
// Walk 4bit*18 pieces = 72bit
// Incense 6bit*4 pieces = 24bit
// Katsura 6bit*4 pieces = 24bit
// Silver 6bit*4 pieces = 24bit
// Gold 6bit* 4 pieces = 24bit
// corner 8bit* 2 pieces = 16bit
// Fly 8bit* 2 pieces = 16bit
// -------
// 241bit + 1bit (turn) + 7bit × 2 (King's position after) = 256bit
//
// When the piece on the board moves to the hand piece, the piece on the board becomes empty, so the box on the board can be expressed with 1 bit,
// Since the hand piece can be expressed by 1 bit less than the piece on the board, the total number of bits does not change in the end.
// Therefore, in this expression, any aspect can be expressed by this bit number.
// It is a hand piece and no flag is required, but if you include this, the bit number of the piece on the board will be -1
// Since the total number of bits can be fixed, we will include this as well.

// Huffman Encoding
//
// Empty  xxxxxxx0
// Pawn   xxxxx001 + 1 bit (Side to move)
// Knight xxxxx011 + 1 bit (Side to move)
// Bishop xxxxx101 + 1 bit (Side to move)
// Rook   xxxxx111 + 1 bit (Side to move)

struct HuffmanedPiece
{
  int code; // how it will be coded
  int bits; // How many bits do you have
};

HuffmanedPiece huffman_table[] =
{
  {0b0000,1}, // NO_PIECE
  {0b0001,4}, // PAWN
  {0b0011,4}, // KNIGHT
  {0b0101,4}, // BISHOP
  {0b0111,4}, // ROOK
  {0b1001,4}, // QUEEN
};

enum SfPieceType {
  NO_PIECE_TYPE, PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING
};

// NO_PIECE_TYPE, PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING,
Piece SenpTwoPieceMap[8] = {
  Piece_None, Pawn, Knight, Bishop, Rook, Queen, King, Piece_None
};

std::string SenpTwoPieceNames[8] = {
  "Piece_None", "Pawn", "Knight", "Bishop", "Rook", "Queen", "King", "Piece_None8"
};

Square sf_square_to_senp_square(int sf_sq) {
  File fl = File(sf_sq & 0x7);
  Rank rk = Rank(sf_sq >> 3);
  return square_make(fl, rk);
}

// Class for compressing/decompressing sfen
// sfen can be packed to 256bit (32bytes) by Huffman coding.
// This is proven by mini. The above is Huffman coding.
//
// Internal format = 1-bit turn + 7-bit king position *2 + piece on board (Huffman coding) + hand piece (Huffman coding)
// Side to move (White = 0, Black = 1) (1bit)
// White King Position (6 bits)
// Black King Position (6 bits)
// Huffman Encoding of the board
// Castling availability (1 bit x 4)
// En passant square (1 or 1 + 6 bits)
// Rule 50 (6 bits)
// Game play (8 bits)
//
// TODO(someone): Rename SFEN to FEN.
//
struct SfenPacker
{
/*
  // Pack sfen and store in data[32].
  void pack(const Position& pos)
  {

    memset(data, 0, 32);
    stream.set_data(data);

    // turn
    // Side to move.
    stream.write_one_bit((int)(pos.side_to_move()));

    // 7-bit positions for leading and trailing balls
    // White king and black king, 6 bits for each.
    for(auto c: Colors)
      stream.write_n_bit(pos.king_square(c), 6);

    // Write the pieces on the board other than the kings.
    for (Rank r = RANK_8; r >= RANK_1; --r)
    {
      for (File f = FILE_A; f <= FILE_H; ++f)
      {
        Piece pc = pos.piece_on(make_square(f, r));
        if (type_of(pc) == KING)
          continue;
        write_board_piece_to_stream(pc);
      }
    }

    // TODO(someone): Support chess960.
    stream.write_one_bit(pos.can_castle(WHITE_OO));
    stream.write_one_bit(pos.can_castle(WHITE_OOO));
    stream.write_one_bit(pos.can_castle(BLACK_OO));
    stream.write_one_bit(pos.can_castle(BLACK_OOO));

    if (pos.ep_square() == SQ_NONE) {
      stream.write_one_bit(0);
    }
    else {
      stream.write_one_bit(1);
      stream.write_n_bit(static_cast<int>(pos.ep_square()), 6);
    }

    stream.write_n_bit(pos.state()->rule50, 6);

    stream.write_n_bit(1 + (pos.game_ply()-(pos.side_to_move() == BLACK)) / 2, 8);

    assert(stream.get_cursor() <= 256);
  }
*/
  // sfen packed by pack() (256bit = 32bytes)
  // Or sfen to decode with unpack()
  uint8_t *data; // uint8_t[32];

//private:
  // Position::set_from_packed_sfen(uint8_t data[32]) I want to use these functions, so the line is bad, but I want to keep it public.

  BitStream stream;
/*
  // Output the board pieces to stream.
  void write_board_piece_to_stream(Piece pc)
  {
    // piece type
    PieceType pr = type_of(pc);
    auto c = huffman_table[pr];
    stream.write_n_bit(c.code, c.bits);
 
    if (pc == NO_PIECE)
      return;

    // first and second flag
    stream.write_one_bit(color_of(pc));
  }
*/
  // Read one board piece from stream
  Piece_Side read_board_piece_from_stream()
  {
    int pr = NO_PIECE_TYPE;
    int code = 0, bits = 0;

    bool found = false; 
    while (true) {
      code |= (stream.read_one_bit() << bits);
      ++bits;

      assert(bits <= 6);

      for (pr = NO_PIECE_TYPE; pr < KING; ++pr) {
        if (huffman_table[pr].code == code && 
            huffman_table[pr].bits == bits) {
            found = true;
            break;
        }
      }

      if (found) {
        break;
      }
    }

    if (pr == NO_PIECE_TYPE) {
      return Side_NoPc;
    }

    // first and second flag
    Side c = (Side)stream.read_one_bit();
    
    return piece_side_make(SenpTwoPieceMap[pr], c);
  }
};


const std::string pc_names[13] = {
  "wPawn", "bPawn", "wKnight", "bKinght", "wBishop", "bBishop", 
  "wRook", "bRook", "wQueen", "bQueen", "wKing", "bKing", "None"
};


void pos_from_sfen(Pos & pos, PackedSfen & sfen, bool mirror) {
/*
	SfenPacker packer;
	auto& stream = packer.stream;
	stream.set_data((uint8_t*)&sfen);

	std::memset(this, 0, sizeof(Position));
	std::memset(si, 0, sizeof(StateInfo));
  std::fill_n(&pieceList[0][0], sizeof(pieceList) / sizeof(Square), SQ_NONE);
  st = si;

	// Active color
	sideToMove = (Color)stream.read_one_bit();

	// clear evalList. It is cleared when memset is cleared to zero above...
	evalList.clear();

	// In updating the PieceList, we have to set which piece is where,
	// A counter of how much each piece has been used
  PieceNumber next_piece_number = PIECE_NUMBER_ZERO;

  pieceList[W_KING][0] = SQUARE_NB;
  pieceList[B_KING][0] = SQUARE_NB;

	// First the position of the ball
	if (mirror)
	{
		for (auto c : Colors)
			board[Mir((Square)stream.read_n_bit(6))] = make_piece(c, KING);
	}
	else
	{
		for (auto c : Colors)
			board[stream.read_n_bit(6)] = make_piece(c, KING);
	}

  // Piece placement
  for (Rank r = RANK_8; r >= RANK_1; --r)
  {
    for (File f = FILE_A; f <= FILE_H; ++f)
    {
      auto sq = make_square(f, r);
      if (mirror) {
        sq = Mir(sq);
      }

      // it seems there are already balls
      Piece pc;
      if (type_of(board[sq]) != KING)
      {
        assert(board[sq] == NO_PIECE);
        pc = packer.read_board_piece_from_stream();
      }
      else
      {
        pc = board[sq];
        board[sq] = NO_PIECE; // put_piece() will catch ASSERT unless you remove it all.
      }

      // There may be no pieces, so skip in that case.
      if (pc == NO_PIECE)
        continue;

      put_piece(Piece(pc), sq);

      // update evalList
      PieceNumber piece_no =
        (pc == B_KING) ?PIECE_NUMBER_BKING :// Move ball
        (pc == W_KING) ?PIECE_NUMBER_WKING :// Backing ball
        next_piece_number++; // otherwise

      evalList.put_piece(piece_no, sq, pc); // Place the pc piece in the sq box

      //cout << sq << ' ' << board[sq] << ' ' << stream.get_cursor() << endl;

      if (stream.get_cursor()> 256)
        return 1;
      //assert(stream.get_cursor() <= 256);

    }
  
  }

  // Castling availability.
  // TODO(someone): Support chess960.
  st->castlingRights = 0;
  if (stream.read_one_bit()) {
    Square rsq;
    for (rsq = relative_square(WHITE, SQ_H1); piece_on(rsq) != W_ROOK; --rsq) {}
    set_castling_right(WHITE, rsq);
  }
  if (stream.read_one_bit()) {
    Square rsq;
    for (rsq = relative_square(WHITE, SQ_A1); piece_on(rsq) != W_ROOK; ++rsq) {}
    set_castling_right(WHITE, rsq);
  }
  if (stream.read_one_bit()) {
    Square rsq;
    for (rsq = relative_square(BLACK, SQ_H1); piece_on(rsq) != B_ROOK; --rsq) {}
    set_castling_right(BLACK, rsq);
  }
  if (stream.read_one_bit()) {
    Square rsq;
    for (rsq = relative_square(BLACK, SQ_A1); piece_on(rsq) != B_ROOK; ++rsq) {}
    set_castling_right(BLACK, rsq);
  }

  // En passant square. Ignore if no pawn capture is possible
  if (stream.read_one_bit()) {
    Square ep_square = static_cast<Square>(stream.read_n_bit(6));
    if (mirror) {
      ep_square = Mir(ep_square);
    }
    st->epSquare = ep_square;

    if (!(attackers_to(st->epSquare) & pieces(sideToMove, PAWN))
      || !(pieces(~sideToMove, PAWN) & (st->epSquare + pawn_push(~sideToMove))))
      st->epSquare = SQ_NONE;
  }
  else {
    st->epSquare = SQ_NONE;
  }

  // Halfmove clock
  st->rule50 = static_cast<Square>(stream.read_n_bit(6));

  // Fullmove number
  gamePly = static_cast<Square>(stream.read_n_bit(8));
  // Convert from fullmove starting from 1 to gamePly starting from 0,
  // handle also common incorrect FEN with fullmove = 0.
  gamePly = std::max(2 * (gamePly - 1), 0) + (sideToMove == BLACK);

  assert(stream.get_cursor() <= 256);

  //chess960 = false;
  //thisThread = th;
  //set_state(st);

  //std::cout << *this << std::endl;

  assert(pos_is_ok());
*/

  Bit castling_rooks;
  Bit piece_side[12];
  Bit all_pcs;

  pos.clear();
  int wksq = -1;
  int bksq = -1;

  ////////////////////////////

  SfenPacker packer;
	auto& stream = packer.stream;
	stream.set_data((uint8_t*)&sfen);
  all_pcs = Bit(0);
  std::memset(piece_side, 0, sizeof(piece_side));

	// Active color
	Side sideToMove = (Side)stream.read_one_bit();

	// First the position of the ball
	if (mirror)
	{
		///for (auto c : Colors) {
    //  board[Mir((Square)stream.read_n_bit(6))] = make_piece(c, KING);
	  //}
  }	
	else
	{
    wksq = stream.read_n_bit(6);
    bksq = stream.read_n_bit(6);
    bit::set(piece_side[w_King], sf_square_to_senp_square(wksq));
    bit::set(piece_side[b_King], sf_square_to_senp_square(bksq));
    bit::set(all_pcs, sf_square_to_senp_square(wksq));
    bit::set(all_pcs, sf_square_to_senp_square(bksq));
	}

  // Piece placement
  for (Rank r = Rank_8; r >= Rank_1; r = r - 1) {
    for (File f = File_A; f <= File_H; f = f + 1) {
      Square sq = square_make(f, r);
      if (mirror) {
        //sq = Mir(sq);
      }

      // it seems there are already balls
      Piece_Side pc;
      if (!bit::has(piece_side[w_King], sq) && !bit::has(piece_side[b_King], sq))
      {
        assert(!bit::has(all_pcs, sq));
        pc = packer.read_board_piece_from_stream();
      }
      else
      {
        // skip king squares
        continue;
      }

      // There may be no pieces, so skip in that case.
      if (pc == Side_NoPc) {
        continue;
      }

      //put_piece(Piece(pc), sq);
      bit::set(piece_side[pc], sq);
      bit::set(all_pcs, sq);

      if (stream.get_cursor() > 256) {
        return;
      }

      assert(stream.get_cursor() <= 256);
    }
  }

  // Castling availability.
  castling_rooks = (Bit)(0);
  if (stream.read_one_bit()) {
    bit::set(castling_rooks, H1);
  }
  if (stream.read_one_bit()) {
    bit::set(castling_rooks, A1);
  }
  if (stream.read_one_bit()) {
    bit::set(castling_rooks, H8);
  }
  if (stream.read_one_bit()) {
    bit::set(castling_rooks, A8);
  }

  // En passant square. Ignore if no pawn capture is possible
  Square epSquare = Square_None;
  if (stream.read_one_bit()) {
    epSquare = sf_square_to_senp_square(stream.read_n_bit(6));
    if (mirror) {
      //ep_square = Mir(ep_square);
    }
  } else {
    epSquare = Square_None;
  }

  // Halfmove clock
  int sf_rule50 = static_cast<Square>(stream.read_n_bit(6));

  // Fullmove number
  int gamePly = static_cast<Square>(stream.read_n_bit(8));
  // Convert from fullmove starting from 1 to gamePly starting from 0,
  // handle also common incorrect FEN with fullmove = 0.
  gamePly = std::max(2 * (gamePly - 1), 0) + (sideToMove == Black);

  assert(stream.get_cursor() <= 256);

  ////////////////////////////

  create_pos(pos, sideToMove, piece_side, castling_rooks, epSquare);
}

void read_sfen_file(string input_filename) {

  std::ofstream outf("xxx.log");

  clear_pawn_table();

	std::ifstream fs(input_filename, std::ios::binary);

  Pos pos;
  int cnt = 0;

	while (fs)
	{
		PackedSfenValue p;
		if (fs.read((char*)&p, sizeof(PackedSfenValue))) {
			pos_from_sfen(pos, p.sfen, false);
      cnt++;

      std::string fenstr = pos_to_fen(pos);
      std::cout << cnt << ": " << fenstr << std::endl;

      int eval_val = (int)eval(pos, pos.turn());
      outf << fenstr << " " << eval_val << std::endl;
		} else {
      std::cout << "Not enough bits read..." << std::endl;
			break;
		}
	}

  outf.close();
}


////////

//// Vectors

void SparseVector::clear() {
  indices.clear();
  values.clear();

  indices.reserve(32);
  values.reserve(32);
}

void SparseVector::insert(int idx, double val) {
  indices.push_back(idx);
  values.push_back(val);
}

double SparseVector::dotProduct(DenseVector & dv) {
  double result = 0;
  for (int i = 0; i < indices.size(); i++) {
    int idx = indices[i];
    double prod = dv.get(idx) * values[i];
    result += prod;
  }
  return result;
}

int SparseVector::size() {
  return indices.size();
}
int SparseVector::getIndex(int i) {
  assert(i >= 0 && i < indices.size());
  return indices[i];
}
double SparseVector::getValue(int i) {
  assert(i >= 0 && i < values.size());
  return values[i];
}



DenseVector::DenseVector() {
  length = MAX_LEN;
}
DenseVector::DenseVector(int sz) {
  assert(sz <= MAX_LEN);
  length = sz;
}
DenseVector::DenseVector(DenseVector &src_dv) {
  length = src_dv.size();
  for (int i = 0; i < length; i++) {
    values[i] = src_dv.get(i);
  }
}
DenseVector::DenseVector(SparseVector &src_sv, int len) {
  length = len;
  clear();
  int sparseSize = src_sv.size();
  for (int i = 0; i < sparseSize; i++) {
    int idx = src_sv.getIndex(i);
    values[idx] = src_sv.getValue(i);
  }
}
void DenseVector::clear() {
  for (int i = 0; i < length; i++) {
    values[i] = 0;
  }
}
void DenseVector::set(int idx, double val) {
  assert(idx >= 0 && idx < length);
  values[idx] = val;
}
int DenseVector::size() {
  return length;
}
void DenseVector::setSize(int sz) {
  length = sz;
}

double DenseVector::get(int idx) {
  assert(idx >= 0 && idx < length);
  return values[idx];
}

void DenseVector::addSparse(SparseVector & sv) {
  for (int i = 0; i < sv.size(); i++) {
    int idx = sv.getIndex(i);
    double val = sv.getValue(i);
    values[idx] += val;
  }
}

void DenseVector::addSparseAndMulti(SparseVector & sv, double factor) {
  for (int i = 0; i < sv.size(); i++) {
    int idx = sv.getIndex(i);
    double val = sv.getValue(i) * factor;
    values[idx] += val;
  }
}

void DenseVector::addDense(DenseVector & dv) {
  assert(this->length == dv.size());
  for (int i = 0; i < this->length; i++) {
    values[i] += dv.get(i);
  }
}

void DenseVector::divide(double deno) {
  for (int i = 0; i < this->length; i++) {
    values[i] /= deno;
  }
}

void DenseVector::multiply(double mult) {
  for (int i = 0; i < this->length; i++) {
    values[i] *= mult;
  }
}

bool isDoubleNumber(double num) {
  int i1 = (int)num;
  double f2 = (double)i1;
  if (num == f2) {
    return false;
  }
  return true;
}
bool isFloatNumber(float num) {
  int i1 = (int)num;
  float f2 = (float)i1;
  if (num == f2) {
    return false;
  }
  return true;
}
/////

int64_t byte_cnt[4];



struct TrainingExample {
  SparseVector feature;
  double y_truth;
  double y_pred;
  double game_result;

  void fromFeatAndFctr(FeatureAndFactor &featFactor) {

    feature.clear();

    for (int i = 0; i < featFactor.sv.size(); i++) {
      int idx = featFactor.sv.getIndex(i);
      double phi = ((double)featFactor.sv.getValue(i)) * ((double)featFactor.mg_factor);
      feature.insert(idx, phi / 24);
    }

    for (int i = 0; i < featFactor.sv.size(); i++) {
      int idx = featFactor.sv.getIndex(i) + HALF_WEIGHT_DIM;
      double phi = ((double)featFactor.sv.getValue(i)) * ((double)featFactor.eg_factor);
      feature.insert(idx, phi / 24);
    }

    y_truth = featFactor.score;
    y_pred = 0;
    game_result = featFactor.result;
  }
/*
  // do feature expansion (with game_phase factors) and inference together
  void fromFeatAndFctrAndDoInference(FeatureAndFactor &featFactor, DenseVector &weight) {

    feature.clear();
    double productSum = 0;

    for (int i = 0; i < featFactor.sv.size(); i++) {
      int idxMg = featFactor.sv.getIndex(i);
      double phiMg = ((double)featFactor.sv.getValue(i)) * ((double)featFactor.mg_factor);
      int idxEg = idxMg + HALF_WEIGHT_DIM;
      double phiEg = ((double)featFactor.sv.getValue(i)) * ((double)featFactor.eg_factor);

      feature.insert(idxMg, phiMg / 24);
      feature.insert(idxEg, phiEg / 24);

      productSum += (weight.get(idxMg) * phiMg / 24);
      productSum += (weight.get(idxEg) * phiEg / 24);
    }

    y_truth = featFactor.score;
    y_pred = productSum;
  }
*/
  // do feature expansion (with game_phase factors) and inference together
  void fromFeatAndFctrAndDoInference(FeatureAndFactor &featFactor, DenseVector &weight, bool scaling, DenseVector *featScalar) {

    feature.clear();
    double productSum = 0;

    for (int i = 0; i < featFactor.sv.size(); i++) {
      int idxMg = featFactor.sv.getIndex(i);
      double phiMg = ((double)featFactor.sv.getValue(i)) * ((double)featFactor.mg_factor);
      int idxEg = idxMg + HALF_WEIGHT_DIM;
      double phiEg = ((double)featFactor.sv.getValue(i)) * ((double)featFactor.eg_factor);

      if (scaling) {
        double scalar = featScalar->get(idxMg);
        assert(scalar > 0);
        phiMg /= scalar;
        phiEg /= scalar;
      }

      feature.insert(idxMg, phiMg / 24);
      feature.insert(idxEg, phiEg / 24);

      productSum += (weight.get(idxMg) * phiMg / 24);
      productSum += (weight.get(idxEg) * phiEg / 24);
    }

    y_truth = featFactor.score;
    y_pred = productSum;
    game_result = featFactor.result;
  }
};

class FeatureMatrix {
public:
  int n_helpers;
  std::vector<TrainingExample> rows[16];

  uint64_t countTotalRows() {
    uint64_t sum = 0;
    for (int i = 0; i < n_helpers; i++) {
      sum += rows[i].size();
    }
    return sum;
  }

  FeatureMatrix(int n_hpr) { n_helpers = n_hpr; }
  FeatureMatrix() { n_helpers = 16; }
};

/////

enum WorkType {
  FEATURIZE_AND_INFERENCE,
  INFERENCE_ONLY,
  READ_AND_INFERENCE,
  UNKNOWN
};


class SenpLock {
protected:
  mutable std::mutex p_mutex;
public:
  void lock   () const { p_mutex.lock(); }
  void unlock () const { p_mutex.unlock(); }
};

class SenpWaiter : public SenpLock {
  std::condition_variable_any p_cond;
public:
   void wait   () { p_cond.wait(p_mutex); }
   void signal () { p_cond.notify_one(); }
};


class SharedList {
private:
  DenseVector *weight_ptr;
  std::vector<PackedSfenValue> *examples_ptr;
  std::vector<FeatureAndFactor> *featAndFacs_ptr;
  FeatureMatrix *feature_matrix;
  WorkType workType;

  SenpLock dataLock;
  SenpLock ioLock;

  bool slave_register[32];

public:

  void init(std::vector<PackedSfenValue> *exmp_ptr,
            DenseVector *w_ptr,
            FeatureMatrix *feat_mat) {

    dataLock.lock();
    examples_ptr = exmp_ptr;
    featAndFacs_ptr = nullptr;
    weight_ptr = w_ptr;
    feature_matrix = feat_mat;
    
    //currentIdx = 0;
    //endIdx = examples_ptr->size();

    for (int i = 0; i < 32; i++) {
      slave_register[i] = false;
    }
    workType = FEATURIZE_AND_INFERENCE;
    dataLock.unlock();
  }

  void initFeat(std::vector<FeatureAndFactor> *feat_ptr,
                DenseVector *w_ptr,
                FeatureMatrix *feat_mat) {

    dataLock.lock();
    examples_ptr = nullptr;
    featAndFacs_ptr = feat_ptr;
    weight_ptr = w_ptr;
    feature_matrix = feat_mat;

    for (int i = 0; i < 32; i++) {
      slave_register[i] = false;
    }
    workType = INFERENCE_ONLY;
    dataLock.unlock();
  }

  void initReadInference(DenseVector *w_ptr, 
                         FeatureMatrix *feat_mat) {
    dataLock.lock();
    examples_ptr = nullptr;
    featAndFacs_ptr = nullptr;
    weight_ptr = w_ptr;
    feature_matrix = feat_mat;

    for (int i = 0; i < 32; i++) {
      slave_register[i] = false;
    }
    workType = READ_AND_INFERENCE;
    dataLock.unlock();
  }

  void register_worker(int idx) {
    assert(idx >= 0 && idx < 32);
    dataLock.lock();
    slave_register[idx] = true;
    dataLock.unlock();
  }
/*
  bool takeTask(PackedSfenValue* &result) {
    bool ok = false;
    result = nullptr;

    dataLock.lock();
    if (currentIdx < endIdx) {
      ok = true;
      result = &(examples_ptr->at(currentIdx));
      currentIdx++;
    }
    dataLock.unlock();
    return ok;
  }
*/
  PackedSfenValue* getExample(int i) {
    return &(examples_ptr->at(i));
  }

  FeatureAndFactor* getFeatAndFac(int i) {
    return &(featAndFacs_ptr->at(i));
  }

  DenseVector * getWeight() {
    return weight_ptr;
  }

  WorkType getWorkType() {
    return workType;
  }

  void writeResult(TrainingExample &exmple, int workerId) {
    assert(workerId >= 0 && workerId < 16);
    feature_matrix->rows[workerId].push_back(exmple);
  }

  void leave(int idx) {
    assert(idx >= 0 && idx < 32);
    dataLock.lock();
    slave_register[idx] = false;
    dataLock.unlock();
  }

  int workers_remain() {
    int remain = 0;
    for (int i = 0; i < 32; i++) {
      if (slave_register[i]) {
        remain++;
      }
    }
    return remain;
  }

  void mutexPrint(const char format[], ...) {
		// lock
		ioLock.lock();

		va_list arg_list;
		char str[4096];

		va_start(arg_list, format);
		vsprintf(str, format, arg_list);
		va_end(arg_list);

		// output it!
		fprintf(stdout,"%s",str);

		// unlock
		ioLock.unlock();
  }

};

double inference(SparseVector &phi, DenseVector &weight) {
  return phi.dotProduct(weight);
}


class ThreadWorker {
private:
  SharedList *list_ptr;
  SenpWaiter wait_cond;
  int startIdx, endIdx;
  int requiredExampleCount; // when read from binary feature file blocks
  bool doFeatScaling;
  DenseVector *featScalar;

public:

  ThreadWorker(int i) {
    id = i;
    start();
  }

  void sendWork(SharedList* sl_ptr, int s, int e) {
    list_ptr = sl_ptr;
    list_ptr->register_worker(id);
    startIdx = s;
    endIdx = e;
  }

  void sendDataReadAndWork(SharedList* sl_ptr, int requestExmpCnt, bool scaling, DenseVector *fsclr) {
    list_ptr = sl_ptr;
    list_ptr->register_worker(id);
    requiredExampleCount = requestExmpCnt;
    doFeatScaling = scaling;
    featScalar = fsclr;
  }

  void start() {
    t1 = 0;
    t2 = 0;
    stop = false;
    is_working = false;
  }

  void wake_up() {
    wait_cond.signal();
  }

  void wait_for_task() {
    is_working = false;
    wait_cond.wait();
    is_working = true;
  }


  void quit_now() {
    stop = true;
  }

  bool should_quit() {
    return stop;
  }

  SharedList * getList() {
    return list_ptr;
  }

  int getBeginIdx() { return startIdx; }
  int getEndIdx() { return endIdx; }
  int getRequestExampleSize() { return requiredExampleCount; }
  bool getDoScaling() { return doFeatScaling; }
  DenseVector* getFeatScalar() { return featScalar; }

  int id;
  std::atomic<bool> stop;
  std::atomic<bool> is_working;
  BinFeatureReader reader;

  int t1; // for timing only
  int t2; // for timing only
  int t5;


  // main work function
  void idle_loop_main() {   
    UtilTimer timer;
    Pos pos;

    SharedList * sl_ptr;
    sl_ptr = this->getList();

    uint64_t cnt = 0;


    UtilTimer::time_t start_t5 = UtilTimer::now();

    if (sl_ptr->getWorkType() == WorkType::READ_AND_INFERENCE) { // read and inference

      UtilTimer::time_t start_t1 = UtilTimer::now();
      std::vector<FeatureAndFactor> featExamples;
      reader.read_one_mini_batch(requiredExampleCount, featExamples);
      t1 += (timer.time(start_t1));

      UtilTimer::time_t start_t2 = UtilTimer::now();
      for (int i = 0; i < featExamples.size(); i++) {
        TrainingExample trnExmp;
        //trnExmp.fromFeatAndFctr(featExamples[i]);
        //trnExmp.y_pred = inference(trnExmp.feature, *(sl_ptr->getWeight()));
        trnExmp.fromFeatAndFctrAndDoInference(featExamples[i], *(sl_ptr->getWeight()), 
                                              doFeatScaling, featScalar);
        sl_ptr->writeResult(trnExmp, this->id);
        cnt++;
      }
      t2 += (timer.time(start_t2));

    } else {  // inference on given examples

      //while (sl_ptr->takeTask(example_ptr)) {
      int startIdx = this->getBeginIdx();
      int endIdx = this->getEndIdx();
      PackedSfenValue* example_ptr;
      for (int i = startIdx; i <= endIdx; i++) {
        // work on one example
        TrainingExample trnExmp;

        if (sl_ptr->getWorkType() == WorkType::FEATURIZE_AND_INFERENCE) {
          // featurizing
          example_ptr = sl_ptr->getExample(i);
          pos_from_sfen(pos, example_ptr->sfen, false);
          eval_featurize(pos, trnExmp.feature);
          trnExmp.y_truth = example_ptr->score;
          trnExmp.y_pred = inference(trnExmp.feature, *(sl_ptr->getWeight()));
        } else if (sl_ptr->getWorkType() == WorkType::INFERENCE_ONLY) {
          // directly inference without featurization
          //trnExmp.fromFeatAndFctr(*(sl_ptr->getFeatAndFac(i)));
          //trnExmp.y_pred = inference(trnExmp.feature, *(sl_ptr->getWeight()));
          trnExmp.fromFeatAndFctrAndDoInference(*(sl_ptr->getFeatAndFac(i)), *(sl_ptr->getWeight()), 
                                                doFeatScaling, featScalar);
        } else {
          std::cerr << "unknown thread work type! " << sl_ptr->getWorkType() << std::endl;
        }

        sl_ptr->writeResult(trnExmp, this->id);
        cnt++;
      }

    }


    sl_ptr->leave(this->id);

    //sl_ptr->mutexPrint("[%d] processed %llu examples.\n", worker.id, cnt);

    // wait until all slaves done;
    while (sl_ptr->workers_remain() > 0) {
      usleep(100);
    }

    t5 += (timer.time(start_t5));
  }
};


void idle_loop(ThreadWorker * worker) {
  UtilTimer timer;
  Pos pos;

  while (!worker->should_quit()) {

    // sleep when nothing to do
    //std::cout << "go to sleep." << std::endl;
    worker->wait_for_task();
    //std::cout << "wakeup!" << std::endl;

    if (worker->should_quit()) {
      //std::cout << "BYE." << std::endl;
      break; // no work to do anymore
    }

    uint64_t cnt = 0;
    SharedList * sl_ptr;
    sl_ptr = worker->getList();


    UtilTimer::time_t start_t5 = UtilTimer::now();

    if (sl_ptr->getWorkType() == WorkType::READ_AND_INFERENCE) { // read and inference

      UtilTimer::time_t start_t1 = UtilTimer::now();
      std::vector<FeatureAndFactor> featExamples;
      worker->reader.read_one_mini_batch(worker->getRequestExampleSize(), featExamples);
      worker->t1 += (timer.time(start_t1));

      UtilTimer::time_t start_t2 = UtilTimer::now();
      for (int i = 0; i < featExamples.size(); i++) {
        TrainingExample trnExmp;
        //trnExmp.fromFeatAndFctr(featExamples[i]);
        //trnExmp.y_pred = inference(trnExmp.feature, *(sl_ptr->getWeight()));
        trnExmp.fromFeatAndFctrAndDoInference(featExamples[i], *(sl_ptr->getWeight()), 
                                              worker->getDoScaling(), worker->getFeatScalar());
        sl_ptr->writeResult(trnExmp, worker->id);
        cnt++;
      }
      worker->t2 += (timer.time(start_t2));

    } else {  // inference on given examples

      //std::cout << "slptr = " << sl_ptr << std::endl;

      PackedSfenValue* example_ptr;
      //while (sl_ptr->takeTask(example_ptr)) {
      int startIdx = worker->getBeginIdx();
      int endIdx = worker->getEndIdx();
      for (int i = startIdx; i <= endIdx; i++) {
        // work on one example
        TrainingExample trnExmp;

        if (sl_ptr->getWorkType() == WorkType::FEATURIZE_AND_INFERENCE) {

          // featurizing
          example_ptr = sl_ptr->getExample(i);
          //std::cout << example_ptr << " " << sl_ptr << std::endl;
          pos_from_sfen(pos, example_ptr->sfen, false);
          eval_featurize(pos, trnExmp.feature);
          trnExmp.y_truth = example_ptr->score;
          trnExmp.y_pred = inference(trnExmp.feature, *(sl_ptr->getWeight()));

        } else if (sl_ptr->getWorkType() == WorkType::INFERENCE_ONLY) {

          // directly inference without featurization
          //trnExmp.fromFeatAndFctr(*(sl_ptr->getFeatAndFac(i)));
          //trnExmp.y_pred = inference(trnExmp.feature, *(sl_ptr->getWeight()));
          trnExmp.fromFeatAndFctrAndDoInference(*(sl_ptr->getFeatAndFac(i)), *(sl_ptr->getWeight()), 
                                                  worker->getDoScaling(), worker->getFeatScalar());

        } else {
          std::cerr << "unknown thread work type! " << sl_ptr->getWorkType() << std::endl;
        }

        sl_ptr->writeResult(trnExmp, worker->id);
        cnt++;
      }

    }


    sl_ptr->leave(worker->id);

    worker->t5 += (timer.time(start_t5));
    //sl_ptr->mutexPrint("[%d] processed %llu examples.\n", worker->id, cnt);
  }

  std::cout << worker->id << " bye." << std::endl;
}

uint32_t getRndIndex(uint32_t start, uint32_t len) {
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

class LinRegTrainer {

  const double PawnValueEg = 206;

public:

  int iterations = 10;
  int miniBatchSize = 100;
  double learningRate = 0;
  double lambda = 0.001; // regularizer
  double nnueLambda = 1.0; // nnue loss iterpolation
  bool doShuffle = true;
  int n_helpers = 1;

  bool trnFnPrinted;
  bool valFnPrinted;

  void printLearningParams() {
    std::cout << "n_helpers = " << this->n_helpers << std::endl;
    std::cout << "iterations = " << this->iterations << std::endl;
    std::cout << "miniBatchSize = " << this->miniBatchSize << std::endl;
    std::cout << "learningRate = " << this->learningRate << std::endl;
    std::cout << "lambda = " << this->lambda << std::endl;
    std::cout << "doShuffle = " << this->doShuffle << std::endl;
  }

  void shuffleTrainingFile(std::string trainFile, std::string tmpFile) {
    std::vector<PackedSfenValue> allExmps;
    
    std::ifstream inf(trainFile, std::ios::binary);
    uint64_t n_cnt = 0;
    
    while (inf) {
      PackedSfenValue p;
		  if (inf.read((char*)&p, sizeof(PackedSfenValue))) {
        allExmps.push_back(p);
        n_cnt++;
		  } else {
			  break;
		  }

      //if (n_cnt % 1000000 == 0) {
      //  std::cout << "Read training example " << n_cnt << std::endl;
      //}
	  }
    inf.close();

    std::ofstream outf(tmpFile, std::ios::binary);
    for (unsigned int i = 0; i < allExmps.size(); i++) {
      uint32_t j = getRndIndex(i, allExmps.size() - i);
      PackedSfenValue cur = allExmps[j];
      allExmps[j] = allExmps[i];
      outf.write((char*)(&(cur)), sizeof(PackedSfenValue));
    }
    outf.close();

    std::cout << "Write shuffle file " << tmpFile << std::endl;
  }

  void train(std::string trainFile, std::string validFile) {

    //std::string shuffleFn = "./shuffle.bin";

    assert(n_helpers >= 1);
    ThreadWorker* helpers[16];
    std::thread all_threads[16];
    for (int i = 0; i < n_helpers; i++) {
      helpers[i] = new ThreadWorker(i);
    }
    // start threads
    for (int i = 1; i < n_helpers; i++) {
      all_threads[i] = std::thread(idle_loop, (helpers[i]));
    }

    SfenBufferedReader reader(doShuffle); // reader only do shuffle in each buffer
    reader.initSingleFile(trainFile);

    UtilTimer timer;
    trnFnPrinted = false;
    valFnPrinted = false;

    // print learning parameters
    printLearningParams();

    // weight to learn
    DenseVector weight(WEIGHT_DIM);
    Pos pos;

    initWeight(weight);
    // prepare save folder
    std::system("mkdir -p ./save");

    int iter;
    for (iter = 0; iter < iterations; iter++) {
      //if (doShuffle) {
      //  shuffleTrainingFile(trainFile, shuffleFn);
      //} else {
      //  shuffleFn = trainFile;
      //}
      reader.clear(); // prepare for the next cycle

      int t1 = 0;
      int t2 = 0;
      int t3 = 0;
      UtilTimer::time_t start_iter = UtilTimer::now();

      std::vector<PackedSfenValue> trainExamples;
      //std::ifstream train_fstream(shuffleFn, std::ios::binary);

      int64_t totalCnt = 0;
      int actualBatchSize = 0;
      int epoch = 0;
      do {

        UtilTimer::time_t start_t1 = UtilTimer::now();
        //actualBatchSize = fetch_one_mini_batch(train_fstream, trainExamples);
        actualBatchSize = reader.read_one_mini_batch(miniBatchSize, trainExamples);
        totalCnt += actualBatchSize;
        t1 += (timer.time(start_t1));

        if (actualBatchSize > 0) {
          FeatureMatrix featureMatrix;
/*
          std::vector<SparseVector> features;
          std::vector<double> y_truths;
          std::vector<double> y_preds;

          features.resize(actualBatchSize); 
          y_truths.resize(actualBatchSize); 
          y_preds.resize(actualBatchSize);
*/
          UtilTimer::time_t start_t2 = UtilTimer::now();
/*
          for (int j = 0; j < actualBatchSize; j++) {
            PackedSfenValue & exmp = trainExamples[j];
            SparseVector phi;

            // featurizer
            pos_from_sfen(pos, exmp.sfen, false);
            eval_featurize(pos, phi);

            double yhat = inference(phi, weight);

            //features.push_back(phi);
            //y_truths.push_back(exmp.score);
            //y_preds.push_back(yhat);
            features[j] = (phi);
            y_truths[j] = (exmp.score);
            y_preds[j] = (yhat);
          }
*/

          SharedList slist;
          //slist.init(&trainExamples, &weight, &features, &y_truths, &y_preds);
          slist.init(&trainExamples, &weight, &featureMatrix);

          int eachCnt = trainExamples.size() / n_helpers;
          int lastEnd = -1;
          for (int i = 0; i < n_helpers; i++) {
            int startIdx = lastEnd + 1;
            int endIdx = std::min(lastEnd + eachCnt, (int)trainExamples.size() - 1);
            helpers[i]->sendWork(&slist, startIdx, endIdx);

            lastEnd = endIdx;
          }
          // wake up slaves
          for (int i = 1; i < n_helpers; i++) {
            helpers[i]->wake_up();
          }
          helpers[0]->idle_loop_main();

          t2 += (timer.time(start_t2));

          // run optimization
          UtilTimer::time_t start_t3 = UtilTimer::now();
          //optimizeWeight(weight, featureMatrix);
          optimizeWeightNNUE(weight, featureMatrix);
          t3 += (timer.time(start_t3));
        }

      } while (actualBatchSize > 0);

      //train_fstream.close();

      // check the loss after one iteration

      UtilTimer::time_t start_valid = UtilTimer::now();
      //double loss = computeLoss(validExamples, weight);
      double loss = readValidSetAndComputeLoss(validFile, weight, 50000);
      std::cout << "validation time consumed " << (timer.time(start_valid) / 1000) << " secs." << std::endl;
      std::cout << "t1 = " << t1 << " t2 = " << t2 << " t3 = " << t3 << std::endl;

      std::cout << "Iteration " << iter << " Loss = " << loss << std::endl;

      // save weight snapshot
      dumpWeight(weight, getFilename("./save/weights", iter));
      dumpWeightInt100(weight, getFilename("./save/weights_int100", iter));

      int iter_secs = timer.time(start_iter) / 1000;
      std::cout << "Iteration time consumed " << iter_secs << " secs." << std::endl;
    }

    std::cout << "Done training." << std::endl;

    std::string outFilename = "./weights.txt";
    dumpWeight(weight, outFilename);
    dumpWeightInt100(weight, "./weights_int100.txt");
    std::cout << "Dump weight to " << outFilename << "." << std::endl;

    // stop all helpers
    for (int i = 0; i < n_helpers; i++) {
      helpers[i]->quit_now();
      helpers[i]->wake_up();
      usleep(100);
      delete helpers[i];
    }
    for (int i = 1; i < n_helpers; i++) {
      all_threads[i].join();
    }
  }

  void validate(std::string validFile, DenseVector &weight) {
    std::cout << "Load sfen file " << validFile << std::endl;
    double loss = readValidSetAndComputeLoss(validFile, weight, 50000);
    std::cout << "Validation Loss = " << loss << std::endl;
  }

  std::string getFilename(std::string prefix, int iter) {
    std::stringstream ss("");
    ss << prefix << "_" << iter << ".txt";
    return ss.str();
  }

  void initWeight(DenseVector & dv) {
    for (int i = 0; i < dv.size(); i++) {
      dv.set(i, 0);
    }
  }



  int fetch_one_mini_batch(std::ifstream &train_fstream, std::vector<PackedSfenValue> &examples) {
    examples.clear();

	  while (train_fstream) {
      PackedSfenValue p;
		  if (train_fstream.read((char*)&p, sizeof(PackedSfenValue))) {
        examples.push_back(p);
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

  void readValidSet(std::string filename, std::vector<PackedSfenValue> &examples) {
    std::ifstream inf(filename, std::ios::binary);

    int cnt = 0;
    examples.clear();

	  while (inf) {
      PackedSfenValue p;
		  if (inf.read((char*)&p, sizeof(PackedSfenValue))) {
        examples.push_back(p);
        cnt++;
		  } else {
        std::cout << "Done reading " << filename << std::endl;
			  break;
		  }
	  }
  }

  // ordinary sigmoid function
  double sigmoid(double x) {
    return 1.0 / (1.0 + std::exp(-x));
  }

  // A function that converts the evaluation value to the winning rate [0,1]
  double winning_percentage(double value) {
    // 1/(1+10^(-Eval/4))
    // = 1/(1+e^(-Eval/4*ln(10))
    // = sigmoid(Eval/4*ln(10))
    return sigmoid(value / PawnValueEg / 4.0 * log(10.0));
  }

  /*
  double dsigmoid(double x)
  {
    // Sigmoid function
    // f(x) = 1/(1+exp(-x))
    // the first derivative is
    // f'(x) = df/dx = f(x)・{ 1-f(x)}
    // becomes

    return sigmoid(x) * (1.0 - sigmoid(x));
  }
  */

  double computeLoss(std::vector<PackedSfenValue> &examples, DenseVector &weight) {
    /*
    Pos pos;

    double mseSum = 0;
    for (int i = 0; i < examples.size(); i++) {
      SparseVector phi;

      pos_from_sfen(pos, examples[i].sfen, false);
      eval_featurize(pos, phi);

      double yhat = inference(phi, weight);
      double y = examples[i].score;

      //std::cout << i << ": " << yhat << " " << y << std::endl;

      double mseSingle = (y - yhat) * (y - yhat);
      mseSum += mseSingle;
    }
    */

    if (examples.size() == 0) {
      return 0;
    }

    //double mseSum = computeLossSum(examples, weight);
    double mseSum = computeNnueLossSum(examples, weight);
    int count = examples.size();
    return (mseSum / ((double)count));
  }

  // loss without per example average
  double computeLossSum(std::vector<PackedSfenValue> &examples, DenseVector &weight) {
    Pos pos;

    double mseSum = 0;
    for (int i = 0; i < examples.size(); i++) {
      SparseVector phi;

      pos_from_sfen(pos, examples[i].sfen, false);
      eval_featurize(pos, phi);

      double yhat = inference(phi, weight);
      double y = examples[i].score;

      //std::cout << i << ": " << yhat << " " << y << std::endl;

      double mseSingle = (y - yhat) * (y - yhat);
      mseSum += mseSingle;
    }

    return (mseSum);
  }

  // nnue-style loss without per example average
  double computeNnueLossSum(std::vector<PackedSfenValue> &examples, DenseVector &weight) {
    Pos pos;
    const double epsilon = 0.000001;

    double lossSum = 0;
    for (int i = 0; i < examples.size(); i++) {
      SparseVector phi;

      pos_from_sfen(pos, examples[i].sfen, false);
      eval_featurize(pos, phi);

      double yhat = inference(phi, weight);
      double y = examples[i].score;

      double q = winning_percentage(yhat);
      double p = winning_percentage(y);
      double t = ((double)examples[i].game_result + 1.0) / 2.0;

	    // If the evaluation value in deep search exceeds ELMO_LAMBDA_LIMIT, apply ELMO_LAMBDA2 instead of ELMO_LAMBDA.
	    const double interpolate = nnueLambda; //(abs(deep) >= ELMO_LAMBDA_LIMIT) ? ELMO_LAMBDA2 : ELMO_LAMBDA;

	    double m = (1.0 - interpolate) * t + interpolate * p;

      double cross_entropy_eval =
        (-p * std::log(q + epsilon) - (1.0 - p) * std::log(1.0 - q + epsilon));
      double cross_entropy_win =
        (-t * std::log(q + epsilon) - (1.0 - t) * std::log(1.0 - q + epsilon));
      double entropy_eval =
        (-p * std::log(p + epsilon) - (1.0 - p) * std::log(1.0 - p + epsilon));
      double entropy_win =
        (-t * std::log(t + epsilon) - (1.0 - t) * std::log(1.0 - t + epsilon));

      double cross_entropy =
        (-m * std::log(q + epsilon) - (1.0 - m) * std::log(1.0 - q + epsilon));
      double entropy =
        (-m * std::log(m + epsilon) - (1.0 - m) * std::log(1.0 - m + epsilon));

      lossSum += cross_entropy;
      //std::cout << q << " " << p << " " << t << std::endl;
    }

    return (lossSum);
  }

  double readValidSetAndComputeLoss(std::string filename, 
                                    DenseVector &weight,
                                    int maxBatchSize) {

    std::ifstream inf(filename, std::ios::binary);

    double totalLoss = 0;
    int64_t cnt = 0;
    std::vector<PackedSfenValue> validExamples; // buffer size

	  while (inf) {
      PackedSfenValue p;
		  if (inf.read((char*)&p, sizeof(PackedSfenValue))) {

        validExamples.push_back(p);
        cnt++;

        // process one mini batch
        if (maxBatchSize > 0) {
          if (validExamples.size() >= maxBatchSize) {
            totalLoss += computeLossSum(validExamples, weight);
            //totalLoss += computeNnueLossSum(validExamples, weight);
            validExamples.clear();
          }
        }
		  } else {
        if (!valFnPrinted) {
          std::cout << "Done reading " << filename << std::endl;
          valFnPrinted = true;
        }
			  break;
		  }
	  }

    // for the last incomplete batch
    if (validExamples.size() > 0) {
      totalLoss += computeLossSum(validExamples, weight);
      //totalLoss += computeNnueLossSum(validExamples, weight);
    }
    
    if (cnt == 0) {
      return 0;
    }

    std::cout << "Total_loss = " << totalLoss << " example_count = " << cnt << std::endl;
    return (totalLoss / ((double)cnt));
  }
/*
  void optimizeWeight(DenseVector &weight, 
                      std::vector<SparseVector> &features, 
                      std::vector<double> &y_preds, 
                      std::vector<double> &y_truths) {

    if (features.size() == 0) {
      return; // nothing to optimize
    }

    assert(y_preds.size() == features.size());
    assert(y_preds.size() == y_truths.size());

    DenseVector gradient(weight.size());
    gradient.clear();

    // compute gradient
    for (int i = 0; i < features.size(); i++) {
      gradient.addSparseAndMulti(features[i], -1 * (y_preds[i] - y_truths[i]));
    }
    gradient.divide((double)features.size());

    gradient.multiply(learningRate);
    weight.addDense(gradient);
  }
*/
  void optimizeWeight(DenseVector &weight, 
                      FeatureMatrix &featureMatrix) {

    DenseVector gradient(weight.size());
    gradient.clear();

    // compute gradient
    int rowCnt = 0;
    for (int i = 0; i < n_helpers; i++) {
      for (int j = 0; j < featureMatrix.rows[i].size(); j++) {
        gradient.addSparseAndMulti(featureMatrix.rows[i][j].feature, 
                                   -1 * (featureMatrix.rows[i][j].y_pred - featureMatrix.rows[i][j].y_truth));
        rowCnt++;
      }
    }
    gradient.divide((double)rowCnt);

    if (rowCnt == 0) {
      return;
    }

    //for (int i = 0; i < features.size(); i++) {
    //  gradient.addSparseAndMulti(features[i], -1 * (y_preds[i] - y_truths[i]));
    //}
    //gradient.divide((double)features.size());
  
    if (lambda != 0) {
      DenseVector regularize(weight);
      regularize.multiply(-1 * lambda);
      gradient.addDense(regularize);
    }

    gradient.multiply(learningRate);
    weight.addDense(gradient);
  }

  void optimizeWeightNNUE(DenseVector &weight, 
                          FeatureMatrix &featureMatrix) {

    DenseVector gradient(weight.size());
    gradient.clear();

    double denoScale = (PawnValueEg * 4.0) / log(10.0); // denoScale = 1 / 358 = 0.00280898876

    // compute gradient
    int rowCnt = 0;
    for (int i = 0; i < n_helpers; i++) {
      for (int j = 0; j < featureMatrix.rows[i].size(); j++) {

        //double yhat = inference(phi, weight);
        //double y = examples[i].score;

        double q = winning_percentage(featureMatrix.rows[i][j].y_pred);
        double p = winning_percentage(featureMatrix.rows[i][j].y_truth);
        double t = ((double)featureMatrix.rows[i][j].game_result + 1.0) / 2.0;

        double gradFactor = nnueLambda * (q - p) + (1.0 - nnueLambda) * (q - t);
        gradient.addSparseAndMulti(featureMatrix.rows[i][j].feature, 
                                   -1 * gradFactor);
        //std::cout << q << " " << p << " " << t << " gradFactor = " << gradFactor << std::endl;
        rowCnt++;
      }
    }
    gradient.divide((double)rowCnt);

    if (rowCnt == 0) {
      return;
    }

    //for (int i = 0; i < features.size(); i++) {
    //  gradient.addSparseAndMulti(features[i], -1 * (y_preds[i] - y_truths[i]));
    //}
    //gradient.divide((double)features.size());
  
    if (lambda != 0) {
      DenseVector regularize(weight);
      regularize.multiply(-1 * lambda);
      gradient.addDense(regularize);
    }

    gradient.multiply(learningRate);
    //gradient.divide(denoScale);
    weight.addDense(gradient);
  }

  void dumpWeight(DenseVector &weight, std::string outFilename) {
    std::ofstream outf;
    outf.open(outFilename);

    int halfDim = WEIGHT_DIM / 2;
    for (int i = 0; i < halfDim; i++) {
      outf << "\tScore_Pair(" << weight.get(i) << "," << weight.get(i + halfDim) << ")," << std::endl;
    }
    outf << std::endl;
    outf.close();
  }

  void dumpWeightInt100(DenseVector &weight, std::string outFilename) {
    std::ofstream outf;
    outf.open(outFilename);

    int halfDim = WEIGHT_DIM / 2;
    for (int i = 0; i < halfDim; i++) {
      outf << "\tScore_Pair(";
      outf << weightCentPawn(weight.get(i)) << ",";
      outf << weightCentPawn(weight.get(i + halfDim));
      outf << ")," << std::endl;
    }
    outf << std::endl;
    outf.close();
  }

  int weightCentPawn(double w) {
    return (int)(w * 100.00);
  }

}; // end of LinRegTrainer


std::string getPathFileName(std::string path) {
  int startIdx = 0;
  for (int i = 0; i < path.length(); i++) {
    if (path[i] == '/') {
      startIdx = i + 1;
    }
  }
  int len = path.length() - startIdx;
  return path.substr(startIdx, len);
}

class FastLinRegTrainer : public LinRegTrainer {

  std::random_device rd;
  std::mt19937 rnd_gen{rd()};
  DenseVector featScalar;

public:

  bool doFeatureNormalization;

  void distributeFileList(std::vector<std::string> &trainFileList, ThreadWorker* helpers[]) {
    std::vector<std::string> lists[16];
    for (int i = 0; i < trainFileList.size(); i++) {
      int idx = i % n_helpers;
      lists[idx].push_back(trainFileList[i]);
    }

    for (int j = 0; j < n_helpers; j++) {
      helpers[j]->reader.initFileList(lists[j]);
      /*
      std::cout << "Init helper[" << j << "] file: ";
      for (int i = 0; i < lists[j].size(); i++) {
        if (i > 0) {
          std::cout << ","; 
        }
        std::cout << getPathFileName(lists[j][i]);
      }
      std::cout << std::endl;
      */
    }
  }

  void shuffleTrainFileList(std::vector<std::string> &fileList) {
    //randomize the file order so that different helpers will get different feature blocks
    //std::random_shuffle(fileList.begin(), fileList.end());
    std::shuffle(fileList.begin(), fileList.end(), rnd_gen);
    std::cout << "Shuffled block files allocation." << std::endl;
  }

  void printLearningParamsAdditional() {
    printLearningParams();
    std::cout << "doFeatNormalization = " << this->doFeatureNormalization << std::endl;
  }

  void train(std::vector<std::string> trainFileList, std::string validFile) {
    std::cout << "Reading from compressed binary features." << std::endl;

    assert(n_helpers >= 1);
    ThreadWorker* helpers[16];
    std::thread all_threads[16];
    for (int i = 0; i < n_helpers; i++) {
      helpers[i] = new ThreadWorker(i);
    }
    // start threads
    for (int i = 1; i < n_helpers; i++) {
      all_threads[i] = std::thread(idle_loop, (helpers[i]));
    }

    UtilTimer timer;
    trnFnPrinted = false;
    valFnPrinted = false;

    // print learning parameters
    printLearningParamsAdditional();

    // weight to learn
    DenseVector weight(WEIGHT_DIM);
    Pos pos;

    initWeight(weight);
    // prepare save folder
    std::system("mkdir -p ./save");

    if (doFeatureNormalization) {
      checkFeatureScale(trainFileList);
    }

    int iter;
    for (iter = 0; iter < iterations; iter++) {
      if (doShuffle) {
        //shuffleTrainingFile(trainFile, shuffleFn);
        shuffleTrainFileList(trainFileList);
      }
      distributeFileList(trainFileList, helpers);

      int t1 = 0;
      int t2 = 0;
      int t3 = 0;
      int t4 = 0;
      for (int i = 0; i < n_helpers; i++) {
        helpers[i]->t1 = 0;
        helpers[i]->t2 = 0;
        helpers[i]->t5 = 0;
      }
      UtilTimer::time_t start_iter = UtilTimer::now();

      //std::vector<FeatureAndFactor> featExamples;

      int batchId = 0;
      int64_t totalCnt = 0;
      int actualBatchSize = 0;
      //int epoch = 0;
      do {

        // batch id
        //if (batchId % 250 == 0) {
          //std::cout << "batchId = " << batchId << std::endl;
        //}
        batchId++;

        FeatureMatrix featureMatrix(n_helpers);

        UtilTimer::time_t start_t4 = UtilTimer::now();
        SharedList slist;
        slist.initReadInference(&weight, &featureMatrix);
        
        int eachCnt = miniBatchSize / n_helpers;
        int totalCnt = 0;
        for (int i = 0; i < n_helpers; i++) {
          int requestCnt = eachCnt; // the number of examples/feature_rows need to be read from file
          if (i == (n_helpers - 1)) { // last one
            requestCnt = miniBatchSize - totalCnt;
          }
          helpers[i]->sendDataReadAndWork(&slist, requestCnt, doFeatureNormalization, &featScalar);
          totalCnt += requestCnt;
        }
        t4 += (timer.time(start_t4));


        UtilTimer::time_t start_t2 = UtilTimer::now();

        // wake up slaves
        for (int i = 1; i < n_helpers; i++) {
          helpers[i]->wake_up();
        }
        helpers[0]->idle_loop_main();

        t2 += (timer.time(start_t2));
        
        // run optimization
        actualBatchSize = featureMatrix.countTotalRows();
        if (actualBatchSize > 0) {
          UtilTimer::time_t start_t3 = UtilTimer::now();
          optimizeWeight(weight, featureMatrix);
          //optimizeWeightNNUE(weight, featureMatrix);
          t3 += (timer.time(start_t3));
        }

      } while (actualBatchSize > 0);


      // check the loss after one iteration

      UtilTimer::time_t start_valid = UtilTimer::now();
      DenseVector weightScaled(weight);
      if (doFeatureNormalization) {
        scaleWeightVector(weightScaled);
        std::cout << "Weight is scaled for validation." << std::endl;
      }
      double loss = readValidSetAndComputeLoss(validFile, weightScaled, 50000);
      std::cout << "validation time consumed " << (timer.time(start_valid) / 1000) << " secs." << std::endl;
      std::cout << "t1 = " << t1 << " t2 = " << t2 << " t3 = " << t3 << std::endl;
      std::cout << "t4 = " << t4 << std::endl;
      int t1Sum = 0;
      int t2Sum = 0;
      for (int hid = 0; hid < n_helpers; hid++) {
        std::cout << "helper-" << hid << ": t1 = " << helpers[hid]->t1 << " t2 = " << helpers[hid]->t2 << " t5 = " << helpers[hid]->t5 << std::endl;
        t1Sum += (helpers[hid]->t1); 
        t2Sum += (helpers[hid]->t2); 
      }
      std::cout << "accum_t1 = " << t1Sum << " accum_t2 = " << t2Sum << std::endl;

      std::cout << "Iteration " << iter << " Loss = " << loss << std::endl;

      // save weight snapshot
      dumpWeight(weight, getFilename("./save/weights", iter));
      dumpWeightInt100(weight, getFilename("./save/weights_int100", iter));
      if (doFeatureNormalization) {
        dumpWeightInt100Scaled(weight, getFilename("./save/weights_scaled_int100", iter), doFeatureNormalization);
      }

      int iter_secs = timer.time(start_iter) / 1000;
      std::cout << "Iter time consumed " << iter_secs << " secs." << std::endl;
    }

    std::cout << "Done training." << std::endl;

    std::string outFilename = "./weights.txt";
    dumpWeight(weight, outFilename);
    dumpWeightInt100(weight, "./weights_int100.txt");
    std::cout << "Dump weight to " << outFilename << "." << std::endl;

    // stop all helpers
    for (int i = 0; i < n_helpers; i++) {
      helpers[i]->quit_now();
      helpers[i]->wake_up();
      usleep(100);
      delete helpers[i];
    }
    for (int i = 1; i < n_helpers; i++) {
      all_threads[i].join();
    }
  }

  void checkFeatureScale(std::vector<std::string> trainFileList) {
    BinFeatureReader reader;
    reader.initFileList(trainFileList);

    double featRange[2048][2];
    for (int i = 0; i < 2048; i++) {
      featRange[i][0] = 0;
      featRange[i][1] = 1;
    }

    uint64_t n_cnt = 0;
    int batchSize = 40000;
    int actualBatchSize = 0;
    do {
      std::vector<FeatureAndFactor> featExamples;
      actualBatchSize = reader.read_one_mini_batch(batchSize, featExamples);
      for (int i = 0; i < featExamples.size(); i++) {
        DenseVector dv(featExamples[i].sv, HALF_WEIGHT_DIM);
        for (int j = 0; j < dv.size(); j++) {
          int idx = j;
          double value = std::abs(dv.get(idx));
          featRange[idx][0] = std::min(featRange[idx][0], value);
          featRange[idx][1] = std::max(featRange[idx][1], value);
        }
        n_cnt++;
      }
    } while (actualBatchSize > 0);


    featScalar.setSize(HALF_WEIGHT_DIM);
    featScalar.clear();

    for (int i = 0; i < HALF_WEIGHT_DIM; i++) {
      //assert(featRange[i][0] == 0);
      //assert(featRange[i][1] >= featRange[i][0]);

      int roundedScale = std::ceil(featRange[i][1] - featRange[i][0]);
      featScalar.set(i, (double)roundedScale);
    }

    for (int i = 0; i < HALF_WEIGHT_DIM; i++) {
      std::cout << i << ": " << featRange[i][0] << " " << featRange[i][1] << " => " << featScalar.get(i) <<  std::endl;
    }
  }

  void saveFeatureScaleFile(std::string fn, double** featScale, DenseVector &dv) {

  }

  void readFeatureScaleFile(std::string fn) {

  }

  void dumpWeightInt100Scaled(DenseVector &weight, std::string outFilename, bool featScaled) {
    std::ofstream outf;
    outf.open(outFilename);

    int halfDim = WEIGHT_DIM / 2;
    for (int i = 0; i < halfDim; i++) {
      int mgIdx = i;
      int egIdx = i + halfDim;
      double weightScale = 1;
      if (featScaled) {
        if (featScalar.get(mgIdx) > 0) {
          weightScale = featScalar.get(mgIdx);
        }
        //assert((weight.get(mgIdx) != 0 || weight.get(egIdx) != 0) && (weightScale > 0));
        //assert((weight.get(mgIdx) == 0 && weight.get(egIdx) == 0) && (weightScale == 0));
        if ((weight.get(mgIdx) != 0 || weight.get(egIdx) != 0) && (weightScale < 1.0)) {
          std::cerr << "Weight scale is less than 1.0: " << mgIdx << " = " << weightScale << std::endl;
        }
      }
      double mgWeight = weight.get(mgIdx) / weightScale;
      double egWeight = weight.get(egIdx) / weightScale;
      
      outf << "\tScore_Pair(";
      outf << weightCentPawn(mgWeight) << ",";
      outf << weightCentPawn(egWeight);
      outf << ")," << std::endl;
    }
    outf << std::endl;
    outf.close();
  }

  void scaleWeightVector(DenseVector &weight) {
    int halfDim = WEIGHT_DIM / 2;
    for (int i = 0; i < halfDim; i++) {
      int mgIdx = i;
      int egIdx = i + halfDim;
      double weightScale = 1;

      if (featScalar.get(mgIdx) > 0) {
        weightScale = featScalar.get(mgIdx);
      }
      if ((weight.get(mgIdx) != 0 || weight.get(egIdx) != 0) && (weightScale < 1.0)) {
        std::cerr << "Weight scale is less than 1.0: " << mgIdx << " = " << weightScale << std::endl;
      }
      double mgWeightScaled = weight.get(mgIdx) / weightScale;
      double egWeightScaled = weight.get(egIdx) / weightScale;

      weight.set(mgIdx, mgWeightScaled);
      weight.set(egIdx, egWeightScaled);
    }
  }

};



void parse_line(std::string line, double &w1, double &w2) {
  std::stringstream ss("");

  int flag = 0;
  for (int i = 0; i < line.length(); i++) {

    if (line[i] == '(') {
      flag++;
    } else if (line[i] == ')') {
      flag--;
    } else {
      if (flag > 0) {
        if (line[i] == ',') {
          line[i] = ' ';
        }
        ss << line[i];
      }
    }
  }

  ss >> w1;
  ss >> w2;
}

void load_weight_from_file(std::string weightFn, DenseVector &weight) {

  std::ifstream inf;
  inf.open(weightFn);

  double values[1000][2];
  int len = 0;

  std::string line;
  while (inf) {
    std::getline(inf ,line);
    if (line == "") {
      continue;
    }

    double w_mg, w_eg;
    parse_line(line, w_mg, w_eg);
    values[len][0] = w_mg;
    values[len][1] = w_eg;
    len++;
  }

  weight.setSize(len * 2);
  for (int i = 0; i < len; i++) {
    weight.set(i, values[i][0] / 100.00);
    weight.set(i + len, values[i][1] / 100.00);
  }

  std::cout << "LENGTH = " << len << std::endl;
  inf.close();
}

void run_validate(std::string valFn, std::string weightFn) {
  LinRegTrainer trainer;
  DenseVector weight;

  load_weight_from_file(weightFn, weight);
  trainer.validate(valFn, weight);
}
/*
void run_training(std::string trnFn, std::string valFn) {
  LinRegTrainer trainer;

  trainer.iterations = 200;
  trainer.miniBatchSize = 40000;
  trainer.learningRate = 1;
  trainer.lambda = 0.00001;
  trainer.n_helpers = 16;
  trainer.doShuffle = false;
  trainer.nnueLambda = 1;

  std::cout << "Training file: " << trnFn << std::endl;
  std::cout << "Validation file: " << valFn << std::endl;

  trainer.train(trnFn, valFn);
}

void run_training_with_reader(std::vector<std::string> trnList, std::string valFn) {
  FastLinRegTrainer trainer;

  trainer.iterations = 200;
  trainer.miniBatchSize = 100000;
  trainer.learningRate = 0.002;
  trainer.lambda = 0.001;
  trainer.n_helpers = 16;
  trainer.doShuffle = true;
  trainer.doFeatureNormalization = false;

  std::cout << "Training files:" << std::endl;
  for (int i = 0; i < trnList.size(); i++) {
    std::cout << "  " << trnList[i] << std::endl;
  }
  std::cout << "Validation file: " << valFn << std::endl;

  trainer.train(trnList, valFn);
}
*/
void run_training_sfen(TrainingArguments &args) {
  LinRegTrainer trainer;

  trainer.iterations = args.epochs;
  trainer.miniBatchSize = args.miniBatchSize;
  trainer.learningRate = args.learningRate;
  trainer.lambda = args.regLambda;
  trainer.n_helpers = args.n_helpers;
  trainer.doShuffle = args.doShuffle;
  trainer.nnueLambda = args.nnueLambda;

  std::cout << "Training file: " << args.trnFn << std::endl;
  std::cout << "Validation file: " << args.valFn << std::endl;

  trainer.train(args.trnFn, args.valFn);
}

void run_training_feature_bin_chunks(TrainingArguments &args) {
  FastLinRegTrainer trainer; // fast trainer takes feature block binary files as input

  trainer.iterations = args.epochs;
  trainer.miniBatchSize = args.miniBatchSize;
  trainer.learningRate = args.learningRate;
  trainer.lambda = args.regLambda;
  trainer.n_helpers = args.n_helpers;
  trainer.doShuffle = args.doShuffle;
  trainer.nnueLambda = args.nnueLambda;
  trainer.doFeatureNormalization = args.doFeatureNormalization;

  std::cout << "Training files:" << std::endl;
  for (int i = 0; i < args.trnList.size(); i++) {
    std::cout << "  " << args.trnList[i] << std::endl;
  }
  std::cout << "Validation file: " << args.valFn << std::endl;

  trainer.train(args.trnList, args.valFn);
}

void load_training_data_to_mem(std::string trnFn) {

  std::ifstream inf(trnFn, std::ios::binary);

  std::vector<TrainingExample> trainingData;

  Pos pos;
  PackedSfenValue p;
  while (inf) {
		if (inf.read((char*)&p, sizeof(PackedSfenValue))) {
      // featurizing
      TrainingExample trnExmp;
      pos_from_sfen(pos, p.sfen, false);
      eval_featurize(pos, trnExmp.feature);
      trnExmp.y_truth = p.score;
      trnExmp.y_pred = 0;

      //// save to memory
      trainingData.push_back(trnExmp);
		} else {
			 break;
		}

    uint64_t n_cnt = trainingData.size();
    if (n_cnt % 1000000 == 0) {
      std::cout << "Load training example " << n_cnt << " to memory" << std::endl;
    }
	}

  inf.close();
}

class BufferedFileWriter {

  static const int MAX_BUF_SIZE = 65536;
  std::ofstream *outf;
  char buffer[MAX_BUF_SIZE];
  int curSize;
  char* head;

public:
  int getSize() {
    return curSize;
  }

  void init(std::ofstream* ptr) {
    outf = ptr;
    head = buffer;
    curSize = 0;
  }

  void flush() {
    assert((head - buffer) == curSize);
    outf->write(buffer, curSize);
    curSize = 0;
    head = buffer;
  }

  void flushWithSize() {
    assert(curSize <= 65536);
    uint16_t byteSize = curSize;
    outf->write((char*)&byteSize, sizeof(uint16_t));
    flush();
  }

  void write(char* src, int nBytes) {
    assert((curSize + nBytes) <= MAX_BUF_SIZE);
    memcpy(head, src, nBytes);
		head += nBytes;
    curSize += nBytes;
  }

  void writeNow(char* src, int nBytes) {
    outf->write(src, nBytes);
  }
};


//void writeFileCompress(std::ofstream &outf, FeatureAndFactor &fv, int y_truth) {
void writeFileCompress(BufferedFileWriter &outf, FeatureAndFactor &fv, int y_truth, int y_result) {
  DenseVector dv(fv.sv, WEIGHT_DIM);
  for (int i = 0; i < dv.size(); i++) {
    double v = dv.get(i);
    if (v != 0) {
      uint16_t idx = i;
      float fv = v;
      uint16_t flag = 0;

      //std::cout << "[" << (int)idx << "] = " << fv << std::endl;
        
      bool isFloat = isFloatNumber(fv);
      if (isFloat) { // 4 bytes
        //outf << idx << fv;
        outf.write((char*)&idx, sizeof(uint16_t));
        outf.write((char*)&fv, sizeof(float));

        byte_cnt[0]++;
      } else { // ?
        int32_t intfv = (int32_t)fv;
        if (intfv >= -127 && intfv <= 127) {
          int8_t b8 = (int8_t)fv;
          flag = (1 << 14);
          idx = idx | flag;

          outf.write((char*)&idx, sizeof(uint16_t));
          outf.write((char*)&b8, sizeof(int8_t));

          byte_cnt[1]++;
        } else if (intfv >= -32767 && intfv <= 32767) {
          int16_t b16 = (int16_t)fv;
          flag = (2 << 14);
          idx = idx | flag;

          //outf << idx << b16;
          outf.write((char*)&idx, sizeof(uint16_t));
          outf.write((char*)&b16, sizeof(int16_t));

          byte_cnt[2]++;
        } else {
          flag = (3 << 14);
          idx = idx | flag;

          //outf << idx << intfv;
          outf.write((char*)&idx, sizeof(uint16_t));
          outf.write((char*)&intfv, sizeof(int32_t));

          byte_cnt[3]++;
        }
      }
    }
  }
  int16_t ygold = y_truth; // [-1, 0, 1] --> [0, 1, 2]
  uint16_t result = y_result + 1; // []
  uint16_t classIdx = WEIGHT_DIM | (result << 14);
  //outf << classIdx << ygold;
  outf.write((char*)&classIdx, sizeof(uint16_t));
  outf.write((char*)&ygold, sizeof(int16_t));

  uint8_t mg_fctr = fv.mg_factor;
  uint8_t eg_fctr = fv.eg_factor;

  //outf << mg_fctr;
  //outf << eg_fctr;
  outf.write((char*)&mg_fctr, sizeof(uint8_t));
  outf.write((char*)&eg_fctr, sizeof(uint8_t));
  //std::cout << ygold << " " << (int)mg_fctr << " " << (int)eg_fctr << std::endl;
}

void convert_sfen_to_bin(std::string trnFn, std::string outputFn) {

  byte_cnt[0] = 0;
  byte_cnt[1] = 0;
  byte_cnt[2] = 0;
  byte_cnt[3] = 0;

  std::ifstream inf(trnFn, std::ios::binary);
  std::ofstream outf(outputFn, std::ios::binary);

  char myBuff[65536];
  std::filebuf* buf_ptr = outf.rdbuf();
  buf_ptr->pubsetbuf(myBuff, 65536);
  BufferedFileWriter writer;
  writer.init(&outf);
  
  int flushLimit = 10;
  int flushCnt = 0;

  int maxByteCnt = 0;
  uint64_t n_cnt = 0;
  Pos pos;
  PackedSfenValue p;
  while (inf) {
		if (inf.read((char*)&p, sizeof(PackedSfenValue))) {
      // featurizing
      //TrainingExample trnExmp;
      FeatureAndFactor featFactor;
      pos_from_sfen(pos, p.sfen, false);
      
      //eval_featurize(pos, trnExmp.feature);
      //trnExmp.y_truth = p.score;
      //trnExmp.y_pred = 0;

      eval_featurize_with_factor(pos, featFactor);

      //// save to file
      //writeFileCompressWithByteCnt(outf, featFactor, p.score, byteCnt);
      //writeFileCompress(outf, featFactor, p.score);
      writeFileCompress(writer, featFactor, p.score, p.game_result);
      flushCnt++;
      int byteCnt = writer.getSize();
      if (maxByteCnt < byteCnt) {
        maxByteCnt = byteCnt;
      }

      if (flushCnt >= flushLimit) {
        writer.flushWithSize();
        flushCnt = 0;
      }

      n_cnt++;
		} else {
			 break;
		}

    if (n_cnt % 1000000 == 0) {
      std::cout << "Convert training example " << n_cnt << " to csv" << std::endl;
      std::cout << "max byte count = " << maxByteCnt << std::endl;
    }
	}

  // last block
  if (flushCnt >= 0) {
    writer.flushWithSize();
    flushCnt = 0;
  }

  outf.close();
  inf.close();

  for (int i = 0; i < 4; i++) {
    std::cout << byte_cnt[i] << std::endl;
  }
  std::cout << "max byte count = " << maxByteCnt << std::endl;
}

std::string get_chunck_file_name(std::string baseOutputName, int idx) {
  std::string name = baseOutputName;
  for (int i = (baseOutputName.length() - 1); i >= 0; i--) {
    if (baseOutputName[i] == '.') {
      name = baseOutputName.substr(0, i);
      break;
    }
  }

  std::stringstream ss("");
  ss << name << "-" << idx << ".bin";
  return ss.str();
}

void convert_sfen_to_bin_blocks(std::string trnFn, 
                                std::string outputFldr, 
                                std::string baseOutputName, 
                                uint64_t chunkSize) {

  byte_cnt[0] = 0;
  byte_cnt[1] = 0;
  byte_cnt[2] = 0;
  byte_cnt[3] = 0;

  std::ifstream inf(trnFn, std::ios::binary);

  int fileIdx = -1;
  std::vector<std::string> fns;
  std::ofstream outf;

  BufferedFileWriter writer;
  writer.init(&outf);
  

  uint64_t n_cnt = 0;
  uint64_t n_chunk = 0;

  int flushLimit = 10;
  int flushCnt = 0;
  
  Pos pos;
  PackedSfenValue p;
  while (inf) {
		if (inf.read((char*)&p, sizeof(PackedSfenValue))) {

      FeatureAndFactor featFactor;
      pos_from_sfen(pos, p.sfen, false);
      eval_featurize_with_factor(pos, featFactor);

      if (n_chunk >= chunkSize || n_cnt == 0) {
        n_chunk = 0;
        if (fileIdx >= 0) {
          // last block
          if (flushCnt >= 0) {
            writer.flushWithSize();
            flushCnt = 0;
          }
          
          // close current
          outf.close();
          outf.clear();
          std::cout << "Done writing file " << fns[fileIdx] << std::endl;
        }

        // prepare for the next
        fileIdx++;
        std::string newFn = outputFldr + "/" + get_chunck_file_name(baseOutputName, fileIdx);
        fns.push_back(newFn);
        outf.open(newFn, std::ios::binary);
        std::cout << "Start writing file " << std::endl;
      }

      //// save to file
      writeFileCompress(writer, featFactor, p.score, p.game_result);
      flushCnt++;

      if (flushCnt >= flushLimit) {
        writer.flushWithSize();
        flushCnt = 0;
      }
      
      n_cnt++;
      n_chunk++;
		} else {
			break;
		}


    if (n_cnt % 1000000 == 0) {
      std::cout << "Convert training example " << n_cnt << " to csv" << std::endl;
    }

    //if (n_cnt >= 160000) {
    //  break;
    //}
	}

  // last block
  if (flushCnt >= 0) {
    writer.flushWithSize();
    flushCnt = 0;
  }

  outf.close();
  inf.close();

  for (int i = 0; i < 4; i++) {
    std::cout << byte_cnt[i] << std::endl;
  }

  std::cout << "Dump files:" << std::endl;
  for (int i = 0; i < fns.size(); i++) {
    std::cout << fns[i] << std::endl;
  }

  // dump the file list
  std::ofstream listf;
  listf.open(outputFldr + "/filelist.txt");
  for (int i = 0; i < fns.size(); i++) {
    listf << fns[i] << std::endl;
  }
  listf.close();
}

void test_shuffle() {

  const int exmpCnt = 10000000;
  std::string fn1 = "tmp1.bin";

  std::vector<PackedSfenValue> testExmps;
  for (uint32_t i = 0; i < exmpCnt; i++) {
    PackedSfenValue p;
    for (int j = 0; j < 32; j++) {
      p.sfen.data[j] = rand();
    }
    uint32_t msk1 = 0xffff0000;
    uint32_t msk2 = 0x0000ffff;
    p.game_result = 0;
    p.gamePly = (i & msk1) >> 16;
    p.move = i & msk2;
    testExmps.push_back(p);
  }

  std::ofstream outf(fn1, std::ios::binary);
  for (unsigned int i = 0; i < testExmps.size(); i++) {
    outf.write((char*)(&(testExmps[i])), sizeof(PackedSfenValue));
  }
  outf.close();

  std::cout << "Done gnerating..." << std::endl;


  LinRegTrainer trainer;

  std::string fn2 = "tmp2.bin";
  trainer.shuffleTrainingFile(fn1, fn2);

  std::cout << "Done shuffle..." << std::endl;
  
  // read and mark

  std::ifstream inf(fn2, std::ios::binary);
  while (inf) {
    PackedSfenValue p;
		if (inf.read((char*)&p, sizeof(PackedSfenValue))) {
      uint32_t idx = p.gamePly;
      idx = (idx << 16) | p.move;
      testExmps[idx].game_result++;
      for (int j = 0; j < 32; j++) {
        assert(p.sfen.data[j] == testExmps[idx].sfen.data[j]);
      }
		} else {
			 break;
		}
	}
  inf.close();

  std::cout << "Done read..." << std::endl;

  // final check
  for (unsigned int i = 0; i < testExmps.size(); i++) {
    assert(testExmps[i].game_result == 1);
  }

  std::cout << "Done check..." << std::endl;
}


void generate_senpai_sfen_bin(std::string inputFn, std::string outputFn) {

  std::ifstream inf(inputFn, std::ios::binary);
  std::ofstream outf(outputFn, std::ios::binary);

  uint64_t n_cnt = 0;
  
  Pos pos;
  PackedSfenValue p;

  while (inf) {
		if (inf.read((char*)(&p), sizeof(PackedSfenValue))) {
      pos_from_sfen(pos, p.sfen, false);
      int spenScore = eval_nonreduce(pos);
      
      //std::cout << spenScore << " " << p.score << std::endl;
      
      p.score = spenScore;
      outf.write((char*)(&p), sizeof(PackedSfenValue));

      n_cnt++;
		} else {
			break;
		}

    if (n_cnt % 1000000 == 0) {
      std::cout << "Convert training example " << n_cnt << " to csv" << std::endl;
    }
	}

  outf.close();
  inf.close();
}


void check_feature_scale_bin(std::string trnFn) {

  std::ifstream inf(trnFn, std::ios::binary);
  uint64_t n_cnt = 0;

  double featScale[2048][2];
  for (int i = 0; i < 2048; i++) {
    featScale[i][0] = 9999999;
    featScale[i][1] = -9999999;
  }

  Pos pos;
  PackedSfenValue p;
  while (inf) {
		if (inf.read((char*)&p, sizeof(PackedSfenValue))) {
      FeatureAndFactor featFactor;
      pos_from_sfen(pos, p.sfen, false);
      eval_featurize_with_factor(pos, featFactor);

      DenseVector dv(featFactor.sv, HALF_WEIGHT_DIM);
      for (int j = 0; j < dv.size(); j++) {
        int idx = j;
        double value = std::abs(dv.get(j));
        featScale[idx][0] = std::min(featScale[idx][0], value);
        featScale[idx][1] = std::max(featScale[idx][1], value);
      }

      n_cnt++;
		} else {
			break;
		}

    if (n_cnt % 1000000 == 0) {
      std::cout << "Convert training example " << n_cnt << " to csv" << std::endl;
    }

    //if (n_cnt >= 160000) {
    //  break;
    //}
	}

  inf.close();

  for (int i = 0; i < HALF_WEIGHT_DIM; i++) {
    std::cout << i << ": " << featScale[i][0] << " " << featScale[i][1] << std::endl;
  }
}
