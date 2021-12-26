// includes

#include <algorithm>
#include <cctype>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <string>
#include <cmath>
#include <cstdint>
#include <random>
#include <cstring>

#include "senp2_eval.hpp"
#include "readsfen.hpp"


// functions

bool square_is_ok(int fl, int rk) {
   return file_is_ok(fl) && rank_is_ok(rk);
}

bool square_is_ok(int sq) {
   return sq >= 0 && sq < Square_Size;
}

Square square_make(int fl, int rk) {
   assert(square_is_ok(fl, rk));
   return square_make((fl << 3) | rk); // file major for pawns
}

Square square_make(int fl, int rk, Side sd) {
   assert(square_is_ok(fl, rk));
   return square_make(fl, rank_side(Rank(rk), sd));
}

Square square_make(int sq) {
   assert(square_is_ok(sq));
   return Square(sq);
}

File square_file(Square sq) {
   return File(sq >> 3);
}

Rank square_rank(Square sq) {
   return Rank(sq & 7);
}

Rank square_rank(Square sq, Side sd) {
   return rank_side(square_rank(sq), sd);
}

bool square_is_promotion(Square sq) {
   return ((square_rank(sq) + 1) & 7) < 2;
}

int square_colour(Square sq) {
   return (square_file(sq) ^ square_rank(sq)) & 1;
}

Inc square_inc(Side sd) { // not used externally
   return Inc(1 - sd * 2);
}

Square square_front(Square sq, Side sd) {
   return square_make(sq + square_inc(sd));
}

Square square_rear(Square sq, Side sd) {
   return square_make(sq - square_inc(sd));
}

Square square_prom(Square sq, Side sd) {
   return square_make(square_file(sq), Rank_8, sd);
}

int square_dist(Square s0, Square s1) {
   return std::max(square_dist_file(s0, s1), square_dist_rank(s0, s1));
}

int square_dist_file(Square s0, Square s1) {
   return std::abs(square_file(s0) - square_file(s1));
}

int square_dist_rank(Square s0, Square s1) {
   return std::abs(square_rank(s0) - square_rank(s1));
}


bool file_is_ok(int fl) {
   return fl >= 0 && fl < File_Size;
}

bool rank_is_ok(int rk) {
   return rk >= 0 && rk < Rank_Size;
}

File file_make(int fl) {
   assert(file_is_ok(fl));
   return File(fl);
}

Rank rank_make(int rk) {
   assert(rank_is_ok(rk));
   return Rank(rk);
}

File file_opp(File fl) {
   return File(fl ^ 7);
}

Rank rank_opp(Rank rk) {
   return Rank(rk ^ 7);
}

Rank rank_side(Rank rk, Side sd) {
   return Rank((rk ^ -sd) & 7);
}



Vec vector_make(int df, int dr) {

   assert(std::abs(df) < File_Size);
   assert(std::abs(dr) < Rank_Size);

   return Vec((dr + (Rank_Size - 1)) * Vector_File_Size + (df + (File_Size - 1)));
}

Square square_add(Square from, Vec vec) {

   Vec to = vector_make(square_file(from), square_rank(from)) + vec - Vector_Max;

   int df = to % Vector_File_Size - (File_Size - 1);
   int dr = to / Vector_File_Size - (Rank_Size - 1);

   return !square_is_ok(df, dr) ? Square_None : square_make(df, dr);
}

bool piece_is_ok(int pc) {
   return pc >= 0 && pc < Piece_Size;
}

Piece piece_make(int pc) {
   if (!piece_is_ok(pc)) {
      std::cout << "pc = " << pc << std::endl;
   }
   assert(piece_is_ok(pc));
   return Piece(pc);
}

bool piece_is_minor(Piece pc) {
   return pc == Knight || pc == Bishop;
}




bool side_is_ok(int sd) {
   return sd >= 0 && sd < Side_Size;
}

Side side_make(int sd) {
   assert(side_is_ok(sd));
   return Side(sd);
}

Side side_opp(Side sd) {
   return Side(sd ^ 1);
}


bool piece_side_is_ok(int ps) { // excludes Empty
   return ps >= 0 && ps < Piece_Side_Size;
}

Piece_Side piece_side_make(int ps) {
   assert(piece_side_is_ok(ps));
   return Piece_Side(ps);
}

Piece_Side piece_side_make(Piece pc, Side sd) {
   assert(pc != Piece_None);
   return piece_side_make((pc << 1) | sd);
}

Piece piece_side_piece(Piece_Side ps) {
   assert(ps != Empty);
   return piece_make(ps >> 1);
}

Side piece_side_side(Piece_Side ps) {
   assert(ps != Empty);
   return side_make(ps & 1);
}

Bit::Bit() {
   p_bit = 0;
}

Bit::Bit(uint64 bit) {
   p_bit = bit;
}

Bit::operator uint64() const {
   return p_bit;
}

void Bit::operator|=(Bit b) {
   p_bit |= uint64(b);
}

void Bit::operator&=(uint64 b) {
   p_bit &= b;
}

void Bit::operator^=(Bit b) {
   p_bit ^= uint64(b);
}


///// libmy.cpp ///////////////////////////////////////

namespace ml {

// functions

// math

uint64 rand_int_64() {
   static std::mt19937_64 gen;
   return gen();
}

int round(double x) {
   return int(floor(x + 0.5));
}

int div(int a, int b) {

   assert(b > 0);

   if (b <= 0) {
      std::cerr << "ml::div(): divide error" << std::endl;
      std::exit(EXIT_FAILURE);
   }

   int div = a / b;
   if (a < 0 && a != b * div) div--; // fix buggy C semantics

   return div;
}

int div_round(int a, int b) {
   assert(b > 0);
   return div(a + b / 2, b);
}

bool is_power_2(int64 n) {
   assert(n >= 0);
   return (n & (n - 1)) == 0 && n != 0;
}

int log_2(int64 n) {

   assert(n > 0);

   int ln = -1;

   for (; n != 0; n >>= 1) {
      ln++;
   }

   assert(ln >= 0);
   return ln;
}

}

///// math.cpp ///////////////////////////////////////

namespace math {

// constants

const int Table_Size = 256;

// variables

static float Sqrt [Table_Size];
static float Log_2[Table_Size];

// functions

void init() {

   for (int i = 0; i < Table_Size; i++) {

      double x = double(i);

      Sqrt[i]  = std::sqrt(x);
      Log_2[i] = std::log2(x);
   }
}

double sqrt(int n) {
   assert(n >= 0 && n < Table_Size);
   return Sqrt[n];
}

double log_2(int n) {
   assert(n > 0 && n < Table_Size);
   return Log_2[n];
}

}



////// score.cpp /////////////////////////////////////

namespace score {

// functions

bool is_ok(int sc) {
   return sc >= -Inf && sc <= +Inf;
}

bool is_win_loss(Score sc) {
   return std::abs(sc) > Eval_Inf;
}

bool is_win(Score sc) {
   return sc > +Eval_Inf;
}

bool is_loss(Score sc) {
   return sc < -Eval_Inf;
}

bool is_eval(Score sc) {
   return std::abs(sc) <= Eval_Inf;
}

Score clamp(Score sc) {

   if (is_win(sc)) {
      sc = +Eval_Inf;
   } else if (is_loss(sc)) {
      sc = -Eval_Inf;
   }

   assert(is_eval(sc));
   return sc;
}

Score add_safe(Score sc, Score inc) {

   if (is_eval(sc)) {
      return clamp(sc + inc);
   } else {
      return sc;
   }
}

int ply(Score sc) {
   assert(is_win_loss(sc));
   return Inf - std::abs(sc);
}

}


// includes
/*
#include <algorithm>

#include "attack.hpp"
#include "bit.hpp"
#include "common.hpp"
#include "eval.hpp" // for piece_mat
#include "pos.hpp"
*/

// prototypes

static bool in_check (const Pos & pos, Side sd);

static Bit slider_attacks_to (const Pos & pos, Side sd, Square to);

static Bit  attacks_to        (const Pos & pos, Side sd, Square to, Bit pieces);
static Bit  pseudo_attacks_to (const Pos & pos, Side sd, Square to);

// functions
/*
bool is_legal(const Pos & pos) {
   return !in_check(pos, side_opp(pos.turn()));
}

bool in_check(const Pos & pos) {
   return checks(pos) != 0;
}

static bool in_check(const Pos & pos, Side sd) {
   return has_attack(pos, side_opp(sd), pos.king(sd));
}
*/
bool has_attack(const Pos & pos, Side sd, Square to) {
   return has_attack(pos, sd, to, pos.pieces());
}

bool has_attack(const Pos & pos, Side sd, Square to, Bit pieces) {

   for (Bit b = pseudo_attacks_to(pos, sd, to); b != 0; b = bit::rest(b)) {
      Square from = bit::first(b);
      if (bit::line_is_empty(from, to, pieces)) return true;
   }

   return false;
}

Bit attacks_to(const Pos & pos, Side sd, Square to, Bit pieces) {

   Bit froms = Bit(0);

   for (Bit b = pseudo_attacks_to(pos, sd, to); b != 0; b = bit::rest(b)) {
      Square from = bit::first(b);
      if (bit::line_is_empty(from, to, pieces)) bit::set(froms, from);
   }

   return froms;
}

Bit pseudo_attacks_to(const Pos & pos, Side sd, Square to) {

   Bit froms = Bit(0);

   for (int p = 0; p < Piece_Size; p++) {
      Piece pc = piece_make(p);
      froms |= pos.pieces(pc, sd) & bit::piece_attacks_to(pc, sd, to);
   }

   return froms;
}

bool is_pinned(const Pos & pos, Square king, Square sq, Side sd) {

   Side xd = side_opp(sd);

   Bit pieces = bit::remove(pos.pieces(), sq);

   for (Bit b = pos.sliders(xd) & bit::beyond(king, sq); b != 0; b = bit::rest(b)) {

      Square ds = bit::first(b);
      Piece  dp = pos.piece(ds);

      if (bit::piece_attack(dp, xd, ds, king, pieces)) return true;
   }

   return false;
}

Square pinned_by(const Pos & pos, Square king, Square sq, Side sd) {

   Side xd = side_opp(sd);

   Bit pieces = bit::remove(pos.pieces(), sq);

   for (Bit b = pos.sliders(xd) & bit::beyond(king, sq); b != 0; b = bit::rest(b)) {

      Square ds = bit::first(b);
      Piece  dp = pos.piece(ds);

      if (bit::piece_attack(dp, xd, ds, king, pieces)) return ds;
   }

   return Square_None;
}

Bit pins(const Pos & pos, Square king) {

   Bit pins = Bit(0);

   Side sd = pos.side(king);
   Side xd = side_opp(sd);

   for (Bit b = slider_attacks_to(pos, xd, king); b != 0; b = bit::rest(b)) {

      Square ds = bit::first(b);

      Bit between = bit::between(ds, king) & pos.pieces();
      if (bit::is_single(between)) pins |= between;
   }

   return pins;
}

static Bit slider_attacks_to(const Pos & pos, Side sd, Square to) {
   return ((pos.pieces(Bishop, sd) | pos.pieces(Queen, sd)) & bit::piece_attacks_to(Bishop, sd, to))
        | ((pos.pieces(Rook,   sd) | pos.pieces(Queen, sd)) & bit::piece_attacks_to(Rook,   sd, to));
}

void Attack_Info::init(const Pos & pos) {

   for (int s = 0; s < Side_Size; s++) {

      Side sd = side_make(s);

      p_attacks[sd] = Bit(0);
      p_support[sd] = Bit(0);

      Piece pc;
      Bit froms;

      // pawns

      pc    = Pawn;
      froms = pos.pieces(pc, sd);

      p_le_pieces [sd][pc] = froms;
      p_le_attacks[sd][pc] = Bit(0);

      Bit tos = bit::pawn_attacks(sd, froms);

      p_le_attacks[sd][pc] |= tos;
      p_support[sd] |= tos & p_attacks[sd];
      p_attacks[sd] |= tos;

      // knight

      pc    = Knight;
      froms = pos.pieces(pc, sd);

      p_le_pieces [sd][pc] = p_le_pieces [sd][pc - 1] | froms;
      p_le_attacks[sd][pc] = p_le_attacks[sd][pc - 1];

      for (Bit b = froms; b != 0; b = bit::rest(b)) {

         Square from = bit::first(b);
         Bit    tos  = bit::knight_attacks(from);

         p_piece_attacks[from] = tos;
         p_le_attacks[sd][pc] |= tos;
         p_support[sd] |= tos & p_attacks[sd];
         p_attacks[sd] |= tos;
      }

      // bishop

      pc    = Bishop;
      froms = pos.pieces(pc, sd);

      p_le_pieces [sd][pc] = p_le_pieces [sd][pc - 1] | froms;
      p_le_attacks[sd][pc] = p_le_attacks[sd][pc - 1];

      for (Bit b = froms; b != 0; b = bit::rest(b)) {

         Square from = bit::first(b);
         Bit    tos  = bit::bishop_attacks(from, pos.pieces());

         p_piece_attacks[from] = tos;
         p_le_attacks[sd][pc] |= tos;
         p_support[sd] |= tos & p_attacks[sd];
         p_attacks[sd] |= tos;
      }

      // rook

      pc    = Rook;
      froms = pos.pieces(pc, sd);

      p_le_pieces [sd][pc] = p_le_pieces [sd][pc - 1] | froms;
      p_le_attacks[sd][pc] = p_le_attacks[sd][pc - 1];

      for (Bit b = froms; b != 0; b = bit::rest(b)) {

         Square from = bit::first(b);
         Bit    tos  = bit::rook_attacks(from, pos.pieces());

         p_piece_attacks[from] = tos;
         p_le_attacks[sd][pc] |= tos;
         p_support[sd] |= tos & p_attacks[sd];
         p_attacks[sd] |= tos;
      }

      // queen

      pc    = Queen;
      froms = pos.pieces(pc, sd);

      p_le_pieces [sd][pc] = p_le_pieces [sd][pc - 1] | froms;
      p_le_attacks[sd][pc] = p_le_attacks[sd][pc - 1];

      for (Bit b = froms; b != 0; b = bit::rest(b)) {

         Square from = bit::first(b);
         Bit    tos  = bit::queen_attacks(from, pos.pieces());

         p_piece_attacks[from] = tos;
         p_le_attacks[sd][pc] |= tos;
         p_support[sd] |= tos & p_attacks[sd];
         p_attacks[sd] |= tos;
      }

      // king

      pc    = King;
      froms = pos.pieces(pc, sd);

      p_le_pieces [sd][pc] = p_le_pieces [sd][pc - 1] | froms;
      p_le_attacks[sd][pc] = p_le_attacks[sd][pc - 1];

      for (Bit b = froms; b != 0; b = bit::rest(b)) {

         Square from = bit::first(b);
         Bit    tos  = bit::king_attacks(from);

         p_piece_attacks[from] = tos;
         p_le_attacks[sd][pc] |= tos;
         p_support[sd] |= tos & p_attacks[sd];
         p_attacks[sd] |= tos;
      }

      // wrap up

      p_le_pieces [sd][Knight] |= p_le_pieces [sd][Bishop];
      p_le_attacks[sd][Knight] |= p_le_attacks[sd][Bishop];
   }
}

Bit Attack_Info::piece_attacks(Square from) const {
   return p_piece_attacks[from];
}

Bit Attack_Info::attacks(Side sd) const {
   return p_attacks[sd];
}

Bit Attack_Info::pawn_attacks(Side sd) const {
   return p_le_attacks[sd][Pawn];
}

Bit Attack_Info::queen_attacks(Side sd) const {
   return p_le_attacks[sd][Queen];
}

Bit Attack_Info::queen_safe(Side sd) const {

   Side xd = side_opp(sd);

   return ~p_le_attacks[xd][King]
        | (~p_le_attacks[xd][Queen] & p_support[sd]);
}


// includes
/*
#include <cstdio>
#include <iostream>

#include "bit.hpp"
#include "common.hpp"
*/
namespace bit {

// constants

const int  Bishop_Bits {  9 };
const int  Rook_Bits   { 12 };

const int  Bishop_Size { 1 << Bishop_Bits };
const int  Rook_Size   { 1 << Rook_Bits };

// variables

Bit Pawn_Squares;
Bit Promotion_Squares;
Bit Colour_Squares[2];

Bit FileBtoH;
Bit FileAtoG;
Bit Rank1to7;
Bit Rank2to8;

static Bit File_[File_Size];
static Bit Rank_[Rank_Size];

static Bit Pawn_Moves   [Side_Size][Square_Size];
static Bit Pawn_Attacks [Side_Size][Square_Size];
static Bit Piece_Attacks[Side_Size][Piece_Size_2][Square_Size];

#if BMI
static Bit Bishop_Attacks[Square_Size][Bishop_Size];
static Bit Rook_Attacks  [Square_Size][Rook_Size];
#endif

static Bit Blocker[Square_Size];

static Bit Ray    [Square_Size][Square_Size];
static Bit Beyond [Square_Size][Square_Size];
static Bit Between[Square_Size][Square_Size];

// prototypes

static Bit ray_1      (Square from, Vec vec);
static Bit ray_almost (Square from, Vec vec);
static Bit ray        (Square from, Vec vec);

static Bit piece_attacks (Square from, Bit tos, Bit pieces);

// functions

static void print_bitboard(Bit bb) {
   int i, j, tval;
	char outchar;

	printf("  -------64BIT------\n");
	for (i = 7; i >= 0; i--) {
		printf("%d  ", i+1);
		for (j = 0; j <= 7; j++) {
         int sq = square_make(j, i);
			if (bb & ml::bit(sq)) {
				outchar = '*';
         } else {
				outchar = '.';
         }
			printf("%c ", outchar);
		}
		printf("\n");
	}
	printf("  -------64bit----- \n");
}

void init() {

   // files and ranks

   for (int s = 0; s < Square_Size; s++) {

      Square sq = square_make(s);

      set(File_[square_file(sq)], sq);
      set(Rank_[square_rank(sq)], sq);

      set(Colour_Squares[square_colour(sq)], sq);
   }

   Pawn_Squares      = rect(0, 1, File_Size, Rank_Size - 1);
   Promotion_Squares = ~Pawn_Squares;

   // piece init

   Vec vec_nw  = vector_make(-1, +1);
   Vec vec_n   = vector_make( 0, +1);
   Vec vec_ne  = vector_make(+1, +1);
   Vec vec_w   = vector_make(-1,  0);
   Vec vec_e   = vector_make(+1,  0);
   Vec vec_sw  = vector_make(-1, -1);
   Vec vec_s   = vector_make( 0, -1);
   Vec vec_se  = vector_make(+1, -1);

   Vec vec_nnw = vector_make(-1, +2);
   Vec vec_nne = vector_make(+1, +2);
   Vec vec_nww = vector_make(-2, +1);
   Vec vec_nee = vector_make(+2, +1);
   Vec vec_sww = vector_make(-2, -1);
   Vec vec_see = vector_make(+2, -1);
   Vec vec_ssw = vector_make(-1, -2);
   Vec vec_sse = vector_make(+1, -2);

   const Vec Queen_Vec[8] {
      vec_nw, vec_n, vec_ne, vec_w, vec_e, vec_sw, vec_s, vec_se,
   };

   const Vec Knight_Vec[8] {
      vec_nnw, vec_nne, vec_nww, vec_nee, vec_sww, vec_see, vec_ssw, vec_sse,
   };

   // piece attacks

   for (int f = 0; f < Square_Size; f++) {

      Square from = square_make(f);

      Bit knight = Bit(0);
      Bit bishop = Bit(0);
      Bit rook = Bit(0);
      Bit king = Bit(0);

      for (int dir = 0; dir < 8; dir++) {
         Vec vec = Knight_Vec[dir];
         knight |= ray_1(from, vec);
      }

      bishop |= ray(from, vec_nw);
      bishop |= ray(from, vec_ne);
      bishop |= ray(from, vec_sw);
      bishop |= ray(from, vec_se);

      rook |= ray(from, vec_n);
      rook |= ray(from, vec_w);
      rook |= ray(from, vec_e);
      rook |= ray(from, vec_s);

      for (int dir = 0; dir < 8; dir++) {
         Vec vec = Queen_Vec[dir];
         king |= ray_1(from, vec);
      }

      Pawn_Moves[White][from] = ray_1(from, vec_n);
      Pawn_Moves[Black][from] = ray_1(from, vec_s);

      if (square_rank(from, White) == Rank_2) Pawn_Moves[White][from] |= ray_1(square_front(from, White), vec_n);
      if (square_rank(from, Black) == Rank_2) Pawn_Moves[Black][from] |= ray_1(square_front(from, Black), vec_s);

      Pawn_Attacks[White][from] = ray_1(from, vec_nw) | ray_1(from, vec_ne);
      Pawn_Attacks[Black][from] = ray_1(from, vec_sw) | ray_1(from, vec_se);

      Piece_Attacks[White][Pawn][from] = Pawn_Attacks[White][from];
      Piece_Attacks[Black][Pawn][from] = Pawn_Attacks[Black][from];

      Piece_Attacks[White][Knight][from] = knight;
      Piece_Attacks[Black][Knight][from] = knight;

      Piece_Attacks[White][Bishop][from] = bishop;
      Piece_Attacks[Black][Bishop][from] = bishop;

      Piece_Attacks[White][Rook][from] = rook;
      Piece_Attacks[Black][Rook][from] = rook;

      Piece_Attacks[White][Queen][from] = bishop | rook;
      Piece_Attacks[Black][Queen][from] = bishop | rook;

      Piece_Attacks[White][King][from] = king;
      Piece_Attacks[Black][King][from] = king;
   }

   // range attacks

   for (int f = 0; f < Square_Size; f++) {

      Square from = square_make(f);

      for (int dir = 0; dir < 8; dir++) {

         Vec vec = Queen_Vec[dir];

         Blocker[from] |= ray_almost(from, vec);

         for (Bit b = ray(from, vec); b != 0; b = rest(b)) {

            Square to = first(b);

            Ray    [from][to] = ray(from, vec);
            Beyond [from][to] = ray(to, vec);
            Between[from][to] = ray(from, vec) & ~bit(to) & ~ray(to, vec);
         }
      }
   }

   // slider attacks

#if BMI

   for (int f = 0; f < Square_Size; f++) {

      Square from = square_make(f);

      // bishop

      {
         Bit tos  = piece_attacks(Bishop, from);
         Bit mask = tos & Blocker[from];

         for (int index = 0; index < (1 << count(mask)); index++) {
            Bit blockers = Bit(ml::pdep(uint64(index), mask));
            Bishop_Attacks[from][index] = piece_attacks(from, tos, blockers);
         }
      }

      // rook

      {
         Bit tos  = piece_attacks(Rook, from);
         Bit mask = tos & Blocker[from];

         for (int index = 0; index < (1 << count(mask)); index++) {
            Bit blockers = Bit(ml::pdep(uint64(index), mask));
            Rook_Attacks[from][index] = piece_attacks(from, tos, blockers);
         }
      }
   }

#endif

   FileBtoH = (file(File_B) | file(File_C) | file(File_D) | file(File_E) | 
               file(File_F) | file(File_G) | file(File_H));
   FileAtoG = (file(File_B) | file(File_C) | file(File_D) | file(File_E) | 
               file(File_F) | file(File_G) | file(File_A));

   Rank1to7 = (rank(Rank_1) | rank(Rank_2) | rank(Rank_3) | rank(Rank_4) | 
               rank(Rank_5) | rank(Rank_6) | rank(Rank_7));
   Rank2to8 = (rank(Rank_8) | rank(Rank_2) | rank(Rank_3) | rank(Rank_4) | 
               rank(Rank_5) | rank(Rank_6) | rank(Rank_7));
/*
   for (int i = 0; i < 64; i++) {
      std::cout << "sq " << i << std::endl;
      Bit b = bit((Square)i);
      std::cout << b << std::endl;
      print_bitboard(pawn_moves(White, b));
      print_bitboard(pawn_attacks(White, b));
      print_bitboard(pawn_moves(Black, b));
      print_bitboard(pawn_attacks(Black, b));
   }
*/
}



static Bit ray_1(Square from, Vec vec) {

   Bit b = Bit(0);

   Square to = square_add(from, vec);
   if (to != Square_None) set(b, to);

   return b;
}

static Bit ray_almost(Square from, Vec vec) {

   Bit b = Bit(0);

   for (Square sq = square_add(from, vec); sq != Square_None && square_add(sq, vec) != Square_None; sq = square_add(sq, vec)) {
      set(b, sq);
   }

   return b;
}

static Bit ray(Square from, Vec vec) {

   Bit b = Bit(0);

   for (Square sq = square_add(from, vec); sq != Square_None; sq = square_add(sq, vec)) {
      set(b, sq);
   }

   return b;
}

Bit bit(Square sq) {
   return Bit(ml::bit(sq));
}

bool has(Bit b, Square sq) {
   return (b & bit(sq)) != 0;
}

int bit(Bit b, Square sq) {
   return (b >> sq) & 1;
}

bool is_single(Bit b) {
   return b != 0 && rest(b) == 0;
}

bool is_incl(Bit b0, Bit b1) {
   return (b0 & ~b1) == 0;
}

void set(Bit & b, Square sq) {
   b |= bit(sq);
}

void clear(Bit & b, Square sq) {
   b &= ~bit(sq);
}

void flip(Bit & b, Square sq) {
   b ^= bit(sq);
}

Bit add(Bit b, Square sq) {
   assert(!has(b, sq));
   return b ^ bit(sq);
}

Bit remove(Bit b, Square sq) {
   assert(has(b, sq));
   return b ^ bit(sq);
}

Square first(Bit b) {
   assert(b != 0);
   return Square(ml::bit_first(b));
}

Bit rest(Bit b) {
   assert(b != 0);
   return b & (b - 1);
}

int count(Bit b) {
   return ml::bit_count(b);
}

Bit file(File fl) {
   return File_[fl];
}

Bit rank(Rank rk) {
   return Rank_[rk];
}

Bit rank(Rank rk, Side sd) {
   return rank(rank_side(rk, sd));
}

Bit rect(int left, int bottom, int right, int top) {

   if (left   < 0) left   = 0;
   if (bottom < 0) bottom = 0;

   if (right > File_Size) right = File_Size;
   if (top   > Rank_Size) top   = Rank_Size;

   assert(0 <= left   && left   <= right && right <= File_Size);
   assert(0 <= bottom && bottom <= top   && top   <= Rank_Size);

   Bit files = Bit(0);
   Bit ranks = Bit(0);

   for (int fl = left; fl < right; fl++) {
      files |= file(file_make(fl));
   }

   for (int rk = bottom; rk < top; rk++) {
      ranks |= rank(rank_make(rk));
   }

   return files & ranks;
}

Bit ray(Square from, Square to) {
   return Ray[from][to];
}

Bit beyond(Square from, Square to) {
   return Beyond[from][to];
}

Bit between(Square from, Square to) {
   return Between[from][to];
}

Bit line(Square from, Square to) {

   Bit line = between(from, to);
   set(line, from);
   set(line, to);

   return line;
}
/*
Bit pawn_moves(Side sd, Bit froms) {

   if (sd == White) {
      return Bit(froms << Inc_N);
   } else {
      return Bit(froms >> Inc_N);
   }
}

Bit pawn_attacks(Side sd, Bit froms) {

   if (sd == White) {
      return Bit(froms >> Inc_SW) | Bit(froms << Inc_NW);
   } else {
      return Bit(froms << Inc_SW) | Bit(froms >> Inc_NW);
   }
}
*/
Bit pawn_moves(Side sd, Bit froms) {

   if (sd == White) {
      return Bit((froms & Rank1to7) << Inc_N);
   } else {
      return Bit((froms & Rank2to8) >> Inc_N);
   }
}

Bit pawn_attacks(Side sd, Bit froms) {
   Bit fbh = froms & FileBtoH;
   Bit fag = froms & FileAtoG;
   if (sd == White) {
      return Bit((fbh & Rank1to7) >> Inc_SW) | Bit((fag & Rank1to7) << Inc_NW);
   } else {
      return Bit((fag & Rank2to8) << Inc_SW) | Bit((fbh & Rank2to8) >> Inc_NW);
   }
   /*
   if (sd == WHITECOLOR) {
	   return (fbh << 7) | (fag << 9);
   } else {
	   return (fag >> 7) | (fbh >> 9);
   }
   */
}

Bit pawn_moves_to(Side sd, Bit tos) {
   return pawn_moves(side_opp(sd), tos); // HACK: does not work for double pushes #
}

Bit pawn_attacks_to(Side sd, Bit tos) {
   return pawn_attacks(side_opp(sd), tos);
}

Bit pawn_moves(Side sd, Square from) {
   return Pawn_Moves[sd][from];
}

Bit pawn_attacks(Side sd, Square from) {
   return Pawn_Attacks[sd][from];
}

Bit pawn_attacks_to(Side sd, Square to) {
   return pawn_attacks(side_opp(sd), to);
}

Bit piece_attacks(Piece pc, Square from) {
   return Piece_Attacks[White][pc][from];
}

Bit piece_attacks(Piece pc, Side sd, Square from) {
   return Piece_Attacks[sd][pc][from];
}

Bit piece_attacks(Piece pc, Square from, Bit pieces) {

   assert(pc != Pawn);

#if BMI

   switch (pc) {
      case Bishop : return bit::bishop_attacks(from, pieces);
      case Rook :   return bit::rook_attacks  (from, pieces);
      case Queen :  return bit::queen_attacks (from, pieces);
      default :     return bit::piece_attacks(pc, from);
   }

#else

   return piece_attacks(from, piece_attacks(pc, from), pieces);

#endif
}

static Bit piece_attacks(Square from, Bit tos, Bit pieces) {

   for (Bit b = tos & Blocker[from] & pieces; b != 0; b = rest(b)) {
      Square to = first(b);
      tos &= ~Beyond[from][to];
   }

   return tos;
}

Bit piece_attacks_to(Piece pc, Side sd, Square to) {
   return piece_attacks(pc, side_opp(sd), to);
}

Bit piece_attacks_to(Piece pc, Square to, Bit pieces) {
   assert(pc != Pawn);
   return piece_attacks(pc, to, pieces);
}

bool piece_attack(Piece pc, Side sd, Square from, Square to) {
   return has(piece_attacks(pc, sd, from), to);
}

bool piece_attack(Piece pc, Side sd, Square from, Square to, Bit pieces) {
   return piece_attack(pc, sd, from, to) && line_is_empty(from, to, pieces);
}

bool line_is_empty(Square from, Square to, Bit pieces) {
   return (between(from, to) & pieces) == 0;
}

Bit knight_attacks(Square from) {
   return Piece_Attacks[White][Knight][from];
}

Bit bishop_attacks(Square from, Bit pieces) {

#if BMI

   Bit mask  = Piece_Attacks[White][Bishop][from] & Blocker[from];
   int index = int(ml::pext(mask & pieces, mask));

   return Bishop_Attacks[from][index];

#else

   return piece_attacks(from, piece_attacks(Bishop, from), pieces);

#endif
}

Bit rook_attacks(Square from, Bit pieces) {

#if BMI

   Bit mask  = Piece_Attacks[White][Rook][from] & Blocker[from];
   int index = int(ml::pext(mask & pieces, mask));

   return Rook_Attacks[from][index];

#else

   return piece_attacks(from, piece_attacks(Rook, from), pieces);

#endif
}

Bit queen_attacks(Square from, Bit pieces) {

#if BMI
   return bishop_attacks(from, pieces) | rook_attacks(from, pieces);
#else
   return piece_attacks(from, piece_attacks(Queen, from), pieces);
#endif
}

Bit king_attacks(Square from) {
   return Piece_Attacks[White][King][from];
}

}


// includes
/*
#include "bit.hpp"
#include "common.hpp"
#include "pawn.hpp"
#include "pos.hpp"
*/

namespace pawn {

// variables

static Bit File_[Square_Size];
static Bit Rank_[Square_Size];

static Bit Files[Square_Size];
static Bit Ranks[Square_Size];

static Bit File_Both[Square_Size];

static Bit Rank_Front[Side_Size][Square_Size];
static Bit Rank_Rear [Side_Size][Square_Size];

static Bit Ranks_Front[Side_Size][Square_Size];
static Bit Ranks_Rear [Side_Size][Square_Size];

// prototypes

static Bit bit_sides (Bit b);

static Bit pawns (const Pos & pos);

static Bit pawns_sd (const Pos & pos, Side sd);
static Bit pawns_xd (const Pos & pos, Side sd);

static Bit attacks_sd (const Pos & pos, Side sd);
static Bit attacks_xd (const Pos & pos, Side sd);

static Bit unsafe_sd (const Pos & pos, Side sd);
static Bit unsafe_xd (const Pos & pos, Side sd);

// functions

void init() {

   for (int r = 0; r < Rank_Size; r++) {

      for (int f = 0; f < File_Size; f++) {

         File fl = file_make(f);
         Rank rk = rank_make(r);

         Square sq = square_make(fl, rk);

         File_[sq] = bit::file(fl);
         Rank_[sq] = bit::rank(rk);

         if (fl > 0)             File_Both[sq] |= bit::file(fl - 1);
         if (fl < File_Size - 1) File_Both[sq] |= bit::file(fl + 1);

         if (rk > 0)             Rank_Rear [White][sq] = bit::rank(rk - 1);
         if (rk < Rank_Size - 1) Rank_Front[White][sq] = bit::rank(rk + 1);

         Rank_Front[Black][sq] = Rank_Rear [White][sq];
         Rank_Rear [Black][sq] = Rank_Front[White][sq];

         Ranks_Front[White][sq] = bit::rect(0, rk + 1, File_Size, Rank_Size);
         Ranks_Rear [White][sq] = bit::rect(0, 0,      File_Size, rk);

         Ranks_Front[Black][sq] = Ranks_Rear [White][sq];
         Ranks_Rear [Black][sq] = Ranks_Front[White][sq];

         Files[sq] = File_[sq] | File_Both[sq];
         Ranks[sq] = Rank_Front[White][sq] | Rank_[sq] | Rank_Rear[White][sq];
      }
   }
}

Bit weak(const Pos & pos, Side sd) {

   Bit pawns = pawns_sd(pos, sd);
   Bit safe = ~unsafe_sd(pos, sd);

   Bit weak = pawns;

   // forward

   Bit forward = pawns;

   while (true) {

      Bit next = forward | (bit::pawn_moves(sd, forward) & safe);
      if (next == forward) break;

      forward = next;
   }

   weak &= ~(bit_sides(forward) | bit::pawn_attacks(sd, forward));

   // backward

   Bit backward = bit_sides(pawns);

   while (true) {

      Bit next = backward | bit::pawn_moves_to(sd, backward & safe);
      if (next == backward) break;

      backward = next;
   }

   weak &= ~backward;

   // wrap up

   return weak;
}

Bit strong(const Pos & pos, Side sd) { // squares not attackable by opponent pawns

   Side xd = side_opp(sd);

   Bit safe = ~unsafe_xd(pos, sd);

   // forward

   Bit forward = pawns_xd(pos, sd);

   while (true) {

      Bit next = forward | (bit::pawn_moves(xd, forward) & safe);
      if (next == forward) break;

      forward = next;
   }

   Bit strong = ~(pawns(pos) | bit::pawn_attacks(xd, forward));

   Bit bad = Bit(0);

   for (Bit b = strong; b != 0; b = bit::rest(b)) {
      Square sq = bit::first(b);
      if (!is_strong(pos, sq, sd)) bit::set(bad, sq);
   }

   return ~bit::pawn_attacks(xd, forward);
}

Bit blocked(const Pos & pos, Side sd) {
   Bit unsafe = unsafe_sd(pos, sd);
   return pawns_sd(pos, sd) & bit::pawn_moves_to(sd, unsafe);
}

static Bit bit_sides(Bit b) {
   return Bit(b >> Inc_W) | Bit(b << Inc_W);
   //Bit b1 = Bit(b);
   //Bit b2 = Bit(b >> Inc_W) | Bit(b << Inc_W);
   //return b2;
}

bool is_passed(const Pos & pos, Square sq, Side sd) {
   return (pawns_xd(pos, sd) & (Files[sq] & Ranks_Front[sd][sq])) == 0
       && (pawns_sd(pos, sd) & (File_[sq] & Ranks_Front[sd][sq])) == 0;
}

bool is_duo(const Pos & pos, Square sq, Side sd) {
   return (pawns_sd(pos, sd) & (File_Both[sq] & Rank_[sq])) != 0;
}

bool is_protected(const Pos & pos, Square sq, Side sd) {
   return (pawns_sd(pos, sd) & (File_Both[sq] & Rank_Rear[sd][sq])) != 0;
}

bool is_ram(const Pos & pos, Square sq, Side sd) {
   return (pawns_xd(pos, sd) & (File_[sq] & Rank_Front[sd][sq])) != 0;
}

bool is_open(const Pos & pos, Square sq, Side sd) {
   return (pawns_sd(pos, sd) & File_[sq]) == 0;
}

bool is_strong(const Pos & pos, Square sq, Side sd) {
   return (pawns_xd(pos, sd) & (File_Both[sq] & Ranks_Front[sd][sq])) == 0;
}

Bit file(Square sq) {
   return File_[sq];
}

Bit rank(Square sq) {
   return Rank_[sq];
}

Bit fronts(Square sq, Side sd) {
   return Ranks_Front[sd][sq];
}

Bit rears(Square sq, Side sd) {
   return Ranks_Rear[sd][sq];
}

static Bit pawns(const Pos & pos) {
   return pos.pieces(Pawn);
}

static Bit pawns_sd(const Pos & pos, Side sd) {
   return pos.pawns(sd);
}

static Bit pawns_xd(const Pos & pos, Side sd) {
   return pos.pawns(side_opp(sd));
}

static Bit attacks_sd(const Pos & pos, Side sd) {
   return bit::pawn_attacks(sd, pos.pawns(sd));
}

static Bit attacks_xd(const Pos & pos, Side sd) {
   Side xd = side_opp(sd);
   return bit::pawn_attacks(xd, pos.pawns(xd));
}

static Bit unsafe_sd(const Pos & pos, Side sd) {
   return pawns(pos) | (attacks_xd(pos, sd) & ~attacks_sd(pos, sd));
}

static Bit unsafe_xd(const Pos & pos, Side sd) {
   return pawns(pos) | (attacks_sd(pos, sd) & ~attacks_xd(pos, sd));
}

}


// includes
/*
#include <algorithm>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>

#include "attack.hpp"
#include "bit.hpp"
#include "common.hpp"
#include "eval.hpp"
#include "pawn.hpp"
#include "pos.hpp"
*/
// constants

const int  Pawn_Table_Bit  { 12 };
const int  Pawn_Table_Size { 1 << Pawn_Table_Bit };
const int  Pawn_Table_Mask { Pawn_Table_Size - 1 };

const int  Scale { 100 }; // units per cp

// types

class Score_Pair {

private :

   int64 p_vec;

public :

   Score_Pair ();
   explicit Score_Pair (int sc);
   Score_Pair (int mg, int eg);

   void operator += (Score_Pair sp);
   void operator -= (Score_Pair sp);

   friend Score_Pair operator + (Score_Pair sp);
   friend Score_Pair operator - (Score_Pair sp);

   friend Score_Pair operator + (Score_Pair s0, Score_Pair s1);
   friend Score_Pair operator - (Score_Pair s0, Score_Pair s1);

   friend Score_Pair operator * (Score_Pair weight, int n);
   friend Score_Pair operator * (Score_Pair weight, double x);

   int mg () const;
   int eg () const;

private :

   static Score_Pair make (int64 vec);
};


class EvalVector {
public:
   static const int FeatSize = 759;
   double features[2][FeatSize];
   double passed_pawn[2];
   int mg_factor;
   int eg_factor;
   int denominater;

   //int length;

   void setFactor(int mgEnum, int egEnum, int deno) {
      mg_factor = mgEnum;
      eg_factor = egEnum;
      denominater = deno;
   }

   inline void setValue(int side, int index, double sc) {
      features[side][index] = sc;
   }

   inline void incValue(int side, int index, double sc) {
      features[side][index] += sc;
   }

   inline void incUnstoppable(int side, double sc) {
      passed_pawn[side] += sc;
   }

   inline void clear() {
      for (int i = 0; i < FeatSize; i++) {
         features[White][i] = 0;
         features[Black][i] = 0;
      }
      passed_pawn[White] = 0;
      passed_pawn[Black] = 0;
   }
/*
   int productWeight(Score_Pair * sc_pair, Side side, Score_Pair & standard_sc) {

      double result1 = 0;
      double result2 = 0;
      Side xside = side_opp(side);

      for (int i = 0; i < FeatSize; i++) {
         double phi = (features[side][i] - features[xside][i]) * mg_factor;
         result1 += (phi * ((double)sc_pair[i].mg()));
      }

      for (int i = 0; i < FeatSize; i++) {
         double phi = (features[side][i] - features[xside][i]) * eg_factor;
         result2 += (phi * ((double)sc_pair[i].eg()));
      }

      result1 += (passed_pawn[side] - passed_pawn[xside]) * mg_factor;
      result2 += (passed_pawn[side] - passed_pawn[xside]) * eg_factor;
      
      //std::cout << result1 << " " << standard_sc.mg() << "  "  << result2 << " " << standard_sc.eg() << std::endl;
      return ml::div_round(result1 + result2, denominater);
   }
*/
   int productWeight(Score_Pair * sc_pair, Side side, Score_Pair & standard_sc) {

      double resultMg = 0;
      double resultEg = 0;
      Side xside = side_opp(side);

      for (int i = 0; i < FeatSize; i++) {
         double phi = (features[side][i] - features[xside][i]);// * mg_factor;
         resultMg += (phi * ((double)sc_pair[i].mg()));
      }

      for (int i = 0; i < FeatSize; i++) {
         double phi = (features[side][i] - features[xside][i]);// * eg_factor;
         resultEg += (phi * ((double)sc_pair[i].eg()));
      }

      double passedMg = (passed_pawn[side] - passed_pawn[xside]);// * mg_factor;
      double passedEg = (passed_pawn[side] - passed_pawn[xside]);// * eg_factor;

      double enumerator = (resultMg + passedMg) * mg_factor + (resultEg + passedEg) * eg_factor;
      return ml::div_round(enumerator, denominater);
   }

   void printFeat(Side side) {
      std:: cout << "========" << std::endl;

      Side xside = side_opp(side);

      int idx = 0;
      for (int i = 0; i < FeatSize; i++) {
         double phi = (features[side][i] - features[xside][i]) * mg_factor;
         if (phi != 0) {
            std:: cout << idx << " = " << phi / 24 << std::endl;
         }
         idx++;
      }
      std:: cout << "---------" << std::endl;
      for (int i = 0; i < FeatSize; i++) {
         double phi = (features[side][i] - features[xside][i]) * eg_factor;
         if (phi != 0) {
            std:: cout << idx << " = " << phi / 24 << std::endl;
         }
         idx++;
      }

      std:: cout << "==========" << std::endl;

   }

   void toSparseVector(Side side, SparseVector &sv) {
      sv.clear();

      Side xside = side_opp(side);

      int idx = 0;
      for (int i = 0; i < FeatSize; i++) {
         double phi = (features[side][i] - features[xside][i]) * mg_factor;
         if (phi != 0) {
            //std:: cout << idx << " = " << phi / 24 << std::endl;
            sv.insert(idx, phi / 24);
            //sv.insert(idx, phi);
         }
         idx++;
      }

      for (int i = 0; i < FeatSize; i++) {
         double phi = (features[side][i] - features[xside][i]) * eg_factor;
         if (phi != 0) {
            //std:: cout << idx << " = " << phi / 24 << std::endl;
            sv.insert(idx, phi / 24);
            //sv.insert(idx, phi);
         }
         idx++;
      }
   }

   void toFeatureAndFactor(Side side, FeatureAndFactor &fv) {
      fv.sv.clear();

      Side xside = side_opp(side);

      int idx = 0;
      for (int i = 0; i < FeatSize; i++) {
         double phi = (features[side][i] - features[xside][i]);
         if (phi != 0) {
            fv.sv.insert(idx, phi);
         }
         idx++;
      }

      // factors
      fv.eg_factor = eg_factor;
      fv.mg_factor = mg_factor;
   }

private:

};

struct Pawn_Info {
   Key key;
   Score_Pair score[Side_Size];
   Bit passed[Side_Size];
   Bit strong[Side_Size];
   float centre_file, centre_rank;
};

// "constants"

Score_Pair W[] = { // 10000 units = 1 pawn
   Score_Pair(9049, 12537),
   Score_Pair(29594, 34965),
   Score_Pair(32125, 34190),
   Score_Pair(44928, 61719),
   Score_Pair(109411, 111079),
   Score_Pair(0, 0),
   Score_Pair(2401, 5495),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(-2397, -2337),
   Score_Pair(-1202, -984),
   Score_Pair(-1163, -2026),
   Score_Pair(-1328, -2398),
   Score_Pair(-2594, -1854),
   Score_Pair(-2325, -649),
   Score_Pair(-2419, -2331),
   Score_Pair(-2543, -2677),
   Score_Pair(-1454, -1489),
   Score_Pair(-1470, -366),
   Score_Pair(-856, -2582),
   Score_Pair(-756, -3333),
   Score_Pair(-895, 1005),
   Score_Pair(-513, 1150),
   Score_Pair(654, -790),
   Score_Pair(452, -1523),
   Score_Pair(2233, 4345),
   Score_Pair(3282, 5158),
   Score_Pair(3339, 3007),
   Score_Pair(3618, 2163),
   Score_Pair(847, 2012),
   Score_Pair(2658, 4050),
   Score_Pair(1707, 2395),
   Score_Pair(2132, 2390),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(-510, -2179),
   Score_Pair(138, -73),
   Score_Pair(1331, 561),
   Score_Pair(1695, 1449),
   Score_Pair(-665, -1382),
   Score_Pair(-230, -589),
   Score_Pair(311, -312),
   Score_Pair(195, 373),
   Score_Pair(-2164, -1906),
   Score_Pair(-2316, -1857),
   Score_Pair(-982, -330),
   Score_Pair(-1122, -338),
   Score_Pair(407, 73),
   Score_Pair(666, 1116),
   Score_Pair(2047, 1886),
   Score_Pair(1738, 2914),
   Score_Pair(-1223, -509),
   Score_Pair(-606, 4),
   Score_Pair(414, 1079),
   Score_Pair(1040, 1402),
   Score_Pair(-2352, -722),
   Score_Pair(-381, -141),
   Score_Pair(1644, 1273),
   Score_Pair(1572, 1205),
   Score_Pair(-2431, -1084),
   Score_Pair(-1439, -719),
   Score_Pair(993, 474),
   Score_Pair(1522, 1057),
   Score_Pair(-2075, -2610),
   Score_Pair(-811, -620),
   Score_Pair(361, 1558),
   Score_Pair(640, 1223),
   Score_Pair(-1433, -4592),
   Score_Pair(-1148, -2541),
   Score_Pair(696, -1434),
   Score_Pair(554, -88),
   Score_Pair(-3293, -3598),
   Score_Pair(-1999, -2566),
   Score_Pair(-498, -187),
   Score_Pair(53, 625),
   Score_Pair(-1110, -1805),
   Score_Pair(615, 1104),
   Score_Pair(2127, 2903),
   Score_Pair(2068, 3194),
   Score_Pair(-1570, -1179),
   Score_Pair(976, 1533),
   Score_Pair(2911, 3036),
   Score_Pair(3451, 3251),
   Score_Pair(-2391, -1132),
   Score_Pair(290, 969),
   Score_Pair(2153, 2728),
   Score_Pair(3897, 2646),
   Score_Pair(-4069, -2417),
   Score_Pair(-755, 318),
   Score_Pair(1993, 1214),
   Score_Pair(2518, 1847),
   Score_Pair(-4256, -3217),
   Score_Pair(-1721, -1336),
   Score_Pair(531, 1072),
   Score_Pair(681, 951),
   Score_Pair(-2205, -2216),
   Score_Pair(-316, -79),
   Score_Pair(575, 1282),
   Score_Pair(211, 1237),
   Score_Pair(-4225, -2535),
   Score_Pair(-2652, -1299),
   Score_Pair(-534, -736),
   Score_Pair(-486, -559),
   Score_Pair(-3800, -2644),
   Score_Pair(-2019, -2244),
   Score_Pair(-463, -473),
   Score_Pair(-586, -474),
   Score_Pair(-3789, -3370),
   Score_Pair(-2833, -1876),
   Score_Pair(-1316, -1292),
   Score_Pair(-1926, -789),
   Score_Pair(-1656, -1534),
   Score_Pair(-787, 211),
   Score_Pair(778, 1305),
   Score_Pair(1150, 1366),
   Score_Pair(-1615, -370),
   Score_Pair(-194, 333),
   Score_Pair(373, 1619),
   Score_Pair(1470, 1391),
   Score_Pair(-1494, 1390),
   Score_Pair(1117, 1687),
   Score_Pair(2613, 2700),
   Score_Pair(3361, 2743),
   Score_Pair(-27, 1724),
   Score_Pair(2148, 3002),
   Score_Pair(3807, 3827),
   Score_Pair(4186, 3842),
   Score_Pair(25, -794),
   Score_Pair(735, 800),
   Score_Pair(2187, 2128),
   Score_Pair(1382, 1865),
   Score_Pair(-3402, -3217),
   Score_Pair(-1498, -2246),
   Score_Pair(-356, -1401),
   Score_Pair(537, 510),
   Score_Pair(-2646, -2878),
   Score_Pair(-1460, -1867),
   Score_Pair(313, 1),
   Score_Pair(1175, 1644),
   Score_Pair(-440, -982),
   Score_Pair(850, 483),
   Score_Pair(2079, 2385),
   Score_Pair(2641, 2728),
   Score_Pair(-1308, -480),
   Score_Pair(578, 849),
   Score_Pair(1534, 1850),
   Score_Pair(2657, 2046),
   Score_Pair(-1851, -1342),
   Score_Pair(-547, 1058),
   Score_Pair(1075, 1520),
   Score_Pair(2004, 884),
   Score_Pair(-2335, -1151),
   Score_Pair(1, 583),
   Score_Pair(1792, 1168),
   Score_Pair(2194, 2047),
   Score_Pair(-1546, -569),
   Score_Pair(221, 348),
   Score_Pair(2064, 1881),
   Score_Pair(1843, 1619),
   Score_Pair(-632, -571),
   Score_Pair(445, 478),
   Score_Pair(1267, 1402),
   Score_Pair(1675, 1581),
   Score_Pair(4734, -688),
   Score_Pair(3926, 928),
   Score_Pair(-1467, 110),
   Score_Pair(-3130, -1816),
   Score_Pair(6686, 524),
   Score_Pair(3255, 1096),
   Score_Pair(-2387, 1217),
   Score_Pair(-4713, 526),
   Score_Pair(2341, 158),
   Score_Pair(2166, 536),
   Score_Pair(-2022, 213),
   Score_Pair(-3550, -152),
   Score_Pair(-777, -1106),
   Score_Pair(-384, -233),
   Score_Pair(-1039, -403),
   Score_Pair(-2352, -1232),
   Score_Pair(-1090, -1399),
   Score_Pair(113, 269),
   Score_Pair(-714, 33),
   Score_Pair(-1074, -468),
   Score_Pair(-459, -447),
   Score_Pair(1321, 1830),
   Score_Pair(1186, 1903),
   Score_Pair(226, 896),
   Score_Pair(-987, -1465),
   Score_Pair(1529, 1780),
   Score_Pair(1840, 2224),
   Score_Pair(934, 1291),
   Score_Pair(-4136, -5749),
   Score_Pair(-111, -186),
   Score_Pair(284, 255),
   Score_Pair(432, 484),
   Score_Pair(993, 1324),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(2266, 3128),
   Score_Pair(1744, 2917),
   Score_Pair(1597, 2408),
   Score_Pair(1753, 2220),
   Score_Pair(361, 1621),
   Score_Pair(1091, 1735),
   Score_Pair(1680, 1684),
   Score_Pair(797, 558),
   Score_Pair(1474, 724),
   Score_Pair(1737, 951),
   Score_Pair(1526, 1488),
   Score_Pair(-1309, 1911),
   Score_Pair(2069, 3730),
   Score_Pair(1174, 2816),
   Score_Pair(340, 2310),
   Score_Pair(256, 2100),
   Score_Pair(1526, 2026),
   Score_Pair(1860, 1347),
   Score_Pair(748, 672),
   Score_Pair(177, 632),
   Score_Pair(310, 974),
   Score_Pair(682, 1392),
   Score_Pair(333, 1855),
   Score_Pair(-1797, 2057),
   Score_Pair(1590, 3157),
   Score_Pair(920, 2918),
   Score_Pair(276, 2766),
   Score_Pair(590, 2706),
   Score_Pair(1156, 985),
   Score_Pair(852, 1443),
   Score_Pair(551, 2004),
   Score_Pair(-308, 1822),
   Score_Pair(496, 1584),
   Score_Pair(261, 1148),
   Score_Pair(-194, 899),
   Score_Pair(561, 1662),
   Score_Pair(2170, 1368),
   Score_Pair(1551, 1402),
   Score_Pair(982, 1343),
   Score_Pair(816, 1446),
   Score_Pair(1530, -1092),
   Score_Pair(1189, -438),
   Score_Pair(448, 709),
   Score_Pair(274, 1354),
   Score_Pair(386, 1610),
   Score_Pair(587, 1426),
   Score_Pair(130, 1484),
   Score_Pair(974, 505),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(5135, 5285),
   Score_Pair(5381, 6995),
   Score_Pair(5998, 6099),
   Score_Pair(3863, 4189),
   Score_Pair(6184, 6120),
   Score_Pair(-598, 2435),
   Score_Pair(0, 0),
   Score_Pair(3226, 3735),
   Score_Pair(5583, 4777),
   Score_Pair(3666, 5480),
   Score_Pair(8205, 4303),
   Score_Pair(171, 2484),
   Score_Pair(2380, 3553),
   Score_Pair(0, 0),
   Score_Pair(4987, 5564),
   Score_Pair(4548, 5494),
   Score_Pair(5338, 6323),
   Score_Pair(33, 3105),
   Score_Pair(1059, 2978),
   Score_Pair(1487, 3035),
   Score_Pair(0, 0),
   Score_Pair(5507, 5818),
   Score_Pair(7837, 4619),
   Score_Pair(-11, 2642),
   Score_Pair(166, 1979),
   Score_Pair(183, 3359),
   Score_Pair(-18, 2364),
   Score_Pair(0, 0),
   Score_Pair(8547, 9148),
   Score_Pair(1350, 5324),
   Score_Pair(1993, 3158),
   Score_Pair(3238, 3139),
   Score_Pair(1606, 1915),
   Score_Pair(-2865, -2087),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(4846, 84),
   Score_Pair(1896, 2292),
   Score_Pair(4968, 563),
   Score_Pair(4483, -381),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(-4467, -5814),
   Score_Pair(-3922, -2676),
   Score_Pair(-3369, -2734),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(-1407, -1194),
   Score_Pair(-4183, -3416),
   Score_Pair(-2544, -2264),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(-9909, -8818),
   Score_Pair(-2359, -1914),
   Score_Pair(-3156, -3071),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(-1421, -1029),
   Score_Pair(-5470, -4098),
   Score_Pair(-1944, -2004),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(1377, -856),
   Score_Pair(820, 106),
   Score_Pair(1200, -193),
   Score_Pair(411, 41),
   Score_Pair(345, -392),
   Score_Pair(119, -59),
   Score_Pair(1507, 328),
   Score_Pair(-235, 988),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(3108, -1492),
   Score_Pair(1610, -411),
   Score_Pair(2140, -938),
   Score_Pair(844, -319),
   Score_Pair(1670, -472),
   Score_Pair(841, -199),
   Score_Pair(2044, 2019),
   Score_Pair(242, 3736),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(2827, -733),
   Score_Pair(2429, 185),
   Score_Pair(2428, -276),
   Score_Pair(1368, 297),
   Score_Pair(2263, 365),
   Score_Pair(1464, -87),
   Score_Pair(3734, 3513),
   Score_Pair(1875, 4100),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(1650, 1078),
   Score_Pair(2571, 1805),
   Score_Pair(2612, 1809),
   Score_Pair(2117, 1475),
   Score_Pair(2582, 1825),
   Score_Pair(2038, 1330),
   Score_Pair(3092, 2311),
   Score_Pair(3057, 2666),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(135, -591),
   Score_Pair(2888, -242),
   Score_Pair(1319, 1043),
   Score_Pair(607, 1425),
   Score_Pair(0, 0),
   Score_Pair(744, -1340),
   Score_Pair(4, -1722),
   Score_Pair(31, -688),
   Score_Pair(-662, -577),
   Score_Pair(947, 1200),
   Score_Pair(3785, 4814),
   Score_Pair(0, 0),
   Score_Pair(782, 187),
   Score_Pair(1421, 1757),
   Score_Pair(1681, 1413),
   Score_Pair(1396, 1889),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(-676, -216),
   Score_Pair(-1208, -931),
   Score_Pair(-401, -284),
   Score_Pair(1420, 629),
   Score_Pair(2001, 1675),
   Score_Pair(4079, 4480),
   Score_Pair(223, 139),
   Score_Pair(1145, 386),
   Score_Pair(1263, 746),
   Score_Pair(1271, 854),
   Score_Pair(-613, -11),
   Score_Pair(-752, -305),
   Score_Pair(-558, 111),
   Score_Pair(215, -37),
   Score_Pair(307, 670),
   Score_Pair(982, 754),
   Score_Pair(1674, 1084),
   Score_Pair(2649, -140),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(2020, 670),
   Score_Pair(1918, 1426),
   Score_Pair(1277, 397),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(3061, 2658),
   Score_Pair(2017, 1496),
   Score_Pair(900, 338),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(-516, -940),
   Score_Pair(-265, -2),
   Score_Pair(1976, -1328),
   Score_Pair(692, -870),
   Score_Pair(-3020, -3391),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(-109, 1039),
   Score_Pair(-185, 352),
   Score_Pair(-2298, 901),
   Score_Pair(-1038, 598),
   Score_Pair(928, 1673),
   Score_Pair(312, 1625),
   Score_Pair(797, 562),
   Score_Pair(47, -578),
   Score_Pair(-1127, -543),
   Score_Pair(-946, -459),
   Score_Pair(621, -431),
   Score_Pair(-742, -815),
   Score_Pair(1205, 1749),
   Score_Pair(680, 780),
   Score_Pair(-212, 667),
   Score_Pair(102, -289),
   Score_Pair(684, -685),
   Score_Pair(-472, -338),
   Score_Pair(-981, -27),
   Score_Pair(-1337, -1007),
   Score_Pair(565, 3196),
   Score_Pair(80, 1534),
   Score_Pair(-2185, 2377),
   Score_Pair(-2186, 373),
   Score_Pair(2971, 3870),
   Score_Pair(1995, 3921),
   Score_Pair(451, 1829),
   Score_Pair(-725, -112),
   Score_Pair(-1031, -1996),
   Score_Pair(-1304, -1788),
   Score_Pair(-316, -1151),
   Score_Pair(-1491, -1325),
   Score_Pair(720, 912),
   Score_Pair(-666, -1704),
   Score_Pair(842, -1414),
   Score_Pair(-451, -1047),
   Score_Pair(-616, 1203),
   Score_Pair(166, 1877),
   Score_Pair(279, 1820),
   Score_Pair(286, 1560),
   Score_Pair(1701, 5046),
   Score_Pair(-48, 2378),
   Score_Pair(534, 4380),
   Score_Pair(-1517, 1200),
   Score_Pair(2345, 3083),
   Score_Pair(4659, 6182),
   Score_Pair(1535, 2268),
   Score_Pair(-133, 149),
   Score_Pair(-2431, -1162),
   Score_Pair(-1876, -2375),
   Score_Pair(-1134, -1941),
   Score_Pair(-1425, -1209),
   Score_Pair(-624, -975),
   Score_Pair(490, -3970),
   Score_Pair(526, -2548),
   Score_Pair(-953, 132),
   Score_Pair(-500, 2441),
   Score_Pair(429, 3835),
   Score_Pair(942, 3423),
   Score_Pair(1363, 2668),
   Score_Pair(3510, 6265),
   Score_Pair(146, 4230),
   Score_Pair(4501, 7460),
   Score_Pair(-64, 896),
   Score_Pair(908, 1259),
   Score_Pair(5160, 7298),
   Score_Pair(2416, 2970),
   Score_Pair(1369, 872),
   Score_Pair(-634, -1076),
   Score_Pair(-1677, -1880),
   Score_Pair(-2703, -1732),
   Score_Pair(-1424, -1363),
   Score_Pair(-1977, -3909),
   Score_Pair(200, -6206),
   Score_Pair(260, -3536),
   Score_Pair(-429, 1632),
   Score_Pair(118, 4613),
   Score_Pair(939, 5260),
   Score_Pair(1457, 4303),
   Score_Pair(3029, 4332),
   Score_Pair(7339, 10839),
   Score_Pair(5339, 6405),
   Score_Pair(8402, 10906),
   Score_Pair(523, 745),
   Score_Pair(-1962, -2745),
   Score_Pair(3586, 4978),
   Score_Pair(2859, 3521),
   Score_Pair(2981, 3521),
   Score_Pair(517, 398),
   Score_Pair(761, -20),
   Score_Pair(-44, 239),
   Score_Pair(-1326, 1250),
   Score_Pair(-6093, -5592),
   Score_Pair(-3591, -4967),
   Score_Pair(-928, -3216),
   Score_Pair(1887, 2700),
   Score_Pair(3601, 5760),
   Score_Pair(4401, 6313),
   Score_Pair(4153, 5870),
   Score_Pair(3571, 3806),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(679, -264),
   Score_Pair(115, 247),
   Score_Pair(-95, 464),
   Score_Pair(-274, 807),
   Score_Pair(881, 79),
   Score_Pair(356, 402),
   Score_Pair(786, 382),
   Score_Pair(1074, 748),
   Score_Pair(-614, 164),
   Score_Pair(1068, 1635),
   Score_Pair(435, 1155),
   Score_Pair(1201, 2215),
   Score_Pair(555, 1601),
   Score_Pair(2713, 3277),
   Score_Pair(2660, 2873),
   Score_Pair(3024, 4183),
   Score_Pair(2906, 3656),
   Score_Pair(5208, 6660),
   Score_Pair(4170, 5632),
   Score_Pair(4697, 6186),
   Score_Pair(632, 853),
   Score_Pair(1066, 1457),
   Score_Pair(588, 782),
   Score_Pair(590, 819),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(1689, 1277),
   Score_Pair(1543, 890),
   Score_Pair(1237, 1303),
   Score_Pair(1475, 1857),
   Score_Pair(20, 1238),
   Score_Pair(977, 1128),
   Score_Pair(1646, 937),
   Score_Pair(1799, 1644),
   Score_Pair(235, 1369),
   Score_Pair(1428, 1643),
   Score_Pair(2008, 2094),
   Score_Pair(2791, 2343),
   Score_Pair(2194, 4559),
   Score_Pair(3442, 4838),
   Score_Pair(5197, 4822),
   Score_Pair(5347, 5690),
   Score_Pair(1680, 2397),
   Score_Pair(3078, 4090),
   Score_Pair(2879, 3642),
   Score_Pair(1294, 1740),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(1891, 858),
   Score_Pair(782, 685),
   Score_Pair(836, 910),
   Score_Pair(1536, 1324),
   Score_Pair(1285, 279),
   Score_Pair(1073, 309),
   Score_Pair(1070, 393),
   Score_Pair(971, 666),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(-1285, -279),
   Score_Pair(-1073, -309),
   Score_Pair(-1070, -393),
   Score_Pair(-971, -666),
   Score_Pair(-1891, -858),
   Score_Pair(-782, -685),
   Score_Pair(-836, -910),
   Score_Pair(-1536, -1324),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(0, 0),
   Score_Pair(-278, 208),
   Score_Pair(-665, -812),
   Score_Pair(-642, -316),
   Score_Pair(-781, -241),
   Score_Pair(0, 0),
   Score_Pair(-307, -105),
   Score_Pair(-132, -476),
   Score_Pair(-650, 66),
   Score_Pair(-175, -276),
   Score_Pair(-484, -209),
   Score_Pair(-619, -163),
   Score_Pair(0, 0),
   Score_Pair(-2523, -1052),
   Score_Pair(1641, -2392),
   Score_Pair(-67, -403),
   Score_Pair(-505, -1184),
   Score_Pair(1361, 1467),
};

// variables

std::vector<Pawn_Info> G_Pawn_Table;

// prototypes

static int  eval (const Pos & pos);

static void comp_pawn_info (Pawn_Info & pi, const Pos & pos);

static bool two_knights    (const Pos & pos, Side sd);
static bool rook_pawn_draw (const Pos & pos, Side sd, File fl);

static Bit  king_zone (Square sq, Side sd);

static bool pawn_is_free        (const Pos & pos, Square sq, Side sd, const Attack_Info & ai);
static bool pawn_is_unstoppable (const Pos & pos, Square sq, Side sd, const Attack_Info & ai);

static int eval_vectorized(const Pos & pos, EvalVector &eval_vec);
static int comp_pawn_info_vectorized(Pawn_Info & pi, const Pos & pos, EvalVector & eval_vec);

// functions

void clear_pawn_table() {

   Pawn_Info entry {
      Key(1),
      { Score_Pair(0), Score_Pair(0) },
      { Bit(0), Bit(0) },
      { Bit(0), Bit(0) },
      0.0, 0.0,
   };

   G_Pawn_Table.clear();
   G_Pawn_Table.resize(Pawn_Table_Size, entry);
}

Score eval(const Pos & pos, Side sd) {

   EvalVector eval_vec;

   int sc = eval(pos);
   int sc2 = eval_vectorized(pos, eval_vec);

   //std::cout << sc << " == " << sc2 << std::endl; 
   //assert(sc == sc2);
   if (score::side(Score(sc), sd) != Score(sc2)) {
      std::cout << score::side(Score(sc), sd) << " != " << sc2 << std::endl; 
   }

   // drawish?

   Side win  = (sc >= 0) ? White : Black;
   Side lose = side_opp(win);

   int fw = pos::force(pos, win);
   int fl = pos::force(pos, lose);

   if (fw < 6) {

      Bit pw = pos.pawns(win);
      Bit pl = pos.pawns(lose);

      Bit minors = pos.pieces(Knight, lose) | pos.pieces(Bishop, lose);

      if (false) {

      } else if (fw == 0 && pw == 0) { // lone king

         sc = 0;

      } else if (rook_pawn_draw(pos, win, File_A)) {

         sc /= 64;

      } else if (rook_pawn_draw(pos, win, File_H)) {

         sc /= 64;

      } else if (pw == 0) {

         if (fw <= 1) { // insufficient material
            sc /= 16;
         } else if (fw == 2 && two_knights(pos, win) && pl == 0) {
            sc /= 16;
         } else if (fw - fl <= 1) {
            sc /= 4;
         }

      } else if (bit::is_single(pw)) {

         Square pawn = bit::first(pw);
         bool blocked = (pawn::file(pawn) & pawn::fronts(pawn, win) & pos.pieces(King, lose)) != 0;

         if (fw <= 1 && minors != 0) { // minor sacrifice
            sc /= 8;
         } else if (fw == 2 && two_knights(pos, win) && pl == 0 && minors != 0) { // minor sacrifice
            sc /= 8;
         } else if (fw == fl && blocked) { // blocked by king
            sc /= 4;
         } else if (fw == fl && minors != 0) { // minor sacrifice
            sc /= 2;
         }

      } else if (pos::opposit_bishops(pos) && std::abs(pos.count(Pawn, White) - pos.count(Pawn, Black)) <= 2) {

         sc /= 2;
      }
   }

   return score::clamp(score::side(Score(sc), sd)); // for sd
}

static int eval(const Pos & pos) {

   Key key = pos.key_pawn();
   Pawn_Info & entry = G_Pawn_Table[hash::index(key, Pawn_Table_Mask)];

   if (entry.key != key) {
      comp_pawn_info(entry, pos);
      entry.key = key;
   }

   Pawn_Info pi = entry;

   Attack_Info ai;
   ai.init(pos);

   Score_Pair sc;

   for (int s = 0; s < Side_Size; s++) {

      Side sd = side_make(s);
      Side xd = side_opp(sd);

      Bit pawns_sd = pos.pawns(sd);
      Bit pawns_xd = pos.pawns(xd);

      Square king_sd = pos.king(sd);
      Square king_xd = pos.king(xd);

      Bit king_zone_sd = king_zone(king_sd, sd);
      Bit king_zone_xd = king_zone(king_xd, xd);

      int var;

      // material

      var = 0;

      for (int p = Pawn; p <= Queen; p++) {

         Piece pc = piece_make(p);

         int mat = pos.count(pc, sd);
         sc += W[var + pc] * mat;
      }

      var = 6;

      if (pos.count(Bishop, sd) > 1) sc += W[var];

      // pawns

      Piece pc = Pawn;

      Bit blocked_sd = pawn::blocked(pos, sd);

      sc += pi.score[sd]; // pawn-only score

      // pawn mobility

      var = 199 + pc * 12;

      int mob = bit::count(bit::pawn_moves(sd, pawns_sd) & pos.empties());
      sc += W[var] * mob;

      // pawn captures

      var = 271 + pc * Piece_Size;

      for (Bit b = ai.pawn_attacks(sd) & pos.non_pawns(xd); b != 0; b = bit::rest(b)) {
         Square to = bit::first(b);
         sc += W[var + pos.piece(to)];
      }

      // passed pawns

      for (Bit b = pi.passed[sd] & pos.pawns(sd); b != 0; b = bit::rest(b)) {

         Square sq = bit::first(b);

         Rank rank = std::max(square_rank(sq, sd), Rank_3);

         if (pawn_is_unstoppable(pos, sq, sd, ai)) {

            int gain = piece_mat(Queen) - piece_mat(Pawn);
            sc += Score_Pair(gain * Scale * (rank - Rank_2) / 6);

         } else {

            var = 486 + rank * 20;

            Square stop = square_front(sq, sd);

            sc += W[var + 0];
            if (!pos.is_side(stop, xd))        {sc += W[var + 1];}
            if (pawn_is_free(pos, sq, sd, ai)) sc += W[var + 2];
            sc += W[var +  4 + square_dist(king_sd, stop)];
            sc += W[var + 12 + square_dist(king_xd, stop)];
         }
      }

      // pawn shield

      var = 397;

      for (Bit b = pawns_sd & king_zone_sd & ~pawn::rears(king_sd, sd); b != 0; b = bit::rest(b)) {

         Square sq = bit::first(b);

         File fl = square_file(sq);
         Rank rk = square_rank(sq, sd);

         if (fl >= File_Size / 2) fl = file_opp(fl);

         sc += W[var + 0 + fl];
         sc += W[var + 4 + rk];
      }

      // pieces

      Bit pawn_safe = ~(pawns_sd | ai.pawn_attacks(xd));

      int attackers = 0;

      for (Bit b = pos.non_king(sd); b != 0; b = bit::rest(b)) {
         Square sq = bit::first(b);
         if ((ai.piece_attacks(sq) & king_zone_xd & pawn_safe) != 0) attackers += 1;
      }

      attackers = std::min(attackers, 4);

      // piece loop

      for (Bit b = pos.non_king(sd); b != 0; b = bit::rest(b)) {

         Square sq = bit::first(b);
         Piece  pc = pos.piece(sq);

         assert(pc != King);

         File fl = square_file(sq);
         Rank rk = square_rank(sq, sd);

         if (fl >= File_Size / 2) fl = file_opp(fl);

         Bit tos = ai.piece_attacks(sq);

         // position

         var = 7 + pc * 32;

         sc += W[var + rk * 4 + fl];

         // mobility

         var = 199 + pc * 12;

         int mob = bit::count(tos & pawn_safe);
         sc += (W[var + 0 + fl] + W[var + 4 + rk]) * math::sqrt(mob);

         // captures

         var = 271 + pc * Piece_Size;

         for (Bit bt = tos & pos.pieces(xd) & ~(pawns_xd & ai.pawn_attacks(xd)); bt != 0; bt = bit::rest(bt)) {
            Square to = bit::first(bt);
            sc += W[var + pos.piece(to)];
         }

         // checks

         if (!bit::has(tos, king_xd)) { // skip if already giving check

            var = 307;

            int check = 0;

            for (Bit bt = tos & ~pos.pieces(sd) & bit::piece_attacks_to(pc, sd, king_xd) & ai.queen_safe(sd); bt != 0; bt = bit::rest(bt)) {

               Square to = bit::first(bt);

               assert(bit::line_is_empty(sq, to, pos.pieces()));
               if (bit::line_is_empty(to, king_xd, pos.pieces())) check += 1;
            }

            sc += W[var + pc] * check;
         }

         // pinned?

         if (is_pinned(pos, king_sd, sq, sd)) {

            var = 313 + pc * Piece_Size;

            Square pin_sq = pinned_by(pos, king_sd, sq, sd);
            Piece  pin_pc = pos.piece(pin_sq);

            if (pin_pc <= pc || (tos & bit::ray(king_sd, sq)) == 0) { // can't move
               sc += W[var + pin_pc];
            }
         }

         // king attack

         if (attackers != 0) {

            assert(attackers >= 1 && attackers <= 4);

            var = 349 + ((attackers - 1) * Piece_Size + pc) * 2;

            int p0 = bit::count(tos & king_zone_xd & pawn_safe & ~ai.queen_attacks(xd));
            int p1 = bit::count(tos & king_zone_xd & pawn_safe &  ai.queen_attacks(xd));

            sc += W[var + 0] * p0;
            sc += W[var + 1] * p1;
         }

         // defended piece

         if (pc != Queen && bit::has(ai.attacks(sd), sq)) {

            var = 409;

            if (pawn::is_protected(pos, sq, sd)) { // by pawn
               sc += W[var +  0 + fl];
               sc += W[var +  4 + rk];
            } else { // by piece
               sc += W[var + 12 + fl];
               sc += W[var + 16 + rk];
            }
         }

         // minor outpost

         if (piece_is_minor(pc)
          && pawn::is_protected(pos, sq, sd)
          && bit::has(pi.strong[sd], sq)
          && (fl >= File_C && fl <= File_F)
          && (rk >= Rank_4 && rk <= Rank_6)
          ) {

            var = 433;

            sc += W[var + pc * Rank_Size + rk];
         }

         // knight distance to pawns

         if (pc == Knight) {

            var = 756;

            double knight_file = double(square_file(sq)) + 0.5;
            double knight_rank = double(square_rank(sq)) + 0.5;

            double df = std::abs(knight_file - double(pi.centre_file));
            double dr = std::abs(knight_rank - double(pi.centre_rank));

            sc += W[var + 0] * df;
            sc += W[var + 1] * dr;
         }

         // bad bishop

         if (pc == Bishop) {

            var = 481;

            Bit bad_pawns = pawns_sd & bit::Colour_Squares[square_colour(sq)];

            int p0 = bit::count(bad_pawns &  blocked_sd);
            int p1 = bit::count(bad_pawns & ~blocked_sd);

            sc += W[var + 0] * p0;
            sc += W[var + 1] * p1;
         }

         // rook on open file

         if (pc == Rook && pawn::is_open(pos, sq, sd)) {

            var = 483;

            if (pawn::is_open(pos, sq, xd)) { // open
               sc += W[var + 0];
            } else { // semi-open
               sc += W[var + 1];
            }
         }

         // rook blocked by own king

         if (pc == Rook
          && bit::count(tos & pawn_safe & ~pos.pieces(King, sd)) < 3
          && rk < Rank_3
          && square_rank(king_sd, sd) == Rank_1
          && !bit::has(pos.castling_rooks(sd), sq)
          && (square_file(king_sd) < File_Size / 2
            ? square_file(sq) <= square_file(king_sd)
            : square_file(sq) >= square_file(king_sd))
          ) {

            var = 485;

            sc += W[var];
         }
      }

      // king

      {
         Square sq = pos.king(sd);
         Piece  pc = King;

         File fl = square_file(sq);
         Rank rk = square_rank(sq, sd);

         if (fl >= File_Size / 2) fl = file_opp(fl);

         Bit tos = ai.piece_attacks(sq);

         // position

         var = 7 + pc * 32;

         sc += W[var + rk * 4 + fl];

         // captures

         var = 271 + pc * Piece_Size;

         for (Bit bt = tos & pos.pieces(xd) & ~ai.pawn_attacks(xd); bt != 0; bt = bit::rest(bt)) {
            Square to = bit::first(bt);
            sc += W[var + pos.piece(to)];
         }

         // distance to pawns

         var = 754;

         double king_file = double(square_file(sq)) + 0.5;
         double king_rank = double(square_rank(sq)) + 0.5;

         double df = std::abs(king_file - double(pi.centre_file));
         double dr = std::abs(king_rank - double(pi.centre_rank));

         sc += W[var + 0] * df;
         sc += W[var + 1] * dr;
      }

      // side-to-move bonus

      if (sd == pos.turn() && !bit::has(ai.attacks(xd), king_sd)) {

         var = 758;

         sc += W[var];
      }

      // prepare for opponent

      sc = -sc;
   }

   // game phase

   int stage = pos::stage(pos);

   return ml::div_round(sc.mg() * (Stage_Size - stage) + sc.eg() * stage, Stage_Size * Scale); // unit -> cp
}

static int eval_vectorized(const Pos & pos, EvalVector &eval_vec) {

   //Key key = pos.key_pawn();
   //Pawn_Info & entry = G_Pawn_Table[hash::index(key, Pawn_Table_Mask)];

   //if (entry.key != key) {
   //   comp_pawn_info(entry, pos);
   //   entry.key = key;
   //}

   //Pawn_Info pi = entry;
   
   eval_vec.clear();

   Pawn_Info pi;
   comp_pawn_info_vectorized(pi, pos, eval_vec);

   Attack_Info ai;
   ai.init(pos);

   Score_Pair sc;

   for (int s = 0; s < Side_Size; s++) {

      Side sd = side_make(s);
      Side xd = side_opp(sd);

      Bit pawns_sd = pos.pawns(sd);
      Bit pawns_xd = pos.pawns(xd);

      Square king_sd = pos.king(sd);
      Square king_xd = pos.king(xd);

      Bit king_zone_sd = king_zone(king_sd, sd);
      Bit king_zone_xd = king_zone(king_xd, xd);

      int var;

      // material

      var = 0;

      for (int p = Pawn; p <= Queen; p++) {

         Piece pc = piece_make(p);

         int mat = pos.count(pc, sd);
         sc += W[var + pc] * mat;

         eval_vec.setValue(s, var + pc, mat);
      }

      var = 6;

      if (pos.count(Bishop, sd) > 1) {
         sc += W[var];
         eval_vec.setValue(s, var, 1);
      }

      // pawns

      Piece pc = Pawn;

      Bit blocked_sd = pawn::blocked(pos, sd);

      sc += pi.score[sd]; // pawn-only score

      // pawn mobility

      var = 199 + pc * 12;

      int mob = bit::count(bit::pawn_moves(sd, pawns_sd) & pos.empties());
      sc += W[var] * mob;
      eval_vec.setValue(s, var, mob);


      // pawn captures

      var = 271 + pc * Piece_Size;

      for (Bit b = ai.pawn_attacks(sd) & pos.non_pawns(xd); b != 0; b = bit::rest(b)) {
         Square to = bit::first(b);
         sc += W[var + pos.piece(to)];
         eval_vec.incValue(s, var + pos.piece(to), 1);
      }

      // passed pawns

      for (Bit b = pi.passed[sd] & pos.pawns(sd); b != 0; b = bit::rest(b)) {

         Square sq = bit::first(b);

         Rank rank = std::max(square_rank(sq, sd), Rank_3);

         if (pawn_is_unstoppable(pos, sq, sd, ai)) {

            int gain = piece_mat(Queen) - piece_mat(Pawn);
            sc += Score_Pair(gain * Scale * (rank - Rank_2) / 6);
            eval_vec.incUnstoppable(sd, gain * Scale * (rank - Rank_2) / 6);

         } else {

            var = 486 + rank * 20;

            Square stop = square_front(sq, sd);

            sc += W[var + 0];
            eval_vec.incValue(s, var + 0, 1);

            if (!pos.is_side(stop, xd)) {
               sc += W[var + 1];
               eval_vec.incValue(s, var + 1, 1);
            }
            if (pawn_is_free(pos, sq, sd, ai)) {
               sc += W[var + 2];
               eval_vec.incValue(s, var + 2, 1);
            }
            sc += W[var +  4 + square_dist(king_sd, stop)];
            sc += W[var + 12 + square_dist(king_xd, stop)];
            eval_vec.incValue(s, var +  4 + square_dist(king_sd, stop), 1);
            eval_vec.incValue(s, var + 12 + square_dist(king_xd, stop), 1);
         }
      }

      // pawn shield

      var = 397;

      for (Bit b = pawns_sd & king_zone_sd & ~pawn::rears(king_sd, sd); b != 0; b = bit::rest(b)) {

         Square sq = bit::first(b);

         File fl = square_file(sq);
         Rank rk = square_rank(sq, sd);

         if (fl >= File_Size / 2) fl = file_opp(fl);

         sc += W[var + 0 + fl];
         sc += W[var + 4 + rk];
         eval_vec.incValue(s, var + 0 + fl, 1);
         eval_vec.incValue(s, var + 4 + rk, 1);
      }

      // pieces

      Bit pawn_safe = ~(pawns_sd | ai.pawn_attacks(xd));

      int attackers = 0;

      for (Bit b = pos.non_king(sd); b != 0; b = bit::rest(b)) {
         Square sq = bit::first(b);
         if ((ai.piece_attacks(sq) & king_zone_xd & pawn_safe) != 0) attackers += 1;
      }

      attackers = std::min(attackers, 4);

      // piece loop

      for (Bit b = pos.non_king(sd); b != 0; b = bit::rest(b)) {

         Square sq = bit::first(b);
         Piece  pc = pos.piece(sq);

         assert(pc != King);

         File fl = square_file(sq);
         Rank rk = square_rank(sq, sd);

         if (fl >= File_Size / 2) fl = file_opp(fl);

         Bit tos = ai.piece_attacks(sq);

         // position

         var = 7 + pc * 32;

         sc += W[var + rk * 4 + fl];
         eval_vec.incValue(s, var + rk * 4 + fl, 1);

         // mobility

         var = 199 + pc * 12;

         int mob = bit::count(tos & pawn_safe);
         sc += (W[var + 0 + fl] + W[var + 4 + rk]) * math::sqrt(mob);
         eval_vec.incValue(s, var + 0 + fl, math::sqrt(mob));
         eval_vec.incValue(s, var + 4 + rk, math::sqrt(mob));

         // captures

         var = 271 + pc * Piece_Size;

         for (Bit bt = tos & pos.pieces(xd) & ~(pawns_xd & ai.pawn_attacks(xd)); bt != 0; bt = bit::rest(bt)) {
            Square to = bit::first(bt);
            sc += W[var + pos.piece(to)];
            eval_vec.incValue(s, var + pos.piece(to), 1);
         }

         // checks

         if (!bit::has(tos, king_xd)) { // skip if already giving check

            var = 307;

            int check = 0;

            for (Bit bt = tos & ~pos.pieces(sd) & bit::piece_attacks_to(pc, sd, king_xd) & ai.queen_safe(sd); bt != 0; bt = bit::rest(bt)) {

               Square to = bit::first(bt);

               assert(bit::line_is_empty(sq, to, pos.pieces()));
               if (bit::line_is_empty(to, king_xd, pos.pieces())) check += 1;
            }

            sc += W[var + pc] * check;
            eval_vec.incValue(s, var + pc, check);
         }

         // pinned?

         if (is_pinned(pos, king_sd, sq, sd)) {

            var = 313 + pc * Piece_Size;

            Square pin_sq = pinned_by(pos, king_sd, sq, sd);
            Piece  pin_pc = pos.piece(pin_sq);

            if (pin_pc <= pc || (tos & bit::ray(king_sd, sq)) == 0) { // can't move
               sc += W[var + pin_pc];
               eval_vec.incValue(s, var + pin_pc, 1);
            }
         }

         // king attack

         if (attackers != 0) {

            assert(attackers >= 1 && attackers <= 4);

            var = 349 + ((attackers - 1) * Piece_Size + pc) * 2;

            int p0 = bit::count(tos & king_zone_xd & pawn_safe & ~ai.queen_attacks(xd));
            int p1 = bit::count(tos & king_zone_xd & pawn_safe &  ai.queen_attacks(xd));

            sc += W[var + 0] * p0;
            sc += W[var + 1] * p1;
            eval_vec.incValue(s, var + 0, p0);
            eval_vec.incValue(s, var + 1, p1);
         }

         // defended piece

         if (pc != Queen && bit::has(ai.attacks(sd), sq)) {

            var = 409;

            if (pawn::is_protected(pos, sq, sd)) { // by pawn
               sc += W[var +  0 + fl];
               sc += W[var +  4 + rk];
               eval_vec.incValue(s, var +  0 + fl, 1);
               eval_vec.incValue(s, var +  4 + rk, 1);
            } else { // by piece
               sc += W[var + 12 + fl];
               sc += W[var + 16 + rk];
               eval_vec.incValue(s, var + 12 + fl, 1);
               eval_vec.incValue(s, var + 16 + rk, 1);
            }
         }

         // minor outpost

         if (piece_is_minor(pc)
          && pawn::is_protected(pos, sq, sd)
          && bit::has(pi.strong[sd], sq)
          && (fl >= File_C && fl <= File_F)
          && (rk >= Rank_4 && rk <= Rank_6)
          ) {

            var = 433;

            sc += W[var + pc * Rank_Size + rk];
            eval_vec.incValue(s, var + pc * Rank_Size + rk, 1);
         }

         // knight distance to pawns

         if (pc == Knight) {

            var = 756;

            double knight_file = double(square_file(sq)) + 0.5;
            double knight_rank = double(square_rank(sq)) + 0.5;

            double df = std::abs(knight_file - double(pi.centre_file));
            double dr = std::abs(knight_rank - double(pi.centre_rank));

            sc += W[var + 0] * df;
            sc += W[var + 1] * dr;
            eval_vec.incValue(s, var + 0, df);
            eval_vec.incValue(s, var + 1, dr);
         }

         // bad bishop

         if (pc == Bishop) {

            var = 481;

            Bit bad_pawns = pawns_sd & bit::Colour_Squares[square_colour(sq)];

            int p0 = bit::count(bad_pawns &  blocked_sd);
            int p1 = bit::count(bad_pawns & ~blocked_sd);

            sc += W[var + 0] * p0;
            sc += W[var + 1] * p1;
            eval_vec.incValue(s, var + 0, p0);
            eval_vec.incValue(s, var + 1, p1);
         }

         // rook on open file

         if (pc == Rook && pawn::is_open(pos, sq, sd)) {

            var = 483;

            if (pawn::is_open(pos, sq, xd)) { // open
               sc += W[var + 0];
               eval_vec.incValue(s, var + 0, 1);
            } else { // semi-open
               sc += W[var + 1];
               eval_vec.incValue(s, var + 1, 1);
            }
         }

         // rook blocked by own king

         if (pc == Rook
          && bit::count(tos & pawn_safe & ~pos.pieces(King, sd)) < 3
          && rk < Rank_3
          && square_rank(king_sd, sd) == Rank_1
          && !bit::has(pos.castling_rooks(sd), sq)
          && (square_file(king_sd) < File_Size / 2
            ? square_file(sq) <= square_file(king_sd)
            : square_file(sq) >= square_file(king_sd))
          ) {

            var = 485;

            sc += W[var];
            eval_vec.incValue(s, var, 1);
         }
      }

      // king

      {
         Square sq = pos.king(sd);
         Piece  pc = King;

         File fl = square_file(sq);
         Rank rk = square_rank(sq, sd);

         if (fl >= File_Size / 2) fl = file_opp(fl);

         Bit tos = ai.piece_attacks(sq);

         // position

         var = 7 + pc * 32;

         sc += W[var + rk * 4 + fl];
         eval_vec.setValue(s, var + rk * 4 + fl, 1);

         // captures

         var = 271 + pc * Piece_Size;

         for (Bit bt = tos & pos.pieces(xd) & ~ai.pawn_attacks(xd); bt != 0; bt = bit::rest(bt)) {
            Square to = bit::first(bt);
            sc += W[var + pos.piece(to)];
            eval_vec.incValue(s, var + pos.piece(to), 1);
         }

         // distance to pawns

         var = 754;

         double king_file = double(square_file(sq)) + 0.5;
         double king_rank = double(square_rank(sq)) + 0.5;

         double df = std::abs(king_file - double(pi.centre_file));
         double dr = std::abs(king_rank - double(pi.centre_rank));

         sc += W[var + 0] * df;
         sc += W[var + 1] * dr;
         eval_vec.setValue(s, var + 0, df);
         eval_vec.setValue(s, var + 1, dr);
      }

      // side-to-move bonus

      if (sd == pos.turn() && !bit::has(ai.attacks(xd), king_sd)) {

         var = 758;

         sc += W[var];
         eval_vec.setValue(s, var, 1);
      }

      // prepare for opponent

      sc = -sc;
   }

   // game phase

   //int stage = pos::stage(pos);

   //return ml::div_round(sc.mg() * (Stage_Size - stage) + sc.eg() * stage, Stage_Size * Scale); // unit -> cp

   int stage = pos::stage(pos);
   eval_vec.setFactor((Stage_Size - stage), stage, Stage_Size * Scale);

   int prod_result = eval_vec.productWeight(W, pos.turn(), sc);

   //eval_vec.printFeat(pos.turn());
   return prod_result;
}

static int comp_pawn_info_vectorized(Pawn_Info & pi, const Pos & pos, EvalVector & eval_vec) {
  for (int sd = 0; sd < Side_Size; sd++) {
      pi.passed[sd] = Bit(0);
      pi.strong[sd] = pawn::strong(pos, Side(sd));
   }

   pi.centre_file = 0.0;
   pi.centre_rank = 0.0;

   for (int s = 0; s < Side_Size; s++) {

      Side sd = side_make(s);

      Score_Pair sc;

      int var;

      // init

      Piece pc = Pawn;

      Bit weak_sd = pawn::weak(pos, sd);

      // pawn loop

      for (Bit b = pos.pawns(sd); b != 0; b = bit::rest(b)) {

         Square sq = bit::first(b);

         File fl = square_file(sq);
         Rank rk = square_rank(sq, sd);

         if (fl >= File_Size / 2) fl = file_opp(fl);

         // position

         var = 7 + pc * 32;

         sc += W[var + rk * 4 + fl];
         eval_vec.incValue(s, var + rk * 4 + fl, 1);

         // space

         var = 646;

         if (pawn::is_duo(pos, sq, sd)) {
            sc += W[var +  0 + rk * 4 + fl];
            eval_vec.incValue(s, var +  0 + rk * 4 + fl, 1);
         }
         if (pawn::is_protected(pos, sq, sd)) {
            sc += W[var + 32 + rk * 4 + fl];
            eval_vec.incValue(s, var + 32 + rk * 4 + fl, 1);
         }
         if (pawn::is_ram(pos, sq, sd)) {
            sc += W[var + 64 + rk * 4 + fl];
            eval_vec.incValue(s, var + 64 + rk * 4 + fl, 1);
         }

         // weak?

         if (bit::has(weak_sd, sq)) {

            var = 742;

            sc += W[var + 0 + fl];
            sc += W[var + 4 + rk];
            eval_vec.incValue(s, var + 0 + fl, 1);
            eval_vec.incValue(s, var + 4 + rk, 1);
         }

         // passed?

         if (pawn::is_passed(pos, sq, sd)) {
            bit::set(pi.passed[sd], sq);
         }

         // centre

         pi.centre_file += double(square_file(sq)) + 0.5;
         pi.centre_rank += double(square_rank(sq)) + 0.5;
      }

      pi.score[sd] = sc;
   }

   int pawn_size = bit::count(pos.pieces(Pawn));

   if (pawn_size == 0) { // no pawns => board centre
      pi.centre_file = double(File_Size) / 2.0;
      pi.centre_rank = double(Rank_Size) / 2.0;
   } else {
      pi.centre_file /= double(pawn_size);
      pi.centre_rank /= double(pawn_size);
   }
}

static int compute_dot_product() {

}

static void comp_pawn_info(Pawn_Info & pi, const Pos & pos) {

   for (int sd = 0; sd < Side_Size; sd++) {
      pi.passed[sd] = Bit(0);
      pi.strong[sd] = pawn::strong(pos, Side(sd));
   }

   pi.centre_file = 0.0;
   pi.centre_rank = 0.0;

   for (int s = 0; s < Side_Size; s++) {

      Side sd = side_make(s);

      Score_Pair sc;

      int var;

      // init

      Piece pc = Pawn;

      Bit weak_sd = pawn::weak(pos, sd);

      // pawn loop

      for (Bit b = pos.pawns(sd); b != 0; b = bit::rest(b)) {

         Square sq = bit::first(b);

         File fl = square_file(sq);
         Rank rk = square_rank(sq, sd);

         if (fl >= File_Size / 2) fl = file_opp(fl);

         // position

         var = 7 + pc * 32;

         sc += W[var + rk * 4 + fl];

         // space

         var = 646;

         if (pawn::is_duo      (pos, sq, sd)) sc += W[var +  0 + rk * 4 + fl];
         if (pawn::is_protected(pos, sq, sd)) sc += W[var + 32 + rk * 4 + fl];
         if (pawn::is_ram      (pos, sq, sd)) sc += W[var + 64 + rk * 4 + fl];

         // weak?

         if (bit::has(weak_sd, sq)) {

            var = 742;

            sc += W[var + 0 + fl];
            sc += W[var + 4 + rk];
         }

         // passed?

         if (pawn::is_passed(pos, sq, sd)) bit::set(pi.passed[sd], sq);

         // centre

         pi.centre_file += double(square_file(sq)) + 0.5;
         pi.centre_rank += double(square_rank(sq)) + 0.5;
      }

      pi.score[sd] = sc;
   }

   int pawn_size = bit::count(pos.pieces(Pawn));

   if (pawn_size == 0) { // no pawns => board centre
      pi.centre_file = double(File_Size) / 2.0;
      pi.centre_rank = double(Rank_Size) / 2.0;
   } else {
      pi.centre_file /= double(pawn_size);
      pi.centre_rank /= double(pawn_size);
   }
}

static bool two_knights(const Pos & pos, Side sd) {

   Bit pieces = pos.non_king(sd);
   if (pieces != pos.pieces(Knight, sd)) return false;

   if (bit::count(pieces) != 2) return false;

   return true;
}

static bool rook_pawn_draw(const Pos & pos, Side sd, File fl) {

   Bit pawns = pos.pawns(sd);
   if (pawns == 0 || !bit::is_incl(pawns, bit::file(fl))) return false;

   Bit bishops = pos.non_king(sd);
   if (bishops != pos.pieces(Bishop, sd)) return false;

   Square prom = square_make(fl, Rank_8, sd);

   Side xd = side_opp(sd);
   if (square_dist(pos.king(xd), prom) > 1) return false;

   Bit squares = bit::Colour_Squares[square_colour(prom)];
   if ((bishops & squares) != 0) return false;

   return true;
}

static Bit king_zone(Square sq, Side sd) {

   File fl = math::clamp(square_file(sq), File_B, File_G);
   Rank rk = math::clamp(square_rank(sq), Rank_2, Rank_7);

   sq = square_make(fl, rk);
   return bit::bit(sq) | bit::piece_attacks(King, sd, sq); // 3x3 square
}

static bool pawn_is_free(const Pos & pos, Square sq, Side sd, const Attack_Info & ai) {

   Side xd = side_opp(sd);

   Square stop = square_front(sq, sd);
   Bit fronts = pawn::file(sq) & pawn::fronts(sq, sd);

   return (fronts & (pos.pieces(xd) | ~ai.queen_safe(sd))) == 0 // free path
       && !is_pinned(pos, stop, sq, sd); // not attacked from behind by major
}

static bool pawn_is_unstoppable(const Pos & pos, Square sq, Side sd, const Attack_Info & ai) {

   Side xd = side_opp(sd);
   if (!pos::lone_king(pos, xd)) return false;

   Bit fronts = pawn::file(sq) & pawn::fronts(sq, sd);
   if ((fronts & pos.pieces()) != 0) return false;

   Square king_sd = pos.king(sd);
   Square king_xd = pos.king(xd);

   Rank rk   = square_rank(sq, sd);
   Rank rank = std::max(rk, Rank_3);

   Square prom = square_prom(sq, sd);

   int md = Rank_8 - rank;
   int od = square_dist(king_xd, prom);

   if (pos.turn() == xd) md += 1;

   return md < od // faster than opponent king
       || bit::is_incl(fronts, ai.piece_attacks(king_sd)); // protected path
}

Score piece_mat(Piece pc) {

   assert(pc != Piece_None);

   const int mat[Piece_Size + 1] { 100, 325, 325, 500, 1000, 10000, 0 };
   return Score(mat[pc]);
}

Score_Pair::Score_Pair() : Score_Pair(0) {
}

Score_Pair::Score_Pair(int sc) : Score_Pair(sc, sc) {
}

Score_Pair::Score_Pair(int mg, int eg) {
   p_vec = (int64(mg) << 32) + int64(eg); // HACK: "eg"'s sign leaks to "mg"
}

void Score_Pair::operator+=(Score_Pair sp) {
   p_vec += sp.p_vec;
}

void Score_Pair::operator-=(Score_Pair sp) {
   p_vec -= sp.p_vec;
}

int Score_Pair::mg() const {
   return p_vec >> 32;
}

int Score_Pair::eg() const {
   return int(p_vec); // extend sign
}

Score_Pair Score_Pair::make(int64 vec) {
   Score_Pair sp;
   sp.p_vec = vec;
   return sp;
}

Score_Pair operator+(Score_Pair sp) {
   return Score_Pair::make(+sp.p_vec);
}

Score_Pair operator-(Score_Pair sp) {
   return Score_Pair::make(-sp.p_vec);
}

Score_Pair operator+(Score_Pair s0, Score_Pair s1) {
   return Score_Pair::make(s0.p_vec + s1.p_vec);
}

Score_Pair operator-(Score_Pair s0, Score_Pair s1) {
   return Score_Pair::make(s0.p_vec - s1.p_vec);
}

Score_Pair operator*(Score_Pair weight, int n) {
   return Score_Pair::make(weight.p_vec * n);
}

Score_Pair operator*(Score_Pair weight, double x) {
   return Score_Pair(ml::round(double(weight.mg()) * x),
                     ml::round(double(weight.eg()) * x));
}

/*
// denominator = 64, return enumerator
int drawish_reduce_factor(const Pos & pos, Side sd) {

   EvalVector eval_vec;

   int sc = eval(pos);
   int sc2 = eval_vectorized(pos, eval_vec);

   // drawish?

   int factor = 64;

   Side win  = (sc >= 0) ? White : Black;
   Side lose = side_opp(win);

   int fw = pos::force(pos, win);
   int fl = pos::force(pos, lose);

   if (fw < 6) {

      Bit pw = pos.pawns(win);
      Bit pl = pos.pawns(lose);

      Bit minors = pos.pieces(Knight, lose) | pos.pieces(Bishop, lose);

      if (fw == 0 && pw == 0) { // lone king

         factor = 0; //sc = 0;

      } else if (rook_pawn_draw(pos, win, File_A)) {

         factor = 1; //sc /= 64;

      } else if (rook_pawn_draw(pos, win, File_H)) {

         factor = 1; //sc /= 64;

      } else if (pw == 0) {

         if (fw <= 1) { // insufficient material
            factor = 4; //sc /= 16;
         } else if (fw == 2 && two_knights(pos, win) && pl == 0) {
            factor = 4; //sc /= 16;
         } else if (fw - fl <= 1) {
            factor = 16; //sc /= 4;
         }

      } else if (bit::is_single(pw)) {

         Square pawn = bit::first(pw);
         bool blocked = (pawn::file(pawn) & pawn::fronts(pawn, win) & pos.pieces(King, lose)) != 0;

         if (fw <= 1 && minors != 0) { // minor sacrifice
            factor = 8; //sc /= 8;
         } else if (fw == 2 && two_knights(pos, win) && pl == 0 && minors != 0) { // minor sacrifice
            factor = 8; //sc /= 8;
         } else if (fw == fl && blocked) { // blocked by king
            factor = 16; //sc /= 4;
         } else if (fw == fl && minors != 0) { // minor sacrifice
            factor = 32; //sc /= 2;
         }

      } else if (pos::opposit_bishops(pos) && std::abs(pos.count(Pawn, White) - pos.count(Pawn, Black)) <= 2) {

         factor = 32; //sc /= 2;
      }
   }

   return factor;
   //return score::clamp(score::side(Score(sc), sd)); // for sd
}
*/

int eval_featurize(const Pos & pos, SparseVector &sv) {
   EvalVector eval_vec;
   int sc2 = eval_vectorized(pos, eval_vec);
   eval_vec.toSparseVector(pos.turn(), sv);
   return sc2;
}

int eval_featurize_with_factor(const Pos & pos, FeatureAndFactor &fv) {
   EvalVector eval_vec;
   int sc2 = eval_vectorized(pos, eval_vec);
   eval_vec.toFeatureAndFactor(pos.turn(), fv);
   return sc2;
}

int eval_nonreduce(const Pos & pos) {
   int sc = eval(pos);
   int sideSc = score::side(Score(sc), pos.turn());
   return sideSc;
}