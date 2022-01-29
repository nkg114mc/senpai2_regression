
////// libmy.cpp /////////////////////////////////////////


#ifndef LIBMY_HPP
#define LIBMY_HPP

// includes

#include <cstdint>
#include <iostream>
#include <string>
#include <chrono>

// constants

#undef FALSE
#define FALSE 0

#undef TRUE
#define TRUE 1

#ifdef DEBUG
#  undef DEBUG
#  define DEBUG TRUE
#else
#  define DEBUG FALSE
#endif

#ifdef BMI
#  undef BMI
#  define BMI TRUE
#else
#  define BMI FALSE
#endif

#ifdef _MSC_VER
#include <intrin.h>
#pragma intrinsic(_BitScanForward64)
#pragma intrinsic(_BitScanReverse64)
#  if BMI
#include <immintrin.h>
#pragma intrinsic(_pext_u64)
#pragma intrinsic(_pdep_u64)
#  endif
#endif

// macros

#if DEBUG
#  undef NDEBUG
#else
#  define NDEBUG
#endif

#include <cassert> // needs NDEBUG

// types

typedef std::int8_t  int8;
typedef std::int16_t int16;
typedef std::int32_t int32;
typedef std::int64_t int64;

typedef std::uint8_t  uint8;
typedef std::uint16_t uint16;
typedef std::uint32_t uint32;
typedef std::uint64_t uint64;

// functions

namespace ml {

   // math

   uint64 rand_int_64 ();

   int  round (double x);

   int  div       (int a, int b);
   int  div_round (int a, int b);

   bool is_power_2 (int64 n);
   int  log_2      (int64 n);

   inline uint64 bit       (int n) { return uint64(1) << n; }
   inline uint64 bit_mask  (int n) { return bit(n) - 1; }

   inline void bit_set   (uint64 & b, int n) { b |=  bit(n); }
   inline void bit_clear (uint64 & b, int n) { b &= ~bit(n); }
   inline bool bit_has   (uint64   b, int n) { return (b & bit(n)) != 0; }

#ifdef _MSC_VER

   inline int bit_first (uint64 b) { assert(b != 0); unsigned long i; _BitScanForward64(&i, b); return i; }
   inline int bit_count (uint64 b) { return int(__popcnt64(b)); }
#if BMI
   inline uint64 pext (uint64 a, uint64 b) { return _pext_u64(a, b); }
   inline uint64 pdep (uint64 a, uint64 b) { return _pdep_u64(a, b); }
#endif

#else // assume GCC/Clang

   inline int bit_first (uint64 b) { assert(b != 0); return __builtin_ctzll(b); }
   inline int bit_count (uint64 b) { return __builtin_popcountll(b); }
#if BMI
   inline uint64 pext (uint64 a, uint64 b) { return __builtin_ia32_pext_di(a, b); }
   inline uint64 pdep (uint64 a, uint64 b) { return __builtin_ia32_pdep_di(a, b); }
#endif

#endif
}

#endif // !defined LIBMY_HPP



///// common.hpp ////////////////////////

#ifndef COMMON_HPP
#define COMMON_HPP

// includes

#include <string>

//#include "libmy.hpp"

// constants

const int File_Size { 8 };
const int Rank_Size { 8 };
const int Square_Size { File_Size * Rank_Size };

const int Vector_File_Size { File_Size * 2 - 1 };
const int Vector_Rank_Size { Rank_Size * 2 - 1 };

const int Side_Size { 2 };
const int Piece_Size { 6 };
const int Piece_Size_2 { 1 << 3 }; // for array index
const int Piece_Side_Size { Piece_Size * Side_Size }; // excludes Empty #

const int Move_Index_Size { 1 << 10 };

const int Stage_Size { 24 };

// types

enum Square : int { Square_None = -1 };
enum File   : int { File_A, File_B, File_C, File_D, File_E, File_F, File_G, File_H };
enum Rank   : int { Rank_1, Rank_2, Rank_3, Rank_4, Rank_5, Rank_6, Rank_7, Rank_8 };

enum Vec : int { Vector_Max = (Rank_Size - 1) * Vector_File_Size + (File_Size - 1) };

enum Inc : int {
   Inc_N  = 1,
   Inc_SW = File_Size - 1,
   Inc_W  = File_Size,
   Inc_NW = File_Size + 1,
};

enum Side  : int { White, Black };
enum Piece : int { Pawn, Knight, Bishop, Rook, Queen, King, Piece_None };

enum Piece_Side : int { Empty = Piece_Side_Size };

enum class Key  : uint64;
enum class Move : int;

enum Move_Index : int { Move_Index_None = -1 };

enum Depth : int;
enum Ply   : int;
enum Score : int;

enum class Flag : int {
   None  = 0,
   Upper = 1 << 0,
   Lower = 1 << 1,
   Exact = Upper | Lower,
};

class Bit {

private :

   uint64 p_bit;

public :

   Bit ();
   explicit Bit (uint64 bit);

   operator uint64 () const;

   void operator |= (Bit b);
   void operator &= (uint64 b);
   void operator ^= (Bit b);
};

// operators

inline File operator + (File fl, int inc) { return File(int(fl) + inc); }
inline File operator - (File fl, int inc) { return File(int(fl) - inc); }

inline Rank operator + (Rank rk, int inc) { return Rank(int(rk) + inc); }
inline Rank operator - (Rank rk, int inc) { return Rank(int(rk) - inc); }

inline Vec  operator + (Vec v0, Vec v1) { return Vec(int(v0) + int(v1)); }
inline Vec  operator - (Vec v0, Vec v1) { return Vec(int(v0) - int(v1)); }

inline Inc  operator + (Inc inc) { return Inc(+int(inc)); }
inline Inc  operator - (Inc inc) { return Inc(-int(inc)); }

inline Inc  operator * (Inc inc, int steps) { return Inc(int(inc) * steps); }

inline void operator ^= (Key & k0, Key k1) { k0 = Key(uint64(k0) ^ uint64(k1)); }

inline Depth operator + (Depth d0, Depth d1) { return Depth(int(d0) + int(d1)); }
inline Depth operator - (Depth d0, Depth d1) { return Depth(int(d0) - int(d1)); }

inline Ply  operator + (Ply p0, Ply p1) { return Ply(int(p0) + int(p1)); }
inline Ply  operator - (Ply p0, Ply p1) { return Ply(int(p0) - int(p1)); }

inline Score operator + (Score sc) { return Score(+int(sc)); }
inline Score operator - (Score sc) { return Score(-int(sc)); }

inline Score operator + (Score s0, Score s1) { return Score(int(s0) + int(s1)); }
inline Score operator - (Score s0, Score s1) { return Score(int(s0) - int(s1)); }

inline void operator += (Score & s0, Score s1) { s0 = s0 + s1; }
inline void operator -= (Score & s0, Score s1) { s0 = s0 - s1; }

inline Flag operator | (Flag f0, Flag f1) { return Flag(int(f0) | int(f1)); }

inline void operator |= (Flag & f0, Flag f1) { f0 = f0 | f1; }

inline Bit  operator ~ (Bit b) { return Bit(~uint64(b)); }

inline Bit  operator | (Bit b0, Bit    b1) { return Bit(uint64(b0) | uint64(b1)); }
inline Bit  operator & (Bit b0, uint64 b1) { return Bit(uint64(b0) & b1); }
inline Bit  operator ^ (Bit b0, Bit    b1) { return Bit(uint64(b0) ^ uint64(b1)); }

// functions

bool   square_is_ok (int fl, int rk);
bool   square_is_ok (int sq);
Square square_make  (int fl, int rk);
Square square_make  (int fl, int rk, Side sd);
Square square_make  (int sq);

File square_file (Square sq);
Rank square_rank (Square sq);

Rank square_rank         (Square sq, Side sd);
bool square_is_promotion (Square sq);
int  square_colour       (Square sq);

Inc    square_inc   (Side sd);
Square square_front (Square sq, Side sd);
Square square_rear  (Square sq, Side sd);
Square square_prom  (Square sq, Side sd);

int  square_dist      (Square s0, Square s1);
int  square_dist_file (Square s0, Square s1);
int  square_dist_rank (Square s0, Square s1);

std::string square_to_string(Square sq);


bool file_is_ok (int fl);
bool rank_is_ok (int rk);
File file_make  (int fl);
Rank rank_make  (int rk);

File file_opp  (File fl);
Rank rank_opp  (Rank rk);
Rank rank_side (Rank rk, Side sd);

Vec    vector_make (int df, int dr);
Square square_add  (Square from, Vec vec);

bool  piece_is_ok (int pc);
Piece piece_make  (int pc);
bool  piece_is_minor (Piece pc);

bool side_is_ok (int sd);
Side side_make  (int sd);
Side side_opp   (Side sd);


bool       piece_side_is_ok (int ps);
Piece_Side piece_side_make  (int ps);
Piece_Side piece_side_make  (Piece pc, Side sd);
Piece      piece_side_piece (Piece_Side ps);
Side       piece_side_side  (Piece_Side ps);

#endif // !defined COMMON_HPP


///// math.cpp ///////////////////////////////////////


#ifndef MATH_HPP
#define MATH_HPP

namespace math {

// functions

void init ();

template <typename T>
inline T clamp(T x, T min, T max) {

   if (x < min) {
      return min;
   } else if (x > max) {
      return max;
   } else {
      return x;
   }
}

double sqrt  (int n);
double log_2 (int n);

}

#endif // !defined MATH_HPP


////// score.cpp /////////////////////////////////////
#ifndef SCORE_HPP
#define SCORE_HPP

namespace score {

// constants

const Score Inf      = Score(10000);
const Score Eval_Inf = Inf - Score(100);
const Score None     = -Inf - Score(1);

// functions

template <typename T>
inline T side(T sc, Side sd) {
   return (sd == White) ? +sc : -sc;
}

//bool  is_ok (int sc);

Score clamp    (Score sc);
//Score add_safe (Score sc, Score inc);
/*
int  ply (Score sc);
*/
}

#endif // !defined SCORE_HPP

#ifndef BIT_HPP
#define BIT_HPP

// includes

//#include "common.hpp"

namespace bit {

// variables

extern Bit Pawn_Squares;
extern Bit Promotion_Squares;
extern Bit Colour_Squares[2];

// functions

void init ();

Bit  bit (Square sq);

bool has (Bit b, Square sq);
int  bit (Bit b, Square sq);

bool is_single (Bit b);
bool is_incl   (Bit b0, Bit b1);

void set   (Bit & b, Square sq);
void clear (Bit & b, Square sq);
void flip  (Bit & b, Square sq);

Bit  add    (Bit b, Square sq);
Bit  remove (Bit b, Square sq);

Square first (Bit b);
Bit    rest  (Bit b);
int    count (Bit b);

Bit  file (File fl);
Bit  rank (Rank rk);
Bit  rank (Rank rk, Side sd);

Bit  rect (int left, int bottom, int right, int top);

Bit  ray     (Square from, Square to);
Bit  beyond  (Square from, Square to);
Bit  between (Square from, Square to);
Bit  line    (Square from, Square to);

Bit  pawn_moves   (Side sd, Bit froms);
Bit  pawn_attacks (Side sd, Bit froms);

Bit  pawn_moves_to   (Side sd, Bit tos);
Bit  pawn_attacks_to (Side sd, Bit tos);

Bit  pawn_moves      (Side sd, Square from);
Bit  pawn_attacks    (Side sd, Square from);
Bit  pawn_attacks_to (Side sd, Square to);

Bit  piece_attacks    (Piece pc, Square from);
Bit  piece_attacks_to (Piece pc, Square to);

Bit  piece_attacks    (Piece pc, Side sd, Square from);
Bit  piece_attacks_to (Piece pc, Side sd, Square to);

Bit  piece_attacks    (Piece pc, Square from, Bit pieces);
Bit  piece_attacks_to (Piece pc, Square to,   Bit pieces);

bool piece_attack (Piece pc, Side sd, Square from, Square to);
bool piece_attack (Piece pc, Side sd, Square from, Square to, Bit pieces);

Bit  knight_attacks (Square from);
Bit  bishop_attacks (Square from, Bit pieces);
Bit  rook_attacks   (Square from, Bit pieces);
Bit  queen_attacks  (Square from, Bit pieces);
Bit  king_attacks   (Square from);

bool line_is_empty (Square from, Square to, Bit pieces);

}

#endif // !defined BIT_HPP


#ifndef ATTACK_HPP
#define ATTACK_HPP

// includes

//#include "common.hpp"

class Pos;

// types

class Attack_Info {

private :

   Bit p_piece_attacks[Square_Size];

   Bit p_attacks[Side_Size];
   Bit p_support[Side_Size];

   Bit p_le_pieces [Side_Size][Piece_Size_2];
   Bit p_le_attacks[Side_Size][Piece_Size_2];

public :

   void init (const Pos & pos);

   Bit piece_attacks (Square sq) const;

   Bit attacks       (Side sd) const;
   Bit pawn_attacks  (Side sd) const;
   Bit queen_attacks (Side sd) const;
   Bit queen_safe    (Side sd) const;
};

// functions

bool has_attack (const Pos & pos, Side sd, Square to);
bool has_attack (const Pos & pos, Side sd, Square to, Bit pieces);

bool   is_pinned (const Pos & pos, Square king, Square sq, Side sd);
Square pinned_by (const Pos & pos, Square king, Square sq, Side sd);
Bit    pins      (const Pos & pos, Square king);

#endif // !defined ATTACK_HPP


#ifndef PAWN_HPP
#define PAWN_HPP

// includes

//#include "common.hpp"

class Pos;

namespace pawn {

// functions

void init ();

Bit  weak    (const Pos & pos, Side sd);
Bit  strong  (const Pos & pos, Side sd);
Bit  blocked (const Pos & pos, Side sd);

bool is_passed    (const Pos & pos, Square sq, Side sd);
bool is_duo       (const Pos & pos, Square sq, Side sd);
bool is_protected (const Pos & pos, Square sq, Side sd);
bool is_ram       (const Pos & pos, Square sq, Side sd);

bool is_open   (const Pos & pos, Square sq, Side sd);
bool is_strong (const Pos & pos, Square sq, Side sd);

Bit  file   (Square sq);
Bit  rank   (Square sq);

Bit  fronts (Square sq, Side sd);
Bit  rears  (Square sq, Side sd);

}

#endif // !defined PAWN_HPP


#ifndef EVAL_HPP
#define EVAL_HPP

// includes

//#include "common.hpp"

class Pos;

// functions

void clear_pawn_table ();

Score eval (const Pos & pos, Side sd);

Score piece_mat (Piece pc);

#endif // !defined EVAL_HPP





























///////////////////////////////////////////////////////////////////////////

#ifndef POS_HPP
#define POS_HPP

// includes

//#include "bit.hpp"
//#include "common.hpp"
//#include "senp2_eval.hpp"

// types

class Pos { // 200 bytes

private :

   const Pos * p_parent;

   Bit p_piece[Piece_Size];
   Bit p_side[Side_Size];
   Bit p_all;
   Side p_turn;

   Square p_ep_sq;
   Bit p_castling_rooks;
   int p_ply;
   int p_rep;

   int8 p_pc[Square_Size];

   Move p_last_move;
   Square p_cap_sq;
   Key p_key_piece;
   Key p_key_pawn;
   Key p_key_full;

public :

   Pos ();
   Pos (Side turn, Bit piece_side[], Bit castling_rooks);

   Pos  succ (Move mv) const;
   Pos  null ()        const;

   Side turn () const { return p_turn; }

   Bit  empties ()                  const { return ~p_all; }
   Bit  pieces  ()                  const { return p_all; }
   Bit  pieces  (Piece pc)          const { return p_piece[pc]; }
   Bit  pieces  (Side sd)           const { return p_side[sd]; }
   Bit  pieces  (Piece pc, Side sd) const { return p_piece[pc] & p_side[sd]; }

   int  count (Piece pc, Side sd) const { return bit::count(pieces(pc, sd)); }

   Bit    pawns     (Side sd) const { return pieces(Pawn, sd); }
   Bit    sliders   (Side sd) const { return pieces(Bishop, sd) | pieces(Rook, sd) | pieces(Queen, sd); }
   Bit    non_pawns (Side sd) const { return pieces(sd) ^ pawns(sd); }
   Bit    non_king  (Side sd) const { return pieces(sd) ^ pawns(sd) ^ pieces(King, sd); }
   Square king      (Side sd) const { return bit::first(pieces(King, sd)); }

   bool is_empty (Square sq)           const { return p_pc[sq] == Piece_None; }
   bool is_piece (Square sq, Piece pc) const { return p_pc[sq] == pc; }
   bool is_side  (Square sq, Side sd)  const { return bit::has(pieces(sd), sq); }

   Piece piece (Square sq) const { return piece_make(p_pc[sq]); }
   Side  side  (Square sq) const { return side_make(bit::bit(pieces(Black), sq)); }

   Bit    castling_rooks (Side sd) const { return p_castling_rooks & pieces(sd); }
   Bit    castling_rooks_both  ()  const { return p_castling_rooks; }
   Square ep_sq          ()        const { return p_ep_sq; }

   Move   last_move () const { return p_last_move; }
   Square cap_sq    () const { return p_cap_sq; }
   Key    key       () const { return p_key_full; }
   Key    key_pawn  () const { return p_key_pawn; }

   int  ply () const { return p_ply; }

   //bool is_draw () const;

   void clear  ();
   void update ();
   void switch_turn ();
   void add_piece    (Piece pc, Side sd, Square sq);

   void set_castling_rooks(Bit castling_rooks) { p_castling_rooks = castling_rooks; }
   void set_eq_sq(Square sq) { p_ep_sq = sq; }

private :

   Pos  castle (Move mv) const;
   void move_piece   (Piece pc, Side sd, Square from, Square to);
   void remove_piece (Piece pc, Side sd, Square sq);
   bool is_rep () const;
};

namespace pos { // ###

// variables

extern Pos Start;

// functions

void init ();

double phase (const Pos & pos);
int    stage (const Pos & pos);

bool lone_king       (const Pos & pos, Side sd);
bool opposit_bishops (const Pos & pos);

int  force (const Pos & pos, Side sd);

}

namespace move {

Move make (Square from, Square to, Piece prom = Piece_None);
bool is_promotion  (Move mv);
bool is_capture    (Move mv, const Pos & pos);
std::string to_uci   (Move mv, const Pos & pos);

}

// constants

const std::string Start_FEN { "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" };

// functions

Pos pos_from_fen (const std::string & s);
std::string pos_to_fen(const Pos & pos);
void create_pos(Pos & pos, Side turn, Bit piece_side[], Bit castling_rooks, Square ep_sq);
void read_sfen_file(std::string input_filename);
bool in_check(const Pos & pos, Side sd);


namespace hash {

// functions

void init ();

int    index (Key key, int mask);
//uint32 lock  (Key key);

}

#endif // !defined POS_HPP