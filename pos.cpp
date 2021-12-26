
// includes

#include <cstdlib>
#include <iostream>
#include <sstream>
#include <string>
#include <utility>

//#include "attack.hpp"
//#include "bit.hpp"
//#include "common.hpp"
//#include "pos.hpp"
#include "senp2_eval.hpp"

// constants

const std::string Piece_Char { "PNBRQK" };

char file_to_char (File fl);
char rank_to_char (Rank rk);

File file_from_char (char c);
Rank rank_from_char (char c);

std::string square_to_string   (Square sq);
Square      square_from_string (const std::string & s);

char  piece_to_char   (Piece pc);
Piece piece_from_char (char c);

std::string side_to_string (Side sd);

class Bad_Input : public std::exception {
};

// functions

int find (char c, const std::string & s);

/////////////////////////////

namespace move {

// functions

const Move None { Move(-1) };
const Move Null { Move( 0) };

Square from (Move mv);
Square to   (Move mv);
Piece  prom (Move mv);

bool is_promotion      (Move mv);
bool is_underpromotion (Move mv);
bool is_castling       (Move mv);
bool is_en_passant     (Move mv);

bool is_tactical   (Move mv, const Pos & pos);
bool is_capture    (Move mv, const Pos & pos);
bool is_conversion (Move mv, const Pos & pos);

Square castling_king_to (Move mv);
Square castling_rook_to (Move mv);

Piece piece   (Move mv, const Pos & pos);
Piece capture (Move mv, const Pos & pos);
Side  side    (Move mv, const Pos & pos);


Move make(Square from, Square to, Piece prom) {
   return Move((prom << 12) | (from << 6) | (to << 0));
}

Square from(Move mv) {
   assert(mv != None);
   assert(mv != Null);
   return Square((int(mv) >> 6) & 077);
}

Square to(Move mv) {
   assert(mv != None);
   assert(mv != Null);
   return Square((int(mv) >> 0) & 077);
}

Piece prom(Move mv) {
   assert(mv != None);
   assert(mv != Null);
   return Piece((int(mv) >> 12) & 7);
}

bool is_promotion(Move mv) {
   Piece prom = move::prom(mv);
   return prom >= Knight && prom <= Queen;
}

bool is_underpromotion(Move mv) {
   Piece prom = move::prom(mv);
   return prom >= Knight && prom <= Rook;
}

bool is_castling(Move mv) {
   return prom(mv) == King;
}

bool is_en_passant(Move mv) {
   return prom(mv) == Pawn;
}

bool is_tactical(Move mv, const Pos & pos) {
   return is_capture(mv, pos) || is_promotion(mv);
}

bool is_capture(Move mv, const Pos & pos) {
   return (!pos.is_empty(to(mv)) && !is_castling(mv))
       || is_en_passant(mv);
}
/*
bool is_recapture(Move mv, const Pos & pos) {
   return to(mv) == pos.cap_sq() && move_is_win(mv, pos);
}
*/
bool is_conversion(Move mv, const Pos & pos) {
   return pos.is_piece(from(mv), Pawn) || is_capture(mv, pos);
}

Square castling_king_to(Move mv) {

   assert(is_castling(mv));

   Square from = move::from(mv);
   Square to   = move::to(mv);

   return (square_file(to) > square_file(from))
        ? square_make(File_G, square_rank(to))
        : square_make(File_C, square_rank(to));
}

Square castling_rook_to(Move mv) {

   assert(is_castling(mv));

   Square from = move::from(mv);
   Square to   = move::to(mv);

   return (square_file(to) > square_file(from))
        ? square_make(File_F, square_rank(to))
        : square_make(File_D, square_rank(to));
}

Piece piece(Move mv, const Pos & pos) {
   assert(mv != None);
   assert(mv != Null);
   return pos.piece(from(mv));
}

Piece capture(Move mv, const Pos & pos) {

   assert(mv != None);
   assert(mv != Null);

   if (is_castling(mv))   return Piece_None;
   if (is_en_passant(mv)) return Pawn;

   return pos.piece(to(mv));
}

Side side(Move mv, const Pos & pos) {
   assert(mv != None);
   assert(mv != Null);
   return pos.side(from(mv));
}

Move_Index index(Move mv, const Pos & pos) {

   assert(mv != None);
   assert(mv != Null);

   Piece pc = piece(mv, pos);
   Side  sd = pos.turn();

   return Move_Index((sd << 9) | (pc << 6) | (to(mv) << 0));
}

Move_Index index_last_move(const Pos & pos) {

   Move mv = pos.last_move();
   if (mv == move::None || mv == move::Null) return Move_Index_None;

   Piece pc = move::is_castling(mv) ? King : pos.piece(to(mv));
   Side  sd = side_opp(pos.turn());

   return Move_Index((sd << 9) | (pc << 6) | (to(mv) << 0));
}

bool pseudo_is_legal(Move mv, const Pos & pos) {

   assert(mv != None);
   assert(mv != Null);

   Square from = move::from(mv);
   Square to   = move::to(mv);

   Piece pc = pos.piece(from);
   Side  sd = pos.side(from);

   Side xd = side_opp(sd);

   Square king = pos.king(sd);

   Bit pieces = pos.pieces();

   if (is_castling(mv)) {
      bit::clear(pieces, to); // remove rook
      to = castling_king_to(mv);
   }

   bit::clear(pieces, from);
   bit::set(pieces, to);

   // king move?

   if (pc == King) return !has_attack(pos, xd, to, pieces);

   // pinned piece?

   Bit beyond = bit::beyond(king, from);

   if (is_en_passant(mv)) {

      Square sq = square_rear(to, sd);

      bit::clear(pieces, sq);
      beyond |= bit::beyond(king, sq);
   }

   for (Bit b = pos.sliders(xd) & beyond; b != 0; b = bit::rest(b)) {

      Square ds = bit::first(b);
      Piece  dp = pos.piece(ds);

      if (bit::piece_attack(dp, xd, ds, king, pieces) && ds != to) return false;
   }

   return true;
}

bool is_check(Move mv, const Pos & pos) {

   assert(mv != None);
   assert(mv != Null);

   Square from = move::from(mv);
   Square to   = move::to(mv);

   Bit pieces = pos.pieces();

   if (is_castling(mv)) {

      bit::clear(pieces, from); // remove king

      from = to;
      to = castling_rook_to(mv);
   }

   Piece pc = pos.piece(from);
   Side  sd = pos.side(from);

   Square king = pos.king(side_opp(sd));

   bit::clear(pieces, from);
   bit::set(pieces, to);

   // direct check?

   if (is_promotion(mv)) pc = prom(mv);
   if (bit::piece_attack(pc, sd, to, king, pieces)) return true;

   // discovered check?

   Bit beyond = bit::beyond(king, from);

   if (is_en_passant(mv)) {

      Square sq = square_rear(to, sd);

      bit::clear(pieces, sq);
      beyond |= bit::beyond(king, sq);
   }

   for (Bit b = pos.sliders(sd) & beyond; b != 0; b = bit::rest(b)) {

      Square ds = bit::first(b);
      Piece  dp = pos.piece(ds);

      if (bit::piece_attack(dp, sd, ds, king, pieces)) return true;
   }

   return false;
}

} // namespace move


namespace hash {

Key  key       (const Pos & pos);
Key  key_piece (const Pos & pos);
Key  key_pawn  (const Pos & pos);
Key  key_full  (const Pos & pos);

Key  key_turn       ();
Key  key_turn       (Side sd);
Key  key_piece      (Piece pc, Side sd, Square sq);
Key  key_castling   (Side sd, Bit rooks);
Key  key_en_passant (File fl);

} // namespace hash


// functions

Pos::Pos() {
}

Pos::Pos(Side turn, Bit piece_side[], Bit castling_rooks) {

   clear();

   // set up position

   for (int p = 0; p < Piece_Side_Size; p++) {

      Piece_Side ps = piece_side_make(p);

      Piece pc = piece_side_piece(ps);
      Side  sd = piece_side_side(ps);

      for (Bit b = piece_side[ps]; b != 0; b = bit::rest(b)) {
         Square sq = bit::first(b);
         add_piece(pc, sd, sq);
      }
   }

   p_castling_rooks = castling_rooks;

   if (turn != p_turn) switch_turn();

   update();
}

void create_pos(Pos & pos, Side turn, Bit piece_side[], Bit castling_rooks, Square ep_sq) {

   pos.clear();

   // set up position

   for (int p = 0; p < Piece_Side_Size; p++) {

      Piece_Side ps = piece_side_make(p);

      Piece pc = piece_side_piece(ps);
      Side  sd = piece_side_side(ps);

      for (Bit b = piece_side[ps]; b != 0; b = bit::rest(b)) {
         Square sq = bit::first(b);
         pos.add_piece(pc, sd, sq);
      }
   }

   //p_castling_rooks = castling_rooks;
   pos.set_castling_rooks(castling_rooks);

   if (turn != pos.turn()) {
      pos.switch_turn();
   }

   pos.set_eq_sq(ep_sq);

   pos.update();
}

void Pos::update() {

   p_all = p_side[White] | p_side[Black];

   p_key_full = p_key_piece;

   p_key_full ^= hash::key_castling(White, castling_rooks(White));
   p_key_full ^= hash::key_castling(Black, castling_rooks(Black));

   if (p_ep_sq != Square_None) p_key_full ^= hash::key_en_passant(square_file(p_ep_sq));
}

void Pos::clear() {

   p_parent = nullptr;

   for (int pc = 0; pc < Piece_Size; pc++) {
      p_piece[pc] = Bit(0);
   }

   for (int sd = 0; sd < Side_Size; sd++) {
      p_side[sd] = Bit(0);
   }

   p_all = Bit(0);

   p_turn = White;

   p_castling_rooks = Bit(0);
   p_ep_sq = Square_None;
   p_ply = 0;
   p_rep = 0;

   for (int sq = 0; sq < Square_Size; sq++) {
      p_pc[sq] = Piece_None;
   }

   p_last_move = move::None;
   p_cap_sq = Square_None;
   p_key_piece = hash::key_turn(p_turn);
   p_key_pawn = Key(0);
}

Pos Pos::succ(Move mv) const {

   if (mv == move::Null) return null(); // moves in a PV can be "null"
   if (move::is_castling(mv)) return castle(mv);

   Square from = move::from(mv);
   Square to   = move::to(mv);

   Side sd = p_turn;
   Side xd = side_opp(sd);

   assert( is_side(from, sd));
   assert(!is_side(to,   sd));

   Pos pos = *this;

   pos.p_parent = this;

   pos.p_ep_sq = Square_None;
   pos.p_ply = move::is_conversion(mv, *this) ? 0 : p_ply + 1;
   pos.p_rep = pos.p_ply;

   pos.p_last_move = mv;
   pos.p_cap_sq = Square_None;

   Piece pc = piece_make(p_pc[from]);
   Piece cp = Piece(p_pc[to]); // can be Piece_None

   if (cp != Piece_None) { // capture

      assert(cp != King);

      pos.remove_piece(cp, xd, to);
      pos.p_cap_sq = to;

   } else if (move::is_en_passant(mv)) {

      pos.remove_piece(Pawn, xd, square_rear(to, sd));
      pos.p_cap_sq = to;
   }

   if (move::is_promotion(mv)) {

      pos.remove_piece(pc, sd, from);
      pos.add_piece(move::prom(mv), sd, to);

      pos.p_cap_sq = to;

   } else {

      pos.move_piece(pc, sd, from, to);
   }

   // special moves

   if (pc == Pawn
    && square_rank(from, sd) == Rank_2
    && square_rank(to,   sd) == Rank_4) {

      Square sq = square_make((from + to) / 2);
      if ((pos.pawns(xd) & bit::pawn_attacks_to(xd, sq)) != 0) pos.p_ep_sq = sq;

   } else if (pc == King) {

      pos.p_castling_rooks &= ~bit::rank(Rank_1, sd);
   }

   pos.switch_turn();

   pos.update();
   return pos;
}

Pos Pos::castle(Move mv) const {

   assert(move::is_castling(mv));

   Side sd = p_turn;

   Square kf = move::from(mv);
   Square rf = move::to(mv);

   Square kt, rt;

   Rank rk = rank_side(Rank_1, sd);

   if (square_file(rf) > square_file(kf)) {
      kt = square_make(File_G, rk);
      rt = square_make(File_F, rk);
   } else {
      kt = square_make(File_C, rk);
      rt = square_make(File_D, rk);
   }

   Pos pos = *this;

   pos.p_parent = this;

   pos.p_ep_sq = Square_None;
   pos.p_ply = move::is_conversion(mv, *this) ? 0 : p_ply + 1;
   pos.p_rep = pos.p_ply;

   pos.p_last_move = mv;
   pos.p_cap_sq = Square_None;

   pos.remove_piece(Rook, sd, rf);
   pos.move_piece  (King, sd, kf, kt);
   pos.add_piece   (Rook, sd, rt);

   pos.p_castling_rooks &= ~bit::rank(Rank_1, sd);

   pos.switch_turn();

   pos.update();
   return pos;
}

Pos Pos::null() const {

   Pos pos = *this;

   pos.p_parent = this;

   pos.switch_turn();

   pos.p_ep_sq = Square_None;
   pos.p_ply = p_ply + 1;
   pos.p_rep = 0; // don't detect repetition across a null move

   pos.p_last_move = move::Null;
   pos.p_cap_sq = Square_None;

   pos.update();
   return pos;
}

void Pos::switch_turn() {
   p_turn = side_opp(p_turn);
   p_key_piece ^= hash::key_turn();
}

void Pos::move_piece(Piece pc, Side sd, Square from, Square to) {
   remove_piece(pc, sd, from);
   add_piece(pc, sd, to);
}

void Pos::add_piece(Piece pc, Side sd, Square sq) {

   assert(pc != Piece_None);

   assert(!bit::has(p_piece[pc], sq));
   assert(!bit::has(p_side[sd], sq));

   bit::flip(p_piece[pc], sq);
   bit::flip(p_side[sd], sq);

   assert(p_pc[sq] == Piece_None);
   p_pc[sq] = pc;

   p_key_piece ^= hash::key_piece(pc, sd, sq);
   if (pc == Pawn) p_key_pawn ^= hash::key_piece(pc, sd, sq);
}

void Pos::remove_piece(Piece pc, Side sd, Square sq) {

   assert(pc != Piece_None);

   assert(bit::has(p_piece[pc], sq));
   assert(bit::has(p_side[sd], sq));

   bit::flip(p_piece[pc], sq);
   bit::flip(p_side[sd], sq);

   bit::clear(p_castling_rooks, sq); // moved or captured

   assert(p_pc[sq] == pc);
   p_pc[sq] = Piece_None;

   p_key_piece ^= hash::key_piece(pc, sd, sq);
   if (pc == Pawn) p_key_pawn ^= hash::key_piece(pc, sd, sq);
}
/*
bool Pos::is_draw() const {

   if (p_ply >= 100) {
      return !is_mate(*this);
   } else if (p_rep >= 4) {
      return is_rep();
   } else {
      return false;
   }
}
*/
bool Pos::is_rep() const {

   const Pos * pos = this;

   for (int i = 0; i < p_rep / 2; i++) {
      pos = pos->p_parent->p_parent;
      if (pos->key() == key()) return true;
   }

   return false;
}

namespace pos { // ###

// variables

Pos Start;

// functions

void init() {
   Start = pos_from_fen(Start_FEN);
}

double phase(const Pos & pos) {

   double phase = double(stage(pos)) / double(Stage_Size);

   assert(phase >= 0.0 && phase <= 1.0);
   return phase;
}

int stage(const Pos & pos) {

   int stage = 24 - (force(pos, White) + force(pos, Black));
   if (stage < 0) stage = 0;

   assert(stage >= 0 && stage <= Stage_Size);
   return stage;
}

bool lone_king(const Pos & pos, Side sd) {
   return pos.non_pawns(sd) == pos.pieces(King, sd);
}

bool opposit_bishops(const Pos & pos) {

   Bit white = pos.non_king(White);
   Bit black = pos.non_king(Black);

   return white == pos.pieces(Bishop, White)
       && black == pos.pieces(Bishop, Black)
       && bit::is_single(white)
       && bit::is_single(black)
       && square_colour(bit::first(white)) != square_colour(bit::first(black));
}

int force(const Pos & pos, Side sd) {

   return pos.count(Knight, sd) * 1
        + pos.count(Bishop, sd) * 1
        + pos.count(Rook,   sd) * 2
        + pos.count(Queen,  sd) * 4;
}

}


////////////////////////////////////////////////////////////////////////////////

// constants

const std::string Piece_Side_Char { "PpNnBbRrQqKk" };
const std::string Side_Char       { "wb" };

// prototypes

static Square fen_square (int sq);

// functions

Pos pos_from_fen(const std::string & s) {

   int i = 0;

   // pieces

   if (s[i] == ' ') i++; // HACK to help parsing

   Bit piece_side[Piece_Side_Size];

   for (int ps = 0; ps < Piece_Side_Size; ps++) {
      piece_side[ps] = Bit(0);
   }

   int sq = 0;
   int run = 0;

   while (true) {

      char c = s[i++];
      if (c == '\0' || c == ' ') break;

      if (c == '/') {

         sq += run;
         run = 0;

         if (sq >= Square_Size) throw Bad_Input();

      } else if (std::isdigit(c)) { // run of empty squares

         run = run * 10 + (c - '0');

      } else { // piece

         sq += run;
         run = 0;

         if (sq >= Square_Size) throw Bad_Input();

         Piece_Side ps = Piece_Side(find(c, Piece_Side_Char));
         bit::set(piece_side[ps], fen_square(sq));
         sq += 1;
      }
   }

   if (sq + run != Square_Size) throw Bad_Input();

   // turn

   if (s[i] == ' ') i++;

   Side turn = White;
   if (s[i] != '\0') turn = side_make(find(s[i++], Side_Char));

   // castling rights

   if (s[i] == ' ') i++;

   Bit castling_rooks = Bit(0);

   while (s[i] != '\0' && s[i] != ' ') {

      char c = s[i++];
      if (c == '-') continue;

      Side sd;

      if (std::isupper(c)) {

         sd = White;

         if (c == 'K') c = 'H';
         if (c == 'Q') c = 'A';

      } else {

         sd = Black;

         if (c == 'k') c = 'h';
         if (c == 'q') c = 'a';
      }

      bit::set(castling_rooks, square_make(file_from_char(std::tolower(c)), rank_side(Rank_1, sd)));
   }

   // wrap up

   return Pos(turn, piece_side, castling_rooks);
}

static Square fen_square(int sq) {
   int fl = sq % 8;
   int rk = sq / 8;
   return square_make(fl, 7 - rk);
}


std::string pos_to_fen(const Pos & pos) {

   const Square H1 = square_make(File_H, Rank_1);
   const Square A1 = square_make(File_A, Rank_1);
   const Square H8 = square_make(File_H, Rank_8);
   const Square A8 = square_make(File_A, Rank_8);

   std::stringstream ss("");

   for (Rank r = Rank_8; r >= Rank_1; r = r - 1) {
      int lastEmpty = 0;
      for (File f = File_A; f <= File_H; f = f + 1) {
         Square sq = square_make(f, r);
         if (pos.is_empty(sq)) {
            lastEmpty++;
         } else {
            if (lastEmpty > 0) {
               ss << lastEmpty;
               lastEmpty = 0;
            }
            Piece pc_type = pos.piece(sq);
            Side color = pos.side(sq);
            Piece_Side ps = piece_side_make(pc_type, color);
            ss << Piece_Side_Char[ps];
         }
      }

      if (lastEmpty > 0) {
         ss << lastEmpty;
      }

      if (r > Rank_1) {
         ss << "/";
      }
   }

   ss << " ";

   ss << Side_Char[pos.turn()];

   ss << " ";

   Bit crks = pos.castling_rooks_both();
   if (crks == 0) {
      ss << "-";
   } else {
      if (crks & bit::bit(H1)) ss << "K";
      if (crks & bit::bit(A1)) ss << "Q";
      if (crks & bit::bit(H8)) ss << "k";
      if (crks & bit::bit(A8)) ss << "q";
   }

   ss << " ";

   if (pos.ep_sq() == Square_None) {
      ss << "-";
   } else {
      ss << square_to_string(pos.ep_sq());
   }

   ss << " ";

   ss << "0 1";

   return ss.str();
}


////////////////////////////////////////////////////////////////////////////////

namespace hash {

// variables

static Key Key_Turn;
static Key Key_Piece[Side_Size][Piece_Size_2][Square_Size];
static Key Key_Castling[Side_Size][File_Size];
static Key Key_En_Passant[File_Size];

// functions

void init() {

   // hash keys

   Key_Turn = Key(ml::rand_int_64());

   for (int pc = 0; pc < Piece_Size; pc++) {
      for (int sd = 0; sd < Side_Size; sd++) {
         for (int sq = 0; sq < Square_Size; sq++) {
            Key_Piece[sd][pc][sq] = Key(ml::rand_int_64());
         }
      }
   }

   for (int fl = 0; fl < File_Size; fl++) {

      Key_Castling[White][fl] = Key(ml::rand_int_64());
      Key_Castling[Black][fl] = Key(ml::rand_int_64());

      Key_En_Passant[fl] = Key(ml::rand_int_64());
   }
}

Key key(const Pos & pos) {
   return key_full(pos);
}

Key key_piece(const Pos & pos) {

   Key key = Key(0);

   // pieces

   for (int s = 0; s < Side_Size; s++) {

      Side sd = side_make(s);

      for (Bit b = pos.pieces(sd); b != 0; b = bit::rest(b)) {

         Square sq = bit::first(b);
         Piece  pc = pos.piece(sq);

         key ^= key_piece(pc, sd, sq);
      }
   }

   // turn

   key ^= key_turn(pos.turn());

   return key;
}

Key key_pawn(const Pos & pos) {

   Key key = Key(0);

   // pawns

   for (int s = 0; s < Side_Size; s++) {

      Side sd = side_make(s);

      for (Bit b = pos.pawns(sd); b != 0; b = bit::rest(b)) {

         Square sq = bit::first(b);
         Piece  pc = pos.piece(sq);

         key ^= key_piece(pc, sd, sq);
      }
   }

   return key;
}

Key key_full(const Pos & pos) {

   Key key = key_piece(pos);

   // castling

   key ^= hash::key_castling(White, pos.castling_rooks(White));
   key ^= hash::key_castling(Black, pos.castling_rooks(Black));

   // en passant

   if (pos.ep_sq() != Square_None) key ^= hash::key_en_passant(square_file(pos.ep_sq()));

   return key;
}

Key key_turn() {
   return Key_Turn;
}

Key key_turn(Side sd) {
   return (sd == White) ? Key(0) : Key_Turn;
}

Key key_piece(Piece pc, Side sd, Square sq) {
   assert(pc != Piece_None);
   return Key_Piece[sd][pc][sq];
}

Key key_castling(Side sd, Bit rooks) {

   Key key = Key(0);

   for (Bit b = rooks; b != 0; b = bit::rest(b)) {
      Square sq = bit::first(b);
      key ^= Key_Castling[sd][square_file(sq)];
   }

   return key;
}

Key key_en_passant(File fl) {
   return Key_En_Passant[fl];
}

int index(Key key, int mask) {
   return int(key) & mask;
}

uint32 lock(Key key) {
   return uint32(uint64(key) >> 32);
}

}

/////////////////////////

Square square_from_string(const std::string & s) {

   if (s.size() != 2) throw Bad_Input();

   File fl = file_from_char(s[0]);
   Rank rk = rank_from_char(s[1]);

   return square_make(fl, rk);
}

std::string square_to_string(Square sq) {

   std::string s;
   s += file_to_char(square_file(sq));
   s += rank_to_char(square_rank(sq));

   return s;
}

char file_to_char(File fl) {
   return 'a' + fl;
}

char rank_to_char(Rank rk) {
   return '1' + rk;
}

File file_from_char(char c) {

   int fl = c - 'a';
   if (!file_is_ok(fl)) throw Bad_Input();

   return File(fl);
}

Rank rank_from_char(char c) {

   int rk = c - '1';
   if (!rank_is_ok(rk)) throw Bad_Input();

   return Rank(rk);
}

std::string side_to_string(Side sd) {
   return (sd == White) ? "white" : "black";
}

Piece piece_from_char(char c) {
   return piece_make(find(c, Piece_Char));
}

char piece_to_char(Piece pc) {
   assert(pc != Piece_None);
   return Piece_Char[pc];
}

///// utils.cpp ////////////////////////

int find(char c, const std::string & s) {

   auto i = s.find(c);
   if (i == std::string::npos) throw Bad_Input();

   return int(i);
}