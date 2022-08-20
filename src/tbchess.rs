/*
 * (c) 2015 basil, all rights reserved,
 * Modifications Copyright (c) 2016-2019 by Jon Dart
 * Modifications Copyright (c) 2020-2020 by Andrew Grant
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

use crate::tbconfig::TablebaseConfig;

const TB_MAX_MOVES: usize = 256;

type Move = u16;

const BLACK: bool = false;
const WHITE: bool = true;

const PAWN: i32 = 1;
const KNIGHT: i32 = 2;
const BISHOP: i32 = 3;
const ROOK: i32 = 4;
const QUEEN: i32 = 5;
const KING: i32 = 6;

const WPAWN: i32 = 1;
const WKNIGHT: i32 = 2;
const WBISHOP: i32 = 3;
const WROOK: i32 = 4;
const WQUEEN: i32 = 5;
const WKING: i32 = 6;

const BPAWN: i32 = 9;
const BKNIGHT: i32 = 10;
const BBISHOP: i32 = 11;
const BROOK: i32 = 12;
const BQUEEN: i32 = 13;
const BKING: i32 = 14;

const PROMOTES_NONE: u32 = 0;
const PROMOTES_QUEEN: u32 = 1;
const PROMOTES_ROOK: u32 = 2;
const PROMOTES_BISHOP: u32 = 3;
const PROMOTES_KNIGHT: u32 = 4;

const PROMOSQS: u64 = 0xFF000000000000FF;
const PRIME_WKING: u64 = 00000000000000000000;
const PRIME_WQUEEN: u64 = 11811845319353239651;
const PRIME_WROOK: u64 = 10979190538029446137;
const PRIME_WBISHOP: u64 = 12311744257139811149;
const PRIME_WKNIGHT: u64 = 15202887380319082783;
const PRIME_WPAWN: u64 = 17008651141875982339;
const PRIME_BKING: u64 = 00000000000000000000;
const PRIME_BQUEEN: u64 = 15484752644942473553;
const PRIME_BROOK: u64 = 18264461213049635989;
const PRIME_BBISHOP: u64 = 15394650811035483107;
const PRIME_BKNIGHT: u64 = 13469005675588064321;
const PRIME_BPAWN: u64 = 11695583624105689831;
const PRIME_NONE: u64 = 00000000000000000000;

#[derive(Clone)]
struct Position {
    white: u64,
    black: u64,
    kings: u64,
    queens: u64,
    rooks: u64,
    bishops: u64,
    knights: u64,
    pawns: u64,
    rule50: u8,
    ep: u8,
    turn: bool,
}

fn move_from(m: Move) -> u16 {
    (m >> 6) & 0x3F
}
fn move_to(m: Move) -> u16 {
    m & 0x3F
}
fn move_promotes(m: Move) -> u16 {
    (m >> 12) & 0x07
}

fn colour_of_piece(piece: u8) -> i32 {
    i32::from((piece >> 3) == 0)
}
fn type_of_piece(piece: u8) -> u8 {
    piece & 0x07
}

fn test_bit(bb: u64, sq: u32) -> bool {
    (bb >> sq) & 1 == 1
}
fn enable_bit(bb: &mut u64, sq: u32) {
    *bb |= 1 << sq
}
fn disable_bit(bb: &mut u64, sq: u32) {
    *bb &= !(1 << sq)
}
fn promo_square(sq: u32) -> bool {
    (PROMOSQS >> sq) & 1 == 1
}
fn pawn_start_square(colour: bool, sq: u32) -> bool {
    (sq >> 3) == (if colour { 1 } else { 6 })
}

static PIECE_TO_CHAR: [u8; 15] = *b" PNBRQK  pnbrqk";

fn pieces_by_type(pos: &Position, colour: bool, piece: i32) -> u64 {
    assert!((PAWN..=KING).contains(&piece));
    assert!(colour == WHITE || colour == BLACK);

    let side = if colour == WHITE {
        pos.white
    } else {
        pos.black
    };

    match piece {
        PAWN => pos.pawns & side,
        KNIGHT => pos.knights & side,
        BISHOP => pos.bishops & side,
        ROOK => pos.rooks & side,
        QUEEN => pos.queens & side,
        KING => pos.kings & side,
        _ => panic!("Invalid piece type"),
    }
}

fn char_to_piece_type(c: u8) -> i32 {
    for i in PAWN..=KING {
        if PIECE_TO_CHAR[i as usize] == c {
            return i;
        }
    }
    0
}

fn calc_key<I: TablebaseConfig>(pos: &Position, mirror: bool) -> u64 {
    let white = if mirror { pos.black } else { pos.white };
    let black = if mirror { pos.white } else { pos.black };

    I::popcount(white & pos.queens).wrapping_mul(PRIME_WQUEEN)
        + I::popcount(white & pos.rooks).wrapping_mul(PRIME_WROOK)
        + I::popcount(white & pos.bishops).wrapping_mul(PRIME_WBISHOP)
        + I::popcount(white & pos.knights).wrapping_mul(PRIME_WKNIGHT)
        + I::popcount(white & pos.pawns).wrapping_mul(PRIME_WPAWN)
        + I::popcount(white & pos.kings).wrapping_mul(PRIME_WKING)
        + I::popcount(black & pos.queens).wrapping_mul(PRIME_BQUEEN)
        + I::popcount(black & pos.rooks).wrapping_mul(PRIME_BROOK)
        + I::popcount(black & pos.bishops).wrapping_mul(PRIME_BBISHOP)
        + I::popcount(black & pos.knights).wrapping_mul(PRIME_BKNIGHT)
        + I::popcount(black & pos.pawns).wrapping_mul(PRIME_BPAWN)
}

fn calc_key_from_pcs(piece_counts: &[u32], mirror: bool) -> u64 {
    let xor = if mirror { 8 } else { 0 };

    (piece_counts[WQUEEN as usize ^ xor] as u64).wrapping_mul(PRIME_WQUEEN)
        + (piece_counts[WROOK as usize ^ xor] as u64).wrapping_mul(PRIME_WROOK)
        + (piece_counts[WBISHOP as usize ^ xor] as u64).wrapping_mul(PRIME_WBISHOP)
        + (piece_counts[WKNIGHT as usize ^ xor] as u64).wrapping_mul(PRIME_WKNIGHT)
        + (piece_counts[WPAWN as usize ^ xor] as u64).wrapping_mul(PRIME_WPAWN)
        + (piece_counts[WKING as usize ^ xor] as u64).wrapping_mul(PRIME_WKING)
        + (piece_counts[BQUEEN as usize ^ xor] as u64).wrapping_mul(PRIME_BQUEEN)
        + (piece_counts[BROOK as usize ^ xor] as u64).wrapping_mul(PRIME_BROOK)
        + (piece_counts[BBISHOP as usize ^ xor] as u64).wrapping_mul(PRIME_BBISHOP)
        + (piece_counts[BKNIGHT as usize ^ xor] as u64).wrapping_mul(PRIME_BKNIGHT)
        + (piece_counts[BPAWN as usize ^ xor] as u64).wrapping_mul(PRIME_BPAWN)
}

fn calc_key_from_pieces(pieces: &[u8]) -> u64 {
    static PRIMES: [u64; 16] = [
        PRIME_NONE,
        PRIME_WPAWN,
        PRIME_WKNIGHT,
        PRIME_WBISHOP,
        PRIME_WROOK,
        PRIME_WQUEEN,
        PRIME_WKING,
        PRIME_NONE,
        PRIME_NONE,
        PRIME_BPAWN,
        PRIME_BKNIGHT,
        PRIME_BBISHOP,
        PRIME_BROOK,
        PRIME_BQUEEN,
        PRIME_BKING,
        PRIME_NONE,
    ];

    let mut key = 0;
    for &piece in pieces {
        key += PRIMES[piece as usize];
    }

    key
}

fn do_bb_move(bb: u64, from: u32, to: u32) -> u64 {
    (((bb >> from) & 0x1) << to) | (bb & (!(1 << from) & !(1 << to)))
}

fn make_move(promote: u32, from: u32, to: u32) -> Move {
    (((promote & 0x7) << 12) | ((from & 0x3F) << 6) | (to & 0x3F)) as Move
}

fn add_move(moves: &mut [Move], mut idx: usize, promotes: bool, from: u32, to: u32) -> usize {
    if promotes {
        moves[idx] = make_move(PROMOTES_NONE, from, to);
        idx += 1;
    } else {
        moves[idx] = make_move(PROMOTES_QUEEN, from, to);
        idx += 1;
        moves[idx] = make_move(PROMOTES_KNIGHT, from, to);
        idx += 1;
        moves[idx] = make_move(PROMOTES_ROOK, from, to);
        idx += 1;
        moves[idx] = make_move(PROMOTES_BISHOP, from, to);
        idx += 1;
    }

    idx
}

fn gen_captures<I: TablebaseConfig>(pos: &Position, moves: &mut [Move], mut idx: usize) -> usize {
    let us = if pos.turn { pos.white } else { pos.black };
    let them = if pos.turn { pos.black } else { pos.white };
    let mut b;
    let mut att;

    // Generate captures for the King
    b = us & pos.kings;
    while b != 0 {
        att = I::king_attacks(I::lsb(b)) & them;
        while att != 0 {
            idx = add_move(moves, idx, false, I::lsb(b), I::lsb(att));
            I::poplsb(&mut att);
        }
        I::poplsb(&mut b);
    }

    // Generate captures for the Rooks & Queens
    b = us & (pos.rooks | pos.queens);
    while b != 0 {
        att = I::rook_attacks(I::lsb(b), us | them) & them;
        while att != 0 {
            idx = add_move(moves, idx, false, I::lsb(b), I::lsb(att));
            I::poplsb(&mut att);
        }
        I::poplsb(&mut b);
    }

    // Generate captures for the Bishops & Queens
    b = us & (pos.bishops | pos.queens);
    while b != 0 {
        att = I::bishop_attacks(I::lsb(b), us | them) & them;
        while att != 0 {
            idx = add_move(moves, idx, false, I::lsb(b), I::lsb(att));
            I::poplsb(&mut att);
        }
        I::poplsb(&mut b);
    }

    // Generate captures for the Knights
    b = us & pos.knights;
    while b != 0 {
        att = I::knight_attacks(I::lsb(b)) & them;
        while att != 0 {
            idx = add_move(moves, idx, false, I::lsb(b), I::lsb(att));
            I::poplsb(&mut att);
        }
        I::poplsb(&mut b);
    }

    // Generate captures for the Pawns
    b = us & pos.pawns;
    while b != 0 {
        // Generate Enpassant Captures
        if pos.ep != 0 && test_bit(I::pawn_attacks(I::lsb(b), pos.turn), pos.ep.into()) {
            idx = add_move(moves, idx, false, I::lsb(b), pos.ep.into());
        }

        // Generate non-Enpassant Captures
        att = I::pawn_attacks(I::lsb(b), pos.turn) & them;
        while att != 0 {
            idx = add_move(
                moves,
                idx,
                promo_square(I::lsb(att)),
                I::lsb(b),
                I::lsb(att),
            );
            I::poplsb(&mut att);
        }

        I::poplsb(&mut b);
    }

    idx
}

fn gen_moves<I: TablebaseConfig>(pos: &Position, moves: &mut [Move], mut idx: usize) -> usize {
    let forward: i32 = if pos.turn { 8 } else { -8 };

    let us = if pos.turn { pos.white } else { pos.black };
    let them = if pos.turn { pos.black } else { pos.white };
    let mut b;
    let mut att;

    // Generate moves for the King
    b = us & pos.kings;
    while b != 0 {
        att = I::king_attacks(I::lsb(b)) & !us;
        while att != 0 {
            idx = add_move(moves, idx, false, I::lsb(b), I::lsb(att));
            I::poplsb(&mut att);
        }
        I::poplsb(&mut b);
    }

    // Generate moves for the Rooks & Queens
    b = us & (pos.rooks | pos.queens);
    while b != 0 {
        att = I::rook_attacks(I::lsb(b), us | them) & !us;
        while att != 0 {
            idx = add_move(moves, idx, false, I::lsb(b), I::lsb(att));
            I::poplsb(&mut att);
        }
        I::poplsb(&mut b);
    }

    // Generate moves for the Bishops & Queens
    b = us & (pos.bishops | pos.queens);
    while b != 0 {
        att = I::bishop_attacks(I::lsb(b), us | them) & !us;
        while att != 0 {
            idx = add_move(moves, idx, false, I::lsb(b), I::lsb(att));
            I::poplsb(&mut att);
        }
        I::poplsb(&mut b);
    }

    // Generate moves for the Knights
    b = us & pos.knights;
    while b != 0 {
        att = I::knight_attacks(I::lsb(b)) & !us;
        while att != 0 {
            idx = add_move(moves, idx, false, I::lsb(b), I::lsb(att));
            I::poplsb(&mut att);
        }
        I::poplsb(&mut b);
    }

    // Generate moves for the Pawns
    b = us & pos.pawns;
    while b != 0 {
        let from = I::lsb(b);
        let forward_one = (from as i32 + forward) as u32;
        let forward_two = (from as i32 + 2 * forward) as u32;

        // Generate Enpassant Captures
        if pos.ep != 0 && test_bit(I::pawn_attacks(from, pos.turn), pos.ep.into()) {
            idx = add_move(moves, idx, false, from, pos.ep.into());
        }

        // Generate any single pawn pushes
        if !test_bit(us | them, forward_one) {
            idx = add_move(moves, idx, false, from, forward_one);
        }

        // Generate any double pawn pushes
        if pawn_start_square(pos.turn, from)
            && !test_bit(us | them, forward_one)
            && !test_bit(us | them, forward_two)
        {
            idx = add_move(moves, idx, false, from, forward_two);
        }

        // Generate non-Enpassant Captures
        att = I::pawn_attacks(from, pos.turn) & them;
        while att != 0 {
            idx = add_move(moves, idx, promo_square(I::lsb(att)), from, I::lsb(att));
            I::poplsb(&mut att);
        }

        I::poplsb(&mut b);
    }

    idx
}

fn gen_legal<I: TablebaseConfig>(pos: &Position, moves: &mut [Move]) -> usize {
    let mut _moves = [0; TB_MAX_MOVES];
    let end = gen_moves::<I>(pos, &mut _moves, 0);
    let mut results = 0;

    for &m in &_moves[0..end] {
        if legal_move::<I>(pos, m) {
            moves[results] = m;
            results += 1;
        }
    }

    results
}

fn is_pawn_move(pos: &Position, m: Move) -> bool {
    let us = if pos.turn { pos.white } else { pos.black };
    test_bit(us & pos.pawns, move_from(m).into())
}

fn is_en_passant(pos: &Position, m: Move) -> bool {
    is_pawn_move(pos, m) && move_to(m) == pos.ep.into() && pos.ep != 0
}

fn is_capture(pos: &Position, m: Move) -> bool {
    let them = if pos.turn { pos.black } else { pos.white };
    test_bit(them, move_to(m).into()) || is_en_passant(pos, m)
}

fn is_legal<I: TablebaseConfig>(pos: &Position) -> bool {
    let us = if pos.turn { pos.white } else { pos.black };
    let them = if pos.turn { pos.black } else { pos.white };
    let sq = I::lsb(pos.kings & us);

    0 == (I::king_attacks(sq) & pos.kings & them)
        && 0 == (I::rook_attacks(sq, us | them) & (pos.rooks | pos.queens) & them)
        && 0 == (I::bishop_attacks(sq, us | them) & (pos.bishops | pos.queens) & them)
        && 0 == (I::knight_attacks(sq) & pos.knights & them)
        && 0 == (I::pawn_attacks(sq, !pos.turn) & pos.pawns & them)
}

fn is_check<I: TablebaseConfig>(pos: &Position) -> bool {
    let us = if pos.turn { pos.white } else { pos.black };
    let them = if pos.turn { pos.black } else { pos.white };
    let sq = I::lsb(pos.kings & us);

    0 != (I::rook_attacks(sq, us | them) & ((pos.rooks | pos.queens) & them))
        || 0 != (I::bishop_attacks(sq, us | them) & ((pos.bishops | pos.queens) & them))
        || 0 != (I::knight_attacks(sq) & (pos.knights & them))
        || 0 != (I::pawn_attacks(sq, pos.turn) & (pos.pawns & them))
}

fn is_mate<I: TablebaseConfig>(pos: &Position) -> bool {
    if !is_check::<I>(pos) {
        return false;
    }

    let mut scratch = pos.clone();
    let mut moves = [0; TB_MAX_MOVES];
    let end = gen_moves::<I>(pos, &mut moves, 0);

    for &m in &moves[..end] {
        if do_move::<I>(&mut scratch, pos, m) {
            return false;
        }
    }
    true
}

fn do_move<I: TablebaseConfig>(scratch: &mut Position, pos: &Position, m: Move) -> bool {
    let from = move_from(m).into();
    let to = move_to(m).into();
    let promotes = move_promotes(m).into();

    scratch.turn = !pos.turn;
    scratch.white = do_bb_move(pos.white, from, to);
    scratch.black = do_bb_move(pos.black, from, to);
    scratch.kings = do_bb_move(pos.kings, from, to);
    scratch.queens = do_bb_move(pos.queens, from, to);
    scratch.rooks = do_bb_move(pos.rooks, from, to);
    scratch.bishops = do_bb_move(pos.bishops, from, to);
    scratch.knights = do_bb_move(pos.knights, from, to);
    scratch.pawns = do_bb_move(pos.pawns, from, to);
    scratch.ep = 0;

    // Promotions reset the Fifty-Move Rule and add a piece
    if promotes != PROMOTES_NONE {
        disable_bit(&mut scratch.pawns, to);
        match promotes {
            PROMOTES_QUEEN => enable_bit(&mut scratch.queens, to),
            PROMOTES_ROOK => enable_bit(&mut scratch.rooks, to),
            PROMOTES_BISHOP => enable_bit(&mut scratch.bishops, to),
            PROMOTES_KNIGHT => enable_bit(&mut scratch.knights, to),
            _ => {}
        }
        scratch.rule50 = 0;
    }
    // Pawn moves can be Enpassant, or allow a future Enpassant
    else if test_bit(pos.pawns, from) {
        scratch.rule50 = 0;

        // Check for a double push by White
        if from ^ to == 16
            && pos.turn == WHITE
            && (I::pawn_attacks(from + 8, WHITE) & pos.pawns & pos.black) != 0
        {
            scratch.ep = (from + 8) as u8;
        }

        // Check for a double push by Black
        if from ^ to == 16
            && pos.turn == BLACK
            && (I::pawn_attacks(from - 8, BLACK) & pos.pawns & pos.white) != 0
        {
            scratch.ep = (from - 8) as u8;
        }
        // Check for an Enpassant being played
        else if to == pos.ep.into() {
            let sq = if pos.turn { to - 8 } else { to + 8 };
            disable_bit(&mut scratch.white, sq);
            disable_bit(&mut scratch.black, sq);
            disable_bit(&mut scratch.pawns, sq);
        }
    }
    // Any other sort of capture also resets the Fifty-Move Rule
    else if test_bit(pos.white | pos.black, to) {
        scratch.rule50 = 0;
    } else {
        scratch.rule50 = pos.rule50 + 1;
    }

    // Provide the caller with information about legality
    is_legal::<I>(pos)
}

fn legal_move<I: TablebaseConfig>(pos: &Position, m: Move) -> bool {
    let mut scratch = pos.clone();
    do_move::<I>(&mut scratch, pos, m)
}
