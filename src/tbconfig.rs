

pub trait TablebaseConfig {
    fn popcount(bb: u64) -> u64;
    fn lsb(bb: u64) -> u32;
    fn poplsb(bb: &mut u64);

    fn pawn_attacks(sq: u32, c: bool) -> u64;
    fn knight_attacks(sq: u32) -> u64;
    fn bishop_attacks(sq: u32, occ: u64) -> u64;
    fn rook_attacks(sq: u32, occ: u64) -> u64;
    fn queen_attacks(sq: u32, occ: u64) -> u64;
    fn king_attacks(sq: u32) -> u64;

    const VALUE_PAWN: i32;
    const VALUE_MATE: i32;
    const VALUE_DRAW: i32;
    const MAX_MATE_PLY: i32;
}

