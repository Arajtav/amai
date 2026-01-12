#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Value(u64);

#[allow(unused)]
impl Value {
    pub const BASE: u64     = 0x7FF8000000000000;
    const TAG_MASK: u64     = 0x000F000000000000;
    const PAYLOAD_MASK: u64 = 0x0000FFFFFFFFFFFF;

    const INT_T: u64        = 0x0001000000000000;
    const BOOL_T: u64       = 0x0002000000000000;
    const NIL_T: u64        = 0x0003000000000000;
    const FLOAT_T: u64      = 0x0004000000000000;

    #[inline(always)]
    pub fn from_raw(tag: u64, payload: u64) -> Self {
        Self(Self::BASE | tag | payload)
    }

    #[inline(always)]
    pub fn from_int(x: i64) -> Self {
        let payload = (x as u64) & Self::PAYLOAD_MASK;
        Self::from_raw(Self::INT_T, payload)
    }
    #[inline(always)]
    pub fn to_int(&self) -> i64 {
        let payload = self.0 & Self::PAYLOAD_MASK;
        if payload & 0x0000800000000000 != 0 {
            (payload | 0xFFFF000000000000) as i64
        } else {
            payload as i64
        }
    }
    #[inline(always)]
    pub fn is_int(&self) -> bool {
        (self.0 & Self::TAG_MASK) == (Self::BASE & Self::TAG_MASK) | Self::INT_T
    }

    #[inline(always)]
    pub fn from_bool(x: bool) -> Self {
        let payload = x as u64;
        Self::from_raw(Self::BOOL_T, payload)
    }
    #[inline(always)]
    pub fn to_bool(&self) -> bool {
        assert!(self.is_bool());
        let payload = self.0 & Self::PAYLOAD_MASK;
        payload != 0
    }
    #[inline(always)]
    pub fn is_bool(&self) -> bool {
        (self.0 & Self::TAG_MASK) == Self::BOOL_T
    }

    #[inline(always)]
    pub fn nil() -> Self {
        Self::from_raw(Self::NIL_T, 0)
    }
    #[inline(always)]
    pub fn is_nil(&self) -> bool {
        (self.0 & Self::TAG_MASK) == Self::NIL_T
    }

    #[inline(always)]
    pub fn from_float(x: f64) -> Value {
        if x.is_nan() {
            Value(x.to_bits() | Self::BASE | Self::FLOAT_T)
        } else {
            Value(x.to_bits())
        }
    }
    #[inline(always)]
    pub fn to_float(&self) -> f64 {
        f64::from_bits(self.0)
    }

    #[inline(always)]
    pub fn iadd(&self, other: Self) -> Self {
        Self::from_int(self.to_int() + other.to_int())
    }
    #[inline(always)]
    pub fn isub(&self, other: Self) -> Self {
        Self::from_int(self.to_int() - other.to_int())
    }
    #[inline(always)]
    pub fn imul(&self, other: Self) -> Self {
        Self::from_int(self.to_int() * other.to_int())
    }
    #[inline(always)]
    pub fn idiv(&self, other: Self) -> Option<Self> {
        let o = other.to_int();
        if o == 0 { return None }
        Some(Self::from_int(self.to_int() / o))
    }
    #[inline(always)]
    pub fn irem(&self, other: Self) -> Option<Self> {
        let o = other.to_int();
        if o == 0 { return None }
        Some(Self::from_int(self.to_int() % o))
    }
    #[inline(always)]
    pub fn fadd(&self, other: Self) -> Self {
        Self::from_float(self.to_float() + other.to_float())
    }
    #[inline(always)]
    pub fn fsub(&self, other: Self) -> Self {
        Self::from_float(self.to_float() - other.to_float())
    }
    #[inline(always)]
    pub fn fmul(&self, other: Self) -> Self {
        Self::from_float(self.to_float() * other.to_float())
    }
    #[inline(always)]
    pub fn fdiv(&self, other: Self) -> Option<Self> {
        let o = other.to_float();
        if o == 0.0 { return None }
        Some(Self::from_float(self.to_float() / o))
    }
    #[inline(always)]
    pub fn frem(&self, other: Self) -> Option<Self> {
        let o = other.to_float();
        if o == 0.0 { return None }
        Some(Self::from_float(self.to_float() % o))
    }
    #[inline(always)]
    pub fn bor(&self, other: Self) -> Self {
        Self::from_int(self.to_int() | other.to_int())
    }
    #[inline(always)]
    pub fn band(&self, other: Self) -> Self {
        Self::from_int(self.to_int() & other.to_int())
    }
    #[inline(always)]
    pub fn bxor(&self, other: Self) -> Self {
        Self::from_int(self.to_int() ^ other.to_int())
    }
    #[inline(always)]
    pub fn bnot(&self) -> Self {
        Self::from_int(!self.to_int())
    }
    #[inline(always)]
    pub fn lor(&self, other: Self) -> Self {
        Self::from_bool(self.to_bool() || other.to_bool())
    }
    #[inline(always)]
    pub fn land(&self, other: Self) -> Self {
        Self::from_bool(self.to_bool() && other.to_bool())
    }
    #[inline(always)]
    pub fn lnot(&self) -> Self {
        Self::from_bool(!self.to_bool())
    }
    #[inline(always)]
    pub fn cmeq(&self, other: Self) -> Self {
        Self::from_bool(self.0 == other.0)
    }
    #[inline(always)]
    pub fn cmne(&self, other: Self) -> Self {
        Self::from_bool(self.0 != other.0)
    }
    #[inline(always)]
    pub fn icgt(&self, other: Self) -> Self {
        Self::from_bool(self.to_int() > other.to_int())
    }
    #[inline(always)]
    pub fn iclt(&self, other: Self) -> Self {
        Self::from_bool(self.to_int() < other.to_int())
    }
    #[inline(always)]
    pub fn icge(&self, other: Self) -> Self {
        Self::from_bool(self.to_int() >= other.to_int())
    }
    #[inline(always)]
    pub fn icle(&self, other: Self) -> Self {
        Self::from_bool(self.to_int() <= other.to_int())
    }
    #[inline(always)]
    pub fn fcgt(&self, other: Self) -> Self {
        Self::from_bool(self.to_float() > other.to_float())
    }
    #[inline(always)]
    pub fn fclt(&self, other: Self) -> Self {
        Self::from_bool(self.to_float() < other.to_float())
    }
    #[inline(always)]
    pub fn fcge(&self, other: Self) -> Self {
        Self::from_bool(self.to_float() >= other.to_float())
    }
    #[inline(always)]
    pub fn fcle(&self, other: Self) -> Self {
        Self::from_bool(self.to_float() <= other.to_float())
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn m() {
        use super::Value;
        let v = Value::from_int(5);
        println!("{}", v.is_int());
    }
}