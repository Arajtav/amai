#[derive(Debug, Clone, Copy, PartialEq, Hash, Eq)]
pub struct Value(pub u64);

#[allow(unused)]
impl Value {
    #[inline(always)]
    pub fn from_int(x: i64) -> Self {
        Self(x as u64)
    }
    #[inline(always)]
    pub fn to_int(&self) -> i64 {
        self.0 as i64
    }

    #[inline(always)]
    pub fn from_bool(x: bool) -> Self {
        Self(x as u64)
    }
    #[inline(always)]
    pub fn to_bool(&self) -> bool {
        self.0 != 0
    }

    #[inline(always)]
    pub fn nil() -> Self {
        Self(0x0)
    }

    #[inline(always)]
    pub fn from_float(x: f64) -> Self {
        Self(x.to_bits())
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
    #[inline(always)]
    pub fn ineg(&self) -> Self {
        Self::from_int(-self.to_int())
    }
    #[inline(always)]
    pub fn fneg(&self) -> Self {
        Self::from_float(-self.to_float())
    }
    #[inline(always)]
    pub fn lshf(&self, other: Self) -> Self {
        Self::from_int(self.to_int() << other.to_int())
    }
    #[inline(always)]
    pub fn rshf(&self, other: Self) -> Self {
        Self::from_int(self.to_int() >> other.to_int())
    }
}