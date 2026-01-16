const INT: usize = 0;
const FLOAT: usize = 1;
const BOOL: usize = 3;
const UNIT: usize = 4;

#[derive(Debug, Clone, Copy, Eq, Hash)]
pub struct ValueBuilder {
    pub inner: u64,
    pub tag: usize,
}

impl PartialEq for ValueBuilder {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner && self.tag == other.tag
    }

    fn ne(&self, other: &Self) -> bool {
        self.inner != other.inner || self.tag != other.tag
    }
}

impl ValueBuilder {
    pub fn from_int(x: i64) -> Self {
        Self {
            inner: x as u64,
            tag: INT,
        }
    }
    pub fn from_float(x: f64) -> Self {
        Self {
            inner: x.to_bits(),
            tag: FLOAT,
        }
    }
    pub fn from_bool(x: bool) -> Self {
        Self {
            inner: x as u64,
            tag: BOOL,
        }
    }
    pub fn unit() -> Self {
        Self {
            inner: 0x0,
            tag: UNIT,
        }
    }
}