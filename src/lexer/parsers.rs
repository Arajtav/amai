pub fn parse_int(string: &str) -> Option<i64> {
    let new = string.replace('_', "").to_ascii_lowercase();
    if string.starts_with("0x") {
        i64::from_str_radix(&new[2..], 16).ok()
    } else if string.starts_with("0b") {
        i64::from_str_radix(&new[2..], 2).ok()
    } else if string.starts_with("0o") {
        i64::from_str_radix(&new[2..], 8).ok()
    } else {
        new.parse().ok()
    }
}

pub fn parse_float(string: &str) -> Option<f64> {
    let mut dot_found = false;
    let mut float: f64 = 0.0;
    let mut fraction_idx = 0.1;

    for ch in string.chars() {
        if ch == '.' {
            if dot_found {
                return None;
            }
            dot_found = true;
            continue;
        }
        if ch == '_' {
            continue;
        }
        if !ch.is_ascii_digit() {
            return None;
        }

        let i = f64::from(ch as u8 - b'0');
        if dot_found {
            float += fraction_idx * i;
            fraction_idx *= 0.1;
        } else {
            float = float * 10.0 + i;
        }
    }

    if !dot_found {
        return None;
    }

    Some(float)
}
