use bigdecimal::{num_bigint::BigInt, BigDecimal, FromPrimitive, One, Signed, Zero};

pub fn round(x: BigDecimal, round_digits: i64) -> BigDecimal {
    let (bigint, decimal_part_digits) = x.into_bigint_and_exponent();

    //let need_to_round_digits = decimal_part_digits - round_digits;

    let five = if bigint.is_positive() { 5 } else { -5 };
    let x = BigDecimal::new(bigint, decimal_part_digits);

    // Already rounded or negative digits.
    if round_digits < 0 || decimal_part_digits <= round_digits {
        return x;
    }

    (x + BigDecimal::new(BigInt::from_i32(five).unwrap(), round_digits + 1))
        .with_scale(round_digits)
}

fn bigd(i: i32) -> BigDecimal {
    BigDecimal::from_i32(i).unwrap()
}

fn maclaurin<F: FnMut(i32, &BigDecimal, &BigDecimal, &BigDecimal) -> Option<BigDecimal>>(
    x: BigDecimal,
    num_digits: i64,
    mut f: F,
) -> BigDecimal {
    let mut power = BigDecimal::one();
    let mut factorial = BigDecimal::one();
    let mut tot = BigDecimal::zero();
    let mut i = 0;
    // let imax = x.digits() as i32 * 10;
    let imax = 30000;
    loop {
        let new_tot = f(i, &tot, &power, &factorial);

        if let Some(new_tot) = new_tot {
            let new_tot = round(new_tot, num_digits as i64 * 2);
            // println!(
            //     "new_tot={} power={} factorial={}",
            //     new_tot, power, factorial
            // );
            if new_tot == tot {
                break;
            }
            if i > imax {
                panic!("Sequence did not converge.");
            }
            tot = new_tot;
        }

        power *= &x;
        i += 1;
        factorial *= bigd(i);
        power = round(power, num_digits as i64 * 2);
    }

    round(tot, num_digits as i64 + 1).normalized()
}

pub fn one() -> BigDecimal {
    bigd(1)
}

pub fn half() -> BigDecimal {
    BigDecimal::from_f32(0.5).unwrap()
}

fn ln(mut x: BigDecimal, num_digits: i64) -> Option<BigDecimal> {
    if x.is_zero() || x.is_negative() {
        return None;
    }

    let mut extra = bigd(0);
    let h = half();
    if &x >= &one() {
        // Reduce to exp(0)..exp(0.5)
        let lim = exp(h.clone(), num_digits);
        let scale = exp(-h.clone(), num_digits);
        while x >= lim {
            extra += &h;
            x *= &scale;
        }

        // ln(1 + x): https://en.wikipedia.org/wiki/Taylor_series
        Some(
            maclaurin(x - one(), num_digits, |i, tot, power, _factorial| {
                if i == 0 {
                    None
                } else {
                    let z = bigd(if (i & 1) == 0 { -i } else { i });
                    Some(tot + power / &z)
                }
            }) + extra,
        )
    } else {
        // Reduce to exp(-0.5)..exp(0)
        let lim = exp(-h.clone(), num_digits);
        let scale = exp(h.clone(), num_digits);
        while x <= lim {
            extra -= &h;
            x *= &scale;
        }

        // ln(1 - x): https://en.wikipedia.org/wiki/Taylor_series
        Some(
            maclaurin(one() - x, num_digits, |i, tot, power, _factorial| {
                if i == 0 {
                    None
                } else {
                    let z = bigd(-i);
                    Some(tot + power / &z)
                }
            }) + extra,
        )
    }
}

fn exp(x: BigDecimal, num_digits: i64) -> BigDecimal {
    maclaurin(x, num_digits, |_i, tot, power, factorial| {
        Some(tot + power / factorial)
    })
}

pub fn log(x: BigDecimal, base: BigDecimal, num_digits: i64) -> Option<BigDecimal> {
    if let (Some(lnx), Some(lnbase)) = (ln(x, num_digits), ln(base, num_digits)) {
        Some(lnx / lnbase)
    } else {
        None
    }
}
