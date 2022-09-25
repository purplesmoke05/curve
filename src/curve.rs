use std::ops::{Mul, Sub};

use anchor_lang::prelude::Pubkey;
use bigdecimal::{num_traits::pow, BigDecimal as Decimal, FromPrimitive, ToPrimitive};

use crate::{
    math::{log, round}, error::Error,
};

fn ceiling_division(dividend: &Decimal, mut divisor: Decimal) -> (Decimal, Decimal) {
    let mut quotient = dividend / divisor.clone();
    if quotient.eq(&Decimal::from(0)) {
        return (Decimal::from(0), Decimal::from(0));
    }
    let mut remainder = dividend.clone() % round(divisor.clone(), 0);
    if remainder.gt(&Decimal::from(0)) {
        quotient = quotient + Decimal::from(1);
        divisor = dividend / quotient.clone();
        remainder = dividend % round(quotient.clone(), 0);
        if remainder.gt(&Decimal::from(0)) {
            divisor = divisor + Decimal::from(1);
        }
    }
    return (quotient, divisor);
}

#[derive(Debug, Eq, PartialEq)]
pub struct SwapParams {
    pub source_mint: Pubkey,
    pub destination_mint: Pubkey,
    pub user_source_token_account: Pubkey,
    pub user_destination_token_account: Pubkey,
    pub user_transfer_authority: Pubkey,
    pub in_amount: Option<u64>,
    pub minimum_out_amount: u64,
    pub open_orders_address: Pubkey,
}

#[derive(Debug, Eq, PartialEq)]
pub struct Curve {
    number_of_currencies: usize,
    amplification_factor: Decimal,
    target_prices: Vec<Decimal>,
}

impl Curve {
    fn new(
        number_of_currencies: usize,
        amplification_factor: Decimal,
        target_prices: Vec<Decimal>,
    ) -> Self {
        Self {
            number_of_currencies,
            amplification_factor,
            target_prices,
        }
    }

    fn set_amplification_factor(&mut self, amp: Decimal) -> bool {
        if self.amplification_factor != amp {
            self.amplification_factor = amp;
            return true;
        }
        return false;
    }

    fn xp(&self, token_amounts: Vec<Decimal>) -> Vec<Decimal> {
        token_amounts
            .iter()
            .enumerate()
            .map(|(i, f)| f * self.target_prices.get(i).unwrap())
            .collect()
    }

    fn compute_d(&self, token_amounts: Vec<Decimal>) -> Decimal {
        let mut d_prev = Decimal::from(1);
        let xp = self.xp(token_amounts);
        let sum: Decimal = xp.clone().iter().sum();
        let mut d: Decimal = sum.clone();
        let leverage = self.amplification_factor.clone()
            * Decimal::from_usize(self.number_of_currencies).unwrap();

        while (d.clone().sub(d_prev).abs()).gt(&Decimal::from(1)) {
            let mut dp = d.clone();
            for x in xp.clone() {
                dp =
                    (dp * d.clone()) / (Decimal::from_usize(self.number_of_currencies).unwrap() * x)
            }
            d_prev = d.clone();

            let numerator = ((leverage.clone() * sum.clone())
                + (dp.clone() * Decimal::from_usize(self.number_of_currencies).unwrap()))
                * d.clone();
            let denominator = ((leverage.clone() - Decimal::from(1)) * d)
                + ((Decimal::from_usize(self.number_of_currencies).unwrap() + Decimal::from(1))
                    * dp);

            d = numerator / denominator;
        }
        d
    }

    fn compute_y(
        &self,
        token_amounts: Vec<Decimal>,
        input_index: usize,
        output_index: usize,
        new_total_amount: Decimal,
    ) -> Decimal {
        let d = self.compute_d(token_amounts.clone());
        let mut xx: Vec<Decimal> = self.xp(token_amounts);
        xx[input_index] = new_total_amount;
        // std::mem::replace(&mut xx[input_index], new_total_amount);
        xx.remove(output_index);
        let ann = self.amplification_factor.clone()
            * Decimal::from_usize(self.number_of_currencies).unwrap();

        let mut cc = d.clone();
        for x in xx.clone() {
            cc = (cc * d.clone()) / (x * Decimal::from_usize(self.number_of_currencies).unwrap());
        }
        cc = (cc * d.clone())
            / (Decimal::from_usize(self.number_of_currencies).unwrap() * ann.clone());
        let b: Decimal = xx.iter().sum::<Decimal>() + (d.clone() / ann) - d.clone();

        let mut y_prev = Decimal::from(0);
        let mut y = d;
        while (y.clone().sub(y_prev).abs()).gt(&Decimal::from(1)) {
            y_prev = y.clone();
            y = (y.clone() * y.clone() + cc.clone()) / ((y * Decimal::from(2)) + b.clone())
        }
        y
    }

    fn mul_array(arr: Vec<Decimal>) -> Decimal {
        let mut ret = Decimal::from(1);
        for a in arr {
            ret = ret * a
        }
        ret
    }

    fn compute_base_y(
        &self,
        token_amounts: Vec<Decimal>,
        input_index: usize,
        output_index: usize,
        amount: Decimal,
    ) -> Decimal {
        let d = self.compute_d(token_amounts.clone());
        let xp = self.xp(token_amounts);
        let nn = Decimal::from_usize(self.number_of_currencies)
            .unwrap()
            .exp();
        let sum: Decimal = xp.iter().sum();
        let product: Decimal = Curve::mul_array(xp.clone());
        let k = (((self.amplification_factor.clone() * nn.clone()) * sum) + d.clone())
            - self.amplification_factor.clone() * d * nn.clone();
        let b = self.amplification_factor.clone() * nn.clone() * nn.clone() * product.clone();
        let c = nn * product * k;

        let numerator = (c.clone() / xp.get(input_index).unwrap()) + b.clone();
        let denominator = (c / xp.get(output_index).unwrap()) + b;
        let input_factor = log(numerator.clone(), Decimal::from(10), 16).unwrap();
        let output_factor = log(denominator.clone(), Decimal::from(10), 16).unwrap();
        let factor = output_factor.clone().sub(&input_factor).abs();
        if input_factor.ge(&output_factor) {
            return ((numerator * amount) / denominator)
                * round(
                    pow::<Decimal>(Decimal::from(10), factor.to_usize().unwrap()),
                    0,
                );
        }
        return ((numerator * amount) / denominator)
            / round(
                pow::<Decimal>(Decimal::from(10), factor.to_usize().unwrap()),
                0,
            );
    }

    pub fn exchange(
        &self,
        token_amounts: Vec<Decimal>,
        input_index: usize,
        output_index: usize,
        swap_amount: Decimal,
        minus_one: bool,
    ) -> Result<Decimal, Error> {
        if token_amounts.len() != self.number_of_currencies {
            return Err(Error::UnMatchLength);
        }
        let xp = self.xp(token_amounts.clone());
        let dx = self
            .target_prices
            .get(input_index)
            .unwrap()
            .mul(swap_amount);

        let new_input_pool_amount = xp.get(input_index).unwrap() + dx;
        let y = self.compute_y(
            token_amounts,
            input_index,
            output_index,
            new_input_pool_amount,
        );
        let mut dy = xp.get(output_index).unwrap() - y;
        if minus_one {
            dy = dy - Decimal::from(1);
        }
        Ok(dy / self.target_prices.get(output_index).unwrap())
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Fee {
    pub fee_numerator: Decimal,
    pub fee_denominator: Decimal,
}

#[derive(Debug, Eq, PartialEq)]
pub struct TokenSwapStable {
    trader_fee: Fee,
    owner_fee: Fee,
    curve: Curve,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct SwapSimulateResult {
    pub is_not_enough_liquidity: bool,
    pub price_impact: Decimal,
    pub fees: Decimal,
    pub expected_output_amount: Decimal,
}

fn calculate_fee_amount(amount: Decimal, fee: &Fee) -> Decimal {
    if fee.fee_numerator.eq(&Decimal::from(0)) || amount.eq(&Decimal::from(0)) {
        return Decimal::from(0);
    }
    let fee_amount = (amount * fee.fee_numerator.clone()) / fee.fee_denominator.clone();
    if fee_amount.eq(&Decimal::from(0)) {
        return Decimal::from(1);
    }
    return fee_amount;
}

impl TokenSwapStable {
    pub fn new(trader_fee: Fee, owner_fee: Fee, amp: Decimal) -> Self {
        Self {
            trader_fee,
            owner_fee,
            curve: Curve {
                number_of_currencies: 2,
                amplification_factor: amp,
                target_prices: vec![Decimal::from(1), Decimal::from(1)],
            },
        }
    }

    pub fn exchange(
        &self,
        token_amounts: Vec<Decimal>,
        swap_amount: Decimal,
        output_index: usize,
    ) -> SwapSimulateResult {
        let mut input_index = 0usize;
        if output_index == 0usize {
            input_index = 1usize
        }
        let expected_output_amount = self.get_expected_output_amount(
            token_amounts.clone(),
            swap_amount.clone(),
            input_index,
            output_index,
        );
        SwapSimulateResult {
            is_not_enough_liquidity: false,
            price_impact: self.get_price_impact(token_amounts, swap_amount.clone(), expected_output_amount.clone(), input_index, output_index),
            fees: self.get_fees(swap_amount),
            expected_output_amount,
        }
    }

    fn get_price_impact(
        &self,
        token_amounts: Vec<Decimal>,
        input_trade_amount: Decimal,
        expected_output_amount: Decimal,
        input_index: usize,
        output_index: usize,
    ) -> Decimal {
        if input_trade_amount.eq(&Decimal::from(0)) {
            return Decimal::from(0);
        }
        if token_amounts[input_index].eq(&Decimal::from(0)) {
            return Decimal::from(0);
        }
        if token_amounts[output_index].eq(&Decimal::from(0)) {
            return Decimal::from(0);
        }
        let no_slippage_output_amount = self.get_expected_output_amount_with_no_slippage(
            token_amounts,
            input_trade_amount,
            input_index,
            output_index,
        );
        (no_slippage_output_amount - expected_output_amount.clone()) / expected_output_amount
    }

    fn get_expected_output_amount_with_no_slippage(
        &self,
        token_amounts: Vec<Decimal>,
        input_trade_amount: Decimal,
        input_index: usize,
        output_index: usize,
    ) -> Decimal {
        let input_trade_amount_less_fees = self.get_input_amount_less_fees(input_trade_amount);
        return self.get_output_amount_with_no_slippage(
            token_amounts,
            input_trade_amount_less_fees,
            input_index,
            output_index,
        );
    }

    fn get_output_amount_with_no_slippage(
        &self,
        token_amounts: Vec<Decimal>,
        input_trade_amount_less_fees: Decimal,
        input_index: usize,
        output_index: usize,
    ) -> Decimal {
        return self.curve.compute_base_y(
            token_amounts,
            input_index,
            output_index,
            input_trade_amount_less_fees,
        );
    }

    fn get_expected_output_amount(
        &self,
        token_amounts: Vec<Decimal>,
        input_trade_amount: Decimal,
        input_index: usize,
        output_index: usize,
    ) -> Decimal {
        let input_trade_amount_less_fees = self.get_input_amount_less_fees(input_trade_amount);
        return self.get_output_amount(
            token_amounts,
            input_trade_amount_less_fees,
            input_index,
            output_index,
        );
    }

    fn get_input_amount_less_fees(&self, input_trade_amount: Decimal) -> Decimal {
        return input_trade_amount.clone() - self.get_fees(input_trade_amount);
    }

    fn get_fees(&self, input_trade_amount: Decimal) -> Decimal {
        let trading_fee = calculate_fee_amount(input_trade_amount.clone(), &self.trader_fee);
        let owner_fee = calculate_fee_amount(input_trade_amount, &self.owner_fee);
        trading_fee + owner_fee
    }

    fn get_output_amount(
        &self,
        token_amounts: Vec<Decimal>,
        input_trade_amount_less_fees: Decimal,
        input_index: usize,
        output_index: usize,
    ) -> Decimal {
        self.curve
            .exchange(
                token_amounts,
                input_index,
                output_index,
                input_trade_amount_less_fees,
                false,
            )
            .unwrap()
    }
}

pub struct TokenSwapConstantProduct {
    trader_fee: Fee,
    owner_fee: Fee,

    fees_on_input: bool,
}

impl TokenSwapConstantProduct {
    pub fn new(trader_fee: Fee, owner_fee: Fee, fees_on_input: bool) -> Self {
        Self {
            trader_fee,
            owner_fee,
            fees_on_input,
        }
    }

    pub fn exchange(
        &self,
        token_amounts: Vec<Decimal>,
        swap_amount: Decimal,
        output_index: usize,
    ) -> SwapSimulateResult {
        let mut input_index = 0usize;
        if output_index == 0usize {
            input_index = 1usize;
        }
        let mut new_input_trade_amount = swap_amount.clone();
        if self.fees_on_input {
            new_input_trade_amount = self.get_amount_less_fees(swap_amount.clone());
        }
        let mut expected_output_amount = self.get_expected_output_amount(
            token_amounts.clone(),
            new_input_trade_amount.clone(),
            input_index,
            output_index,
        );
        let mut fees = self.get_fees(expected_output_amount.clone());
        if self.fees_on_input {
            fees = self.get_fees(swap_amount);
        } else {
            expected_output_amount = self.get_amount_less_fees(expected_output_amount);
        }
        SwapSimulateResult {
            is_not_enough_liquidity: false,
            price_impact: self.get_price_impact(token_amounts, new_input_trade_amount, expected_output_amount.clone(), input_index, output_index),
            fees,
            expected_output_amount,
        }
    }

    fn get_price_impact(
        &self,
        token_amounts: Vec<Decimal>,
        input_trade_amount: Decimal,
        expected_output_amount: Decimal,
        input_index: usize,
        output_index: usize,
    ) -> Decimal {
        if input_trade_amount.eq(&Decimal::from(0)) {
            return Decimal::from(0);
        }
        if token_amounts[input_index].eq(&Decimal::from(0)) {
            return Decimal::from(0);
        }
        if token_amounts[output_index].eq(&Decimal::from(0)) {
            return Decimal::from(0);
        }
        let no_slippage_output_amount_decimal = self.get_expected_output_amount_with_no_slippage(
            token_amounts,
            input_trade_amount,
            input_index,
            output_index,
        );
        return (no_slippage_output_amount_decimal.clone() - expected_output_amount)
            / no_slippage_output_amount_decimal;
    }

    fn get_expected_output_amount_with_no_slippage(
        &self,
        token_amounts: Vec<Decimal>,
        input_trade_amount: Decimal,
        input_index: usize,
        output_index: usize,
    ) -> Decimal {
        if token_amounts[input_index].eq(&Decimal::from(0)) {
            return token_amounts.get(output_index).unwrap().clone();
        }
        let tmp = input_trade_amount * token_amounts.get(output_index).unwrap().clone();
        let expected_output_amount_with_no_slippage =
            tmp / token_amounts.get(input_index).unwrap().clone();
        if self.fees_on_input {
            return expected_output_amount_with_no_slippage;
        }
        return self.get_amount_less_fees(expected_output_amount_with_no_slippage);
    }

    fn get_fees(&self, input_trade_amount: Decimal) -> Decimal {
        let trading_fee = calculate_fee_amount(input_trade_amount.clone(), &self.trader_fee);
        let owner_fee = calculate_fee_amount(input_trade_amount, &self.owner_fee);
        trading_fee + owner_fee
    }

    fn get_amount_less_fees(&self, swap_amount: Decimal) -> Decimal {
        swap_amount.clone() - self.get_fees(swap_amount)
    }

    fn get_expected_output_amount(
        &self,
        token_amounts: Vec<Decimal>,
        input_trade_amount: Decimal,
        input_index: usize,
        output_index: usize,
    ) -> Decimal {
        self.get_output_amount(token_amounts, input_trade_amount, input_index, output_index)
    }

    fn get_output_amount(
        &self,
        token_amounts: Vec<Decimal>,
        input_trade_amount: Decimal,
        input_index: usize,
        output_index: usize,
    ) -> Decimal {
        let pool_input_amount = token_amounts.get(input_index).unwrap().clone();
        let pool_output_amount = token_amounts.get(output_index).unwrap().clone();
        let invariant = pool_input_amount.clone() * pool_output_amount.clone();
        let (ceiling_division0, _) =
            ceiling_division(&invariant, pool_input_amount + input_trade_amount);
        return pool_output_amount - round(ceiling_division0, 0);
    }
}

pub struct Stable {
    trader_fee: Fee,
    _target_prices: Vec<Decimal>,
    curve: Curve,
}

impl Stable {
    pub fn new(
        num_of_currencies: usize,
        amp: Decimal,
        target_prices: Vec<Decimal>,
        trader_fee: Fee,
    ) -> Self {
        Self {
            trader_fee,
            _target_prices: target_prices.clone(),
            curve: Curve::new(num_of_currencies, amp, target_prices),
        }
    }

    pub fn set_amp(&mut self, amp: Decimal) -> bool {
        return self.curve.set_amplification_factor(amp);
    }

    pub fn exchange(
        &self,
        token_amounts: Vec<Decimal>,
        input_trade_amount: Decimal,
        input_index: usize,
        output_index: usize,
    ) -> SwapSimulateResult {
        let output_amount_without_fees = self.get_output_amount(
            token_amounts.clone(),
            input_trade_amount.clone(),
            input_index,
            output_index,
        );
        let fees = self.get_fees(output_amount_without_fees.clone());
        let expected_output_amount = output_amount_without_fees - fees.clone();
        return SwapSimulateResult {
            is_not_enough_liquidity: false,
            price_impact: self.get_price_impact(token_amounts, input_trade_amount, expected_output_amount.clone(), input_index, output_index),
            fees,
            expected_output_amount,
        };
    }
    fn get_output_amount(
        &self,
        token_amounts: Vec<Decimal>,
        input_trade_amount: Decimal,
        input_index: usize,
        output_index: usize,
    ) -> Decimal {
        self.curve
            .exchange(
                token_amounts,
                input_index,
                output_index,
                input_trade_amount,
                true,
            )
            .unwrap()
    }
    fn get_fees(&self, output_amount_without_fees: Decimal) -> Decimal {
        if self.trader_fee.fee_numerator.eq(&Decimal::from(0)) {
            return Decimal::from(0);
        }
        (output_amount_without_fees * self.trader_fee.fee_numerator.clone())
            / self.trader_fee.fee_denominator.clone()
    }
    fn get_price_impact(
        &self,
        token_amounts: Vec<Decimal>,
        input_trade_amount: Decimal,
        expected_output_amount: Decimal,
        input_index: usize,
        output_index: usize,
    ) -> Decimal {
        if input_trade_amount.eq(&Decimal::from(0)) {
            return Decimal::from(0);
        }
        if token_amounts[input_index].eq(&Decimal::from(0)) {
            return Decimal::from(0);
        }
        if token_amounts[output_index].eq(&Decimal::from(0)) {
            return Decimal::from(0);
        }
        let no_slippage_output_amount = self.get_output_amount_with_no_slippage(
            token_amounts,
            input_trade_amount,
            input_index,
            output_index,
        );
        return (no_slippage_output_amount - expected_output_amount.clone())
            / expected_output_amount;
    }

    fn get_output_amount_with_no_slippage(
        &self,
        token_amounts: Vec<Decimal>,
        input_trade_amount: Decimal,
        input_index: usize,
        output_index: usize,
    ) -> Decimal {
        return self.curve.compute_base_y(
            token_amounts,
            input_index,
            output_index,
            input_trade_amount,
        );
    }
}
