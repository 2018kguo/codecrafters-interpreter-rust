use crate::{
    ast::Interpreter,
    scanner::{Literal, LoxFuncPtr},
};
use anyhow::Result;
use std::time::SystemTime;

pub fn clock(_: &Interpreter, _: &Vec<Literal>) -> Result<Literal> {
    let time = std::time::SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs_f64();
    Ok(Literal::Number(time))
}

pub const NATIVE_FUNCS: &[(&str, LoxFuncPtr, usize)] = &[("clock", clock, 0)];
