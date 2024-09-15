use std::collections::HashMap;

use anyhow::Result;

use crate::{
    ast::RuntimeError,
    scanner::{Literal, Token},
};

pub struct Environment {
    values: HashMap<String, Literal>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: &str, value: Literal) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &Token) -> Result<Literal> {
        if let Some(value) = self.values.get(name.lexeme.as_str()) {
            Ok(value.clone())
        } else {
            Err(anyhow::anyhow!(RuntimeError {
                token: name.clone(),
                message: format!("Undefined variable '{}'.", name.lexeme),
            }))
        }
    }
}
