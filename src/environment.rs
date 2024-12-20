use std::collections::HashMap;

use anyhow::Result;

use crate::{
    ast::RuntimeError,
    scanner::{Literal, Token},
};

// Interpreter owns environments, each environment owns a hashmap of variables
//
pub struct EnvironmentContext {
    environments: HashMap<usize, Environment>,
    pub current_environment: usize,
}

pub struct Environment {
    parent_environment: Option<usize>,
    values: HashMap<String, Literal>,
}

impl EnvironmentContext {
    pub fn new() -> Self {
        let mut environments = HashMap::new();
        environments.insert(0, Environment::new());
        EnvironmentContext {
            environments,
            current_environment: 0,
        }
    }

    pub fn get_values_in_global_environment(&self) -> HashMap<String, Literal> {
        self.environments
            .get(&0)
            .map(|env| env.values.clone())
            .unwrap_or_default()
    }

    pub fn begin_scope(&mut self) {
        let new_environment = Environment {
            parent_environment: Some(self.current_environment),
            values: HashMap::new(),
        };
        let new_env_id = self.environments.len();
        self.environments.insert(new_env_id, new_environment);
        self.current_environment = new_env_id;
    }

    pub fn exit_scope(&mut self) -> Result<()> {
        if let Some(current_env) = self.environments.get(&self.current_environment) {
            let parent_env_id = current_env
                .parent_environment
                .ok_or_else(|| anyhow::anyhow!("Attempted to exit global scope"))?;
            let removed_environment = self.current_environment;
            self.current_environment = parent_env_id;
            self.environments.remove(&removed_environment);
            Ok(())
        } else {
            Err(anyhow::anyhow!("Current environment not found"))
        }
    }

    pub fn define(&mut self, name: &str, value: Literal) -> Result<()> {
        if let Some(env) = self.environments.get_mut(&self.current_environment) {
            env.define(name, value);
            Ok(())
        } else {
            Err(anyhow::anyhow!("Current environment not found"))
        }
    }

    pub fn assign(&mut self, name: &Token, value: Literal) -> Result<()> {
        let mut environment = Some(self.current_environment);
        while let Some(env_id) = environment {
            if let Some(env) = self.environments.get_mut(&env_id) {
                if env.get(name).is_ok() {
                    return env.assign(name, value);
                }
                environment = env.parent_environment;
            } else {
                break;
            }
        }
        Err(anyhow::anyhow!(RuntimeError {
            token: name.clone(),
            message: format!("Undefined variable '{}'.", name.lexeme),
        }))
    }

    pub fn get(&self, name: &Token) -> Result<Literal> {
        let mut environment = Some(self.current_environment);
        while let Some(env_id) = environment {
            if let Some(env) = self.environments.get(&env_id) {
                if let Some(value) = env.values.get(name.lexeme.as_str()) {
                    return Ok(value.clone());
                }
                environment = env.parent_environment;
            } else {
                break;
            }
        }
        Err(anyhow::anyhow!(RuntimeError {
            token: name.clone(),
            message: format!("Undefined variable '{}'.", name.lexeme),
        }))
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            values: HashMap::new(),
            parent_environment: None,
        }
    }

    pub fn define(&mut self, name: &str, value: Literal) {
        self.values.insert(name.to_string(), value);
    }

    pub fn assign(&mut self, name: &Token, value: Literal) -> Result<()> {
        if self.values.contains_key(name.lexeme.as_str()) {
            self.values.insert(name.lexeme.clone(), value);
            Ok(())
        } else {
            Err(anyhow::anyhow!(RuntimeError {
                token: name.clone(),
                message: format!("Undefined variable '{}'.", name.lexeme),
            }))
        }
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
