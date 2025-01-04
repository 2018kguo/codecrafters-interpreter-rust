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
    pub max_environment_id: usize,
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
            max_environment_id: 0,
        }
    }

    pub fn begin_scope(&mut self) {
        let new_environment = Environment {
            parent_environment: Some(self.current_environment),
            values: HashMap::new(),
        };
        let new_env_id = self.max_environment_id + 1;
        self.environments.insert(new_env_id, new_environment);
        //println!("new env id: {}", new_env_id);
        //println!("parent env id: {}", self.current_environment);
        self.current_environment = new_env_id;
        self.max_environment_id = new_env_id;
    }

    pub fn exit_scope(&mut self) -> Result<()> {
        if let Some(current_env) = self.environments.get(&self.current_environment) {
            let parent_env_id = current_env
                .parent_environment
                .ok_or_else(|| anyhow::anyhow!("Attempted to exit global scope"))?;
            //let removed_environment = self.current_environment;
            self.current_environment = parent_env_id;
            //self.environments.remove(&removed_environment);
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
                //println!("name: {:?}", name.lexeme);
                //println!("env values: {:?}", env.values);
                //println!("environment: {:?}", environment);
                //println!("parent: {:?}", env.parent_environment);
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

    pub fn debug_print(&self) {
        println!("--- Environment Context ---");
        println!("current environment: {}", self.current_environment);
        for (env_id, env) in self.environments.iter() {
            println!("Environment {}", env_id);
            println!("Parent: {:?}", env.parent_environment);
            for (name, value) in env.values.iter() {
                println!("{}: {:?}", name, value);
            }
        }
        println!("--- End Environment Context ---");
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
