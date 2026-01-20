// Copyright 2022 Oxide Computer Company

use crate::error::PreprocessorError;
use std::collections::HashMap;
use std::fmt::Write;
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct MacroDefinition {
    pub name: String,
    pub body: String,
}

#[derive(Debug, Default)]
pub struct PreprocessorResult {
    pub elements: PreprocessorElements,
    pub lines: Vec<String>,
}

#[derive(Debug, Default)]
pub struct PreprocessorElements {
    pub includes: Vec<String>,

    /// Macro definitions collected from `#define` directives.
    pub macros: Vec<MacroDefinition>,
}

pub fn run(
    source: &str,
    filename: Arc<String>,
) -> Result<PreprocessorResult, PreprocessorError> {
    let mut result = PreprocessorResult::default();
    let mut macros_to_process = Vec::new();
    let mut defined_macros: HashMap<String, String> = HashMap::new();
    let mut current_macro: Option<MacroDefinition> = None;

    //
    // first break the source up into lines
    //

    let lines: Vec<&str> = source.lines().collect();
    let mut new_lines: Vec<String> = Vec::new();
    let mut conditional_stack: Vec<ConditionalFrame> = Vec::new();

    //
    // process each line of the input
    //

    for (i, line) in lines.iter().enumerate() {
        let trimmed = line.trim_start();

        //
        // see if we're in a macro
        //

        match current_macro {
            None => {}
            Some(ref mut m) => {
                if !is_active(&conditional_stack) {
                    if !line.ends_with('\\') {
                        current_macro = None;
                    }
                    new_lines.push("".to_string());
                    continue;
                }
                if !line.ends_with('\\') {
                    write!(m.body, "\n{}", line).unwrap();
                    defined_macros.insert(m.name.clone(), m.body.clone());
                    macros_to_process.push(m.clone());
                    result.elements.macros.push(m.clone());
                    current_macro = None;
                } else {
                    write!(m.body, "\n{}", &line[..line.len() - 1]).unwrap();
                }
                new_lines.push("".to_string());
                continue;
            }
        }

        if trimmed.starts_with('#') {
            let directive = trimmed.trim_start_matches('#').trim_start();
            if directive.starts_with("include") {
                if is_active(&conditional_stack) {
                    process_include(i, trimmed, &mut result, &filename)?;
                }
                new_lines.push("".to_string());
                continue;
            }
            if directive.starts_with("define") {
                if is_active(&conditional_stack) {
                    let (name, value, continued) = process_macro_begin(i, trimmed, &filename)?;
                    let m = MacroDefinition { name, body: value };
                    if !continued {
                        defined_macros.insert(m.name.clone(), m.body.clone());
                        macros_to_process.push(m.clone());
                        result.elements.macros.push(m.clone());
                        current_macro = None;
                    } else {
                        current_macro = Some(m);
                    }
                }
                new_lines.push("".to_string());
                continue;
            }
            if directive.starts_with("undef") {
                if is_active(&conditional_stack) {
                    if let Some(name) = directive.split_whitespace().nth(1) {
                        defined_macros.remove(name);
                        macros_to_process.retain(|m| m.name != name);
                        result.elements.macros.retain(|m| m.name != name);
                    } else {
                        return Err(PreprocessorError {
                            line: i,
                            message: "Invalid #undef".into(),
                            source: line.to_string(),
                            file: filename.clone(),
                        });
                    }
                }
                new_lines.push("".to_string());
                continue;
            }
            if directive.starts_with("ifdef") {
                let name = directive.split_whitespace().nth(1);
                if name.is_none() {
                    return Err(PreprocessorError {
                        line: i,
                        message: "Invalid #ifdef".into(),
                        source: line.to_string(),
                        file: filename.clone(),
                    });
                }
                let parent_active = is_active(&conditional_stack);
                let cond = parent_active && defined_macros.contains_key(name.unwrap());
                push_conditional(
                    &mut conditional_stack,
                    cond,
                );
                new_lines.push("".to_string());
                continue;
            }
            if directive.starts_with("ifndef") {
                let name = directive.split_whitespace().nth(1);
                if name.is_none() {
                    return Err(PreprocessorError {
                        line: i,
                        message: "Invalid #ifndef".into(),
                        source: line.to_string(),
                        file: filename.clone(),
                    });
                }
                let parent_active = is_active(&conditional_stack);
                let cond = parent_active && !defined_macros.contains_key(name.unwrap());
                push_conditional(
                    &mut conditional_stack,
                    cond,
                );
                new_lines.push("".to_string());
                continue;
            }
            if directive.starts_with("if") && directive.chars().nth(2).map(|c| c.is_whitespace()).unwrap_or(true) {
                let expr = directive[2..].trim();
                let parent_active = is_active(&conditional_stack);
                let cond = if parent_active {
                    eval_condition(expr, &defined_macros)
                } else {
                    false
                };
                push_conditional(&mut conditional_stack, cond);
                new_lines.push("".to_string());
                continue;
            }
            if directive.starts_with("elif") {
                let expr = directive[4..].trim();
                handle_elif(i, line, expr, &mut conditional_stack, &defined_macros, &filename)?;
                new_lines.push("".to_string());
                continue;
            }
            if directive.starts_with("else") {
                handle_else(i, line, &mut conditional_stack, &filename)?;
                new_lines.push("".to_string());
                continue;
            }
            if directive.starts_with("endif") {
                if conditional_stack.pop().is_none() {
                    return Err(PreprocessorError {
                        line: i,
                        message: "Unexpected #endif".into(),
                        source: line.to_string(),
                        file: filename.clone(),
                    });
                }
                new_lines.push("".to_string());
                continue;
            }
        }

        if is_active(&conditional_stack) {
            new_lines.push(line.to_string());
        } else {
            new_lines.push("".to_string());
        }
    }

    if !conditional_stack.is_empty() {
        return Err(PreprocessorError {
            line: lines.len().saturating_sub(1),
            message: "Unterminated conditional directive".into(),
            source: "".to_string(),
            file: filename.clone(),
        });
    }

    //println!("macros to process\n{:#?}", macros_to_process);

    //
    // process macros
    //
    for line in &new_lines {
        let mut l = line.clone();
        for m in &macros_to_process {
            l = l.replace(&m.name, &m.body);
        }
        result.lines.push(l);
    }

    Ok(result)
}

fn process_include(
    i: usize,
    line: &str,
    result: &mut PreprocessorResult,
    filename: &Arc<String>,
) -> Result<(), PreprocessorError> {
    let (begin, end) = if let Some(begin) = line.find('<') {
        match line[begin..].find('>') {
            Some(end) => (begin + 1, begin + end),
            None => {
                return Err(PreprocessorError {
                    line: i,
                    message: "Unterminated '<'".into(),
                    source: line.to_string(),
                    file: filename.clone(),
                })
            }
        }
    } else if let Some(begin) = line.find('"') {
        // The file name is quoted by same character "
        // So, we need to find the next " after the first "
        let begin = begin + 1;
        match line[begin..].find('"') {
            Some(end) => (begin, begin + end),
            None => {
                return Err(PreprocessorError {
                    line: i,
                    message: "Unterminated '\"'".into(),
                    source: line.to_string(),
                    file: filename.clone(),
                })
            }
        }
    } else {
        return Err(PreprocessorError {
            line: i,
            message: "Invalid #include".into(),
            source: line.to_string(),
            file: filename.clone(),
        });
    };

    if end < line.len() {
        for c in line[end + 1..].chars() {
            if !c.is_whitespace() {
                return Err(PreprocessorError {
                    line: i,
                    message: format!(
                        "Unexpected character after #include '{}'",
                        c,
                    ),
                    source: line.to_string(),
                    file: filename.clone(),
                });
            }
        }
    }
    result.elements.includes.push(line[begin..end].into());

    Ok(())
}

fn process_macro_begin(
    i: usize,
    line: &str,
    filename: &Arc<String>,
) -> Result<(String, String, bool), PreprocessorError> {
    let mut parts = line.splitn(3, char::is_whitespace);
    parts.next();
    let name = match parts.next() {
        Some(n) if !n.is_empty() => n.into(),
        _ => {
            return Err(PreprocessorError {
                line: i,
                message: "Macros must have a name".into(),
                source: line.to_string(),
                file: filename.clone(),
            })
        }
    };

    let value = match parts.next() {
        Some(v) => v.trim().to_string(),
        None => "".into(),
    };

    let continued = line.ends_with('\\');
    Ok((name, value, continued))
}

#[derive(Debug, Clone)]
struct ConditionalFrame {
    parent_active: bool,
    this_active: bool,
    matched: bool,
    seen_else: bool,
}

fn is_active(stack: &[ConditionalFrame]) -> bool {
    stack.last().map(|f| f.this_active).unwrap_or(true)
}

fn push_conditional(stack: &mut Vec<ConditionalFrame>, condition: bool) {
    let parent_active = stack.last().map(|f| f.this_active).unwrap_or(true);
    let this_active = parent_active && condition;
    stack.push(ConditionalFrame {
        parent_active,
        this_active,
        matched: this_active,
        seen_else: false,
    });
}

fn handle_elif(
    i: usize,
    line: &str,
    expr: &str,
    stack: &mut Vec<ConditionalFrame>,
    macros: &HashMap<String, String>,
    filename: &Arc<String>,
) -> Result<(), PreprocessorError> {
    let frame = match stack.last_mut() {
        Some(f) => f,
        None => {
            return Err(PreprocessorError {
                line: i,
                message: "Unexpected #elif".into(),
                source: line.to_string(),
                file: filename.clone(),
            })
        }
    };
    if frame.seen_else {
        return Err(PreprocessorError {
            line: i,
            message: "Unexpected #elif after #else".into(),
            source: line.to_string(),
            file: filename.clone(),
        });
    }
    if !frame.parent_active || frame.matched {
        frame.this_active = false;
        return Ok(());
    }
    let cond = eval_condition(expr, macros);
    frame.this_active = frame.parent_active && cond;
    if frame.this_active {
        frame.matched = true;
    }
    Ok(())
}

fn handle_else(
    i: usize,
    line: &str,
    stack: &mut Vec<ConditionalFrame>,
    filename: &Arc<String>,
) -> Result<(), PreprocessorError> {
    let frame = match stack.last_mut() {
        Some(f) => f,
        None => {
            return Err(PreprocessorError {
                line: i,
                message: "Unexpected #else".into(),
                source: line.to_string(),
                file: filename.clone(),
            })
        }
    };
    if frame.seen_else {
        return Err(PreprocessorError {
            line: i,
            message: "Duplicate #else".into(),
            source: line.to_string(),
            file: filename.clone(),
        });
    }
    frame.seen_else = true;
    frame.this_active = frame.parent_active && !frame.matched;
    frame.matched = true;
    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
enum ExprToken {
    Identifier(String),
    Number(i64),
    Defined,
    Not,
    And,
    Or,
    LParen,
    RParen,
}

fn eval_condition(expr: &str, macros: &HashMap<String, String>) -> bool {
    let tokens = tokenize_expr(expr);
    let mut parser = ExprParser { tokens: &tokens, pos: 0, macros };
    parser.parse_or().unwrap_or(false)
}

fn tokenize_expr(expr: &str) -> Vec<ExprToken> {
    let mut tokens = Vec::new();
    let mut chars = expr.chars().peekable();

    while let Some(&c) = chars.peek() {
        if c.is_whitespace() {
            chars.next();
            continue;
        }
        if c == '(' {
            chars.next();
            tokens.push(ExprToken::LParen);
            continue;
        }
        if c == ')' {
            chars.next();
            tokens.push(ExprToken::RParen);
            continue;
        }
        if c == '!' {
            chars.next();
            tokens.push(ExprToken::Not);
            continue;
        }
        if c == '&' {
            chars.next();
            if chars.peek() == Some(&'&') {
                chars.next();
                tokens.push(ExprToken::And);
            }
            continue;
        }
        if c == '|' {
            chars.next();
            if chars.peek() == Some(&'|') {
                chars.next();
                tokens.push(ExprToken::Or);
            }
            continue;
        }
        if c.is_ascii_digit() {
            let mut num = String::new();
            while let Some(&d) = chars.peek() {
                if d.is_ascii_digit() {
                    num.push(d);
                    chars.next();
                } else {
                    break;
                }
            }
            let value = num.parse::<i64>().unwrap_or(0);
            tokens.push(ExprToken::Number(value));
            continue;
        }
        if c.is_ascii_alphabetic() || c == '_' {
            let mut ident = String::new();
            while let Some(&d) = chars.peek() {
                if d.is_ascii_alphanumeric() || d == '_' {
                    ident.push(d);
                    chars.next();
                } else {
                    break;
                }
            }
            if ident == "defined" {
                tokens.push(ExprToken::Defined);
            } else {
                tokens.push(ExprToken::Identifier(ident));
            }
            continue;
        }
        chars.next();
    }

    tokens
}

struct ExprParser<'a> {
    tokens: &'a [ExprToken],
    pos: usize,
    macros: &'a HashMap<String, String>,
}

impl<'a> ExprParser<'a> {
    fn parse_or(&mut self) -> Option<bool> {
        let mut value = self.parse_and()?;
        while self.match_token(&ExprToken::Or) {
            let rhs = self.parse_and()?;
            value = value || rhs;
        }
        Some(value)
    }

    fn parse_and(&mut self) -> Option<bool> {
        let mut value = self.parse_unary()?;
        while self.match_token(&ExprToken::And) {
            let rhs = self.parse_unary()?;
            value = value && rhs;
        }
        Some(value)
    }

    fn parse_unary(&mut self) -> Option<bool> {
        if self.match_token(&ExprToken::Not) {
            let value = self.parse_unary()?;
            return Some(!value);
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Option<bool> {
        if self.match_token(&ExprToken::LParen) {
            let value = self.parse_or();
            self.match_token(&ExprToken::RParen);
            return value;
        }
        if self.match_token(&ExprToken::Defined) {
            return Some(self.parse_defined());
        }
        let token = self.tokens.get(self.pos).cloned();
        match token {
            Some(ExprToken::Number(n)) => {
                self.pos += 1;
                Some(n != 0)
            }
            Some(ExprToken::Identifier(name)) => {
                self.pos += 1;
                Some(self.eval_identifier(&name))
            }
            _ => None,
        }
    }

    fn parse_defined(&mut self) -> bool {
        if self.match_token(&ExprToken::LParen) {
            if let Some(ExprToken::Identifier(name)) = self.tokens.get(self.pos).cloned() {
                self.pos += 1;
                self.match_token(&ExprToken::RParen);
                return self.macros.contains_key(&name);
            }
            return false;
        }
        if let Some(ExprToken::Identifier(name)) = self.tokens.get(self.pos).cloned() {
            self.pos += 1;
            return self.macros.contains_key(&name);
        }
        false
    }

    fn eval_identifier(&self, name: &str) -> bool {
        match self.macros.get(name) {
            Some(value) => match value.trim().parse::<i64>() {
                Ok(n) => n != 0,
                Err(_) => true,
            },
            None => false,
        }
    }

    fn match_token(&mut self, expected: &ExprToken) -> bool {
        if let Some(token) = self.peek() {
            if token == expected {
                self.pos += 1;
                return true;
            }
        }
        false
    }

    fn peek(&self) -> Option<&ExprToken> {
        self.tokens.get(self.pos)
    }
}
