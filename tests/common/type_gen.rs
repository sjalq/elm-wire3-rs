// Shared random Elm type definition generator used by fuzz_codegen and fuzz_wire3_types.

use rand::rngs::SmallRng;
use rand::Rng;
use rand::SeedableRng;
use std::collections::HashSet;

const MAX_DEPTH: usize = 4;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TypeKind {
    Record,
    Union,
    SimpleAlias,
}

pub struct ElmTypeGen {
    rng: SmallRng,
    counter: usize,
    /// Type names generated so far (available for cross-references).
    pub available: Vec<String>,
    /// Which of the available types contain Float (transitively).
    pub float_containing: HashSet<String>,
    /// What kind each generated type is.
    pub type_kinds: Vec<TypeKind>,
    /// Constructor names used across ALL union types (Elm requires global uniqueness).
    used_ctors: HashSet<String>,
}

impl ElmTypeGen {
    pub fn new(seed: u64) -> Self {
        ElmTypeGen {
            rng: SmallRng::seed_from_u64(seed),
            counter: 0,
            available: vec![],
            float_containing: HashSet::new(),
            type_kinds: vec![],
            used_ctors: HashSet::new(),
        }
    }

    /// Generate a complete Elm module with `num_types` random type definitions.
    pub fn generate_module(&mut self, num_types: usize) -> String {
        let mut source = "module Generated exposing (..)\n\n".to_string();
        source.push_str("import Dict exposing (Dict)\n");
        source.push_str("import Set exposing (Set)\n\n");
        for _ in 0..num_types {
            source.push_str(&self.generate_type_def());
            source.push_str("\n\n");
        }
        source
    }

    pub fn generate_type_def(&mut self) -> String {
        self.counter += 1;
        let name = format!("Gen{}", self.counter);
        let choice = self.rng.random_range(0..3u32);
        let (def, has_float, kind) = match choice {
            0 => {
                let (d, f) = self.gen_record(&name);
                (d, f, TypeKind::Record)
            }
            1 => {
                let (d, f) = self.gen_union(&name);
                (d, f, TypeKind::Union)
            }
            _ => {
                let (d, f) = self.gen_simple_alias(&name);
                (d, f, TypeKind::SimpleAlias)
            }
        };
        if has_float {
            self.float_containing.insert(name.clone());
        }
        self.available.push(name);
        self.type_kinds.push(kind);
        def
    }

    /// Generate a record type alias. Returns (definition, contains_float).
    fn gen_record(&mut self, name: &str) -> (String, bool) {
        let n = self.rng.random_range(1..=5u32);
        let mut fields = Vec::new();
        let mut used = HashSet::new();
        let mut has_float = false;

        for _ in 0..n {
            let fname = self.pick_field_name(&mut used);
            let (ftype, f) = self.gen_type_inner(0);
            has_float |= f;
            fields.push(format!("{fname} : {ftype}"));
        }

        let def = format!(
            "type alias {name} =\n    {{ {}\n    }}",
            fields.join("\n    , ")
        );
        (def, has_float)
    }

    /// Generate a union type. Returns (definition, contains_float).
    fn gen_union(&mut self, name: &str) -> (String, bool) {
        let n = self.rng.random_range(1..=4u32);
        let mut ctors = Vec::new();
        let mut has_float = false;

        for _ in 0..n {
            let cname = self.pick_ctor_name_global();
            let np = self.rng.random_range(0..=2u32);
            let mut params = Vec::new();
            for _ in 0..np {
                let (expr, needs_parens, f) = self.gen_type_with_parens_info(0);
                has_float |= f;
                if needs_parens {
                    params.push(format!("({expr})"));
                } else {
                    params.push(expr);
                }
            }

            if params.is_empty() {
                ctors.push(format!("    {cname}"));
            } else {
                ctors.push(format!("    {cname} {}", params.join(" ")));
            }
        }

        let def = format!("type {name}\n    = {}", ctors.join("\n    | "));
        (def, has_float)
    }

    /// Generate a simple type alias. Returns (definition, contains_float).
    fn gen_simple_alias(&mut self, name: &str) -> (String, bool) {
        let (type_expr, has_float) = self.gen_type_inner(0);
        (format!("type alias {name} = {type_expr}"), has_float)
    }

    /// Generate a type expression. Returns (expression, contains_float).
    fn gen_type_inner(&mut self, depth: usize) -> (String, bool) {
        let (expr, _, has_float) = self.gen_type_with_parens_info(depth);
        (expr, has_float)
    }

    /// Generate a type expression suitable as a constructor/container argument.
    /// Wraps in parens if needed. Returns (expression, contains_float).
    pub fn gen_type_arg(&mut self, depth: usize) -> (String, bool) {
        let (expr, needs_parens, has_float) = self.gen_type_with_parens_info(depth);
        if needs_parens {
            (format!("({expr})"), has_float)
        } else {
            (expr, has_float)
        }
    }

    /// Core type generation. Returns (expression, needs_parens, contains_float).
    fn gen_type_with_parens_info(&mut self, depth: usize) -> (String, bool, bool) {
        if depth >= MAX_DEPTH {
            let (expr, has_float) = self.random_prim();
            return (expr, false, has_float);
        }

        // 50% primitives/user types, 50% containers
        let use_container = self.rng.random_bool(0.5);

        if !use_container {
            // 80% primitive, 20% user type (if available)
            if !self.available.is_empty() && self.rng.random_bool(0.2) {
                let idx = self.rng.random_range(0..self.available.len());
                let name = self.available[idx].clone();
                let has_float = self.float_containing.contains(&name);
                return (name, false, has_float);
            }
            let (expr, has_float) = self.random_prim();
            return (expr, false, has_float);
        }

        let choice = self.rng.random_range(0..7u32);
        match choice {
            0 => {
                // List T
                let (inner, f) = self.gen_type_arg(depth + 1);
                (format!("List {inner}"), true, f)
            }
            1 => {
                // Maybe T
                let (inner, f) = self.gen_type_arg(depth + 1);
                (format!("Maybe {inner}"), true, f)
            }
            2 => {
                // Set comparable (String or Int only)
                let k = self.random_comparable();
                (format!("Set {k}"), true, false)
            }
            3 => {
                // Dict comparable V
                let k = self.random_comparable();
                let (v, f) = self.gen_type_arg(depth + 1);
                (format!("Dict {k} {v}"), true, f)
            }
            4 => {
                // Result E A
                let (e, f1) = self.gen_type_arg(depth + 1);
                let (a, f2) = self.gen_type_arg(depth + 1);
                (format!("Result {e} {a}"), true, f1 || f2)
            }
            5 => {
                // 2-tuple
                let (a, f1) = self.gen_type_inner(depth + 1);
                let (b, f2) = self.gen_type_inner(depth + 1);
                (format!("( {a}, {b} )"), false, f1 || f2)
            }
            _ => {
                // 3-tuple
                let (a, f1) = self.gen_type_inner(depth + 1);
                let (b, f2) = self.gen_type_inner(depth + 1);
                let (c, f3) = self.gen_type_inner(depth + 1);
                (format!("( {a}, {b}, {c} )"), false, f1 || f2 || f3)
            }
        }
    }

    fn random_prim(&mut self) -> (String, bool) {
        let choice = self.rng.random_range(0..4u32);
        match choice {
            0 => ("Int".to_string(), false),
            1 => ("Float".to_string(), true),
            2 => ("Bool".to_string(), false),
            _ => ("String".to_string(), false),
        }
    }

    fn random_comparable(&mut self) -> String {
        if self.rng.random_bool(0.5) {
            "String".to_string()
        } else {
            "Int".to_string()
        }
    }

    fn pick_field_name(&mut self, used: &mut HashSet<String>) -> String {
        const NAMES: &[&str] = &[
            "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta",
            "iota", "kappa", "mu", "nu", "xi", "rho", "sigma", "tau", "phi",
            "chi", "psi", "omega", "value", "count", "label", "score", "flag",
            "name", "data", "items", "total", "index",
        ];
        loop {
            let i = self.rng.random_range(0..NAMES.len());
            if used.insert(NAMES[i].to_string()) {
                return NAMES[i].to_string();
            }
            if used.len() >= NAMES.len() {
                let n = format!("field{}", used.len());
                used.insert(n.clone());
                return n;
            }
        }
    }

    /// Pick a globally unique constructor name (Elm requires uniqueness across the module).
    fn pick_ctor_name_global(&mut self) -> String {
        const NAMES: &[&str] = &[
            "Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta",
            "Iota", "Kappa", "Mu", "Nu", "Xi", "Rho", "Sigma", "Tau", "Phi",
            "Chi", "Psi", "Omega", "First", "Second", "Third", "Leaf", "Branch",
            "Aaa", "Bbb", "Ccc", "Ddd", "Eee", "Fff", "Ggg", "Hhh", "Iii", "Jjj",
            "Kkk", "Lll", "Mmm", "Nnn", "Ooo", "Ppp", "Qqq", "Rrr", "Sss", "Ttt",
            "Uuu", "Vvv", "Www", "Xxx", "Yyy", "Zzz",
        ];
        loop {
            let i = self.rng.random_range(0..NAMES.len());
            if self.used_ctors.insert(NAMES[i].to_string()) {
                return NAMES[i].to_string();
            }
            if self.used_ctors.len() >= NAMES.len() {
                let n = format!("Ctor{}", self.used_ctors.len());
                self.used_ctors.insert(n.clone());
                return n;
            }
        }
    }
}
