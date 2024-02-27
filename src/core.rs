use crate::env::Environment;
use crate::lang;
use crate::symbol::Symbol;
use crate::values::ToValue;

pub fn load_core(env: &Environment) {
    env.change_or_create_namespace(&Symbol::new("fmlisp.core"));

    for (k, v) in lang::core::internal_symbols(&env) {
        env.insert_var(Symbol::new(k), v.to_rc_value());
    }

    let fns = vec![
        lang::core::core_functions(),
        lang::strings::string_functions(),
        lang::namespaces::namespace_functions(),
    ];

    for fs in fns {
        for (k, v) in fs {
            // Namespace mapping Symbol - Rc<Value>
            // Load them into clojure.core ns
            env.insert_var(Symbol::new(k), v.to_rc_value());
        }
    }
}
