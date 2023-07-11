# Semantics of HM-Inference

## Type System

The Hindley-Milner type system is a type inference algorithm that combines parametric polymorphism (also known as generics) and Hindley-Milner type inference to statically type-check programs. It uses the following type system:

1. **Basic Types**: The type system includes basic types such as `Int`, `Bool`, `String`, etc.

2. **Type Variables**: Type variables represent unknown types and are denoted by lowercase letters (e.g., `'a`, `'b`, `'c`, etc.).

3. **Function Types**: The type system supports function types of the form `T1 -> T2`, where `T1` and `T2` are types.

4. **Type Schemes**: A type scheme represents a polymorphic type and is of the form `∀α.T`, where `α` is a type variable and `T` is a type. The `∀` symbol indicates universal quantification.

5. **Type Environment**: A type environment is a mapping from variables to type schemes. It keeps track of the types of variables in the program.

## Algorithm W

Algorithm W is the core type inference algorithm used in Hindley-Milner. It infers types for expressions based on their usage and constraints imposed by the program.

The algorithm works as follows:

1. **Input**: The algorithm takes an expression `E` and a type environment `Γ` as input.

2. **Fresh Type Variables**: For each type variable in `E` and `Γ`, generate a fresh type variable to avoid variable capture.

3. **Unification**: Use the unification algorithm to find substitutions for type variables to make the types of subexpressions compatible.

4. **Constraints**: Generate constraints on type variables based on the structure of `E`. For example, for an expression `if cond then e1 else e2`, the type of `cond` must be `Bool`, and the types of `e1` and `e2` must be the same.

5. **Constraint Solving**: Solve the generated constraints by applying unification and substitution rules. This process may involve type variable substitutions and may fail if the constraints are unsatisfiable.

6. **Type Inference**: After constraint solving, the algorithm assigns types to subexpressions and constructs a type for the whole expression `E`.

7. **Generalization**: If a type variable is not bound in the type environment, generalize the inferred type by introducing a type scheme with universal quantification.

8. **Output**: The algorithm returns the most general type of the expression `E` based on the given type environment `Γ`.

## Substitution Rules

During the constraint solving phase, the algorithm applies the following substitution rules to propagate type variable substitutions:

1. **Identity Rule**: `α := α`, where `α` is a type variable.

2. **Capture-Avoiding Substitution**: If `α` is a type variable and `β` is another type variable, the substitution `α := β` is applied only if `α` does not occur in the type `β`.

3. **Composition Rule**: If `θ` and `ψ` are two substitutions, the composition `θ ∘ ψ` is defined as applying `θ` followed by `ψ`.

4. **Substitution Rule**: If `α` is a type variable and `T` is a type, the substitution `α := T` is applied to all occurrences of `α` in the constraints and the type `T` is substituted.

---

This markdown provides an overview of the semantics of HM-Inference, including the type system, Algorithm W, and the substitution rules used during constraint solving. It outlines the key components and steps involved in inferring types for expressions. Further details and code examples can be found in academic papers and programming language textbooks related to the Hindley-Milner type system.
