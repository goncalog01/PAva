function metajulia_read()
  input = readline()
  while isa(Meta.parse(input), Expr) && Meta.parse(input).head == :incomplete
    print("   ")
    input *= "\n" * readline()
  end
  input
end

function metajulia_eval(expr, env=global_env)
  is_self_evaluating(expr) ? expr :
  is_quote_node(expr) ? eval_quote_node(expr, env) :
  is_quoted_expression(expr) ? eval_quoted_expression(expr, env) :
  is_and(expr) ? eval_and(expr, env) :
  is_or(expr) ? eval_or(expr, env) :
  is_condexpr(expr) ? eval_condexpr(expr, env) :
  is_block(expr) ? eval_block(expr, env) :
  is_name(expr) ? eval_name(expr, env) :
  is_global(expr) ? eval_global(expr, env) :
  is_function_def(expr) ? eval_function_def(expr, env) :
  is_assignment(expr) ? eval_assignment(expr, env) :
  is_symbol_call(expr) ? eval_symbol_call(expr, env) :
  is_lambda(expr) ? eval_lambda(expr, env) :
  is_let(expr) ? eval_let(expr, env) :
  is_macro_def(expr) ? eval_macro_def(expr, env) :
  is_fexpr_def(expr) ? eval_fexpr_def(expr, env) :
  is_while(expr) ? eval_while(expr, env) :
  throw("Unknown expression type -- $(expr)")
end

function metajulia_repl()
  while true
    print(">> ")
    input = metajulia_read()
    if input == "" && eof(stdin)
      return
    end
    result = metajulia_eval(Meta.parse(input), global_env)
    print_output(result)
  end
end

function print_output(result)
  if !(isa(result, Nothing) || result == :__uninitialized__)
    show(result)
  end
  println()
end


# Environment -----------------------------------------------------------------

struct Environment
  bindings
  parent
end

function Environment(parent)
  Environment(Dict(), parent)
end

function is_global_environment(env)
  isnothing(env.parent)
end

function lookup(symbol, env)
  while !isnothing(env)
    if haskey(env.bindings, symbol)
      return env.bindings[symbol]
    end
    env = env.parent
  end
  throw("Symbol not defined -- $(symbol)")
end

function set!(symbol, value, env)
  env.bindings[symbol] = value
end

function update_name!(symbol, value, env)
  original_env = env
  while !isnothing(env)
    if haskey(env.bindings, symbol)
      if is_global_environment(env) && !is_global_environment(original_env)
        set!(symbol, value, original_env)
      else
        set!(symbol, value, env)
      end
    end
    env = env.parent
  end
  set!(symbol, value, original_env)
end

function augment_environment(env)
  Environment(env)
end

function initial_bindings()
  return Dict{Symbol,Any}(
    :+ => make_primitive(+),
    :- => make_primitive(-),
    :* => make_primitive(*),
    :/ => make_primitive(/),
    :(==) => make_primitive(==),
    :(!=) => make_primitive(!=),
    :< => make_primitive(<),
    :> => make_primitive(>),
    :(<=) => make_primitive(<=),
    :(>=) => make_primitive(>=),
    :! => make_primitive(!),
    :println => make_primitive(println),
    :gensym => make_primitive(gensym),
  )
end


# Primitives ------------------------------------------------------------------

struct Primitive
  func
end

Base.show(io::IO, p::Primitive) = print(io, "<function>")

function make_primitive(f)
  return Primitive(f)
end

function is_primitive(obj)
  return isa(obj, Primitive)
end

function primitive_operation(prim)
  return prim.func
end

function apply_primitive_function(prim, args)
  return Core._apply(primitive_operation(prim), args)
end


# Functions -------------------------------------------------------------------

struct Function
  parameters
  body
  environment
end

Base.show(io::IO, f::Function) = print(io, "<function>")

function function_name(expr)
  expr.args[1].args[1]
end

function function_parameters(expr)
  expr.args[1].args[2:end]
end

function function_body(expr)
  expr.args[2]
end


# Macro -----------------------------------------------------------------------

struct Macro
  parameters
  body
  environment
end

Base.show(io::IO, m::Macro) = print(io, "<macro>")

function macro_name(expr)
  expr.args[1].args[1]
end

function macro_parameters(expr)
  expr.args[1].args[2:end]
end

function macro_body(expr)
  expr.args[2]
end


# Fexprs ----------------------------------------------------------------------

# Evaluated in the calling environent
struct Fexpr
  parameters
  body
  environment
end

Base.show(io::IO, f::Fexpr) = print(io, "<fexpr>")

function fexpr_name(expr)
  expr.args[1].args[1]
end

function fexpr_parameters(expr)
  expr.args[1].args[2:end]
end

function fexpr_body(expr)
  expr.args[2]
end


# Expression recognizers ------------------------------------------------------

function is_self_evaluating(expr)
  isa(expr, Number) ||
    isa(expr, String) ||
    isa(expr, Bool) ||
    isa(expr, Nothing) ||
    isa(expr, LineNumberNode)
end

function is_quote_node(expr)
  isa(expr, QuoteNode)
end

function is_quoted_expression(expr)
  (isa(expr, Expr) && expr.head == :quote)
end

function is_and(expr)
  isa(expr, Expr) && expr.head == :&&
end

function is_or(expr)
  isa(expr, Expr) && expr.head == :||
end

function is_condexpr(expr)
  isa(expr, Expr) && (expr.head == :if || expr.head == :elseif)
end

function is_name(expr)
  isa(expr, Symbol)
end

function is_global(expr)
  isa(expr, Expr) && expr.head == :global
end

function is_assignment(expr)
  isa(expr, Expr) && expr.head == :(=)
end

function is_function_def(expr)
  isa(expr, Expr) && expr.head == :(=) && is_symbol_call(expr.args[1])
end

function is_block(expr)
  isa(expr, Expr) && expr.head == :block
end

function is_symbol_call(expr)
  isa(expr, Expr) && expr.head == :call
end

function is_let(expr)
  isa(expr, Expr) && expr.head == :let
end

function is_lambda(expr)
  isa(expr, Expr) && expr.head == :(->)
end

function is_macro_def(expr)
  isa(expr, Expr) && expr.head == :($=)
end

function is_fexpr_def(expr)
  isa(expr, Expr) && expr.head == :(:=)
end

function is_while(expr)
  isa(expr, Expr) && expr.head == :while 
end

# Expression Evaluators -------------------------------------------------------

function eval_quote_node(expr, env)
  expr.value
end

function rec_eval_quoted_expression(expr, env)
  if isa(expr, Expr)
    if expr.head == :$
      metajulia_eval(expr.args[1], env)
    else
      new_args = [rec_eval_quoted_expression(arg, env) for arg in expr.args]
      expr.args = new_args
      expr
    end
  else
    expr
  end
end

function eval_quoted_expression(expr, env)
  rec_eval_quoted_expression(copy(expr.args[1]), env)
end

function eval_and(expr, env)
  metajulia_eval(expr.args[1], env) && metajulia_eval(expr.args[2], env)
end

function eval_or(expr, env)
  metajulia_eval(expr.args[1], env) || metajulia_eval(expr.args[2], env)
end

function eval_condexpr(expr, env)
  if metajulia_eval(expr.args[1], env)
    metajulia_eval(expr.args[2], env)
  elseif (length(expr.args) == 3)
    metajulia_eval(expr.args[3], env)
  end
end

function eval_name(expr, env)
  ret = lookup(expr, env)
  if ret == :__uninitialized__
    throw("Symbol not defined -- $(expr)")
  else
    ret
  end
end

# Aux global

function global_name(expr)
  return expr.args[1].args[1]
end

function global_value(expr)
  return expr.args[1].args[2]
end

function eval_global(expr, env)
  if is_function_def(expr.args[1])
    set!(function_name(expr.args[1]), Function(function_parameters(expr.args[1]), function_body(expr.args[1]), env), global_env)
  elseif is_assignment(expr.args[1])
    val = metajulia_eval(global_value(expr), env)
    set!(global_name(expr), val, global_env)
  elseif is_fexpr_def(expr.args[1])
    set!(fexpr_name(expr.args[1]), Fexpr(fexpr_parameters(expr.args[1]), fexpr_body(expr.args[1]), env), global_env)
  elseif is_global_environment(env)
    set!(expr.args[1], :__uninitialized__, global_env)
  end
end

function eval_function_def(expr, env)
  update_name!(function_name(expr), Function(function_parameters(expr), function_body(expr), env), env)
end

function eval_assignment(expr, env)
  update_name!(expr.args[1], metajulia_eval(expr.args[2], env), env)
end

function eval_macro_def(expr, env)
  set!(macro_name(expr), Macro(function_parameters(expr), function_body(expr), env), env)
end

function eval_exprs(exprs, env)
  return [metajulia_eval(expr, env) for expr in exprs]
end

function eval_block(expr, env)
  ret = nothing
  for expression in expr.args
    ret = metajulia_eval(expression, env)
  end
  ret
end

# Aux let

function let_assignments(expr)
  expr.args[1]
end

function let_body(expr)
  expr.args[2]
end

function let_inits(expr)
  if is_block(expr.args[1])
    expr.args[1].args
  else
    [expr.args[1]]
  end
end

function eval_let_init(expr, aug_env)
  if isa(expr, Symbol)
    set!(expr, :__uninitialized__, aug_env)
  elseif is_function_def(expr)
    set!(function_name(expr), Function(function_parameters(expr), function_body(expr), aug_env), aug_env)
  elseif is_assignment(expr)
    set!(expr.args[1], metajulia_eval(expr.args[2], aug_env), aug_env)
  else
    throw("Invalid let initializer -- $(expr)")
  end
end

function eval_let(expr, env)
  aug_env = augment_environment(env)
  for init in let_inits(expr)
    eval_let_init(init, aug_env)
  end
  metajulia_eval(let_body(expr), aug_env)
end

# Aux symbol call

function call_operator(expr)
  expr.args[1]
end

function call_operands(expr)
  expr.args[2:end]
end

function eval_macro(expr, env)
  let func = metajulia_eval(call_operator(expr), env), args = call_operands(expr), expansion = apply_function(func, args)
    metajulia_eval(expansion, env)
  end
end

function eval_fexpr(expr, env)
  let func = metajulia_eval(call_operator(expr), env), args = call_operands(expr)
    aug_env = augment_environment(func.environment)
    for (param, arg) in zip(func.parameters, args)
      set!(param, arg, aug_env)
    end
    set!(:eval, make_primitive((arg) -> metajulia_eval(arg, env)), aug_env)
    metajulia_eval(func.body, aug_env)
  end
end

function eval_function(expr, env)
  let func = metajulia_eval(call_operator(expr), env), args = call_operands(expr), eval_args = eval_exprs(args, env)
    apply_function(func, eval_args)
  end
end

function eval_symbol_call(expr, env)
  let func = metajulia_eval(call_operator(expr), env)
    if isa(func, Macro)
      eval_macro(expr, env)
    elseif isa(func, Fexpr)
      eval_fexpr(expr, env)
    else
      eval_function(expr, env)
    end
  end
end

function apply_function(func, args)
  if is_primitive(func)
    apply_primitive_function(func, args)
  else
    aug_env = augment_environment(func.environment)
    for (param, arg) in zip(func.parameters, args)
      set!(param, arg, aug_env)
    end
    metajulia_eval(func.body, aug_env)
  end
end

# Aux lambda

function lambda_parameters(expr)
  if isa(expr.args[1], Expr) && expr.args[1].head == :tuple
    expr.args[1].args
  else
    tuple(expr.args[1])
  end
end

function lambda_body(expr)
  expr.args[2]
end

function eval_lambda(expr, env)
  Function(lambda_parameters(expr), lambda_body(expr), env)
end

function eval_fexpr_def(expr, env)
  set!(fexpr_name(expr), Fexpr(fexpr_parameters(expr), fexpr_body(expr), env), env)
end

# Aux while

function while_cond(expr)
  expr.args[1]
end

function while_block(expr)
  expr.args[2]
end

function eval_while(expr, env)
  aug_env = augment_environment(env)
  while(metajulia_eval(while_cond(expr), aug_env))
    metajulia_eval(while_block(expr), aug_env)
  end
end


# Initialization --------------------------------------------------------------

global_env = Environment(initial_bindings(), nothing);
