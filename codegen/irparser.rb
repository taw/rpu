#!/usr/bin/ruby -w

# This handles parsing of internal representation, as output by OCaml part
# of the compiler.
#
# It is not SSA and it is not optimized an all !
# We get unreachable code elimination for free,
# everything else we need to do on our own.

def parse_variable(v)
    case v
    when /t\$(\d+):float/
        Variable.new(:float, $1.to_i)
    when /t\$(\d+):(?:point|vector|normal)/
        Variable.new(:point, $1.to_i)
    when /t\$(\d+):color/
        Variable.new(:color, $1.to_i)
    when /t\$(\d+):string/
        Variable.new(:string, $1.to_i)
    when /t\$(\d+):matrix/
        Variable.new(:matrix, $1.to_i)
    else
        raise "Parse error: Cannot parse variable #{v}"
    end
end

def function_abi(target, args)
    [target.allocate_to, *args.map{|a|a.allocate_to}]
end

# The IR format isn't really specified (it should be)
# it's something like:
# returnvalue/shadertype function/shader_name(
#     parameter_type_1 parameter_name_1;
#     parameter_type_2 parameter_name_2;
# ) {
#     prelude
# 
#     body
# 
#     epilogue
# }
#
# str can contain any number of functions/shaders
# FIXME: Only a single function is supported now
def parse_ir_def(return_type, parameters, prelude, body, epilogue, str)
    code = {}
    retval = nil
    params = {}
    end_link_in = nil
    const_vars = [] # Actually the only possible variable is PI here

    parameters = parameters.split(/;\s*/).map{|x| x =~ /(\S+)\s+(\S+)/; [$1, $2]}

    magic = {}
    prelude.split(/\s*\n\s*/).each{|x|
        case x
        when /^param\[(\d+)\]\s*->\s*(\S+)$/
            param_loc, var = $1.to_i, $2
            var = parse_variable($2)
            raise "Parameter #{param_loc} defined more than once" if params[param_loc]
            params[param_loc] = var
        when /^magic\((.*)\)\s*->\s*(\S+)$/
            magic[$1] = parse_variable($2)
        else
            raise "Unsupported prelude element: #{x}"
        end
    }
    params = params.safe_to_a

    # FIXME: Make it work
    
    case return_type
    when "surface"
        const_vars.push [magic.delete("PI"), Math::PI]

        const_vars.push [magic.delete("du"), 0.0]
        const_vars.push [magic.delete("dv"), 0.0]

        const_vars.push [magic.delete("Os"), [1.0, 1.0, 1.0]]
        const_vars.push [magic.delete("Ci"), [0.0, 0.0, 0.0]]
        const_vars.push [magic.delete("Oi"), [1.0, 1.0, 1.0]]

        surface_shader_magic = [
            magic.delete("dPdu"),
            magic.delete("dPdv"),
            magic.delete("Ng"),
            magic.delete("N"), # N == Ng
            magic.delete("u"),
            magic.delete("v"),
            magic.delete("P"),
            magic.delete("Cs"),
            magic.delete("s"),
            magic.delete("t"),
            magic.delete("I"),
        ]

        # TODO: E == <0.0, 0.0, 0.0> for now
        # Pass in one of C variables instead ?
        const_vars.push [magic.delete("E"), [0.0, 0.0, 0.0]]

        # FIXME: No support for real lighting yet
        const_vars.push [magic.delete("L"), [0.0, 0.0, 0.0]]
        const_vars.push [magic.delete("Ol"), [0.0, 0.0, 0.0]]
        const_vars.push [magic.delete("Cl"), [1.0, 1.0, 1.0]]

        raise "Unsupported magic: #{magic.keys.join ' '} for surface shaders" unless magic.empty?
    else
        const_vars.push [magic.delete("PI"), Math::PI]
        raise "Unsupported magic: #{magic.keys.join ' '} for functions" unless magic.empty?
    end

    body = body.split(/\s*\n\s*/)
    start_at, *body = *body
    start_at =~ /^start at (\d+)$/ or raise "Parse error in start-at: #{start_at}"
    start_at = $1.to_i

    op_symbols = {
        "+" => :add,
        "-" => :sub,
        "*" => :mul,
        "." => :dp3,
        "^" => :xpd,
        #"/" => :div, # / can only be 1/foo
    }
    # [real_symbol, invert_x_y, invert_else_then]
    cmp_symbols = {
        ">"  => [:ge, true,  true],  #  Emulated
        ">=" => [:ge, false, false], # Supported
        "<"  => [:ge, false, true],  #  Emulated
        "<=" => [:ge, true,  false], #  Emulated
        "==" => [:eq, false, false], # Supported
        "!=" => [:eq, false, true],  #  Emulated
    }
    body.each{|x|
        case x
        when /^(\d+):\s*if\s*\((.*?)\)\s*jump\s*(\d+);\s*->\s*(\d+)$/
            link_in, cmp, link_then, link_else = $1, $2, $3, $4
            label = link_in.to_i
            cmp =~ /^(\S+)\s*(>|>=|<|<=|==|!=)\s*(\S+)$/ or raise "Parse error: Cannot parse comparison"
            cmpop, invert_x_y, invert_else_then = cmp_symbols[$2]
            args = [$1, $3].map{|v| parse_variable(v)}

            # We only support >= and ==, emulate everything else using these two flags
            # That's 6 possible operations (== is symetric, so there's no use in invert_x_y)
            args[0], args[1] = args[1], args[0] if invert_x_y
            link_then, link_else = link_else, link_then if invert_else_then

            link_then = link_then.to_i
            link_else = link_else.to_i
            code[label] = Code.new(:cjmp, [], [cmpop, args[0], args[1]], link_else, link_then)
        when /^(\d+):\s*(.*?)\s*-> (\d+)$/
            link_in, op, link_out = $1, $2, $3
            label = link_in.to_i
            link_out = [link_out.to_i]
            raise "Multiple definitions with the same label" if code[label]
            case op
            when /^nop$/
                code[label] = Code.new(:nop, [], [], link_out)
            when /^(\S+)\s*=\s*(-?\d+\.\d+)$/
                target, source = $1, $2.to_f
                code[label] = Code.new(:mov, [], [parse_variable($1), $2.to_f], link_out)
            when /^(\S+)\s*=\s*\((\S+), (\S+), (\S+)\)$/
                args = [$1,$2,$3,$4].map{|v| parse_variable v}
                code[label] = Code.new(:tuple, [], args, link_out)
            when /^(\S+)\s*=\s*rcp\((\S+)\)$/
                args = [$1,$2].map{|v| parse_variable v}
                code[label] = Code.new(:rcp, [], args, link_out)
            # Is it possible to have something like that in the input ?
            #when /^(\S+)\s*=\s*rsq\((\S+)\)$/
            #    args = [$1,$2].map{|v| parse_variable v}
            #    code[label] = Code.new(:rsq, [], args, link_out)
            # Changed to rcp(...)
            #when /^(\S+)\s*=\s*1\/(\S+)$/
            #    args = [$1,$2].map{|v| parse_variable v}
            #    code[label] = Code.new(:rcp, [], args, link_out)
            when /^(\S+)\s*=\s*"(.*?)"$/
                target = parse_variable $1
                src = $2.compiler_hash
                code[label] = Code.new(:mov, [], [target, src], link_out)
            when /^(\S+)\s*=\s*([^\"\(\) ]+)$/
                args = [$1,$2].map{|v| parse_variable v}
                code[label] = Code.new(:mov, [], args, link_out)
            when /^(\S+)\s*=\s*(\S+)\s+([\+\-\.\^\*])\s+(\S+)$/
                op = op_symbols[$3] or raise "Operator #{$3} unknown"
                args = [$1, $2, $4].map{|v| parse_variable(v)}
                code[label] = Code.new(op, [], args, link_out)
            when /^(\S+)\s*=\s*call\((.*)\)$/
                target = parse_variable $1
                fname, *args = $2.split(/\s*,\s*/)
                fun_args = args.map{|v| parse_variable v}
                abi = function_abi(target, fun_args)
                early_inline(fname, abi, target, fun_args, link_out, code, label)
            else
                raise "Parse error: Cannot parse op: #{op}"
            end
        when /(\d+):\s+END\s*$/
            raise "End label defined multiple times" if end_link_in
            end_link_in = [$1.to_i]
        else
            raise "Parse error in body line: #{x}"
        end
    }

    epilogue_magic = {}
    epilogue.split(/\s*\n\s*/).each{|x|
        case x
        when /^return\s+(\S+)$/
            var = parse_variable($1)
            raise "Return value returned more than once" if retval
            retval = var
        when /^(\S+)\s+->\s+magic\((\S+)\)$/
            epilogue_magic[$2] = parse_variable($1)
        else
            raise "Unsupported epilogue element: #{x}"
        end
    }
    if return_type == "surface"
        ci_m = epilogue_magic.delete("Ci")
        oi_m = epilogue_magic.delete("Oi")
        retval = Variable.fresh(:color)
    end
    raise "Unsupported epilogue magic: #{epilogue_magic.keys.join ' '}" unless epilogue_magic.empty?

    const_vars.each{|v,x|
        if x.is_a? Float
            start_at = Code.new(:mov, [], [v, x], [start_at])
        else
            start_at = Code.new(:tuple, [], [v, *x], [start_at])
        end
    }
    if return_type == "surface"
        start_at = Code.new(:surface_shader_init, [], surface_shader_magic, [start_at])
    end

    code_root = Code_Prelude.new(params, start_at)

    raise "Can't handle epilogue phi-in yet" if end_link_in.size != 1
    raise "#{end_link_in} defined as return label, but some code already there" if code[end_link_in]
    if return_type == "surface"
        code[end_link_in[0]] = Code.new(:mul, [], [retval, ci_m, oi_m], [:real_epilogue])
        code[:real_epilogue] = Code_Epilogue.new(retval)
    else
        code[end_link_in[0]] = Code_Epilogue.new(retval)
    end

    code_root.each_dfs{|cp|
        cp.link_nx {|i,*args|
            # We might have built some real code here
            if i.is_a? Code
                [i, *args]
            else
                raise "Reference to code label #{i} but it's not present" unless code[i]
                [code[i], *args]
            end
        }
    }

    return code_root
end

def parse_ir(str)
    str = str.dup
    while str =~ /\S/
        if str =~ /^\s*(\S+)\s+(\S+)\s*\(\s*(.*?)\s*\)\s*\{\nprelude:\n *(.*?)\nbody:\n *(.*?)\s*\nepilogue:\n *(.*?)\s*\}\n\s*(.*)$/m
            return_type, name, parameters, prelude, body, epilogue, str = $1, $2, $3, $4, $5, $6, $7
            yield(name, parse_ir_def(return_type, parameters, prelude, body, epilogue, str))
        else
            raise "Parse error: the general format is not correct"
        end
    end
end
