#####################################################################
# SSA                                                               #
#####################################################################

# propagate_defs operate on a single variable
# defs - set of definitions of this variable, in format [node, defined_inside]
def propagate_defs(defs, rin, rmid, rout, node)
    # Definitions that reach node entry are those that reach exit
    # of any of its predecessors
    new_node_rin = node.pr.inject(Set[]) {|s,cp| s | (rout[cp] || [])}
    # Definitions that reach inside of the node are either those that
    # reach its entry, or just the one that's defined in SSA-in, if any.
    if defs.include? [node, false]
        new_node_rmid = [[node, false]]
    else
        new_node_rmid = new_node_rin
    end
    # Definitions that reach exit of the node are either those that
    # reach its inside, or just the one that's defined by the inside, if any.
    if defs.include? [node, true]
        new_node_rout = [[node, true]]
    else
        new_node_rout = new_node_rmid
    end
    
    unless new_node_rin == rin[node] and new_node_rmid == rmid[node] and new_node_rout == rout[node]
        rin[node]  = new_node_rin
        rmid[node] = new_node_rmid
        rout[node] = new_node_rout
        node.nx.each{|cp,phi_out|
            propagate_defs(defs, rin, rmid, rout, cp)
        }
    end
end

def ssa!(code_root)
    # First, find all variables with multiple definitions
    definitions = Hash.new([])
    code_root.each_dfs{|cp|
        cp.defined_variables.each{|v,defined_inside|
            definitions[v] += [[cp,defined_inside]]
        }
    }
    variables_with_multiple_defs = []
    definitions.each{|variable,defs|
        variables_with_multiple_defs << variable if defs.size > 1
    }
    # OK, so now we have some variables with multiple definitions
    # We should check where they reach and whether phi-definitions are needed.
    #
    # A phi-definition is needed if:
    # * One of predecessors of a node have rout for V with just one definition
    # * Node rmid has more than one definition
    #
    # Of course that needs a proof, that if there is no such node, no more
    # definitions are necessary.
    #
    # The proof is very simple -
    # * outputs of definition nodes have one definitions
    # * problem nodes have two or more definitions
    # * for each problem node there exists at least one path from some definition
    #   node to a problem node (actually at least two, but it doesn't matter).
    #   Let's look at rout0-rin1-rout1-(rin2-rout2 ...)-rinN chain.
    #   As rout0.size = 1, and rinN.size >= 2. As routK.size == rinK.size (
    #   if the node doesn't define given variable) or routK.size == 1 (if it does),,
    #   the only possible way to increase the number is by increasing it
    #   on some rinK. And the first such increase gets a phi-definition.
    #   If there is no place to put new phi-definitions, then problem nodes
    #   cannot possibly exist.
    variables_with_multiple_defs.each{|variable|
        # Loop as long as new defs are being added
        while true
            defs = definitions[variable]
            # If only one definition reaches somewhere, just replace it
            rin = {}
            rmid = {}
            rout = {}
            propagate_defs(defs.to_set, rin, rmid, rout, code_root)

            phi_defs_added = false
            code_root.each_dfs{|cp|
                if rmid[cp].size >= 2 and cp.pr.any?{|p| rout[p].size <= 1}
                    cp.add_phi_def!(variable)
                    definitions[variable] << [cp, false]
                    phi_defs_added = true
                end
            }
            break unless phi_defs_added
        end
        # Allocate a new variable for each definition
        # After we're done, the old variable won't exist any more
        new_vars = {}
        defs.each{|d|
            new_vars[d] = Variable.fresh(variable.t)
        }
        code_root.each_dfs{|cp|
            new_def_mid = rmid[cp].to_a[0]
            new_def_out = rout[cp].to_a[0]
            # It's possible that no definition reaches some point.
            # It would be a compiler bug if a variable was actually used there,
            # as we add =0 initializations at the function entry for everything.
            new_def_mid = new_def_mid && new_vars[new_def_mid]
            new_def_out = new_def_out && new_vars[new_def_out]
            cp.replace_uses_of_variable!(variable, new_def_mid, new_def_out)
        }
        defs.each{|cp, defined_inside|
            cp.replace_def_of_variable!(variable, defined_inside, new_vars[[cp, defined_inside]])
        }
    }
end
