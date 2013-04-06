#####################################################################
# IR(SSA)-LEVEL OPTIMIZATIONS                                       #
#####################################################################

# Assumes SSA form.
#
# Basically if we ever have:
# mov foo, bar
#
# Then we can safely replace all uses of foo, anywhere, with bar,
# and drop the mov.
#
# Now this may sound a bit hard to believe, so here's a proof.
# Let's say that there is a point where foo is used, but foo != bar.
#
# As all variables used are always initialized (THIS IS 
# IMPORTANT ASSUMPTION, THE COMPILER MAKES SURE THIS IS A CASE),
# the only possible reason is that definition of bar got executed again
# after 'mov foo, bar' was executed, and 'mov foo, bar' wasn't executed,
# that is:
#
# bar=X -> foo=bar -> bar=X' -> Z=foo
# 
# Due to "every used variable is initialized" assumption,
# bar=X can be reached from function entry without executing foo=bar
# (if foo=bar was reached first, it would evaluate uninitialized variable).
# And we assumed that Z=foo can be reached from bar=X without
# executing foo=bar. Z=foo can be reached from function entry without
# executing foo=bar. So foo can be used without being initialized,
# what breaks our rules.
#
# The mov can be dropped because all uses of foo anywhere have been replaced
# by uses of bar.
def copy_prop!(code_root)
    replacements = {}
    code_root.each_dfs{|cp|
        if cp.is_mov?
            t = cp.mov_target
            s = cp.mov_source
            # Replacements can be chained, like
            # mov b,a
            # mov c,b
            #
            # What executes
            # replacements[b] = replacements[a] || a
            # replacements[c] = replacements[b] || b
            # replacements == {b => a, c => a}
            replacements[t] = (replacements[s] || s)
            cp.make_nop!
        end
    }
    code_root.each_dfs{|cp|
        cp.replace_uses_of_variables!(replacements)
    }
end

#####################################################################
# COMMON SUBEXPRESSION ELIMINATION                                  #
#####################################################################
#
# In SSA form, if two definitions C, D are identical, and one is always
# dominated by other (D never reached unless C reached sometime before), like:
#     ...
#   add C, A, B
#     ...
#   add D, A, B
#     ...
# Then we can replace all uses of D by C and drop the second definition.
# The proof is very similar to the proof of the copy propagation.
def cse!(code_root)
    while true
        something_happened = false
        candidates = Hash.new{|ht,k| ht[k] = []}
        code_root.each_dfs{|cp|
           if cp.is_def?
               candidates[cp.def_dsc] << cp
           end
        }
        common_subexpressions = candidates.to_a.reject{|def_dsc, defs| defs.size <= 1}
        common_subexpressions.each{|def_dsc, defs|
            substitution_table = compute_cse_substitutions(code_root, defs)
            something_happened = true unless substitution_table.empty?
            code_root.each_dfs{|cp|
                cp.replace_uses_of_variables!(substitution_table)
            }
        }
        break unless something_happened
    end
end

# Compute which nodes can be reached from the root without
# going through definition.
def compute_nondominance(cp, definition, r)
    return if r[cp]
    return if cp == definition
    r[cp] = true
    cp.nx.each{|n,*args|
        compute_nondominance(n, definition, r)
    }
end

# Using nondominance, we can compute a set of dominant
# definitions and their shadowed definitions.
def compute_cse_substitutions(code_root, defs)
    nondominance = {}
    defs.each{|d|
       nondominance[d] = {}
       compute_nondominance(code_root, d, nondominance[d])
    }
    dominance = {}
    defs.each{|s|
        defs.each{|m|
            next if s == m
            next if nondominance[m][s]
            # s cannot be reached without going through m
            dominance[s] = m
            break
        }
    }
    # Now let's find the dominance leader of each dominance set
    total_dominance = Hash.new{|ht,k|
        dk = dominance[k]
        if dk
            ht[k] = total_dominance[dk] || dk
        else
            ht[k] = nil
        end
    }
    substitution_table = {}
    defs.each{|d|
        dd = total_dominance[d]
        if dd
            substitution_table[d.def_target] = substitution_table[dd.def_target] || dd.def_target
            d.make_nop!
        end
    }
    return substitution_table
end
