#####################################################################
# Instruction Scheduling                                            #
#####################################################################

# FIXME: (GLOBAL) There is a major bug in the compiler.
# The bug:
#
#         /--------------->[NODE 4]
#  [NODE 1]           /---/
#         \-> [NODE 2]<-\
#            /           \
#            |           |
#            \->[NODE 3]-/
# Now let's break NODE 1 -> NODE 2 nx-link.
# Unfortunately there are still incoming links to NODE 2,
# So it doesn't get dropped. And as it doesn't NODE 4
# keeps NODE 2 in its pr. However, as NODE 2 is unreachable,
# it breaks many analyses, in particular instruction scheduling.
#
# The broken invariant is:
# * If X is in @pr of reachable node, X is reachable.
#
# As far as I can tell this assertion is only used
# in the scheduler, and we explicitly work around it there,
# so as far as I can tell it doesn't break anything,
# except for possibly making less optimizations possible.

class Basic_block
    attr_accessor :nx, :pr, :i
    def initialize
        @nx = []
        @pr = []
        @i  = []
    end
    def schedule!
        # Extract first node's @pr and last node's @nx
        # Everything else is just linking from @i[k] to @i[k+1],
        # so we can ignore it.
        @pr = @i[0].pr 
        @nx = @i[-1].nx
        
        # No need to do anything unless the block contains
        # at least two instructions
        return if @i.size <= 1

        # Build a dependency graph.
        # There are 3 types of cases where Y depends on X.
        # * X writes to R,  then Y reads from R
        # * X writes to R,  then Y writes to R
        # * X reads from R, then Y writes to R
        # In particular we need to be cautions about modifying S.*
        @dependencies = {}
        reg_us  = {}
        reg_mod = {}
        @i.each{|i|
            dep = []
            i.us.each{|v|
                r = (v.is_a?(Variable) ? v.allocation : v)
                dep += (reg_mod[r]||[])
            }
            i.mod.each{|v|
                r = (v.is_a?(Variable) ? v.allocation : v)
                dep += (reg_mod[r]||[])
                dep += (reg_us[r]||[])
            }
            i.us.each{|v|
                r = (v.is_a?(Variable) ? v.allocation : v)
                reg_us[r] ||= []
                reg_us[r] << i
            }
            i.mod.each{|v|
                r = (v.is_a?(Variable) ? v.allocation : v)
                reg_mod[r] ||= []
                reg_mod[r] << i
            }
            @dependencies[i] = dep.uniq
        }
        # @rev_dep reverses @dependencies
        # @dependencies[x] = [a, b]
        # @dependencies[y] = [b, c]
        # ->
        # @rev_dep[a] = [x, y]
        # @rev_dep[b] = [x]
        # @rev_dep[c] = [y]
        @rev_dep = {}
        @dependencies.each{|i,deps|
            deps.each{|d|
                @rev_dep[d] ||= []
                @rev_dep[d] << i
            }
        }

        # Remove redundant dependencies
        # If a -> b -> c
        #      \-----/
        # Then we can drop a -> c etc.
        
        # TODO: Actually remove redundant dependencies
        
        # Latency-to-end is a latency of executisg this instruction and
        # all instructions that depend on it (in parallel):
        # For example for this graph:
        #    /--> mul---
        # add           \
        #    \--> dp3 --> add --> END
        # latency-to-end values are:
        #    /-->  8 ---
        #  11           \
        #    \-->  8  -->  3  --> END
        # Use dynamic programming to compute latency_to_end
        # It is guaranteed to end as the dependency graph is a DAG

        latency_to_end = Hash.new {|ht,k|
            rd = @rev_dep[k]
            ht[k] = k.latency + (if rd then rd.map{|d| latency_to_end[d]}.max else 0 end)
        }
        
        #puts "\nDEPS\n"
        #@dependencies.each{|k, ds| p [k, ds]}
        #print "\nREV DEPS\n"
        #@rev_dep.each{|k,ds| p [k, ds]}
        #print "\n\n"
        
        # Do the actual scheduling
        schedule_me = @i.dup
        
        clock = 0
        ready_at_clock = {}
        new_i = []
        while not schedule_me.empty?
            raise "Clock >= 10000, probably a bug, schedule_me = #{schedule_me}" if clock >= 10000
            # ready contains opcodes that can be scheduled right now
            ready = schedule_me.select{|c|
                @dependencies[c].all?{|d|
                    ready_at_clock[d] and
                    ready_at_clock[d] <= clock
                }
            }
            # Nothing ready ? Wait a cycle and retry.
            # This is guaranteed to work (as long as everything has finite latency)
            # because the dependency graph is a DAG,
            # and the only reason ready can be empty
            # while schedule_me is not is that there are some instructions in run.
            if ready.size == 0
                clock += 1
                next
            end
            # Select an instruction to schedule
            if ready.size > 1
                # First criterion is latency to end.
                ready = ready.select_by_max{|i| latency_to_end[i]}
            end
            if ready.size > 1
                # Second criterion is number of dependent instructions
                ready = ready.select_by_max{|i| (@rev_dep[i]||[]).size }
            end
            if ready.size > 1
                # Third criterion is instruction's own latency
                ready = ready.select_by_max{|i| i.latency}
            end
            raise "Internal error: Ready set becabe empty after selecting best candidate" if ready.empty?
            # In case of a tie, just take the first one
            schedule_now = ready[0]
            
            new_i << schedule_now
            ready_at_clock[schedule_now] = clock + schedule_now.latency
            # Remove from the list of instructions to schedule
            schedule_me.delete schedule_now
            clock += 1
        end
        # new_i contains the new @i, just relink it
        @i = new_i
        (0..(@i.size-2)).each{|k|
            @i[k].nx   = [[@i[k+1]]]
            @i[k+1].pr = [@i[k]]
        }
        # SILLY: We'll overwrite it in reconnect! anyway, doesn't do anything
        @i[0].pr = @pr
        @i[-1].nx = @nx
    end
    def reconnect!(asm_in)
        @i[0].pr = @pr.map{|p|
            #raise "Opcode #{p} not in any basic block" unless asm_in[p]
            #asm_in[p].i[-1]

            # FIXME: pr can link to unreachable code.
            if asm_in[p] then asm_in[p].i[-1] else p end
        }
        @i[-1].nx = @nx.map{|p,*args|
            raise "Opcode #{p} not in any basic block" unless asm_in[p]
            [asm_in[p].i[0], *args]
        }
    end
end

def schedule!(asm_root)
    # First split the code into schedulable blocks.
    # They are almost basic blocks but not quite.
    # For example call/trace instructions split our blocks,
    # and cjmp gets its own block.
    # Generally any instruction that has "side effects"
    # (does something different that just modifying @df variables
    # based on @us variables) should break a block.
    
    # List of basic blocks
    basic_blocks = [nil]
    # Which asm op is in which bb
    asm_in = {}

    asm_root.each_dfs{|cp|
        # Start a basic block if needed
        basic_blocks[-1] = Basic_block.new unless basic_blocks[-1]

        unless basic_blocks[-1].i.empty? or cp.can_continue_basic_block?(basic_blocks[-1].i[-1])
            # Block is not fresh and cp cannot continue it - start a new one instead
            basic_blocks << Basic_block.new
        end
        basic_blocks[-1].i << cp
        asm_in[cp] = basic_blocks[-1]
        # Must finish already ?
        unless cp.can_have_basic_block_continuations?
            basic_blocks << nil
        end
    }
    basic_blocks.pop if basic_blocks[-1].nil?

    #asm_root.each_dfs{|c| p c}
    #basic_blocks.each{|bb| p bb}

    basic_blocks.each{|bb|
        bb.schedule!
    }
    basic_blocks.each{|bb|
        bb.reconnect!(asm_in)
    }
    # NOTE: The first basic block is guaranteed to contain only root_nop,
    # So asm_root is still valid.
end

# Basic-block-related methods of Asm
class Asm
    # Instructions can continue a basic block if:
    # * it has only one predecessor
    #   * and it is the last instruction of the current basic block
    # * is not call/trace/cjmp
    # * is not root_nop
    def can_continue_basic_block?(last_instruction_in_basic_block)
        return false unless @pr.size == 1
        return false unless @pr[0] == last_instruction_in_basic_block
        return false if [:call, :trace, :cjmp, :root_nop].include? @op
        return false if [:load, :texload].include? @op
        return true if [:mov_rsq, :mov_rcp].include? @op
        return true if [:mov, :nop, :mul, :add, :frac_minus,
                        :sub, :frac, :setx, :sety, :setz, :neg,
                        :dp3, :mul_m, :getx, :gety, :getz,
                        :get_hit_tri, :get_hit_obj, :get_hit_w]
        raise "I don't know whether #{@op} can continue basic block"
    end
    # Instruction can have others follow it in a basic block if:
    # * is not call/trace/cjmp
    # * is not root_nop
    # 
    # Instruction that can start a block, but cannot extend it:
    # * one with multiple predecessors
    # Instruction that can extend a block, but cannot have continuations:
    # * none (it our definition of a basic block at least)
    def can_have_basic_block_continuations?
        return false if [:call, :trace, :cjmp, :root_nop].include? @op
        return false if [:load, :texload].include? @op
        return true if [:mov_rsq, :mov_rcp].include? @op
        return true if [:mov, :nop, :mul, :add, :frac_minus,
                        :sub, :frac, :setx, :sety, :setz, :neg,
                        :dp3, :mul_m, :getx, :gety, :getz]
        raise "I don't know whether #{@op} can be followed by other instructions in a basic block"
    end
    # Technically the forbidden instructions have some well-defined latency,
    # but for a bug protection we're throwing an exception - the only
    # piece of code that is interested in latencies is scheduler
    # and they cannot be scheduled.
    #
    # NOTE: There is no basis whatsoever for the latencies given here.
    # Change them to whatever values fit the current hardware.
    #
    # NOTE: Please do not use insanely high values like 500 as
    # this will make performance bad. Also they need to be positive integers.
    def latency
        case @op
        when :call, :trace, :cjmp, :root_nop
            raise "Rescheduling #{@op} is illegal !"
        when :nop
            1
        when :mov, :mov_m, :setx, :sety, :setz, :setxy, :setxz, :setyz, :setx0, :setxy0, :setxz0
            2
        when :add
            3
        when :frac, :frac
            3
        when :mul, :dp3, :mad
            5
        when :mov_rsq, :mov_rcp, :dp3_rsq
            6
        # We can safely consider load/texload the slowest instruction
        when :load, :texload
            20
        else
            raise "Unknown operation #{@op}"
        end
    end
    attr_writer :nx, :pr
end

class Asm_Epilogue
    def can_continue_basic_block?(last_instruction_in_basic_block)
        false
    end
    def can_have_basic_block_continuations?
        false
    end
    def latency
        raise "Rescheduling Asm_Epilogue is illegal !"
    end
    attr_writer :nx, :pr
end
