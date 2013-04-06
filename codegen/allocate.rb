#####################################################################
# REGISTER ALLOCATION                                               #
#####################################################################

def allocate!(need_allocation)
    order_of_allocation = []
    # FIXME: Order by same-type (scalar/vector) conflict - it doesn't matter how many variables
    # of the different type coexist, they use different registers anyway.
    #
    # NOTE: We do not throw an exception here if all variables have too many conflicts.
    # It would be possible, but in some more complex schemes we may not know it yet.
    # For example - if we had registers {Ri.xyz, Ri.x, Ri.y, Ri.z} etc. (with obvious conflicts),
    # then scalar variable having 30 conflicts can be allocable (if they're all scalars),
    # or not (if they're vectors). Or not if they're 30 scalars, but cannot be nicely packed
    # into 10 registers due to conflicts somewhere else etc.
    # We'll throw an exception later anyway.
    
    # Order of allocation:
    # * allocate earlier variables with many conflicts.
    # * allocate earlier variables with more friends.
    #   This makes the following allocation work:
    #   add a, X, Y
    #   mov b, a
    #   mov c, b
    #   If we allocate b first, a, b, and c will all end in the same register.
    #   If we allocate (a,c) first, a will end in different register than c,
    #   because it wants to avoid crowded registers.
    while need_allocation.size != 0
        conflicts = []
        enemies_count, friends_count, v, conflicts_of_v = *need_allocation.map{|v|
            conflicts_of_v = (v.enemies.intersection need_allocation)
            [conflicts_of_v.size, v.friends.size, v, conflicts_of_v]
        }.min
        #print "#{v} pushed with conflicts: #{conflicts_of_v.set_inspect}\n"
        need_allocation.delete(v)
        order_of_allocation.push(v)
    end
    #p order_of_allocation
    for v in order_of_allocation.reverse
        if v.t == :float # Scalar registers
            candidates = Machine_register.available_scalar_registers
        else # 3-element floating point registers
            candidates = Machine_register.available_vector_registers
        end
        # Reject R as a candidate allocation of V
        # if some variable that conflict with V has been allocated R
        #print "Candidates for #{v}: #{candidates.join ' '}\n"
        candidates = candidates.reject{|candidate| not (v.enemies.intersection candidate.rev_allocation).empty? }
        #print "Candidates for #{v}: #{candidates.join ' '}\n"
        raise "All allocation candidates for variable #{v} have been rejected" if candidates.size == 0
        candidate_scores = Hash.new() {|ht,k| ht[k] = [0, k.rev_allocation.size, k]}
        v.friends.each{|w|
            candidate_scores[w.allocation][0] -= 1 if w.allocation
        }
        candidates = candidates.select_by_min{|c| candidate_scores[c]}.sort

        mr = candidates[0]

        v.allocation = mr
    end
end
