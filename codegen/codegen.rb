#!/usr/bin/ruby

# asm cannot handle "0.123e-6" notation
class Float
   def to_s
       (sprintf "%.15f", self).sub(/\.(.+?)0*$/){".#{$1}"}
   end
end

# Look for include files in the same directory as the executable
$:.unshift File.dirname(__FILE__)

require 'allocate.rb'
require 'ssa.rb'
require 'ir_optimize.rb'
require 'schedule.rb'
require 'irparser.rb'
require 'digest/md5'
require 'set'
require 'inline.rb'

# These are official lists of supported OPs.
# FIXME: Make sure the lists are complete and that they are fully supported. 
# FIXME: make texload fully supported
CODE_OPS = [:mov, :nop, :call, :mul, :add, :cjmp, :tuple, :sub, :dp3, :rcp, :rsq, :xpd, :surface_shader_init]

# TODO:
# Replace all the ops by a few _m-style ops
# mov/neg/getx/gety/getz -> mov
# mul/mul_m              -> mul
# frac/frac_m            -> frac
#
# TODO: - make _rcp _rsq more general, so we can do dp3_rsq etc.
# At minimum dp3_rsq must be supported, as it is used by length/normalize etc.:
#   # target = normalize(arg)
#   dp3_rsq R15.0, arg, arg
#   mul target, arg, S.w
#
# The following opcodes can have arbitrary source modifiers:
# * add, mul, frac, dp3, dp3_rsq, mad, mov_m
ASM_OPS = [:mov, :nop, :root_nop, :cjmp, :call,
           :mov_rcp, :mov_rsq,
           :trace,
           :setx,  :sety, :setz, :setxy,  :setxz, :setyz,
           :setx0,               :setxy0, :setxz0,
           :mov_m,
           :frac, :mul, :add, :dp3,
           :load, :texload,
           :dp3_rsq, :mad]

#####################################################################
# String hashes                                                     #
#####################################################################
# Strings inside shaders are represented by their hashes.
# As the only supported operations are ==, != and passing them around,
# this is sufficient.
# Of course there is a chance of collision, which is considerable if
# number of strings is about 2^(number of bits / 2), which is
# about 256 strings if we use 16-bit hashes,
# and about 4096 if we use 24-bit hashes.
#
# Compiler will warn you if two different strings in the same compilation
# end with the same hash.
#
# A hash is first N bits of MD5. Cryptographic strength of the hash
# function is irrelevant, it simply needs to be a random enough.


class String
    # Hash is first 24 bits of MD5
    def compiler_hash
        @@compiler_hash ||= {}
        
        h = @@compiler_hash[self]
        return h if h
        
        h = Digest::MD5.hexdigest(self)[0,6].hex.to_f
        @@compiler_hash[self] = h
        
        # No need to keep everything here, it's just to make sure some
        # warning is issued.
        @@reverse_compiler_hash ||= {}
        if @@reverse_compiler_hash[h]
           warn "Different strings: `#{@@reverse_compiler_hash[h]}' and `#{self}' have the same hash #{h}"
        end
        @@reverse_compiler_hash[h] = self
        
        return h
    end
end

#####################################################################
# ID pretty-printing                                                #
#####################################################################

class Hash
    def safe_to_a
        sz = keys.size
        (0...sz).map{|i|
            raise "Trying to convert hash to array, but its keys are not 0..N" unless include? i
            self[i]
        }
    end
end

class NilClass
    # nil can be used in place of any class, so make sure
    # their pretty_ids don't conflict
    def pretty_id
        -1
    end
    def set_inspect
        "nil"
    end
end

# Set mostly, but Arrays should magically work too
module Enumerable
    def set_inspect
        sort.join ' '
    end
end

class Object
    def pretty_id_domain
        @@pretty_id_domain ||= [{}, 0]
        @@pretty_id_domain
    end
    # NOTE:
    # Per-class pretty_id - it's not quite cool, because we sometimes
    # want X and Y<X to have the same ids and sometimes don't,
    # e.g. we definitely do not want all subclases of Object to share ids
    # but we'd like subclasses of Opcode to do so.
    # 
    # In such case overload pretty_id_domain
    def pretty_id
        domain = pretty_id_domain
        unless domain[0][object_id]
            domain[0][object_id] = domain[1]
            domain[1] += 1
        end
        domain[0][object_id]
    end
end

#####################################################################
# Auxiliary stuff                                                   #
#####################################################################

class Symbol
    include Comparable
    def <=>(b)
        to_s <=> b.to_s
    end
    def to_proc()
        lambda{|obj,*args| obj.send(self, *args)}
    end
end

class Array
    def mapsum(start_value)
        res = start_value
        each{|el| res += yield el}
        res
    end
    # Select all elements for which the block yields maximum value
    # [1,3,-3,2].select_by_max{|x| x.abs} => [3,-3]
    def select_by_max(&blk)
        vals = map(&blk)
        real_max = vals.max
        res = []
        vals.each_with_index{|v,i|
           res << self[i] if v == real_max
        }
        res
    end
    def select_by_min(&blk)
        vals = map(&blk)
        real_min = vals.min
        res = []
        vals.each_with_index{|v,i|
           res << self[i] if v == real_min
        }
        res
    end
end

# Connectible nodes are part of graphs
# @nx links forwards
# @pr links backwards
# @pre_nx is used during graph building
# 
# NOTE: If there are multiple links from A.nx to B,
# there should be multiple links from B.pr to A !!!
# FIXME: I'm not sure whether it's always true. 
module Connectible
    attr_reader :nx, :pr
    def dont_nx_link!
        link_nx{|i, *args| [i, *args]}
        @ignore_nx_link = true
    end
    # QA: Multiple links A->B generate multiple links B->A, good.
    def link_nx
        return if @ignore_nx_link
        @pre_nx.each {|i, *args| nx_add(*yield(i, *args)) }
        @pre_nx = nil
    end
    # QA: Multiple links A->B generate multiple links B->A, good.
    def nx_add(c, *args)
        @nx << [c, *args]
        c.pr << self if c
    end
    # TODO: @pr management should be automatic - too easy to introduce
    # bugs here.
    def drop_one_pr_and_stay(c)
        i = @pr.index(c)
        raise "Trying to drop <#{c}> from pr but it's not there: <#{@pr}>" unless i
        @pr.delete_at i
    end
    def drop_one_pr_and_die(c)
        drop_one_pr_and_stay(c)
        drop! if @pr.empty?
    end
    def each_dfs(visited = nil, &blk)
        visited = {} unless visited
        return if visited[self]
        visited[self] = true
        yield self
        @nx.each{|n, *args| n and n.each_dfs(visited, &blk) }
    end
    # NOTE: Does not change the links, only links annotations.
    # So no need to correct @pr
    def nx_map_args!
        @nx = @nx.map{|n,*args| [n, *yield(args)]}
    end
    # NODE: Should drop only nops and other useless stuff
    #                   ?-\
    # [NODE 1] -> [NODE] -> [NODE 3]
    # [NODE 2] -/
    #
    # Changed to:
    #                   ?-\
    # [NODE 1] -----------> [NODE 3]
    # [NODE 2] -----------/
    #
    # That is:
    # * every @nx to NODE changes to @nx to NODE 3
    # * every @pr
    def drop!
        raise "Trying to drop something with non-trivial link-out and non-empty pr: #{self}" unless (@nx.size == 1 and @nx[0].size == 1) or (@pr.empty?)
        # FIXME: Shouldn't we just ignore drop request instead ?
        raise "Trying to drop something that links back to inself" if @pr.include? self
        nx = @nx[0][0]

        # As there is only one nx-link from self to nx,
        # there is only one pr-link from nx to self
        nx.drop_one_pr_and_stay(self)
        # relink_forw haldles nx's @pr
        @pr.each{|prec|
            prec.relink_forw(self, nx)
        }
        @pr = []
        nx.drop! if nx.pr.empty?
    end
    def relink_forw(rel_now, rel_new)
        @nx = @nx.map{|r, *args|
            if r == rel_now
                 rel_new.pr << self
                 [rel_new, *args]
            else
                 [r, *args]
            end
        }
    end
end

module Liveness
    attr_reader :live_in, :live_out
    def propagate_live!
        new_live_out = @nx.mapsum(Set.new) {|c,*args| c.live_in }
        return if new_live_out == @live_out
        @live_out = new_live_out
        @live_in  = new_live_out - df + us(new_live_out)
        @pr.each(&:propagate_live!)
    end
    def initialize_live!
        @live_in  = us.to_set
        @live_out = Set.new
    end
end

class ValueObject
    def self.new(*args)
        @world ||= {}
        #print "Trying to new ValueObject: #{self}(#{args.inspect}) -> #{(@world.include? args) ? 'there' : 'not there'}\n"
        @world[args] = super(*args) unless @world.include? args
        @world[args]
    end
    def self.purge!
        @world = {}
    end
    # NOTE: ValueObject.mass_purge! is the only right way to do it.
    #       Should not be inherited.
    def self.purge_all!
        ObjectSpace.each_object(Class){|k|
            if k.ancestors.include? ValueObject
                k.purge!
            end
        }
    end
end

# FIXME: This is uncool, it includes:
# * IR-level variables,
# * virtual registers,
#
# Now the mapping between the first two is 1:1 in most cases
# (float, point/vector/normal, color, probably string_id also),
# but not all (1 matrix maps to a few 3/4-element registers,
# it's generally very hairy)
# Well, as we don't support matrices I guess we can pretend it's 1:1
class Variable < ValueObject
    attr_reader :t, :i
    attr_reader :allocation
    # MAYBE: It doesn't feel right to provide write access to undelying set,
    # addr_set :friends etc. would be better
    attr_reader :friends
    attr_reader :enemies
    # FIXME: Negative numbers suck, but we don't know what was the highest
    #        variable number in the source
    def self.fresh(type)
        @@fresh_variables_counter ||= -1
        if type == :float or type == :scalar
            new_var = new(:float, @@fresh_variables_counter)
        elsif type == :vector or type == :point or type == :color
            new_var = new(:point, @@fresh_variables_counter)
        else
            raise "Unknown fresh variable type: #{type}"
        end
        @@fresh_variables_counter -= 1
        return new_var
    end
    def allocation=(mr)
        raise "Can't reallocate variables" unless @allocation == nil
        @allocation = mr
        mr.rev_allocation.add self
    end
    # same_allocation(reg("S.w")) -> false
    # same_allocation of two unallocated variables -> false
    def same_allocation?(x)
        x.is_a? Variable and allocation and x.allocation and allocation == x.allocation
    end
    def initialize(t,i)
        raise "Unknown variable type: #{t}" unless [:float, :point, :color, :matrix, :string].include? t
        raise "Matrix variables not supported yet" if t == :matrix
        # String variables are just floats
        if t == :string
            t = :float
        end
        @t = t
        @i = i
        @allocation = nil
        @friends = Set.new
        @enemies = Set.new
    end
    include Comparable
    def <=>(v)
        return -1 if v.is_a? Machine_register
        [t, i] <=> [v.t, v.i]
    end
    def to_s
        return @allocation.to_s if @allocation

        # Use this only for debugging:

        short_name = {:point => "P", :color => "C", :float => "F"}
        "#{short_name[t]}#{i}" + (@allocation ? ":#{@allocation}" : "")
    end
    # Does it allocates to the same type of registers ?
    # so :point and :color have the same type
    def same_type?(x)
        allocate_to == x.allocate_to
    end
    def allocate_to
        return :scalar if t == :float
        return :vector if [:point, :color].include?(t)
        raise "Bad variable type: #{t}"
    end
    def inspect
        to_s
    end
end

# Variable types
def reg(nam)
    Machine_register.new(nam)
end
#####################################################################
# IR level                                                          #
#####################################################################
module PossibleLinkTarget
    attr_reader :ssa_in
    def add_phi_def!(v)
        @ssa_in << v
        @pr.each{|p|
            p.add_phi_out!(self, v)
        }
    end
end

module ConegenConnect
    def codegen_connect!
        @c.each{|cp|
            cp.link_nx{|i, *ssa_out|
                if i == :else or i == :next
                    target, *ssa_out = *@nx[0]
                    [unssa(ssa_out, target.ssa_in, target.codegen_start)]
                elsif i == :then
                    target, *ssa_out = *@nx[1]
                    [unssa(ssa_out, target.ssa_in, target.codegen_start)]
                # Internal links can't contain SSA anyway
                # FIXME: Maybe not true any more. We want to inline things on Code level etc.
                elsif i.is_a? Integer
                    [@c[i]]
                else
                    raise "Unknown connection type: #{i}"
                end
            }
        }
    end
    # FIXME: It can enter infinite recursion in highly pathological cases
    def codegen_start
        @c.size == 0 ? @nx[0][0].codegen_start : @c[0]
    end
end

module PossibleLinkSource
    def unssa(my_ssa_out, target_ssa_in, target_link)
        link = target_link
        raise "Source and target ssa don't match in size" unless my_ssa_out.size == target_ssa_in.size
        ssa_pairs = my_ssa_out.zip(target_ssa_in)
        # Assignments take place simultaneously, so we need to be careful
        # For example we could have:
        # a,b = b,a
        # and we can't convert it to sequential assignment (new temporaries needed)
        # Or we can but have to be careful like:
        # a,b,c = b,c,d
        
        ssa_do = []

        #print __LINE__, ": ", [my_ssa_out, target_ssa_in, link].inspect, "\n"
        while ssa_pairs.size != 0
            # Select a pair that we can assign before all other pairs
            dont_overwrite = ssa_pairs.map{|o,i| o}.to_set
            ssa_next_round = []
            ssa_pairs.each{|o,i|
                if dont_overwrite.include? i
                    ssa_next_round.push [o,i]
                else
                    ssa_do.push [o,i]
                end
            }
            if ssa_pairs.size == ssa_next_round.size
                # NOTE:
                # It is sometimes impossible to expand SSA without
                # introducing temporaries.
                # Introduce a new variable and rewrite:
                #        a,b = b,a ->
                # z = b; a,b = z,a ->
                # Then it becomes:
                # z = b; b = a; a = z
                # It has to finish if SSA was correct,
                # because that was the only definition of a (by definition of SSA)
                # and a was locking the SSA, so now we can reduce its size by at least one
                old_var,i = *ssa_pairs[0]
                new_var = Variable.fresh(i.allocate_to)
                ssa_do.push [old_var, new_var]
                ssa_pairs = ssa_pairs.map{|o,i| [(o == old_var ? new_var : o), i]}
            else
                ssa_pairs = ssa_next_round
            end
        end
        ssa_do.reverse.each{|o,i|
            new_link = Asm.mov(i, o, [link])
            new_link.dont_nx_link!
            #print "Adding #{new_link.pretty_id}: #{o.inspect}->#{i.inspect} SSA (next #{link.pretty_id})\n"
            link = new_link
        }
        return link
    end
end

class Code
    include Connectible
    include PossibleLinkTarget
    include PossibleLinkSource
    include ConegenConnect
    def op=(op)
        raise "Code op #{op} not supported" unless CODE_OPS.include? op
        @op = op
    end
    def initialize(op, ssa_in, args, *pre_nx)
        self.op = op
        @args   = args
        @pre_nx = pre_nx.map{|x| (x.is_a? Integer) ? [x] : x}
        @pr = []
        @nx = []
        @ssa_in = ssa_in
    end
    # Return pairs [variable, defined_inside]
    # defined_inside is true if the variable is defined truly inside
    # false if it's defined as ssa_in
    def defined_variables
        case @op
        when :mov, :sub, :add, :mul, :dp3, :xpd, :rcp, :rsq, :tuple
             [[@args[0], true]]
        when :call
             [[@args[2], true]]
        when :cjmp, :jmp, :nop
             []
        when :surface_shader_init
             @args
        else
            raise "I don't know what variables are defined by #{@op}"
        end + @ssa_in.map{|v| [v, false]}
    end
    def is_def?
        case @op
        when :mov, :nop, :call, :cjmp, :surface_shader_init
            false
        when :mul, :add, :tuple, :sub, :dp3, :rcp, :rsq, :xpd
            true
        else
            raise "Unknown op: #{@op}"
        end
    end
    def def_dsc
        case @op
        when :tuple
            [@op, @args[1], @args[2], @args[3]]
        when :rcp, :rsq
            [@op, @args[1]]
        when :sub, :xpd
            [@op, @args[1], @args[2]]
        when :mul, :add, :dp3
            # NOTE: We would like to have it independent of order of
            # @args[1] and @args[2], but that would require some trickery,
            # as we cannot just sort multitype containers (Floats + Variables).
            [@op, @args[1], @args[2]]
        when :mov, :nop, :call, :cjmp, :surface_shader_init
            raise "#{@op} is not a definition"
        else
            raise "Unknown op: #{@op}"
        end
    end
    def codegen!
        case @op
        when :mov
            @c = [
                Asm.mov(@args[0], @args[1], :next)
            ]
        when :add
            @c = [Asm.add(@args[0], @args[1], @args[2], :next)]
        when :mul
            @c = [Asm.mul(@args[0], @args[1], @args[2], :next)]
        when :dp3
            @c = [Asm.dp3(@args[0], @args[1], @args[2], :next)]
        when :sub
            @c = [Asm.sub(@args[0], @args[1], @args[2], :next)]
        when :xpd
            v1 = Variable.fresh(:vector)
            v2 = Variable.fresh(:vector)
            @c = [
                Asm.mul_m(v1, Source.swizzle(@args[1], "yzx"), Source.swizzle(@args[2], "zxy"), 1),
                Asm.mul_m(v2, Source.swizzle(@args[1], "zxy"), Source.swizzle(@args[2], "yzx"), 2),
                Asm.sub(@args[0], v1, v2, :next)
            ]
        when :rsq
            r = Asm.round_robin_s
            @c = [
                Asm.mov_rsq(r, @args[1], 1),
                Asm.mov(@args[0], reg("S.#{r}"), :next),
            ]
        when :rcp
            r = Asm.round_robin_s
            @c = [
                Asm.mov_rcp(r, @args[1], 1),
                Asm.mov(@args[0], reg("S.#{r}"), :next),
            ]
        when :cjmp
            @c = [
                Asm.new(:cjmp, @args, :else, :then)
            ]
        when :nop
            @c = [
                Asm.nop(:next),
            ]
        when :tuple
            # NOTE: target is not "defined" by any given instruction.
            # It is in partially-initialized state between setx and setz.
            # We are already past SSA at this point.

            # Common special cases are handled:
            # Simplify:
            #   vector a = vector(x, x, x);
            # To:
            #   vector a = x;
            # Especially for x = 0.0.
            # Also: vector(a, a, b), vector(a, b, a), vector(b, a, a).
            if @args[1] == @args[2] and @args[1] == @args[3]
                @c = [
                    Asm.mov(@args[0], @args[1], :next)
                ]
            elsif @args[1] == @args[2]
                @c = [
                    Asm.new(:setxy0, [@args[0], @args[1]], 1),
                    Asm.new(:setz, [@args[0], @args[3]], :next),
                ]
            elsif @args[1] == @args[3]
                @c = [
                    Asm.new(:setxz0, [@args[0], @args[1]], 1),
                    Asm.new(:sety, [@args[0], @args[2]], :next),
                ]
            elsif @args[2] == @args[3]
                @c = [
                    Asm.new(:setx0, [@args[0], @args[1]], 1),
                    Asm.new(:setyz, [@args[0], @args[2]], :next),
                ]
            else
                @c = [
                    Asm.new(:setx0, [@args[0], @args[1]], 1),
                    Asm.new(:sety, [@args[0], @args[2]], 2),
                    Asm.new(:setz, [@args[0], @args[3]], :next),
                ]
            end
        when :call
            fname, abi, target, *args = *@args
            @c = late_inline(fname, abi, target, args)
            unless @c
                # Warn if function is used first and defined later (or never)
                warn "Function #{fname} not known - assuming it's going to be linked" unless $seen_functions[fname]
                # Infer ABI based on number and types of arguments - it won't work in the future.
                # TODO: Move ABI calculations out
                scalar_args_cnt = 0
                vector_args_cnt = 0
                abi_arg_slots = []
                target_abi_type, *args_abi_types = *abi
                
                @c = []
                args.zip(args_abi_types).each{|real_arg, arg_abi_type|
                    if arg_abi_type == :scalar
                        abi_arg_slot = Variable.fresh(:scalar)
                        abi_arg_slot.allocation = reg("R#{scalar_args_cnt}.w")
                        scalar_args_cnt += 1
                    else
                        abi_arg_slot = Variable.fresh(:vector)
                        abi_arg_slot.allocation = reg("R#{vector_args_cnt}.xyz")
                        vector_args_cnt += 1
                    end
                    abi_arg_slots << abi_arg_slot
                    @c << Asm.mov(abi_arg_slot, real_arg, @c.size+1)
                }

                abi_target_slot = Variable.fresh(target_abi_type)
                if target.allocate_to == :scalar
                    abi_target_slot.allocation = reg("R0.w")
                else
                    abi_target_slot.allocation = reg("R0.xyz")
                end
                @c << Asm.new(:call, ["SUBROUTINE_ENTRY_#{fname}", abi_target_slot, *abi_arg_slots], @c.size+1)
                @c << Asm.mov(target, abi_target_slot, :next)
            end
        when :surface_shader_init
            v_dPdu, v_dPdv, v_Ng, v_N, v_u, v_v, v_P, v_Cs, v_s, v_t, v_I = *@args
           
            v_A = Variable.fresh(:vector)
            v_B = Variable.fresh(:vector)
            v_C = Variable.fresh(:vector)
            orig = Variable.fresh(:vector)
            orig.allocation = reg("R0.xyz")
            v_I.allocation = reg("R1.xyz")
            
            t0 = Variable.fresh(:scalar)
            t1 = Variable.fresh(:vector)
            t2 = Variable.fresh(:vector)
            t3 = Variable.fresh(:vector)
            t4 = Variable.fresh(:vector)
            t4l= Variable.fresh(:scalar)
            t5 = Variable.fresh(:scalar)

            r = Asm.round_robin_s

            @c = [
                # Load [A vertex data, B vertex data, C vertex data, Triangle data]
                Asm.new(:load, [reg("I0"), reg("HIT_TRI"), 0], 1),
                Asm.new(:mov, [reg("A"), reg("I0")], 2), 

                Asm.new(:load, [reg("I0"), reg("A0"), 0], 3),
                Asm.new(:load, [reg("I1"), reg("A1"), 0], 4),
                Asm.new(:load, [reg("I2"), reg("A2"), 0], 5),
                Asm.new(:load, [reg("I3"), reg("A3"), 0], 6),

                Asm.mov(v_A, reg("I0.xyz"), 7),
                Asm.mov(v_B, reg("I1.xyz"), 8),
                Asm.mov(v_C, reg("I2.xyz"), 9),
                
                # Calculate vectors between vertices
                Asm.sub(v_dPdu, v_B, v_A, 10),
                Asm.sub(v_dPdv, v_C, v_A, 11),

                # Calculate geometric normal Ng
                Asm.mul_m(t2, Source.swizzle(v_dPdu, "yzx"), Source.swizzle(v_dPdv, "zxy"), 12),
                Asm.mul_m(t3, Source.swizzle(v_dPdu, "zxy"), Source.swizzle(v_dPdv, "yzx"), 13),
                Asm.sub(t4, t2, t3, 14),

                # And normalize it
                Asm.dp3(t4l, t4, t4, 15),
                Asm.mov_rsq(r, t4l, 16),
                Asm.mul(v_Ng, t4, reg("S.#{r}"), 17),

                # Read u/v from HIT variable
                Asm.getx(v_u, reg("HIT"), 18),
                Asm.gety(v_v, reg("HIT"), 19),
                
                # Compute P based on HIT and orig(R0.xyz)/dir(R1.xyz) implicit arguments
                Asm.getz(t0, reg("HIT"), 20),
                Asm.mul(t1, v_I, t0, 21),
                Asm.add(v_P, orig, t1, 22),
                
                # TODO: No vertex normals yet, N == Ng
                Asm.mov(v_N, v_Ng, 23),
                
                # TODO: No texture data yet, s/t == 0.0
                Asm.mov(v_s, 0.0, 24),
                Asm.mov(v_t, 0.0, 25),
                
                # TODO: Only per-triangle color, no per-vertex color
                Asm.mov(v_Cs, reg("I3.xyz"), :next)
            ]
        else
            raise "Unknown opcode: #{@op}"
        end
    end
    def inspect
        "<Code @op=#{@op} @args=#{@args.inspect}>"
    end
    def add_phi_out!(target, v)
        @nx.each_with_index{|n,i|
            @nx[i] << v if n[0] == target
        }
    end
    def replace_def_of_variable!(variable, defined_inside, replacement_var)
        unless defined_inside
            @ssa_in = @ssa_in.map{|v| if v == variable then replacement_var else v end}
            return
        end
        case @op
        when :mov, :add, :sub, :mul, :dp3, :xpd, :rcp, :rsq, :frac, :tuple
            @args[0] = (if @args[0] == variable then replacement_var else @args[0] end)
        when :call
            @args[2] = (if @args[2] == variable then replacement_var else @args[2] end)
        when :surface_shader_init
            @args = @args.map{|a| if a == variable then replacement_var else a end}
        when :nop, :cjmp
             # Nothing defined
        else
            raise "No idea how to use replace_def_of_variable! on #{@op}"
        end
    end
    def replace_uses_of_variables!(replacements)
        replace = lambda{|i| if replacements[@args[i]] then @args[i] = replacements[@args[i]] end}
        case @op
        when :mov, :rcp, :rsq, :frac
             replace.call(1)
        when :add, :sub, :mul, :dp3, :xpd
             replace.call(1)
             replace.call(2)
        when :tuple
             replace.call(1)
             replace.call(2)
             replace.call(3)
        when :nop
             # Nothing to replace
        when :call
             (3...@args.size).each{|i| replace.call(i)}
        when :cjmp
             replace.call(1)
             replace.call(2)
        when :surface_shader_init
             # Nothing to replace
        else
            raise "No idea how to use replace_uses_of_variables on #{@op}"
        end
        nx_map_args!{|phi_out| phi_out.map{|v| replacements[v] || v}}
    end

    def replace_uses_of_variable!(variable, replacement_inside, replacement_phi_out)
        replace = lambda{|i| if @args[i] == variable then @args[i] = replacement_inside end}
        case @op
        when :mov, :rcp, :rsq, :frac
             replace.call(1)
        when :add, :sub, :mul, :dp3, :xpd
             replace.call(1)
             replace.call(2)
        when :tuple
             replace.call(1)
             replace.call(2)
             replace.call(3)
        when :nop
             # Nothing to replace
        when :call
             (3...@args.size).each{|i| replace.call(i)}
        when :cjmp
             replace.call(1)
             replace.call(2)
        when :surface_shader_init
             # Nothing to replace
        else
            raise "No idea how to use replace_uses_of_variable on #{@op}"
        end
        nx_map_args!{|phi_out| phi_out.map{|v| if v == variable then replacement_phi_out else v end}}
    end
    def is_mov?
        @op == :mov
    end
    def mov_target
        raise "mov_target called on something that's not a mov: #{self}" unless is_mov?
        @args[0]
    end
    def def_target
        raise "def_target called on something that's not a def: #{self}" unless is_def?
        @args[0]
    end
    def mov_source
        raise "mov_source called on something that's not a mov: #{self}" unless is_mov?
        @args[1]
    end
    # Instruction is useless, replace with a nop.
    # Do not call on a :cjmp or :call !
    def make_nop!
        self.op = :nop
        @args = []
    end
    # FIXME: (GLOBAL) We may produce unparsable output asm if there are any infinities anywhere
    # asm does not know how to parse Infinity/-Infinity/NaN
    def try_simpler_opcodes!
        # TODO: Optimize away phi-nodes if all pr but one got eliminated
        case @op
        when :mul
            if @args[1].is_a? Float and @args[2].is_a? Float 
                self.op = :mov
                @args = [@args[0], @args[1] * @args[2]]
            elsif @args[1].is_a? Float and @args[1] == 1.0
                self.op = :mov
                @args = [@args[0], @args[2]]
            elsif @args[2].is_a? Float and @args[2] == 1.0
                self.op = :mov
                @args = [@args[0], @args[1]]
            end
        when :add
            if @args[1].is_a? Float and @args[2].is_a? Float 
                self.op = :mov
                @args = [@args[0], @args[1] + @args[2]]
            elsif @args[1].is_a? Float and @args[1] == 0.0
                self.op = :mov
                @args = [@args[0], @args[2]]
            elsif @args[2].is_a? Float and @args[2] == 0.0
                self.op = :mov
                @args = [@args[0], @args[1]]
            end
        when :cjmp
            # TODO: OPTIMIZE ME
        when :tuple
            if @args[1] == @args[2] and @args[2] == @args[3]
                self.op = :mov
                @args = [@args[0], @args[1]]
            end
        when :sub
            if @args[1].is_a? Float and @args[2].is_a? Float 
                self.op = :mov
                @args = [@args[0], @args[1] - @args[2]]
            elsif @args[2].is_a? Float and @args[2] == 0.0
                self.op = :mov
                @args = [@args[0], @args[1]]
            end
        when :rcp
            if @args[1].is_a? Float
                self.op = :mov
                @args = [@args[0], 1.0/@args[1]]
            end
        when :rsq
            if @args[1].is_a? Float
                self.op = :mov
                @args = [@args[0], 1.0/Math.sqrt(@args[1])]
            end
        end
    end
end

class Code_Prelude
    include Connectible
    include ConegenConnect
    include PossibleLinkSource
    def is_def?
        false
    end
    def is_mov?
        false
    end
    def initialize(params, pre_nx)
        vector_registers = Machine_register.available_vector_registers
        scalar_registers = Machine_register.available_scalar_registers
        @c = []
        params.each{|p|
            case p.allocate_to
            when :scalar
                raise "Too many scalar parameters - can't allocate them all" if scalar_registers.size == 0
                abi_var = Variable.fresh(:scalar)
                abi_var.allocation = scalar_registers.shift
            when :vector
                raise "Too many vector parameters - can't allocate them all" if vector_registers.size == 0
                abi_var = Variable.fresh(:vector)
                abi_var.allocation = vector_registers.shift
            else
                raise "Bad allocation class: #{p.allocate_to}"
            end
            @c << Asm.mov(p, abi_var, @c.size+1)
        }
        @c << Asm.nop(:next)
        @pre_nx = [pre_nx]
        @nx = []
        @pr = []
    end
    # FIXME: This is totally *not* true, parameters and magic variables can be defined here
    # (or are they ?)
    def defined_variables
        []
    end
    # @c is generated in initialize
    def codegen!
    end
    def inspect_dfs
        res = ""
        each_dfs{|cp|
            res << cp.inspect << "\n"
        }
        res
    end
    def inspect
        "<Code_Prelude ...>"
    end
    def replace_uses_of_variable!(variable, replacement_inside, replacement_phi_out)
        nx_map_args!{|phi_out| phi_out.map{|v| if v == variable then replacement_phi_out else v end}}
    end
    def replace_uses_of_variables!(replacements)
        nx_map_args!{|phi_out| phi_out.map{|v| replacements[v] || v}}
    end
    def try_simpler_opcodes!
    end
end

class Code_Epilogue
    include Connectible
    include PossibleLinkTarget
    include ConegenConnect
    def is_def?
        false
    end
    def is_mov?
        false
    end
    # NOTE:
    # We can't simply allocate retval to R0.xyz / R0.w
    # Imagine a function:
    # float second(float a, float b) { return b; }
    # By ABI, a is R0.w and b is R1.w, and return value is R0.w
    # So we need to make a fresh scalar variable and allocate it to R0.w
    # In most cases, this mov will be optimized away.
    #
    # NOTE: One ugly detail - @input_retval originally gets
    # the right type, but copy propagation may break it, so we need
    # to remember its type in initializer.
    # Example: vector foo(float x) { return x; }
    def initialize(input_retval)
        @pr = []
        @input_retval = input_retval
        @retval_type = @input_retval.allocate_to
        # Asm.new(:mov, @args, [@args[0]], [@args[1]], :next)
        @pre_nx = []
        @nx = []
        @ssa_in = []
    end
    # No variables are defined in epilogue
    def defined_variables
        []
    end
    def link_nx
    end
    def codegen!
        real_retval = Variable.fresh(@retval_type)
        if @retval_type == :scalar
            real_retval.allocation = reg("R0.w")
        else # FIXME: Well, point/color are technically not the same thing
            real_retval.allocation = reg("R0.xyz")
        end
        @c = [
            Asm.mov(real_retval, @input_retval, 1),
            Asm_Epilogue.new(real_retval)
        ]
    end
    def inspect
        "<Code_Epilogue retval=#{@input_retval}>"
    end
    def replace_uses_of_variable!(variable, replacement_inside, replacement_phi_out)
        if @input_retval == variable
            @input_retval = replacement_inside
        end
    end
    def replace_uses_of_variables!(replacements)
        @input_retval = replacements[@input_retval] || @input_retval
    end
    def replace_def_of_variable!(variable, defined_inside, replacement_var)
        raise "Variable cannot be defined inside epilogue" if defined_inside
        @ssa_in = @ssa_in.map{|v| if v == variable then replacement_var else v end}
    end
    def try_simpler_opcodes!
    end
end

#####################################################################
# SOURCE, possibly with modifiers                                   #
#####################################################################

class Source
    # src.const? - is src constant ?
    # src.const?(5.0) - is src constant and equals 5.0 ?
    def const?(x=nil)
       @v.is_a? Float and (x.nil? or @v == x)
    end
    # TODO: Scalar swizzles are useless, maybe fix it ?
    # TODO: Should we try == with Source.new(other, 1.0, nil) if not a Source ?
    def ==(other)
        return false unless other.is_a? Source
        @v == other.variable and @m == other.m and @sw == other.sw
    end
    def initialize(v, m=1.0, sw=nil)
        @v = v
        @m = m
        @sw = sw
        
        if @v.is_a? Source
            @m *= @v.m
            raise "Illegal multiplier #{@m}" unless [-0.5, -1.0, -2.0, -4.0, 0.5, 1.0, 2.0, 4.0].include? @m
            @sw = Source.reswizzle(@sw, @v.sw)
            @v  = @v.variable
        end
        # Multipliers can be folded,
        # and swizzling scalars does not change them anyway.
        if @v.is_a? Float
            @v  = @v * @m
            @m  = 1.0
            @sw = nil
        end
        unless @v.is_a? Variable or @v.is_a? Float or @v.is_a? Machine_register
            raise "Only Variable, Float and Machine_register can be source base, not #{@v.class}"
        end
    end
    def self.try_new(*args)
        begin
            new(*args)
        rescue
            nil
        end
    end
    def to_s
        mt = {
             0.5 => "0.5*",
             1.0 => "",
             2.0 => "2*",
             4.0 => "4*",
            -0.5 => "-0.5*",
            -1.0 => "-",
            -2.0 => "-2*",
            -4.0 => "-4*",
        }
        ms = mt[@m] or raise "Multiplier #{@m} incorrect"
        if @sw
            vs = @v.to_s.dup
            if vs.sub!(/\.xyz$/, "") # If allocated to vector - good
                ms + vs + "." + @sw
            elsif vs.sub!(/\.w$/, "") # If scalar, cast + get* == mov
                ms + vs
            elsif vs =~ /\.$/
                raise "Internal error: Don't know how to #{@sw}-swizzle #{vs}"
            else # If not allocated - also good
                ms + vs + "." + @sw
            end
        else
            ms + @v.to_s
        end
        #raise "BAD #{inspect}" if r =~ /\..*\./
    end
    # Return new source or nil if substitution is impossible.
    # Example of impossible substitution:
    #  mov b, 0.5 * a
    #  mov c, 0.5 * b
    # ->
    #  mov c, 0.25 * a would be illegal
    def subst(new_v)
        if new_v.is_a? Variable or new_v.is_a? Float or new_v.is_a? Machine_register
            Source.new(new_v, @m, @sw)
        elsif new_v.is_a? Source
            new_m  = m * new_v.m
            return nil unless [-0.5, -1.0, -2.0, -4.0, 0.5, 1.0, 2.0, 4.0].include? new_m
            Source.new(new_v.variable, new_m, Source.reswizzle(@sw, new_v.sw))
        else
            raise "I don't know how to substitute #{new_v.class}"
        end
    end
    # Swizzle by sw2 first, sw1 second
    # Usage scenario:
    #  mov b, a.sw2
    #  mov c, b.sw1
    # ->
    #  mov c, a.sw3
    #
    # Example:
    #  Source.reswizzle("yzx", "yzx") -> "zxy"
    def self.reswizzle(sw1, sw2)
        return sw2 unless sw1
        return sw1 unless sw2
        swm = {"x" => 0, "y" => 1, "z" => 2}
        sw1 =~ /^([xyz])([xyz])([xyz])$/ or raise "Incorrect swizzle: #{sw1}"
        sw1n = [$1, $2, $3].map{|i| swm[i]}

        sw2 =~ /^([xyz])([xyz])([xyz])$/ or raise "Incorrect swizzle: #{sw2}"
        sw2n = [$1, $2, $3].map{|i| swm[i]}
        
        swmr = ["x", "y", "z"]
        return sw2n.map{|i| swmr[sw1n[i]]}.join
    end
    def variable
        @v
    end
    attr_reader :sw
    attr_reader :m
    def self.neg(v)
        new(v, -1.0, nil)
    end
    def self.swizzle(v, sw)
        new(v, 1.0, sw)
    end
end

#####################################################################
# ASM level                                                         #
#####################################################################

class Asm
    # This way we make sure drop!s do not interfere with each_dfs
    # drop!ing a node can change other node's nx,
    # and who knows what happens if we were in the middle of iterating
    # that nx that just changed.
    def drop_dfs_if!
        drop_list = []
        each_dfs{|a| drop_list << a if yield a}
        drop_list.each{|a| a.drop!}
    end
    def compute_enemies!
        each_dfs{|a|
            lo = a.live_out
            lo.each{|x|
                lo.each{|y|
                    next if x == y
                    # FIXME: Does it make sense ?
                    next if x.is_a? Machine_register or y.is_a? Machine_register
                    next unless x.same_type? y
                    x.enemies.add y
                }
            }
        }
    end
    def nop!
        self.op = :nop
        @args = []
    end

    # NOTE: to_s_and_liveness deleted as it is really ugly to keep
    #       two almost-identical printing function.
    #       Reimplement to_s_and_liveness if ever needed.
    #def print_code_and_liveness
    #    output = []
    #    expected = self
    #    referred_labels = Set.new
    #    each_dfs{|a|
    #        referred_labels += a.extra_referred_labels
    #        unless expected == a or not expected
    #            referred_labels.add expected.pretty_id
    #            output.push [:code, nil, "    jmp L#{expected.pretty_id}\n"]
    #        end
    #        expected = a.expected_next
    #        output.push [:label, a.pretty_id, "L#{a.pretty_id}:\n"]
    #        output.push [:code, nil, a.to_s_and_liveness]
    #    }
    #    if expected
    #        output.push [:code, nil, "    jmp L#{expected.pretty_id}\n"]
    #        referred_labels.add expected.pretty_id
    #    end
    #    output.each{|c,a,p|
    #        next if c == :label and not referred_labels.include? a
    #        print p
    #    }
    #end

    def print_code(outfile=STDOUT)
        output = []
        expected = self
        referred_labels = Set.new
        each_dfs{|a|
            referred_labels += a.extra_referred_labels
            unless expected == a or not expected
                referred_labels.add expected.pretty_id
                output.push [:code, nil, "    jmp L#{expected.pretty_id}\n"]
            end
            expected = a.expected_next
            output.push [:label, a.pretty_id, "L#{a.pretty_id}:\n"]
            output.push [:code, nil, "#{a}"]
        }
        if expected
            output.push [:code, nil, "    jmp L#{expected.pretty_id}\n"]
            referred_labels.add expected.pretty_id
        end
        output.each{|c,a,p|
            next if c == :label and not referred_labels.include? a
            outfile.print p
        }
    end
    def self.round_robin_s
        @round_robin_s_allocator ||= 3
        @round_robin_s_allocator  += 1
        @round_robin_s_allocator  %= 4
        return ["x", "y", "z", "w"][@round_robin_s_allocator]
    end
    def self.mov_rsq(r, src, nx)
        Asm.new(:mov_rsq, [r, src], nx)
    end
    def self.mov_rcp(r, src, nx)
        Asm.new(:mov_rcp, [r, src], nx)
    end
    def self.nop(nx)
        new(:nop, [], nx)
    end
    def self.mov(target, source, nx)
        new(:mov, [target, source], nx)
    end
    def self.frac(target, source, nx)
        new(:frac, [target, Source.new(source)], nx)
    end
    def self.frac_minus(target, source, nx)
        new(:frac, [target, Source.neg(source)], nx)
    end
    def self.neg(target, source, nx)
        new(:mov_m, [target, Source.neg(source)], nx)
    end
    def self.getx(target, source, nx)
        new(:mov_m, [target, Source.new(source, 1.0, "x")], nx)
    end
    def self.gety(target, source, nx)
        new(:mov_m, [target, Source.new(source, 1.0, "y")], nx)
    end
    def self.getz(target, source, nx)
        new(:mov_m, [target, Source.new(source, 1.0, "z")], nx)
    end
    def self.add(target, source1, source2, nx)
        new(:add, [target, Source.new(source1), Source.new(source2)], nx)
    end
    def self.sub(target, source1, source2, nx)
        new(:add, [target, Source.new(source1), Source.neg(source2)], nx)
    end
    def self.mul(target, source1, source2, nx)
        new(:mul, [target, Source.new(source1), Source.new(source2)], nx)
    end
    # Call if source1/source2 already Source, not Variable/Float
    def self.mul_m(target, source1, source2, nx)
        new(:mul, [target, source1, source2], nx)
    end
    def self.dp3(target, source1, source2, nx)
        new(:dp3, [target, Source.new(source1), Source.new(source2)], nx)
    end
    def pretty_id_domain
        $asm_pretty_id_domain ||= [{}, 0]
        $asm_pretty_id_domain
    end
    include Connectible
    include Liveness
    attr_reader :args
    def nop?
        (@op == :nop) or (@op == :mov and args[0].same_allocation? args[1])
        #false to treat nops as regular instructions
    end
    def mov?
        @op == :mov
    end
    def root_nop?
        (@op == :root_nop)
    end
    attr_reader :nx
    def op=(op)
        raise "Asm op #{op} not supported" unless ASM_OPS.include? op
        @op = op
    end

    def us(cond=nil)
        case @op
        when :nop, :root_nop
            []
        when :mov, :frac, :mov_m
            if cond.nil? or cond.include? @args[0]
                [@args[1]]
            else
                []
            end
        when :mul, :add, :dp3
            if cond.nil? or cond.include? @args[0]
                [@args[1], @args[2]]
            else
                []
            end
        when :setx, :sety, :setz, :setxy, :setxz, :setyz
            if cond.nil? or cond.include? @args[0]
                [@args[0], @args[1]]
            else
                []
            end
        when :setx0, :setxy0, :setxz0
            if cond.nil? or cond.include? @args[0]
                [@args[1]]
            else
                []
            end
        when :cjmp
            if @args[0] == :ge0
                [@args[1]]
            else 
                [@args[1], @args[2]]
            end
        when :call
            @args[2..-1]
        when :mov_rcp, :mov_rsq
            [@args[1]]
        when :mad
            [@args[1], @args[2], @args[3]]
        when :dp3_rsq
            [@args[1], @args[2]]
        when :trace
            [@args[1], @args[2], @args[3]]
        when :load
            if @args[1] == reg("HIT_TRI") or @args[1] == reg("HIT_OBJ")
                [@args[1]]
            else # using A0 means using A
                [@args[1], reg("A")]
            end
        when :texload
            raise "Sorry, I don't know what texload uses"
        else
            raise "Unknown asm opcode #{@op}"
        end.map{|a|
            if a.is_a? Source
                a.variable
            else
                a
            end
        }.reject{|a| a.is_a? Float}
    end
    def df
        case @op
        when :mov, :frac, :mov_m, :mul, :add, :setx, :sety, :setz, :setxy, :setxz, :setyz, :dp3, :setx0, :setxy0, :setxz0, :mad
            [@args[0]]
        when :nop, :root_nop, :cjmp
            []
        when :call
            [@args[1]]
        when :mov_rcp, :mov_rsq
            [reg("S.#{@args[0]}")]
        when :dp3_rsq
            [reg("S.#{@args[0]}")]
        when :trace
            []
        when :load
            # Loading to I0 means loading to I0.xyz and I0.w
            [@args[0], reg(@args[0].name + ".xyz"), reg(@args[0].name + ".w")]
        when :texload
            raise "Sorry, I don't know what texload defines"
        else
            raise "Unknown asm opcode #{@op}"
        end
    end
    def mod
        case @op
        when :mov, :frac, :mov_m, :mul, :add, :setx, :sety, :setz, :setxy, :setxz, :setyz, :dp3, :setx0, :setxy0, :setxz0, :mad
            []
        when :nop, :root_nop, :cjmp
            []
        when :call
            # This is hopefully the whole list
            [reg("S.x"), reg("S.y"), reg("S.z"), reg("S.w"),
             reg("HIT.w"), reg("HIT_TRI"), reg("HIT_OBJ"),
             reg("I0"), reg("I1"), reg("I2"), reg("I3"),
             reg("I0.xyz"), reg("I1.xyz"), reg("I2.xyz"), reg("I3.xyz"),
             reg("I0.w"), reg("I1.w"), reg("I2.w"), reg("I3.w"),
             reg("A"),
             reg("A.x"), reg("A.y"), reg("A.z"), reg("A.w"),
             reg("A0"), reg("A1"), reg("A2"), reg("A3"),
             ]
        when :mov_rcp, :mov_rsq, :dp3_rsq
            []
        when :trace
            [reg("HIT.w"), reg("HIT_TRI"), reg("HIT_OBJ")]
        when :load
            [reg("I0.xyz"), reg("I0.w")]
        when :texload
            raise "Sorry, I don't know what texload modifies"
        else
            raise "Unknown asm opcode #{@op}"
        end + df
    end
    
    def initialize(op, args, *pre_nx)
        self.op = op
        
        @args = args
        @pre_nx = pre_nx

        @pr = []
        @nx = []

        @live_in  = us.to_set
        @live_out = Set.new()
    end
    def set_allocation!
        @args = @args.map{|a| (a.is_a? Variable and a.allocation) ? a.allocation : a}
    end
    # default next
    def expected_next
        @nx[0][0]
    end
    #def to_s
    #    to_s_simple.sub(/\n/) { " # {" + @assertions.sort.map{|v,a| "#{v} is #{a.inspect}"}.join(" ") + "}\n" }
    #end

    def to_s
        return "" if root_nop?
        return "    nop\n" if nop?
        case @op
        when :nop
            "    nop\n"
        when :cjmp
            case @args[0]
            when :ge
            raise "Only scalars can be compared by >=: #{@args[1]}" unless @args[1].is_a? Float or @args[1].to_s =~ /\.[xyzw]$/
            raise "Only scalars can be compared by >=: #{@args[2]}" unless @args[2].is_a? Float or @args[2].to_s =~ /\.[xyzw]$/
            "    add R15.w, #{@args[1]}, -#{@args[2]} + jmp L#{@nx[1][0].pretty_id}, w (>=0)\n"
            when :ge0
            raise "Only scalars can be compared by >=: #{@args[1]}" unless @args[1].is_a? Float or @args[1].to_s =~ /\.[xyzw]$/
            "    mov R15.w, #{@args[1]} + jmp L#{@nx[1][0].pretty_id}, w (>=0)\n"
            when :eq
            # TODO: Check == on strings, floats, and vectors
            "    add R15.w, #{@args[1]}, -#{@args[2]} + jmp L#{@nx[1][0].pretty_id}, w (=0)\n"
            else
                 raise "Unknown comparison mode: #{@args[0]}"
            end
        when :mov_m
            "    mov #{@args[0]}, #{@args[1]}\n"
        when :setx, :setx0
            if @args[0].allocation
                vr = @args[0].to_s.dup
                vr.sub!(/\.xyz$/,"") or raise "setx target is not .xyz variable: #{@args[0]}"
            else
                vr = @args[0].to_s
            end
            "    mov #{vr}.x, #{@args[1]}\n"
        when :sety
            if @args[0].allocation
                vr = @args[0].to_s.dup
                vr.sub!(/\.xyz$/,"") or raise "#{@op} target is not .xyz variable: #{@args[0]}"
            else
                vr = @args[0].to_s
            end
            "    mov #{vr}.y, #{@args[1]}\n"
        when :setz
            if @args[0].allocation
                vr = @args[0].to_s.dup
                vr.sub!(/\.xyz$/,"") or raise "#{@op} target is not .xyz variable: #{@args[0]}"
            else
                vr = @args[0].to_s
            end
            "    mov #{vr}.z, #{@args[1]}\n"
        when :setxy, :setxy0
            if @args[0].allocation
                vr = @args[0].to_s.dup
                vr.sub!(/\.xyz$/,"") or raise "#{@op} target is not .xyz variable: #{@args[0]}"
            else
                vr = @args[0].to_s
            end
            "    mov #{vr}.xy, #{@args[1]}\n"
        when :setxz, :setxz0
            if @args[0].allocation
                vr = @args[0].to_s.dup
                vr.sub!(/\.xyz$/,"") or raise "#{@op} target is not .xyz variable: #{@args[0]}"
            else
                vr = @args[0].to_s
            end
            "    mov #{vr}.xz, #{@args[1]}\n"
        when :setyz
            if @args[0].allocation
                vr = @args[0].to_s.dup
                vr.sub!(/\.xyz$/,"") or raise "#{@op} target is not .xyz variable: #{@args[0]}"
            else
                vr = @args[0].to_s
            end
            "    mov #{vr}.yz, #{@args[1]}\n"
        when :call
        # NOTE: This code isn't really optimal, but it works.
        # Some examples:
        # * [R4.w, R3.xyz, R3.w] need saving.
        #   Allocating them to save registers in this order gets us:
        #   > mov S0.w, R4.w
        #   > mov S0.xyz, R3.xyz
        #   > mov S1.w, R3.w
        #   If we were smarter we'd allocate:
        #   > mov S1.w, R4.w
        #   > mov S0.xyz, R3.xyz
        #   > mov S0.w, R3.w
        #   Then the scheduler could in principle merge the instructions:
        #   > mov S1.w, R4.w
        #   > mov S0.xyzw, R3.xyzw
        # * Long-lived variable R8.w needs saving across two function calls.
        #   Right now we do:
        #   > save
        #   > call
        #   > restore
        #   > save
        #   > call
        #   > restore
        #   The middle save/restore part just wastes cycles.
        #   etc.
        preserved_scalars = 0
        preserved_vectors = 0
        preserve = ""
        restore = ""
        (@live_in & @live_out).sort.each{|r|
            if r.t == :float
                preserve << "    mov S#{preserved_scalars}.w, #{r}\n"
                restore  << "    mov #{r}, S#{preserved_scalars}.w\n"
                preserved_scalars += 1
            else
                preserve << "    mov S#{preserved_vectors}.xyz, #{r}\n"
                restore  << "    mov #{r}, S#{preserved_vectors}.xyz\n"
                preserved_vectors += 1
            end
        }
        "#{preserve}    call #{@args[0]} push #{[preserved_scalars, preserved_vectors].max}\n#{restore}"
        # Well, other _m instructions can fit here too
        when :mov, :add, :mul, :frac, :dp3, :mad
        "    #{@op} #{@args.join ', '}\n"
        when :mov_rcp, :mov_rsq
        "    #{@op} R15.#{@args[0]}, #{args[1]}\n"
        when :dp3_rsq
        "    #{@op} R15.#{@args[0]}, #{args[1]}, #{args[2]}\n"
        when :load
        "    #{@op} #{@args[0]}, #{args[1]}, #{args[2]}\n"
        when :trace
        "    trace #{@args[1,3].join ', '}\n" +
        "    mov #{@args[0]}, HIT.xyz\n"
        else # Everything should go onto the list above
        warn "Automatic generation for #{@op}"
        "    #{@op} #{@args.join ', '}\n"
        end
    end
    def inspect
        "<Asm: #{@op} #{@args.join ', '} LI={#{@live_in.set_inspect}} LO={#{@live_out.set_inspect}}>"
    end
    # Which labels except for default are used
    def extra_referred_labels
        if @op == :cjmp
            [@nx[1][0].pretty_id]
        else
            []
        end
    end
    def each_variable
        each_dfs{|a|
            (a.live_out + a.df).each{|v| yield v if v.is_a? Variable}
        }
    end
    def has_side_effects?
        return true if [:cjmp, :call, :trace].include? @op
        # mov_rcp/mov_rsq compute S.* based of argument,
        # that doesn't count as side effects
        return false if [:mov_rcp, :mov_rsq, :dp3_rsq].include? @op
        # Load is defined as computation.
        # This means we can eliminate load if we don't use
        # the loaded value. This kind of optimization isn't 100%
        # semantics-preserving, as it can accidentally *eliminate*
        # (but not introduce) local equivalent of segmentation faults.
        return false if [:load].include? @op
        # FIXME: Do setx/sety/setz/get_tri_hit have "side-effects" ?
        # What's the definition anyway ?
        return true if [:setx, :sety, :setz, :setxy, :setxz, :setyz, :setx0, :setxy0, :setxz0, :texload].include? @op
        return false if [:mov, :mov_m, :add, :mul, :dp3, :nop, :root_nop, :frac, :mad].include? @op
        # NOTE: Side effects are defined as any effect except for
        #       writing to target register and incrementing program counter by 1.
        #       We can return true conservatively, but it's better
        #       to decide explicitly in each case.
        #       If something doesn't have side effects and
        #       its results aren't used we throw it away.
        raise "I don't know whether #{@op} has side effect or not"
    end
    def remove_assertions!
        @assertions = {}
    end
    # NOTE: We only have one type of assertions is-assertions
    # They state that "a is b".
    #
    # Because we don't have :fld, :cast anymore, and we do copy propagation,
    # it does little except for S.w forwarding, unless some other optimization
    # introduces code assertions on which we'd like to propagate.
    def propagate_assertions!(visited={}, assertions_in={})
        return if visited[self]
        visited[self] = true
    
        @assertions = assertions_in
        assertions_out = {} 
        # NOTE: Assertions can get broken if we redefine asserted variable or asserted value holder:
        #       a = b [a=b]         a = b [a=b]
        #       a = c [broken]      b = c [broken]
        
        # As for call, it "preserves" everything except for S.*/A/I*/etc. and target, and all are in its @mod
        assertions_in.each{|v,a| assertions_out[v] = a unless (mod.include? v or a[1..-1].any?{|ai| mod.include? ai}) }
        # Exclude allocated (ABI) variables.
        # They are assigned out and die. Their lives must not be prolonged,
        # as it could introduce allocation conflicts.
        #
        # call f
        # mov X, A:R0.w
        # call g ; live instructions saving handled lated
        # mov Y, B:R0.w
        # add Z, X, Y
        # It's not ok to replace the last instruction with `add Z, A:R0.w, B:R0.w'
        # However, as B:R0.w and Y will probably end in the same register,
        # it pretty much does the trick anyway.
        if @op == :mov and ((not @args[1].is_a? Variable) or @args[1].allocation == nil) and (@args[0] != reg("A.x") and @args[0] != reg("A.y") and @args[0] != reg("A.z") and @args[0] != reg("A.w"))
            assertions_out[@args[0]] = [:is, @args[1]]
        end
        if @op == :mov_m and ((not @args[1].variable.is_a? Variable) or @args[1].variable.allocation == nil) and (@args[0] != reg("A.x") and @args[0] != reg("A.y") and @args[0] != reg("A.z") and @args[0] != reg("A.w"))
            assertions_out[@args[0]] = [:is_m, @args[1]]
        end
        if @op == :dp3 and ((not @args[1].variable.is_a? Variable) or @args[1].variable.allocation == nil) and ((not @args[2].variable.is_a? Variable) or @args[2].variable.allocation == nil)
            assertions_out[@args[0]] = [:is_dp3, @args[1], @args[2]]
        end
        if @op == :mul and ((not @args[1].variable.is_a? Variable) or @args[1].variable.allocation == nil) and ((not @args[2].variable.is_a? Variable) or @args[2].variable.allocation == nil)
            assertions_out[@args[0]] = [:is_mul, @args[1], @args[2]]
        end

        # Start with a fresh set in each basic block
        # Actually subblocks get parent's assertion set,
        # like B and C get A's, but D doesn't (yet) in "A if(x) {B} {C}; D"
        @nx.each{|n,*args|
            if n.pr.size == 1
                n.propagate_assertions!(visited, assertions_out)
            else
                n.propagate_assertions!(visited)
            end
        }
    end
    # Forward use of constants
    def use_assertions!
        # Arguments of calls are just liveness informations,
        # not real arguments.
        return false if @op == :call
        
        target, *sources = *@args
        new_sources = sources.map{|v|
            if [:mov_m, :frac, :add, :mul, :dp3, :getx, :gety, :getz, :mad].include? @op
                vv = v.variable
                if @assertions[vv] and (@assertions[vv][0] == :is or @assertions[vv][0] == :is_m)
                   nv = v.subst(@assertions[vv][1])
                   nv || v
                else
                    v
                end
            elsif @assertions[v] and @assertions[v][0] == :is
                @assertions[v][1]
            else
                v
            end
        }
        if new_sources != sources
            *@args = target, *new_sources
            try_simpler_opcodes!
            @live_in = us
            return true
        end
        # Check if we can use mad/dp3_rsq
        if @op == :mov_rsq
            a = @assertions[@args[1]]
            if a and a[0] == :is_dp3
                 self.op = :dp3_rsq
                 @args   = [@args[0], a[1], a[2]]
                 return true
            end
        end
        if @op == :add
            # NOTE:
            # It is usually possible to make a mad even
            # if sources of add are modified
            # Example 1:
            #   mul X, -A, 4*B
            #   add Y, 2*X, D
            # =>
            #   mad Y, -2*A, 4*B, D
            #
            # Example 2:
            #   mul X, -A, 4*B
            #   add Y, 2*X.yzx, D
            # =>
            #   mad Y, -2*A.yzx, 4*B.yzx, D
            #
            # Swizzling and negations always work.
            # Multipliers can go out of range, in which case
            # we do not substitute:
            #   mul X, 4*A, 2*B
            #   add Y, 4*X, D
            # =>
            #   mad X, 8*A, 4*B, D ; wrong
            # We try applying multipliers in both positions, and even spliting it:
            #   mul X, 2*A, 2*B
            #   add Y, 4*X, D
            # =>
            #   mad X, 4*A, 4*B, D           
            
            # add is symetric, try substitution of mad at both positions
            [[@args[1], @args[2]], [@args[2], @args[1]]].each{|arg, other|
                a = @assertions[arg.variable]
                if a and a[0] == :is_mul
                     # Check if source is unmodified by multipliers/swizzling
                     v  = arg.variable
                     m  = arg.m
                     sw = arg.sw
                     
                     # Try applying multiplier to the first argument
                     a1, a2 = Source.try_new(a[1], m, sw), Source.try_new(a[2], 1.0, sw)
                     # Then to the second
                     a1, a2 = Source.try_new(a[1], 1.0, sw), Source.try_new(a[2], m, sw) unless (a1 and a2)
                     # Then try spliting it (if m==4.0 or m==-4.0)
                     a1, a2 = Source.try_new(a[1], m/2.0, sw), Source.try_new(a[2], 2.0, sw) unless (a1 and a2 and m!=4.0 and m!=-4.0)
                     
                     if a1 and a2
                         self.op = :mad
                         @args   = [@args[0], a1, a2, other]
                         return true
                     end
                end
            }
        end
        return false
    end
    # FIXME: This function should be triggered automatically only (and always)
    def try_simpler_opcodes!
        something_changed = false
        case @op
        when :mov_m
            if @args[1].const?
                self.op = :mov
                @args = [@args[0], @args[1].variable]
                something_changed = true
            end
        when :add
            if @args[1].const? and @args[2].const?
                self.op = :mov
                @args = [@args[0], @args[1].variable + @args[2].variable]
                something_changed = true
            elsif @args[1].const?(0.0)
                self.op = :mov_m
                @args = [@args[0], @args[2]]
                something_changed = true
            elsif @args[2].const?(0.0)
                self.op = :mov_m
                @args = [@args[0], @args[1]]
                something_changed = true
            elsif @args[1] == @args[2]
                sn = Source.try_new(@args[1], 2.0)
                if sn
                    self.op = :mov_m
                    @args = [@args[0], sn]
                    something_changed = true
                end
            end
        when :frac
            if @args[1].const?
                self.op = :mov
                v =  @args[1].variable
                @args = [@args[0], v - v.floor]
                something_changed = true
            end
        when :mul
            if @args[1].const? and @args[2].const?
                self.op = :mov
                @args = [@args[0], @args[1].variable * @args[2].variable]
                something_changed = true
            elsif [-0.5, -1.0, -2.0, -4.0, 0.5, 1.0, 2.0, 4.0].include? @args[2].variable
                sn = Source.try_new(@args[1], @args[2].variable)
                if sn
                    self.op = :mov_m
                    @args = [@args[0], sn]
                    something_changed = true
                end
            elsif [-0.5, -1.0, -2.0, -4.0, 0.5, 1.0, 2.0, 4.0].include? @args[1].variable
                sn = Source.try_new(@args[2], @args[1].variable)
                if sn
                    self.op = :mov_m
                    @args = [@args[0], sn]
                    something_changed = true
                end
            end
        # NOTE: It must not break @pr
        when :cjmp
            if @args[0] == :ge0 and @args[1].is_a? Float
                self.op = :nop
                if @args[1] >= 0
                    @nx[0][0].drop_one_pr_and_die self
                    @nx = [@nx[1]]
                else
                    @nx[1][0].drop_one_pr_and_die self
                    @nx = [@nx[0]]
                end
                @args = []
                something_changed = true
            elsif @args[0] == :ge and @args[1].is_a? Float and @args[2].is_a? Float
                self.op = :nop
                if @args[1] >= @args[2]
                    @nx[0][0].drop_one_pr_and_die self
                    @nx = [@nx[1]]
                else
                    @nx[1][0].drop_one_pr_and_die self
                    @nx = [@nx[0]]
                end
                @args = []
                something_changed = true
            elsif @args[0] == :eq and @args[1].is_a? Float and @args[2].is_a? Float
                self.op = :nop
                if @args[1] == @args[2]
                    @nx[0][0].drop_one_pr_and_die self
                    @nx = [@nx[1]]
                else
                    @nx[1][0].drop_one_pr_and_die self
                    @nx = [@nx[0]]
                end
                @args = []
                something_changed = true
            end
        end
        if something_changed
            try_simpler_opcodes!
            return true
        end
        return false
    end
end

# Does nothing except of marking return value as live
class Asm_Epilogue
    include Connectible
    def has_side_effects?
        true
    end
    def try_simpler_opcodes!
    end
    def pretty_id_domain
        $asm_pretty_id_domain ||= [{}, 0]
        $asm_pretty_id_domain
    end
    def propagate_assertions!(visited={}, assertions_in={})
    end
    def remove_assertions!
    end
    def us
        []
    end
    def df
        []
    end
    def mod
        []
    end
    def nop?
        false
    end
    def mov?
        false
    end
    def root_nop?
        false
    end
    def extra_referred_labels
        []
    end
    def expected_next
        nil
    end
    include Connectible
    # retval is ABI retval (one allocated to R0.xyz/R0.w),
    # not one defined in the input, to handle some
    # pathological cases.
    def initialize(retval)
        @pr = []
        @retval = retval
        @pre_nx = []
        @nx = []
    end
    def live_in
        [@retval].to_set
    end
    def live_out
        [@retval].to_set
    end
    def to_s_and_liveness
        to_s
    end
    def to_s
        #"    return\n"
        # FIXME: Yucky, but asm is too strict
        "    mov R0, R0 + return or xyzw (>=0 or <1)\n"
    end
    # live_in is known in advance, no need to propagate
    def propagate_live!
    end
    def initialize_live!
    end
    def use_assertions!
    end
end

#####################################################################
# Machine registers                                                 #
#####################################################################

class Machine_register < ValueObject
    attr_reader :name
    attr :rev_allocation
    attr :sort_order
    def initialize(name)
        @name = name
        @rev_allocation = Set.new
        # "Correct" sort order:
        # R15.w -> ["R", 15, ".w"]
        @sort_order = @name.split(/(\d+)/).map{|x| if x =~ /^\d+$/ then x.to_i else x end}
    end
    include Comparable
    def <=>(other)
        return 1 if other.is_a? Variable
        @sort_order <=> other.sort_order
    end
    def same_allocation?(x)
        false
    end
    # FIXME: Is R15.xyz available or not ? For now it's off.
    def self.available_scalar_registers
        #(0..3).map{|i| new("R#{i}.w") }
        (0..14).map{|i| new("R#{i}.w") }
    end
    def self.available_vector_registers
        #(0..3).map{|i| new("R#{i}.xyz") }
        (0..14).map{|i| new("R#{i}.xyz") }
    end
    def to_s
        @name
    end
    def inspect
        "#{@name} (allocation=#{@rev_allocation.set_inspect})"
    end
end

#####################################################################
# PARSE OPTIONS                                                     #
#####################################################################

require 'optparse'

$cpp      = false
$cleanup  = false
$input_ir = :auto
$outfile  = STDOUT

ARGV.options{|opts|
    opts.banner = "Usage: #{$0} [OPTIONS] [input.sl | input.ir]"
    
    opts.on("-h", "--help", "show this message") {
        puts opts
        exit
    }
    opts.on("-p", "--cpp", "preprocess the RSL sources with cpp [default=false]") {
        $cpp = true
    }
    opts.on("-c", "--cleanup", "remove temporary files [default=false]") {
        $cleanup = true
    }
    opts.on("--ir", "input is in intermediate representation") {
        $input_ir = true
    }
    opts.on("--sl", "input is in RenderMan SL") {
        $input_ir = false
    }
    opts.on("--autodetect-input", "autodetect input type [default]") {
        $input_ir = false
    }
    opts.on("-o", "--output [FILE]", String, "save result to a file [default=STDOUT]") {|fn|
        $outfile = File.open(fn, "w") or raise "Cannot open file #{fn}: #{$!}"
    }
    opts.parse!
}

#####################################################################
# ALMOST-MAIN                                                       #
#####################################################################

def code_gen(name, code_root)
    #code_root.each_dfs{|cp| p cp }

    # Convert to SSA form
    ssa!(code_root)

    #code_root.each_dfs{|cp| p cp }

    # TODO: Should do until fixpoint reached, but this is good enough for now
    5.times{
        # Doing it here can convert many simple instructions to movs:
        # mul x, y, 1.0 -> mov x, y etc.
        code_root.each_dfs{|cp| cp.try_simpler_opcodes!}
        # Copy propagation
        copy_prop!(code_root)
        # Common subexpression elimination
        cse!(code_root)
    }
    #code_root.each_dfs{|cp| p cp }

    code_root.each_dfs{|cp| cp.codegen! }
    code_root.each_dfs{|cp| cp.codegen_connect! }

    # Add a fake Asm object - the first real op can be removed due to optimizations,
    # and we don't want asm_root to change here.
    asm_root_real = code_root.codegen_start
    asm_root = Asm.new(:root_nop, [], [], [])
    asm_root.nx << [asm_root_real]
    asm_root_real.pr << asm_root

    #asm_root.each_dfs{|cp| p cp }

    # Try simpler opcodes, like mul X, Y, -1 => mov X, -Y
    asm_root.each_dfs{|a| a.try_simpler_opcodes! }
    asm_root.drop_dfs_if!{|a| a.nop?}

    # To print code as it was before optimizations and register allocation:
    #print "Code at first:\n"
    #asm_root.print_code

    # Local optimizer pass 1 - do easy things,
    # especially those that reduce register pressure.
    while true
        something_happened = false
        asm_root.each_dfs{|a| a.remove_assertions! }
        asm_root.propagate_assertions!
        asm_root.each_dfs{|a|
            something_happened = true if a.use_assertions!
        }
        break unless something_happened
    end
    
    #asm_root.each_dfs{|a| p a}

    # TODO: propagate should actually go in reverse DFS order
    #       it's O(n * looping depth) in reverse, O(n^2) in straight DFS,
    #       for very loose meaning of O()
    # This is local liveness only, so it's possible that we have to do it many times.
    
    while true
        something_happened = false
        asm_root.each_dfs{|a| a.initialize_live! }
        asm_root.each_dfs{|a| a.propagate_live! }
        # Drop those that do not define something live, and have no side effects.
        # (except for root_nop).
        asm_root.each_dfs{|a|
            next if a.nop?
            dc = ((not a.root_nop?) and (a.live_out.intersection a.df).empty? and (not a.has_side_effects?))
            something_happened = true if dc
            a.nop! if dc
        }
        break unless something_happened
    end

    #print "Code at second:\n"
    #asm_root.print_code_and_liveness
    #asm_root.each_dfs{|a| p a}

    variables_wanting_allocation = Set.new
    asm_root.each_variable{|v|
        variables_wanting_allocation.add v unless v.allocation
    }
    asm_root.compute_enemies!

    #print "Pre-alloc:\n"
    #asm_root.print_code
    #variables_wanting_allocation.each{|v|
    #    print "Enemies of #{v}: #{v.enemies.sort.join ' '}\n"
    #}

    asm_root.each_dfs{|a|
        next unless a.mov?
        x,y = *a.args
        next unless x.is_a? Variable and y.is_a? Variable
        # If x is y's enemy, don't include it on a friend list.
        # NOTE: Such fake friends wouldn't cause a misallocation,
        # but may harm heuristics which try to allocate
        # variables with the most friends first.
        next if x.enemies.include? y
        x.friends.add y
        y.friends.add x
    }

    allocate!(variables_wanting_allocation)
    asm_root.drop_dfs_if!{|a| a.nop?}
    schedule!(asm_root)

    # WARNING: live_in/live_out is invalid here for
    # all instruction that haven't been scheduled !!!
    # asm_root.print_code *USES* this information.
    # The only reason it works is that the information
    # is used only for CALL and it doesn't get rescheduled.
    # If the scheduling starts moving CALL around (not likely),
    # recompute the live_in/live_out here.

    $outfile.print "\n; Generated code:\n"
    $outfile.print "SUBROUTINE_ENTRY_#{name}:\n"
    asm_root.print_code($outfile)
end

#####################################################################
# MAIN                                                              #
#####################################################################

$seen_functions = {}

ARGV.each{|fn|
    begin
        temp_files = []
        fn_type_is_ir = $input_ir
        if fn_type_is_ir == :auto
            if fn =~ /\.ir$/
                fn_type_is_ir = true
            elsif fn =~ /\.(?:sl|rsl|slp|rslp)$/
                fn_type_is_ir = false
            else
                warn "Input type is automatic and the input file name `#{fn}' is neither *.ir nor *.sl"
                fn_type_is_ir = false
            end
        end
        unless fn_type_is_ir
            if $cpp
                pp_fn = fn.sub(/\.(?:sl|rsl)$/,"") + ".slp"
                temp_files << pp_fn
                # -P - no line markers (like # foo.sl 34)
                # -w - no warnings (especially the annoying "warning: no newline at end of file")
                # NOTE: You may need to adjust the options if using cpp different than gcc's
                system "cpp -P -w <#{fn} >#{pp_fn}" or raise "Cannot preprocess #{fn}"
                fn = pp_fn
            end
            fn_ir = fn.sub(/\.(?:sl|rsl|slp|rslp)$/,"") + ".ir"
            temp_files << fn_ir
            system "../slcompiler_minimal/slcompiler #{fn} >#{fn_ir}" or raise "Cannot precompile #{fn}"
            fn = fn_ir
        end
        # UGLY: parse_ir creates ValueObjects - we don't want them to be shared,
        #       so we need to call purge_all! on each iteration. Won't work otherwise.
        source_code = File.read(fn) or raise "Cannot read #{fn}: #{$!}"
        parse_ir(source_code) {|name, code_root|
            $seen_functions[name] = true
            code_gen(name, code_root)
            # Do not preserve values across compilations !
            # Variables can have the same name in different functions.
            # This is somewhat ugly (well, if parser was nicer it wouldn't be needed).
            ValueObject.purge_all!
        }
    ensure
        if $cleanup
            temp_files.each{|fn| File.delete fn}
        end
    end
}
