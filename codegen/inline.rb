#####################################################################
# GENERATE A NEW LABEL                                              #
#####################################################################
$label_counter = 0
def new_label
    $label_counter -= 1
    return $label_counter
end

#####################################################################
# EARLY INLINING                                                    #
#####################################################################
# TODO: Make it reasonably sane
def early_inline(fname, abi, t, a, link_out, code, label)
    # Fresh labels, scalar variables and vector variables on demand.
    # Asking for l[x] the first time generates a new object
    # of the right type. Asking again fetches the same object.
    l = Hash.new{|ht,k| ht[k] = new_label}
    l[0]  = label
    l[-1] = link_out
    s = Hash.new{|ht,k| ht[k] = Variable.fresh(:scalar)}
    v = Hash.new{|ht,k| ht[k] = Variable.fresh(:vector)}
    # This special object makes it possible to specify code
    # expansion in a very convenient way.
    # The definition is pretty ugly, don't worry - it just works :-)
    # RUBY 1.9: For Ruby 1.9 .send needs to be changed to .funcall (or .fcall)
    c = Object.new
    (class << c; self; end).send(:define_method, :[]=) {|lab, dsc|
        code[l[lab]] = Code.new(dsc[0], [], dsc[1], *dsc[2..-1].map{|i| l[i]})
    }

    case fname
    when 'length'
        raise "#{fname} function should have 1 argument" unless a.size == 1
        c[0] = [:dp3, [s[0], a[0], a[0]], 1]
        c[1] = [:rsq, [s[1], s[0]], 2]
        c[2] = [:rcp, [t,    s[1]], -1]
    when 'radians'
        raise "#{fname} function should have 1 argument" unless a.size == 1
        c[0] = [:mul, [t,    a[0], Math::PI/180.0], -1]
    when 'degrees'
        raise "#{fname} function should have 1 argument" unless a.size == 1
        c[0] = [:mul, [t,    a[0], 180.0/Math::PI], -1]
    when 'inversesqrt'
        raise "#{fname} function should have 1 argument" unless a.size == 1
        c[0] = [:rsq, [t,    a[0]], -1]
    when 'sqrt'
        raise "#{fname} function should have 1 argument" unless a.size == 1
        c[0] = [:rsq, [s[0], a[0]], 1]
        c[1] = [:rcp, [t,    s[0]], -1]
    when 'distance'
        raise "#{fname} function should have 2 arguments" unless a.size == 2
        c[0] = [:sub, [v[0], a[0], a[1]], 1]
        c[1] = [:dp3, [s[0], v[0], v[0]], 2]
        c[2] = [:rsq, [s[1], s[0]], 3]
        c[3] = [:rcp, [t,    s[1]], -1]
    when 'normalize'
        raise "#{fname} function should have 1 argument" unless a.size == 1
        # v1 = arg[0] . arg[0]
        # target = rsq(v1) * arg[0]
        c[0] = [:dp3, [s[0], a[0], a[0]], 1]
        c[1] = [:rsq, [s[1], s[0]], 2]
        c[2] = [:mul, [t,    a[0], s[1]], -1]
    else
        #code[l[0]] = Code.new(:call, [], [fname, abi, target, *args], l[-1])
        c[0] = [:call, [fname, abi, t, *a], -1]
    end
end

#####################################################################
# LATE INLINING                                                     #
#####################################################################
def late_inline(fname, abi, target, args)
    case fname
    when 'floor'
    # floor(y)  == y - frac(y)
        v = Variable.fresh(:float)
        c = [
            Asm.frac(v, args[0], 1),
            Asm.sub(target, args[0], v, :next),
        ]
    when 'round'
    # round(y) == floor(y+0.5) = (y+0.5) - frac(y+0.5)
        v1 = Variable.fresh(:float)
        v2 = Variable.fresh(:float)
        c = [
            Asm.add(v1, args[0], 0.5, 1),
            Asm.frac(v2, v1, 2),
            Asm.sub(target, v1, v2, :next),
        ]
    when 'ceil'
    # ceil(y) = -floor(-y)  == -(-y - frac(-y)) = y + frac(-y)
        v = Variable.fresh(:float)
        c = [
            Asm.frac_minus(v, args[0], 1),
            Asm.add(target, v, args[0], :next),
        ]
    when 'abs'
    # Uhm, can it possibly work ?
    # If x > 0 x else -x end
        c = [
            Asm.mov(target, args[0], 1),
            Asm.new(:cjmp, [:ge0, target], 2, :next),
            Asm.neg(target, target, :next),
        ]
    when 'cos'
        raise "cos not supported"
        # cos(x) = cos_interpolation(frac(x/2pi))
        # cos_interpolation uses a texture for storing cos values
        v1 = Variable.fresh(:float)
        v2 = Variable.fresh(:float)
        c = [
            Asm.mul(v1, args[0], 0.5/Math::PI, 1),
            Asm.frac(v2, v1, 2),
            Asm.new(:texload, [target, "cos", v2], :next),
        ]
    when 'sin'
        raise "sin not supported"
        # sin(x) = cos(x-pi/2) = cos_interpolation(frac(x/2pi-0.25))
        # cos_interpolation uses a texture for storing cos values
        v1 = Variable.fresh(:float)
        v2 = Variable.fresh(:float)
        v3 = Variable.fresh(:float)
        c = [
            Asm.mul(v1, args[0], 0.5/Math::PI, 1),
            Asm.add(v2, v1, -0.25, 2),
            Asm.frac(v3, v2, 3),
            Asm.new(:texload, [target, "cos", v3], :next),
        ]
    # Are 2*32 such functions needed ?
    when 'get_Ng'
        r = Asm.round_robin_s
        v4 = Variable.fresh(:vector)
        v5 = Variable.fresh(:vector)
        v6 = Variable.fresh(:vector)
        v7 = Variable.fresh(:vector)
        v8 = Variable.fresh(:vector)
        v9  = Variable.fresh(:float)
        v10 = Variable.fresh(:float)
        c = [
            Asm.new(:load, [reg("I0"), reg("HIT_TRI"), 0], 1),
            Asm.mov(reg("A"), reg("I0"), 2),
            Asm.new(:load, [reg("I0"), reg("A.x"), 0], 3),
            Asm.new(:load, [reg("I1"), reg("A.y"), 0], 4),
            Asm.new(:load, [reg("I2"), reg("A.z"), 0], 5),
            Asm.sub(v4, reg("I1.xyz"), reg("I0.xyz"), 6),
            Asm.sub(v5, reg("I2.xyz"), reg("I0.xyz"), 7),
            Asm.mul_m(v6, Source.swizzle(v4, "yzx"), Source.swizzle(v5, "zxy"), 8),
            Asm.mul_m(v7, Source.swizzle(v4, "zxy"), Source.swizzle(v5, "yzx"), 9),
            Asm.sub(v8, v6, v7, 10),
            Asm.dp3(v9, v8, v8, 11),
            Asm.mov_rsq(r, v9, 12),
            Asm.mul(target, v8, reg("S.#{r}"), :next),
        ]
    when 'get_Cs'
        c = [
            Asm.new(:load, [reg("I0"), reg("HIT_TRI"), 0], 1),
            Asm.mov(reg("A.x"), reg("I0.w"), 2),
            Asm.new(:load, [reg("I0"), reg("A.x"), 0], 3),
            Asm.mov(target, reg("I0.xyz"), :next),
        ]
    when 'get_c0'
        c = [
            Asm.mov(target, reg("C0.xyz"), :next)
        ]
    when 'set_c0'
        c = [
            Asm.mov(target, reg("C0.xyz"), 1),
            Asm.mov(reg("C0.xyz"), args[0], :next),
        ]
    when 'load'
        c = [
            Asm.mov(reg("A.x"), args[0], 1),
            Asm.new(:load, [reg("I0"), reg("A.x"), 0], 2),
            Asm.mov(target, reg("I0.xyz"), :next)
        ]
    # 4 functions like that ? load must return 3-vectors, not 4-vectors,
    # so a silly interface is necessary.
    when 'get_i0_w'
        c = [
            Asm.mov(target, reg("I0.w"), :next)
        ]
    when 'get_hit'
        c = [
            Asm.mov(target, reg("HIT.xyz"), :next)
        ]
    when 'get_hit_w'
        c = [
            Asm.mov(target, reg("HIT.w"), :next)
        ]
    when 'get_hit_tri'
        c = [
            Asm.mov(target, reg("HIT_TRI"), :next)
        ]
    when 'get_hit_obj'
        c = [
            Asm.mov(target, reg("HIT_OBJ"), :next)
        ]
    when 'trace_instr'
        c = [
            Asm.new(:trace, [target, args[0], args[1], args[2]], :next)
        ]
    when 'trace'
        # trace parameter are in C31
        orig = Variable.fresh(:vector)
        dir  = Variable.fresh(:vector)
        rv   = Variable.fresh(:vector)
        orig.allocation = reg("R0.xyz")
        dir.allocation  = reg("R1.xyz")
        rv.allocation   = reg("R0.xyz")
        c = [
            Asm.new(:trace, [target, args[0], args[1], reg("C31")], 1),
            Asm.new(:cjmp, [:ge0, reg("HIT.z")], 2, 3),
            Asm.mov(target, 0.0, :next), # background color
            Asm.mov(orig, args[0], 4),
            Asm.mov(dir, args[1], 5),
            Asm.new(:call, [reg("HIT.w"), rv, orig, dir], 6),
            Asm.mov(target, rv, :next)
        ]
    when 'update_xcomp'
        c = [
            Asm.mov(target, args[0], 1),
            Asm.new(:setx, [target, args[1]], :next),
        ]
    when 'update_ycomp'
        c = [
            Asm.mov(target, args[0], 1),
            Asm.new(:sety, [target, args[1]], :next),
        ]
    when 'update_zcomp'
        c = [
            Asm.mov(target, args[0], 1),
            Asm.new(:setz, [target, args[1]], :next),
        ]
    when 'xcomp'
        c = [
            Asm.getx(target, args[0], :next)
        ]
    when 'ycomp'
        c = [
            Asm.gety(target, args[0], :next)
        ]
    when 'zcomp'
        c = [
            Asm.getz(target, args[0], :next)
        ]
    when 'step'
        c = [
            Asm.new(:cjmp, [:ge, args[1], args[0]], 1, 2),
            Asm.mov(target, 0.0, :next),
            Asm.mov(target, 1.0, :next),
        ]
    # Only index values 0.0 1.0 2.0 are officially supported.
    # For everything else we do:\
    # -infty ... +0.5 is considered 0.0
    # +0.5 ... 1.5    is considered 1.0
    # +1.5 ... +infty is considered 2.0
    when 'comp'
        c = [
            Asm.new(:cjmp, [:ge, args[1], 0.5], 1, 2),
            Asm.getx(target, args[0], :next),
            Asm.new(:cjmp, [:ge, args[1], 1.5], 3, 4),
            Asm.gety(target, args[0], :next),
            Asm.getz(target, args[0], :next),
        ]
    when 'min'
        raise "#{fname} function needs at least 1 argument" if args.size == 0
        if target.allocate_to == :scalar
            v = Variable.fresh(:float)
            c = [
                Asm.mov(v, args[0], 1)
            ]
            args[1..-1].each{|a|
                c << Asm.new(:cjmp, [:ge, a, v], c.size + 1, c.size + 2)
                c << Asm.mov(v, a, c.size + 1)
            }
            c << Asm.mov(target, v, :next)
        else
            vx = Variable.fresh(:float)
            vy = Variable.fresh(:float)
            vz = Variable.fresh(:float)
            c = []
            c << Asm.getx(vx, args[0], c.size + 1)
            args[1..-1].each{|a|
                vt = Variable.fresh(:float)
                c << Asm.getx(vt, a, c.size + 1)
                c << Asm.new(:cjmp, [:ge, vt, vx], c.size + 1, c.size + 2)
                c << Asm.mov(vx, vt, c.size + 1)
            }
            c << Asm.gety(vy, args[0], c.size + 1)
            args[1..-1].each{|a|
                vt = Variable.fresh(:float)
                c << Asm.gety(vt, a, c.size + 1)
                c << Asm.new(:cjmp, [:ge, vt, vy], c.size + 1, c.size + 2)
                c << Asm.mov(vy, vt, c.size + 1)
            }
            c << Asm.getz(vz, args[0], c.size + 1)
            args[1..-1].each{|a|
                vt = Variable.fresh(:float)
                c << Asm.getz(vt, a, c.size + 1)
                c << Asm.new(:cjmp, [:ge, vt, vz], c.size + 1, c.size + 2)
                c << Asm.mov(vz, vt, c.size + 1)
            }
            c << Asm.new(:setx0, [target, vx], c.size + 1)
            c << Asm.new(:sety, [target, vy], c.size + 1)
            c << Asm.new(:setz, [target, vz], :next)
        end
    when 'max'
        raise "#{fname} function needs at least 1 argument" if args.size == 0
        if target.allocate_to == :scalar
            v = Variable.fresh(:float)
            c = [
                Asm.mov(v, args[0], 1)
            ]
            args[1..-1].each{|a|
                c << Asm.new(:cjmp, [:ge, v, a], c.size + 1, c.size + 2)
                c << Asm.mov(v, a, c.size + 1)
            }
            c << Asm.mov(target, v, :next)
        else
            vx = Variable.fresh(:float)
            vy = Variable.fresh(:float)
            vz = Variable.fresh(:float)
            c = []
            c << Asm.getx(vx, args[0], c.size + 1)
            args[1..-1].each{|a|
                vt = Variable.fresh(:float)
                c << Asm.getx(vt, a, c.size + 1)
                c << Asm.new(:cjmp, [:ge, vx, vt], c.size + 1, c.size + 2)
                c << Asm.mov(vx, vt, c.size + 1)
            }
            c << Asm.gety(vy, args[0], c.size + 1)
            args[1..-1].each{|a|
                vt = Variable.fresh(:float)
                c << Asm.gety(vt, a, c.size + 1)
                c << Asm.new(:cjmp, [:ge, vy, vt], c.size + 1, c.size + 2)
                c << Asm.mov(vy, vt, c.size + 1)
            }
            c << Asm.getz(vz, args[0], c.size + 1)
            args[1..-1].each{|a|
                vt = Variable.fresh(:float)
                c << Asm.getz(vt, a, c.size + 1)
                c << Asm.new(:cjmp, [:ge, vz, vt], c.size + 1, c.size + 2)
                c << Asm.mov(vz, vt, c.size + 1)
            }
            c << Asm.new(:setx0, [target, vx], c.size + 1)
            c << Asm.new(:sety, [target, vy], c.size + 1)
            c << Asm.new(:setz, [target, vz], :next)
        end
    when 'clamp'
        if target.allocate_to == :scalar
            # if (b >= a)
            #    t = b;
            # else if (c >= a)
            #    t = c;
            # else
            #    t = a;
            c = [
                Asm.new(:cjmp, [:ge, args[1], args[0]], 2, 1),
                Asm.mov(target, args[1], :next),
                Asm.new(:cjmp, [:ge, args[2], args[0]], 4, 3),
                Asm.mov(target, args[0], :next),
                Asm.mov(target, args[2], :next),
            ]
        else
            # TODO: Component forwarding would be very nice.
            # There is no reason to have getx coded as movs,
            #
            # This:
            #   mov ax.w, a.x
            #   mov mix.w, min.x
            #   cjmp ge, minx.w, ax.w, 4, 3
            # Can be optimized to:
            #   cjmp ge, min.x, a.x, 4, 3
            # etc.
            ax   = Variable.fresh(:float)
            ay   = Variable.fresh(:float)
            az   = Variable.fresh(:float)
            minx = Variable.fresh(:float)
            miny = Variable.fresh(:float)
            minz = Variable.fresh(:float)
            maxx = Variable.fresh(:float)
            maxy = Variable.fresh(:float)
            maxz = Variable.fresh(:float)

            c = [
                Asm.getx(ax, args[0], 1),
                Asm.getx(minx, args[1], 2),
                Asm.new(:cjmp, [:ge, minx, ax], 4, 3),
                Asm.new(:setx0, [target, minx], 8),
                Asm.getx(maxx, args[2], 5),
                Asm.new(:cjmp, [:ge, maxx, ax], 7, 6),
                Asm.new(:setx0, [target, ax], 8),
                Asm.new(:setx0, [target, maxx], 8),

                Asm.gety(ay, args[0], 9),
                Asm.gety(miny, args[1], 10),
                Asm.new(:cjmp, [:ge, miny, ay], 12, 11),
                Asm.new(:sety, [target, miny], 16),
                Asm.gety(maxy, args[2], 13),
                Asm.new(:cjmp, [:ge, maxy, ay], 15, 14),
                Asm.new(:sety, [target, ay], 16),
                Asm.new(:sety, [target, maxy], 16),

                Asm.getz(az, args[0], 17),
                Asm.getz(minz, args[1], 18),
                Asm.new(:cjmp, [:ge, minz, az], 20, 19),
                Asm.new(:setz, [target, minz], :next),
                Asm.getz(maxz, args[2], 21),
                Asm.new(:cjmp, [:ge, maxz, az], 23, 22),
                Asm.new(:setz, [target, az], :next),
                Asm.new(:setz, [target, maxz], :next),
            ]
        end
    # NOTE: Hardware does not support 0.0, so sign(0.0) == 1.0 !!!
    when 'sign'
        c = [
            Asm.new(:cjmp, [:ge0, args[0]], 2, 1),
            Asm.mov(target, 1.0, :next),
            Asm.mov(target, -1.0, :next),
        ]
    when 'mix'
        if args[0].allocate_to == :scalar and args[1].allocate_to == :scalar
            t = :float
        else
            t = :vector
        end
        v1 = Variable.fresh(:float)
        v2 = Variable.fresh(t)
        v3 = Variable.fresh(t)
        c = [
            Asm.sub(v1, 1.0, args[2], 1),
            Asm.mul(v2, args[0], args[2], 2),
            Asm.mul(v3, args[1], v1, 3),
            Asm.add(target, v2, v3, :next),
        ]
    else
        c = nil
    end
    return c
end
