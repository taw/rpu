#!/usr/bin/ruby
require 'test/unit'
require 'driver'
include Math

class Test_Driver < Test::Unit::TestCase
    def test_save
        f = "float f(float x, y) { return x+y; } "
        l = __LINE__
        Driver.save(f, "#{l}.sl")
        g = File.read("#{l}.sl")
        assert_equal(f, g, "Driver.save should work")
    end
    def test_compile
        f = "float f(float x, y) { return x+y; } "
        l = __LINE__

        File.delete "#{l}.shr" if File.exists?("#{l}.shr")
        assert(!File.exists?("#{l}.shr"), "File.delete should work")

        Driver.save(f, "#{l}.sl")
        Driver.compile("#{l}")
        assert(File.exists?("#{l}.shr"), "Driver.compile should work")
    end
end

class Test_Basic < Test::Unit::TestCase
    def test_add_flt
        f = "float f(float x, y) { return x+y; } "
        res = Driver.compile_and_run(__LINE__, f, 1.0, 5.0)
        assert_equal(6.0, res[:scalar], "Scalar addition should work")
    end
    def test_sub_flt
        f = "float f(float x, y) { return x-y; } "
        res = Driver.compile_and_run(__LINE__, f, 1.0, 5.0)
        assert_equal(-4.0, res[:scalar], "Scalar subtraction should work")
    end
    def test_mul_flt
        f = "float f(float x, y) { return x*y; } "
        res = Driver.compile_and_run(__LINE__, f, 2.0, 5.0)
        assert_equal(10.0, res[:scalar], "Scalar multiplication should work")
    end
    def test_div_flt
        f = "float f(float x, y) { return x/y; } "
        res = Driver.compile_and_run(__LINE__, f, 1.0, 5.0)
        assert_equal(0.2, res[:scalar], "Scalar division should work")
    end
    def test_add_vec
        f = "vector f(vector x, y) { return x+y; } "
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0], [5.0, 7.0, 9.0])
        assert_equal([6.0, 9.0, 12.0], res[:vector], "Vector addition should work")
    end
    def test_mul_vec
        f = "vector f(vector x, y) { return x*y; } "
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0], [4.0, 5.0, 6.0])
        assert_equal([4.0, 10.0, 18.0], res[:vector], "Vector multiplication should work")
    end
    def test_sub_vec
        f = "vector f(vector x, y) { return x-y; } "
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0], [5.0, 7.0, 9.0])
        assert_equal([-4.0, -5.0, -6.0], res[:vector], "Vector subtraction should work")
    end
    def test_dot_vec
        f = "float f(vector x, y) { return x.y; } "
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0], [5.0, 7.0, 9.0])
        assert_equal(46.0, res[:scalar], "Dot product should work")
    end
    def test_xpd_vec
        f = "vector f(vector x, y) { return x^y; } "
        a = [1.0, 2.0, 3.0]
        b = [5.0, 7.0, 9.0]
        res = Driver.compile_and_run(__LINE__, f, a, b)
        assert_equal([a[1]*b[2] - a[2]*b[1], a[2]*b[0] - a[0]*b[2], a[0]*b[1] - a[1]*b[0]], res[:vector], "Cross product should work")
    end
    def test_vf_add
        f = "vector f(vector x; float y) { return x+y; } "
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0], 5.0)
        assert_equal([6.0, 7.0, 8.0], res[:vector], "Vector+Float should work")
    end
    def test_cast
        f = "vector f(float x) { return x; } "
        res = Driver.compile_and_run(__LINE__, f, 5.0)
        assert_equal([5.0, 5.0, 5.0], res[:vector], "Scalar to vector casts should work")
    end
    def test_color_cast
        f = "color f(float x) { return x; } "
        res = Driver.compile_and_run(__LINE__, f, 5.0)
        assert_equal([5.0, 5.0, 5.0], res[:vector], "Scalar to color casts should work")
    end
end

# Test control structures
class Test_Control < Test::Unit::TestCase
    def test_if
        f = "float f(float x) { if(x >= 0.0) { return x; } else { return -x; } } "
        res = Driver.compile_and_run(__LINE__, f, 2.0)
        assert_equal(2.0, res[:scalar], "if-else should work")
        res = Driver.compile_and_run(__LINE__, f, -3.0)
        assert_equal(3.0, res[:scalar], "if-else should work")
    end
end

# Test componentwise operations
class Test_Component < Test::Unit::TestCase
    def test_vec_tuple_const
        f = "vector f() { vector v = (1.0, 2.0, 3.0); return v; }"
        res = Driver.compile_and_run(__LINE__, f)
        assert_equal([1.0, 2.0, 3.0], res[:vector], "Defining vectors using tuples should work")
    end
    def test_vec_tuple
        f = "vector f(float a, b, c) { vector v = (a, b, c); return v; }"
        res = Driver.compile_and_run(__LINE__, f, 1.0, 2.0, 3.0)
        assert_equal([1.0, 2.0, 3.0], res[:vector], "Defining vectors using tuples should work")
    end

    def test_col_tuple_const
        f = "color f() { color c = (1.0, 2.0, 3.0); return c; }"
        res = Driver.compile_and_run(__LINE__, f)
        assert_equal([1.0, 2.0, 3.0], res[:vector], "Defining colors using tuples should work")
    end
    def test_col_tuple
        f = "color f(float r, g, b) { color c = (r, g, b); return c; }"
        res = Driver.compile_and_run(__LINE__, f, 1.0, 2.0, 3.0)
        assert_equal([1.0, 2.0, 3.0], res[:vector], "Defining colors using tuples should work")
    end
    def test_vec_get_comp
        f = "float f(vector v) { return xcomp(v); }"
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0])
        assert_equal(1.0, res[:scalar], "xcomp(v) should work")

        f = "float f(vector v) { return ycomp(v); }"
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0])
        assert_equal(2.0, res[:scalar], "ycomp(v) should work")

        f = "float f(vector v) { return zcomp(v); }"
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0])
        assert_equal(3.0, res[:scalar], "zcomp(v) should work")
    end

    # WARNING: THIS IS NOT *REAL* RENDERMAN SHADING LANGUAGE.
    # Real RSL uses set*comp(update type x; float v),
    # which is not supported yet (because update modifier is not supported).
    def test_vec_update_comp
        f = "vector f(vector v; float f) { return update_xcomp(v, f); }"
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0], 4.0)
        assert_equal([4.0, 2.0, 3.0], res[:vector], "update_xcomp(v) should work")

        f = "vector f(vector v; float f) { return update_ycomp(v, f); }"
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0], 4.0)
        assert_equal([1.0, 4.0, 3.0], res[:vector], "update_ycomp(v) should work")

        f = "vector f(vector v; float f) { return update_zcomp(v, f); }"
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0], 4.0)
        assert_equal([1.0, 2.0, 4.0], res[:vector], "update_zcomp(v) should work")
    end

    # WARNING: THIS IS NOT *REAL* RENDERMAN SHADING LANGUAGE.
    # USE comp(color c; float index) FOR COMPATIBILITY.
    def test_col_get_comp
        f = "float f(color c) { return xcomp(c); }"
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0])
        assert_equal(1.0, res[:scalar], "xcomp(color c) should work")

        f = "float f(color c) { return ycomp(c); }"
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0])
        assert_equal(2.0, res[:scalar], "ycomp(color c) should work")

        f = "float f(color c) { return zcomp(c); }"
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0])
        assert_equal(3.0, res[:scalar], "zcomp(color c) should work")
    end

    def test_col_get_comp_official
        f = "float f(color c) { return comp(c, 0.0); }"
        res = Driver.compile_and_run(__LINE__, f, [1.5, 2.5, 3.5])
        assert_equal(1.5, res[:scalar], "comp(color c; float index) should work")

        f = "float f(color c) { return comp(c, 1.0); }"
        res = Driver.compile_and_run(__LINE__, f, [1.5, 2.5, 3.5])
        assert_equal(2.5, res[:scalar], "comp(color c; float index) should work")

        f = "float f(color c) { return comp(c, 2.0); }"
        res = Driver.compile_and_run(__LINE__, f, [1.5, 2.5, 3.5])
        assert_equal(3.5, res[:scalar], "comp(color c; float index) should work")

        f = "float f(color c; float i) { return comp(c, i); }"
        res = Driver.compile_and_run(__LINE__, f, [1.5, 2.5, 3.5], 0.0)
        assert_equal(1.5, res[:scalar], "comp(color c; float index) should work")

        f = "float f(color c; float i) { return comp(c, i); }"
        res = Driver.compile_and_run(__LINE__, f, [1.5, 2.5, 3.5], 1.0)
        assert_equal(2.5, res[:scalar], "comp(color c; float index) should work")

        f = "float f(color c; float i) { return comp(c, i); }"
        res = Driver.compile_and_run(__LINE__, f, [1.5, 2.5, 3.5], 2.0)
        assert_equal(3.5, res[:scalar], "comp(color c; float index) should work")
    end

    def test_min
        f = "vector f(vector a, b) { return min(a,b); }"
        [[[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [1.0, 2.0, 3.0]],
         [[4.0, 5.0, 6.0], [1.0, 2.0, 3.0], [1.0, 2.0, 3.0]],
         [[1.0, 2.0, 3.0], [1.0, 3.0, 2.0], [1.0, 2.0, 2.0]],
         [[1.0, 2.0, 3.0], [0.0, 2.0, 4.0], [0.0, 2.0, 3.0]],
         [[1.0, 2.0, 3.0], [2.0, 1.0, 3.0], [1.0, 1.0, 3.0]],
        ].each{|a,b,o|
            res = Driver.compile_and_run(__LINE__, f, a, b)
            assert_equal(o, res[:vector], "vector min(a,b) should work")
        }
        
        f = "color f(color a, b) { return min(a,b); }"
        [[[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [1.0, 2.0, 3.0]],
         [[4.0, 5.0, 6.0], [1.0, 2.0, 3.0], [1.0, 2.0, 3.0]],
         [[1.0, 2.0, 3.0], [1.0, 3.0, 2.0], [1.0, 2.0, 2.0]],
         [[1.0, 2.0, 3.0], [0.0, 2.0, 4.0], [0.0, 2.0, 3.0]],
         [[1.0, 2.0, 3.0], [2.0, 1.0, 3.0], [1.0, 1.0, 3.0]],
        ].each{|a,b,o|
            res = Driver.compile_and_run(__LINE__, f, a, b)
            assert_equal(o, res[:vector], "color min(a,b) should work")
        }

        f = "vector f(vector a, b, c) { return min(a,b,c); }"
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0], [2.1, 3.1, 1.1], [3.2, 1.2, 2.2])
        assert_equal([1.0, 1.2, 1.1], res[:vector], "vector min(a,b,c) should work")

        f = "color f(color a, b, c) { return min(a,b,c); }"
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0], [2.1, 3.1, 1.1], [3.2, 1.2, 2.2])
        assert_equal([1.0, 1.2, 1.1], res[:vector], "color min(a,b,c) should work")
    end

    def test_max
        f = "vector f(vector a, b) { return max(a,b); }"
        [[[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [4.0, 5.0, 6.0]],
         [[4.0, 5.0, 6.0], [1.0, 2.0, 3.0], [4.0, 5.0, 6.0]],
         [[1.0, 2.0, 3.0], [1.0, 3.0, 2.0], [1.0, 3.0, 3.0]],
         [[1.0, 2.0, 3.0], [0.0, 2.0, 4.0], [1.0, 2.0, 4.0]],
         [[1.0, 2.0, 3.0], [2.0, 1.0, 3.0], [2.0, 2.0, 3.0]],
        ].each{|a,b,o|
            res = Driver.compile_and_run(__LINE__, f, a, b)
            assert_equal(o, res[:vector], "vector max(a,b) should work")
        }
        
        f = "color f(color a, b) { return max(a,b); }"
        [[[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [4.0, 5.0, 6.0]],
         [[4.0, 5.0, 6.0], [1.0, 2.0, 3.0], [4.0, 5.0, 6.0]],
         [[1.0, 2.0, 3.0], [1.0, 3.0, 2.0], [1.0, 3.0, 3.0]],
         [[1.0, 2.0, 3.0], [0.0, 2.0, 4.0], [1.0, 2.0, 4.0]],
         [[1.0, 2.0, 3.0], [2.0, 1.0, 3.0], [2.0, 2.0, 3.0]],
        ].each{|a,b,o|
            res = Driver.compile_and_run(__LINE__, f, a, b)
            assert_equal(o, res[:vector], "color max(a,b) should work")
        }

        f = "vector f(vector a, b, c) { return max(a,b,c); }"
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0], [2.1, 3.1, 1.1], [3.2, 1.2, 2.2])
        assert_equal([3.2, 3.1, 3.0], res[:vector], "vector max(a,b,c) should work")

        f = "color f(color a, b, c) { return max(a,b,c); }"
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0], [2.1, 3.1, 1.1], [3.2, 1.2, 2.2])
        assert_equal([3.2, 3.1, 3.0], res[:vector], "color max(a,b,c) should work")
    end

    def test_clamp
        f = "vector f(vector a, min, max) { return clamp(a, min, max); }"
        [[[0.0, 0.0, 0.0], [1.0, 2.0, 3.0]],
         [[7.0, 7.0, 7.0], [4.0, 5.0, 6.0]],
         [[3.5, 3.5, 3.5], [3.5, 3.5, 3.5]],
         [[1.5, 0.5, 6.5], [1.5, 2.0, 6.0]],
         [[4.5, 3.0, 1.5], [4.0, 3.0, 3.0]],
         [[0.5, 6.5, 4.5], [1.0, 5.0, 4.5]],
        ].each{|i,o|
            res = Driver.compile_and_run(__LINE__, f, i, [1.0, 2.0, 3.0], [4.0, 5.0, 6.0])
            assert_equal(o, res[:vector], "vector clamp(a, min, max) should work")
        }

        f = "color f(color a, min, max) { return clamp(a, min, max); }"
        [[[0.0, 0.0, 0.0], [1.0, 2.0, 3.0]],
         [[7.0, 7.0, 7.0], [4.0, 5.0, 6.0]],
         [[3.5, 3.5, 3.5], [3.5, 3.5, 3.5]],
         [[1.5, 0.5, 6.5], [1.5, 2.0, 6.0]],
         [[4.5, 3.0, 1.5], [4.0, 3.0, 3.0]],
         [[0.5, 6.5, 4.5], [1.0, 5.0, 4.5]],
        ].each{|i,o|
            res = Driver.compile_and_run(__LINE__, f, i, [1.0, 2.0, 3.0], [4.0, 5.0, 6.0])
            assert_equal(o, res[:vector], "color clamp(a, min, max) should work")
        }
    end
end

class Test_Functions < Test::Unit::TestCase
    def test_pi
        f = "float f() { return PI; } "
        res = Driver.compile_and_run(__LINE__, f)
        assert((res[:scalar]-PI).abs <= 0.0001, "PI should work")
    end
    def test_radians
        f = "float f(float x) { return radians(x); } "
        [0.0, 1.0, 10.0].each{|x|
            res = Driver.compile_and_run(__LINE__, f, x)
            assert_in_delta((x*PI/180.0), res[:scalar], 0.0001, "radians(x) should work")
        }
    end
    def test_degrees
        f = "float f(float x) { return degrees(x); } "
        [0.0, 1.0, 10.0].each{|x|
            res = Driver.compile_and_run(__LINE__, f, x)
            assert_in_delta((x*180.0/PI), res[:scalar], 0.0001, "degrees(x) should work")
        }
    end
    def test_sqrt
        f = "float f(float x) { return sqrt(x); } "
        [0.0, 1.0, 4.0].each{|x|
            res = Driver.compile_and_run(__LINE__, f, x)
            assert_equal(sqrt(x), res[:scalar], "sqrt(x) should work")
        }
    end
    def test_inversesqrt
        f = "float f(float x) { return inversesqrt(x); } "
        [1.0, 4.0].each{|x|
            res = Driver.compile_and_run(__LINE__, f, x)
            assert_equal(1.0/sqrt(x), res[:scalar], "inversesqrt(x) should work")
        }
    end
    def test_abs
        f = "float f(float x) { return abs(x); } "
        [-3.0, 0.0, 2.0].each{|x|
            res = Driver.compile_and_run(__LINE__, f, x)
            assert_equal(x.abs, res[:scalar], "abs(x) should work")
        }
    end
    def test_floor
        f = "float f(float x) { return floor(x); } "
        a = [-3.2, -3.6, -3.0, 3.0, 3.2, 3.6]
        b = [-4.0, -4.0, -3.0, 3.0, 3.0, 3.0]
        a.zip(b).each{|x,y|
            res = Driver.compile_and_run(__LINE__, f, x)
            assert_equal(y, res[:scalar], "floor(x) should work")
        }
    end
    def test_ceil
        f = "float f(float x) { return ceil(x); } "
        a = [-3.2, -3.6, -3.0, 3.0, 3.2, 3.6]
        b = [-3.0, -3.0, -3.0, 3.0, 4.0, 4.0]
        a.zip(b).each{|x,y|
            res = Driver.compile_and_run(__LINE__, f, x)
            assert_equal(y, res[:scalar], "ceil(x) should work")
        }
    end
    # NOTE: Hardware does not have 0
    def test_sign
        f = "float f(float x) { return sign(x); } "
        res = Driver.compile_and_run(__LINE__, f, 3.0)
        assert_equal(1.0, res[:scalar], "sign(x) should work")
        res = Driver.compile_and_run(__LINE__, f, -2.0)
        assert_equal(-1.0, res[:scalar], "sign(x) should work")
        #res = Driver.compile_and_run(__LINE__, f, 0.0)
        #assert_equal(0.0, res[:scalar], "sign(x) should work")
    end
    def test_round
        f = "float f(float x) { return round(x); } "
        a = [-3.2, -3.6, -3.0, 3.0, 3.2, 3.6]
        b = [-3.0, -4.0, -3.0, 3.0, 3.0, 4.0]
        a.zip(b).each{|x,y|
            res = Driver.compile_and_run(__LINE__, f, x)
            assert_equal(y, res[:scalar], "round(x) should work")
        }
    end

    def test_length
        f = "float f(vector x) { return length(x); } "
        res = Driver.compile_and_run(__LINE__, f, [3.0, 4.0, 0.0])
        assert_equal(5.0, res[:scalar], "length(x) should work")
    end

    def test_normalize
        f = "vector f(vector x) { return normalize(x); } "
        res = Driver.compile_and_run(__LINE__, f, [3.0, 4.0, 0.0])
        assert_equal([0.6, 0.8, 0.0], res[:vector], "normalize(x) should work")
    end

    def test_distance
        f = "float f(point P1, P2) { return distance(P1, P2); }"
        res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0], [4.0, 6.0, 3.0])
        assert_equal(5.0, res[:scalar], "distance(x) should work")
    end

    def test_step
        f = "float f(float a, b) { return step(a, b); }"
        res = Driver.compile_and_run(__LINE__, f, 2.0, 3.0)
        assert_equal(1.0, res[:scalar], "step(min, value) should work")

        f = "float f(float a, b) { return step(a, b); }"
        res = Driver.compile_and_run(__LINE__, f, 3.0, 2.0)
        assert_equal(0.0, res[:scalar], "step(min, value) should work")
    end

    def test_scalar_max
        f = "float f(float a, b) { return max(a, b); } "
        res = Driver.compile_and_run(__LINE__, f, 1.0, 2.0)
        assert_equal(2.0, res[:scalar], "max(a, b) should work")
        res = Driver.compile_and_run(__LINE__, f, 4.0, 3.0)
        assert_equal(4.0, res[:scalar], "max(a, b) should work")

        s = [1.0, 2.0, 3.0, 4.0, 5.0]
        5.times{|i|
            f = "float f(float a, b, c, d, e) { return max(a, b, c, d, e); } "
            res = Driver.compile_and_run(__LINE__, f, *(0...s.size).map{|j| s[(i+j) % s.size]})
            assert_equal(5.0, res[:scalar], "max(a, b, c, d, e) should work")
        }
    end

    def test_scalar_min
        f = "float f(float a, b) { return min(a, b); } "
        res = Driver.compile_and_run(__LINE__, f, 1.0, 2.0)
        assert_equal(1.0, res[:scalar], "min(a, b) should work")
        res = Driver.compile_and_run(__LINE__, f, 4.0, 3.0)
        assert_equal(3.0, res[:scalar], "min(a, b) should work")

        s = [1.0, 2.0, 3.0, 4.0, 5.0]
        5.times{|i|
            f = "float f(float a, b, c, d, e) { return min(a, b, c, d, e); } "
            res = Driver.compile_and_run(__LINE__, f, *(0...s.size).map{|j| s[(i+j) % s.size]})
            assert_equal(1.0, res[:scalar], "min(a, b, c, d, e) should work")
        }
    end

    def test_scalar_mix
        f = "float f(float x, y; float a) { return mix(x, y, a); } "
        res = Driver.compile_and_run(__LINE__, f, 10.0, 100.0, 0.2)
        assert_equal(82.0, res[:scalar], "mix(x, y, a) should work")

        f = "vector f(vector x, y; float a) { return mix(x, y, a); } "
        res = Driver.compile_and_run(__LINE__, f, [10.0, 20.0, 30.0], [100.0, 1000.0, 10000.0], 0.2)
        assert_equal([82.0, 804.0, 8006.0], res[:vector], "mix(x, y, a) should work")

        f = "color f(color x, y; float a) { return mix(x, y, a); } "
        res = Driver.compile_and_run(__LINE__, f, [10.0, 20.0, 30.0], [100.0, 1000.0, 10000.0], 0.2)
        assert_equal([82.0, 804.0, 8006.0], res[:vector], "mix(x, y, a) should work")
    end
    
    def test_scalar_clamp
        f = "float f(float a, min, max) { return clamp(a, min, max); }"
        [[0.0, 1.0],
         [1.0, 1.0],
         [2.0, 2.0],
         [4.0, 4.0],
         [5.0, 4.0],
        ].each{|i,o|
            res = Driver.compile_and_run(__LINE__, f, i, 1.0, 4.0)
            assert_equal(o, res[:scalar], "float clamp(a, min, max) should work")
        }
    end
end

class Test_Multifunctions < Test::Unit::TestCase
    # Test whether self-recursive functions are aware of their ABI
    def test_fib
        c = "color fibc(color a; float b) {
            if(b<1.5) // 1.5 to make it imprecission-proof, 1 matches, 2 doesn't
                return a;
            else
                return fibc(a,b-1) + fibc(a,b-2);
        }"
        res = Driver.compile_and_run_multifun(__LINE__, c, 'fibc', [1.0, 4.0, 9.0], 8.0)
        assert_equal([34.0, 136.0, 306.0], res[:vector], "Self-recursive function should be aware of their ABI")
    end
    # Test multiple independent function in a single shader file
    def test_indep
        c = "float f() { return 2; }
             float g() { return 3; }"
        res = Driver.compile_and_run_multifun(__LINE__, c, 'f')
        assert_equal(2.0, res[:scalar], "Calling const functions should work")
        res = Driver.compile_and_run_multifun(__LINE__, c, 'g')
        assert_equal(3.0, res[:scalar], "Calling const functions should work")
    end
    def test_const_function
        c = "float f() { return 2; }
             float g() { return 3*f(); }"
        res = Driver.compile_and_run_multifun(__LINE__, c, 'g')
        assert_equal(6.0, res[:scalar], "Calling const functions should work")
    end
    def test_normal_function
        c = "float f(float x; float y) { return x*y; }
             float g() { return 4*f(2, 3); }"
        res = Driver.compile_and_run_multifun(__LINE__, c, 'g')
        assert_equal(24.0, res[:scalar], "Calling functions should work")
    end
    # This one doesn't really test saving registers - f doesn't
    # overwrite any register except for argument/return value,
    # and they are represented explicitly in the calling function.
    def test_function_savereg
        c = "float f(float x; float y) { return x*y; }
             float g() { return f(2,3)*f(4,5); }"
        res = Driver.compile_and_run_multifun(__LINE__, c, 'g')
        assert_equal(120.0, res[:scalar], "Local variables should be preserved across function calls")
    end
    def test_stack_direction
        f = "float h(float a,b,c,d,e) {
                 return a+b+c+d+e;
             }
             float g(float a,b,c,d,e) {
                 return h(a,b,c,d,e)+h(b,c,d,e,a)+h(c,d,e,a,b)+h(d,e,a,b,c)+h(e,a,b,c,d);
             }
             float f(float a,b,c,d,e) {
                 return g(a,b,c,d,e)+g(b,c,d,e,a)+g(c,d,e,a,b)+g(d,e,a,b,c)+g(e,a,b,c,d);
             }"

        res = Driver.compile_and_run_multifun(__LINE__, f, 'h', 1.0, 2.0, 3.0, 4.0, 5.0)
        assert_equal(15.0, res[:scalar], "Stack should expand the correct way")

        res = Driver.compile_and_run_multifun(__LINE__, f, 'g', 1.0, 2.0, 3.0, 4.0, 5.0)
        assert_equal(75.0, res[:scalar], "Stack should expand the correct way")

        res = Driver.compile_and_run_multifun(__LINE__, f, 'f', 1.0, 2.0, 3.0, 4.0, 5.0)
        assert_equal(375.0, res[:scalar], "Stack should expand the correct way")
    end
    def test_function_signature
        f = "float g(vector x) {
                 return x.x;
             }
             float f(float b) {
                 vector a = b;
                 return g(a);
             }"
        res = Driver.compile_and_run_multifun(__LINE__, f, 'f', 2.0)
        assert_equal(12.0, res[:scalar], "Propagation should not")
    end
    def test_preservation
        c = "float f() { return 2.0; }
             vector g(vector a; float b) { return f()*a + b; }"
        res = Driver.compile_and_run_multifun(__LINE__, c, 'g', [1.0, 2.0, 3.0], 5.0)
        assert_equal([7.0, 9.0, 11.0], res[:vector], "Local variables should be preserved accross function calls")
    end
end

# INACTIVE
class Test_Trig # < Test::Unit::TestCase
    def test_cos
        f = "float f(float x) { return cos(x); } "
        [0.0, 0.1, 1.0, 3.14159].each{|x|
            res = Driver.compile_and_run(__LINE__, f, x)
            assert_in_delta(res[:scalar], cos(x), 0.0001, "cos(x) should work")
        }
    end
    def test_sin
        f = "float f(float x) { return sin(x); } "
        [0.0, 0.1, 1.0, 3.14159].each{|x|
            res = Driver.compile_and_run(__LINE__, f, x)
            assert_in_delta(res[:scalar], sin(x), 0.0001, "sin(x) should work")
        }
    end
end

# Test that scheduling actually works
class Test_Scheduling < Test::Unit::TestCase
    def expect(*asms)
        "\n; Generated code:\n"+
        "SUBROUTINE_ENTRY_f:\n"+
        asms.map{|a| "    #{a}\n"}.join+
        "    mov R0, R0 + return or xyzw (>=0 or <1)\n"
    end
    def unparse(code)
        code =~ /^\n; Generated code:\nSUBROUTINE_ENTRY_f:\n(.*)    mov R0, R0 \+ return or xyzw \(>=0 or <1\)\n$/m or raise "Cannot parse code: #{code}"
        $1.strip.split(/\s*\n\s*/)
    end
    def assert_schedules(retval_type, expected_retval, res, expected)
        asm_output, abi_res = *res
        assert_equal(expected_retval, abi_res[retval_type], "Instruction scheduling should work.")

        generated_opcodes = unparse(asm_output).join "\n"
        
        unless generated_opcodes =~ expected
            flunk("Instruction scheduling should work. Got:\n" +
                  generated_opcodes +
                  "\nExpected:\n" +
                  expected.inspect +
                  "\n")
        end
    end
    def test_sched_trivial
        f = "float f(float x, y, z) { return (x+y)+z; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 2.0, 3.0, 4.0)
        assert_schedules(:scalar, 9.0, res, /^add (\S+), R0.w, R1.w\nadd R0.w, \1, R2.w$/)
    end
    def test_sched_1
        f = "float f(float x, y) { return (x+y)*(x*y); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 2.0, 3.0)
        assert_schedules(:scalar, 30.0, res, /^mul (\S+), R0.w, R1.w\nadd (\S+), R0.w, R1.w\nmul R0.w, \2, \1$/)
    end
    def test_sched_2
        f = "vector f(vector a, b) {
            vector z = (a+1)*(b.b);
            vector f = (a.b)+(a*5);
            return z*f;
            }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [1.0, 2.0, 3.0], [4.0, 6.0, 9.0])
        # Second and third opcode can switch freely
        assert_schedules(:vector, [12768.0, 21147.0, 30856.0], res, /^dp3 (\S+), R0.xyz, R1.xyz\ndp3 (\S+), R1.xyz, R1.xyz\nadd (\S+), R0.xyz, 1.0\nmad (\S+), R0.xyz, 5.0, \1\nmul (\S+), \3, \2\nmul R0.xyz, \5, \4$/)
    end
    def test_sched_rcp
        # Test that multiple S registers are actually used
        f = "float f(float x, y) { float a = x/y; float b = y/x; return a+b; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 2.0, 4.0)
        # First and second opcodes can switch freely, then third and fourth also switch.
        assert_schedules(:scalar, 2.5, res, /^mov_rcp R15.([xyzw]), R0.w\nmov_rcp R15.([xyzw]), R1.w\nmul (R\d+.w), R1.w, S.\1\nmad R0.w, R0.w, S.\2, \3$/)
    end
end

# Test that optimization actually works
class Test_Optimize < Test::Unit::TestCase
    def expect(*asms)
        "\n; Generated code:\n"+
        "SUBROUTINE_ENTRY_f:\n"+
        asms.flatten.map{|a| "    #{a}\n"}.join+
        "    mov R0, R0 + return or xyzw (>=0 or <1)\n"
    end
    def assert_optimizes(retval_type, expected_retval, optimized_code, res, msg)
        asm_output, abi_res = *res
        assert_equal(expected_retval, abi_res[retval_type], msg)
        assert_equal(expect(optimized_code), asm_output, msg)
    end
    def test_const_prop
        f = "float f() { return 2; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f)
        assert_optimizes(:scalar, 2.0, "mov R0.w, 2.0", res, "Constant propagation should work")
    end
    def test_const_fold_mul
        f = "float f() { return 3.0 * 5.0; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f)
        assert_optimizes(:scalar, 15.0, "mov R0.w, 15.0", res, "Constant multiplication folding should work")
    end
    def test_const_fold_add
        f = "float f() { return 3.0 + 5.0; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f)
        assert_optimizes(:scalar, 8.0, "mov R0.w, 8.0", res, "Constant addition folding should work")
    end
    def test_const_fold_sub
        f = "float f() { return 3.0 - 5.0; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f)
        assert_optimizes(:scalar, -2.0, "mov R0.w, -2.0", res, "Constant subtraction folding should work")
    end
    def test_const_fold_neg_sub
        f = "float f() { return 3.0 - (-5.0); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f)
        assert_optimizes(:scalar, 8.0, "mov R0.w, 8.0", res, "Constant subtraction folding should work")
    end
    def test_const_round
        f = "float f() { return round(1.2) + floor(1.3) + ceil(1.4); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f)
        assert_optimizes(:scalar, 4.0, "mov R0.w, 4.0", res, "Constant round/floor/ceil folding should work")
    end
    def test_const_abs
        f = "float f() { return abs(2.0); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f)
        assert_optimizes(:scalar, 2.0, "mov R0.w, 2.0", res, "Constant abs folding should work")

        f = "float f() { return abs(-3.0); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f)
        assert_optimizes(:scalar, 3.0, "mov R0.w, 3.0", res, "Constant abs folding should work")
    end
    def test_const_sign
        f = "float f() { return sign(2.0); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f)
        assert_optimizes(:scalar, 1.0, "mov R0.w, 1.0", res, "Constant sign folding should work")

        f = "float f() { return sign(-3.0); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f)
        assert_optimizes(:scalar, -1.0, "mov R0.w, -1.0", res, "Constant sign folding should work")
    end
    def test_comp
        f = "float f(color c) { return comp(c, 0.0); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [2.2, 3.3, 4.4])
        assert_optimizes(:scalar, 2.2, "mov R0.w, R0.x", res, "comp(c, index) simplification should work")
    
        f = "float f(color c) { return comp(c, 1.0); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [2.2, 3.3, 4.4])
        assert_optimizes(:scalar, 3.3, "mov R0.w, R0.y", res, "comp(c, index) simplification should work")

        f = "float f(color c) { return comp(c, 2.0); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [2.2, 3.3, 4.4])
        assert_optimizes(:scalar, 4.4, "mov R0.w, R0.z", res, "comp(c, index) simplification should work")
    end
    def test_min_max
        f = "float f() { return min(1.0, 2.0); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f)
        assert_optimizes(:scalar, 1.0, "mov R0.w, 1.0", res, "min(a, b) simplification should work")

        f = "float f() { return min(3.0, 2.0); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f)
        assert_optimizes(:scalar, 2.0, "mov R0.w, 2.0", res, "min(a, b) simplification should work")

        f = "float f() { return max(1.0, 2.0); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f)
        assert_optimizes(:scalar, 2.0, "mov R0.w, 2.0", res, "max(a, b) simplification should work")

        f = "float f() { return max(3.0, 2.0); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f)
        assert_optimizes(:scalar, 3.0, "mov R0.w, 3.0", res, "max(a, b) simplification should work")
    end
    def test_smod
        f = "float f(float x) { return -x; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 4.0)
        assert_optimizes(:scalar, -4.0, "mov R0.w, -R0.w", res, "Optimizer should take advantage of source modifiers")

        f = "float f(float x) { return x*2; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 4.0)
        assert_optimizes(:scalar, 8.0, "mov R0.w, 2*R0.w", res, "Optimizer should take advantage of source modifiers")

        f = "float f(float x) { return 2*x; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 4.0)
        assert_optimizes(:scalar, 8.0, "mov R0.w, 2*R0.w", res, "Optimizer should take advantage of source modifiers")

        f = "float f(float x) { return x*4; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 4.0)
        assert_optimizes(:scalar, 16.0, "mov R0.w, 4*R0.w", res, "Optimizer should take advantage of source modifiers")

        f = "float f(float x) { return 4*x; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 4.0)
        assert_optimizes(:scalar, 16.0, "mov R0.w, 4*R0.w", res, "Optimizer should take advantage of source modifiers")

        f = "float f(float x) { return x*0.5; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 4.0)
        assert_optimizes(:scalar, 2.0, "mov R0.w, 0.5*R0.w", res, "Optimizer should take advantage of source modifiers")

        f = "float f(float x) { return 0.5*x; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 4.0)
        assert_optimizes(:scalar, 2.0, "mov R0.w, 0.5*R0.w", res, "Optimizer should take advantage of source modifiers")
        
        f = "float f(float x) { return -4*x; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 4.0)
        assert_optimizes(:scalar, -16.0, "mov R0.w, -4*R0.w", res, "Optimizer should take advantage of source modifiers")

        f = "float f(float x) { return x*(-0.5); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 4.0)
        assert_optimizes(:scalar, -2.0, "mov R0.w, -0.5*R0.w", res, "Optimizer should take advantage of source modifiers")
    end
    def test_mov_smod_opt
        f = "float f(float x) { float y = -x; float z = -y; return -z; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 4.0)
        assert_optimizes(:scalar, -4.0, "mov R0.w, -R0.w", res, "Optimizer should take advantage of source modifiers")
        
        f = "float f(float x) { float y = 0.5*x; float z = 2*y; return 2*z; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 4.0)
        assert_optimizes(:scalar, 8.0, "mov R0.w, 2*R0.w", res, "Optimizer should take advantage of source modifiers")

        f = "float f(float x) { float y = 0.5*x; float z = 0.5*y; return 2*z; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 4.0)
        assert_optimizes(:scalar, 2.0, "mov R0.w, 0.5*R0.w", res, "Optimizer should take advantage of source modifiers")
        
        f = "float f(float x) { float y = -0.5*x; float z = -2*y; return -2*z; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 4.0)
        assert_optimizes(:scalar, -8.0, "mov R0.w, -2*R0.w", res, "Optimizer should take advantage of source modifiers")

        f = "float f(float x) { float y = -0.5*x; float z = -0.5*y; return -2*z; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 4.0)
        assert_optimizes(:scalar, -2.0, "mov R0.w, -0.5*R0.w", res, "Optimizer should take advantage of source modifiers")
    end
    def test_generic_smod
        f = "float f(float x, y) { float z = x + x; float w = y * 2; return z+w; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 3.0, 5.0)
        assert_optimizes(:scalar, 16.0, "add R0.w, 2*R0.w, 2*R1.w", res, "Optimizer should take advantage of source modifiers")
    end
    # This optimization will not work without some refactoring
    def test_smod_getcomp_post
        f = "float f(vector x) { return 2*xcomp(x); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [3.0, 5.0, 7.0])
        assert_optimizes(:scalar, 6.0, "mov R0.w, 2*R0.x", res, "Optimizer should take advantage of source modifiers")

        f = "float f(vector x) { return 0.5*ycomp(x); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [3.0, 5.0, 7.0])
        assert_optimizes(:scalar, 2.5, "mov R0.w, 0.5*R0.y", res, "Optimizer should take advantage of source modifiers")

        f = "float f(vector x) { return -4*zcomp(x); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [3.0, 5.0, 7.0])
        assert_optimizes(:scalar, -28.0, "mov R0.w, -4*R0.z", res, "Optimizer should take advantage of source modifiers")
    end
    def inactive_test_tuple_decompose
        f = "float f(float a,b,c) { vector v = vector(a,b,c); return ycomp(v); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 3.0, 5.0, 7.0)
        assert_optimizes(:scalar, 5.0, "mov R0.w, R1.w", res, "Optimizer should efficiently decompose tuples")
    end
    def test_tuple_2component_init
        f = "vector f() { vector v = vector(1.0, 2.0, 1.0); return v; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f)
        assert_optimizes(:vector, [1.0, 2.0, 1.0], ["mov R0.xz, 1.0", "mov R0.y, 2.0"], res, "Optimizer should take advantage of 2-element writeback masks")

        f = "vector f(float a,b) { vector v = vector(a,a,b); return v; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 3.0, 5.0)
        assert_optimizes(:vector, [3.0, 3.0, 5.0], ["mov R0.xy, R0.w", "mov R0.z, R1.w"], res, "Optimizer should take advantage of 2-element writeback masks")

        f = "vector f(float a,b) { vector v = vector(a,b,a); return v; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 3.0, 5.0)
        assert_optimizes(:vector, [3.0, 5.0, 3.0], ["mov R0.xz, R0.w", "mov R0.y, R1.w"], res, "Optimizer should take advantage of 2-element writeback masks")

        f = "vector f(float a,b) { vector v = vector(a,b,b); return v; }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 3.0, 5.0)
        assert_optimizes(:vector, [3.0, 5.0, 5.0], ["mov R0.x, R0.w", "mov R0.yz, R1.w"], res, "Optimizer should take advantage of 2-element writeback masks")
    end
    def test_smod_getcomp_pre
        f = "float f(vector x) { return xcomp(2*x); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [3.0, 5.0, 7.0])
        assert_optimizes(:scalar, 6.0, "mov R0.w, 2*R0.x", res, "Optimizer should take advantage of source modifiers")

        f = "float f(vector x) { return ycomp(0.5*x); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [3.0, 5.0, 7.0])
        assert_optimizes(:scalar, 2.5, "mov R0.w, 0.5*R0.y", res, "Optimizer should take advantage of source modifiers")

        f = "float f(vector x) { return zcomp(-4*x); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [3.0, 5.0, 7.0])
        assert_optimizes(:scalar, -28.0, "mov R0.w, -4*R0.z", res, "Optimizer should take advantage of source modifiers")
    end
    def test_const_fold_sqrt
        f = "float f() { return sqrt(4.0); }"
        res = Driver.compile_run_and_return_asm(__LINE__, f)
        assert_optimizes(:scalar, 2.0, "mov R0.w, 2.0", res, "Constant sqrt folding should work")
    end
    def test_surface_shader_opt
        f = "surface f() {
            Ci = (0.6, 0.6, 0.6);
        }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [0.0, 0.0, 0.0], [1.0, 0.0, 0.0])
        assert_optimizes(:vector, [0.6, 0.6, 0.6], "mov R0.xyz, 0.6", res, "Surface shader code should not compute unused magic variables")
    end
    def test_dp3_rsq
        f = "vector f(vector x) {
            return normalize(x);
        }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [3.0, 4.0, 0.0])
        assert_optimizes(:vector, [0.6, 0.8, 0.0], ["dp3_rsq R15.x, R0.xyz, R0.xyz", "mul R0.xyz, R0.xyz, S.x"], res, "dp3_rsq should be used by optimizer")
    end
    def test_mad
        f = "vector f(vector x, y) {
            return x^y;
        }"
        a = [1.0, 2.0, 3.0]
        b = [5.0, 7.0, 9.0]
        c = [a[1]*b[2] - a[2]*b[1], a[2]*b[0] - a[0]*b[2], a[0]*b[1] - a[1]*b[0]]

        res = Driver.compile_run_and_return_asm(__LINE__, f, a, b)
        assert_optimizes(:vector, c, ["mul R2.xyz, R0.zxy, R1.yzx", "mad R0.xyz, R0.yzx, R1.zxy, -R2.xyz"], res, "mad should be used by optimizer")
    end
    def test_mad_2
        f = "vector f(vector x, y, z) {
            return 2*x + ((-4*y) * (0.5*z));
        }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [1.0, 2.0, 3.0], [4.0, 6.0, 8.0], [2.0, 5.0, 11.0])
        assert_optimizes(:vector, [-14.0, -56.0, -170.0], ["mad R0.xyz, -4*R1.xyz, 0.5*R2.xyz, 2*R0.xyz"], res, "mad should be used by optimizer")
   end
   def test_mad_3
        f = "vector f(vector x, y, z) {
            return 2*x - ((4*y) * (0.5*z));
        }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [1.0, 2.0, 3.0], [4.0, 6.0, 8.0], [2.0, 5.0, 11.0])
        assert_optimizes(:vector, [-14.0, -56.0, -170.0], ["mad R0.xyz, -4*R1.xyz, 0.5*R2.xyz, 2*R0.xyz"], res, "mad should be used by optimizer")
    end
    def test_mad_4
        f = "vector f(vector x, y, z) {
            vector w = (2*y) * (-2*z);
            return 2*x + 4*w;
        }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [1.0, 2.0, 3.0], [4.0, 6.0, 8.0], [2.0, 5.0, 11.0])
        assert_optimizes(:vector, [-126.0, -476.0, -1402.0], ["mad R0.xyz, 4*R1.xyz, -4*R2.xyz, 2*R0.xyz"], res, "mad should be used by optimizer")
    end
end

class Test_Optimize_CSE < Test::Unit::TestCase
    def expect(*asms)
        "\n; Generated code:\n"+
        "SUBROUTINE_ENTRY_f:\n"+
        asms.flatten.map{|a| "    #{a}\n"}.join+
        "    mov R0, R0 + return or xyzw (>=0 or <1)\n"
    end
    def assert_optimizes(retval_type, expected_retval, optimized_code, res, msg)
        asm_output, abi_res = *res
        assert_equal(expected_retval, abi_res[retval_type], msg)
        assert_equal(expect(optimized_code), asm_output, msg)
    end
    def test_cse_simple
        f = "float f(vector x) {
            float y = x.x;
            float z = x.x;
            return y+z;
        }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [2.0, 3.0, 5.0])
        # FIXME: Some other register can be used in place fo R1.w
        assert_optimizes(:scalar, 76.0, ["dp3 R1.w, R0.xyz, R0.xyz",
                                         "mov R0.w, 2*R1.w"], res, "Optimizer should perform common subexpression elimination")
    end
    def test_simplification
        f = "float f(float x) {
            float y = 1*(x+0);
            float z = (0+x)*1;
            return y+z;
        }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 3.0)
        assert_optimizes(:scalar, 6.0, ["mov R0.w, 2*R0.w"], res, "Optimizer should perform common subexpression elimination")
    end
    def test_cse_unify
        f = "vector f(float x) {
            float a = x-0;
            float b = x+0;
            float c = x*1;
            float d = x/1;
            return vector(a+b, c+d, b+x);
        }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, 8.0)
        # NOTE:
        # It won't be fully optimized - R0.w cannot be forwarded, as it has preallocated register
        # (if we forwarded registers like that, we would generate ABI conflicts),
        # and R0.xyz and R1.w cannot be friends because they have different types.
        # We couldn't even "reverse forward" R0.xyz, because it also has preallocated register.
        #
        # I don't know what optimizations to do to get it right.
        
        #assert_optimizes(:vector, [16.0, 16.0, 16.0], "mov R0.xyz, 2*R0.w", res, "Optimizer should perform common subexpression elimination")
        assert_optimizes(:vector, [16.0, 16.0, 16.0], ["mov R1.w, 2*R0.w", "mov R0.xyz, R1.w"], res, "Optimizer should perform common subexpression elimination")
    end
    def test_cse_length
        f = "float f(vector x) {
            float y = length(x);
            float z = length(x);
            return y+z;
        }"
        res = Driver.compile_run_and_return_asm(__LINE__, f, [3.0, 4.0, 0.0])
        # Some other register can be used
        assert_optimizes(:scalar, 10.0, ["dp3_rsq R15.x, R0.xyz, R0.xyz",
                                         "mov_rcp R15.y, S.x",
                                         "mov R0.w, 2*S.y"], res, "Optimizer should perform common subexpression elimination")
    end
end

# Rendered images cannot be tested automatically
# The most we can do is MD5ing them and compared with known-good results
#
# These test cases test using normal function as shaders.
# "Real" shaders are compiled to functions anyway.
class Test_Global_Shaders < Test::Unit::TestCase
    def assert_md5(expected_digest, fn, msg)
        assert_equal(expected_digest, Digest::MD5.hexdigest(File.read(fn)), msg)
    end
    def test_render
        f = "color s(point X) { return color(xcomp(X), ycomp(X), zcomp(X)); }"
        res = Driver.compile_and_run_shader(__LINE__, f, :x => 256, :y => 256)
        assert_md5("5453c1d7ebcb140dd52656f7147c49e7", res, "Trivial `render' shader should work")
    end
    # Equivalent of VM test 1
    def test_hit_miss
        f = "color s(point P) {
            vector orig  = 0.0;
            vector dir0  = vector(0.0, 0.0, 1.0);
            vector right = vector(1.0, 0.0, 0.0);
            vector up    = vector(0.0, 1.0, 0.0);
            vector xofs  = right * (-1.0 + xcomp(P) * 2);
            vector yofs  = up    * ( 1.0 - ycomp(P) * 2);
            vector dir   = normalize(dir0 + xofs + yofs);
            vector param = vector(0.00001, 10000.0, 0.0);
            vector hit   = trace_instr(orig, dir, param);
            color bg = color(0.0, 0.0, 0.0);
            
            if(zcomp(hit) > 0) {
                // float td = get_hit_tri();
                return color(0.0, 1.0, 1.0);
                /*
                if(td == 0.0) {
                    return color(1.0, 0.0, 0.0);
                } else if (td == 1.0) {
                    return color(0.0, 1.0, 0.0);
                } else {
                    return color(0.0, 0.0, 1.0);
                }*/
            } else {
                return bg;
            }
        }"
        res = Driver.compile_and_run_shader(__LINE__, f, :x => 400, :y => 400, :scene => "../hardware_emulator/triangles.dat")
        # bdf3b37d7ed6fc59465797bcc55efa95 is colorless but works
        assert_md5("bdf3b37d7ed6fc59465797bcc55efa95", res, "hit/miss shader should work")
    end

    # Equivalent of Test 7 of the VM
    def test_miniphong_separate_function
        f = "color s(vector orig, dir) {
            vector hit = get_hit();
            vector hit_point = (dir * zcomp(hit)) + orig;
            vector c0 = vector(-0.2, 0.5, 0.0);
            vector hit_to_light = normalize(c0 - hit_point);
            
            vector A = load(get_hit_tri());
            float Aw = get_i0_w();
            
            vector a = load(xcomp(A));
            vector b = load(ycomp(A));
            vector c = load(zcomp(A));
            
            vector d = load(Aw);
            // No vector->color casts even when we want them
            color triangle_color = color(xcomp(d), ycomp(d), zcomp(d));
            
            vector norm = normalize((b-a) ^ (c-a));
            if (norm . dir > 0)
                norm = -norm;
            float uf = clamp(norm . hit_to_light, 0, 1);
            float I  = clamp(0.6 * uf + 0.4, 0, 1);
            return triangle_color * I;
        }
        
        color m(point P) {
            point  orig   = (0.0, 0.0, 0.0);
            vector dir0   = (0.0, 0.0, 1.0);
            vector diri   = (2.0,-2.0, 0.0);
            vector s      = (0.5, 0.5, 0.0);
            vector dir    = normalize(dir0 + (P - s) * diri);
            vector params = (0.01, 1000.0, 0.0); /* (eps, infty, 0) */
            
            vector hit    = trace_instr(orig, dir, params);
            if(zcomp(hit) > 0) {
                return s(orig, dir);
            } else {
                return color(0.2, 0.2, 1.0);
            }
        }"
        res = Driver.compile_and_run_shader(__LINE__, f, :x => 400, :y => 400, :scene => "../hardware_emulator/bunny-compiled.dat", :entry => 'm')
        assert_md5("9f0e196a582f550c3804f954e6904eb7", res, "mini-Phong shader should work")
    end

    # Like test_miniphong_separate_function but a bit higher-level
    def test_miniphong_separate_function_2
        f = "color s(vector orig, dir) {
            vector hit = get_hit();
            vector hit_point = (dir * zcomp(hit)) + orig;
            vector c0 = vector(-0.2, 0.5, 0.0);
            vector hit_to_light = normalize(c0 - hit_point);
            
            vector norm = get_Ng();
            if (norm . dir > 0)
                norm = -norm;
            float uf = clamp(norm . hit_to_light, 0, 1);
            float I  = clamp(0.6 * uf + 0.4, 0, 1);
            return get_Cs() * I;
        }
        
        color m(point P) {
            point  orig   = (0.0, 0.0, 0.0);
            vector dir0   = (0.0, 0.0, 1.0);
            vector diri   = (2.0,-2.0, 0.0);
            vector s      = (0.5, 0.5, 0.0);
            vector dir    = normalize(dir0 + (P - s) * diri);
            vector params = (0.01, 1000.0, 0.0); /* (eps, infty, 0) */
            
            vector hit    = trace_instr(orig, dir, params);
            if(zcomp(hit) > 0) {
                return s(orig, dir);
            } else {
                return color(0.2, 0.2, 1.0);
            }
        }"
        res = Driver.compile_and_run_shader(__LINE__, f, :x => 400, :y => 400, :scene => "../hardware_emulator/bunny-compiled.dat", :entry => 'm')
        assert_md5("9f0e196a582f550c3804f954e6904eb7", res, "mini-Phong shader should work")
    end

    def test_miniphong_separate_function_3
        f = "color s(vector orig, dir) {
            vector hit = get_hit();
            vector hit_point = (dir * zcomp(hit)) + orig;
            vector c0 = vector(-0.2, 0.5, 0.0);
            vector hit_to_light = normalize(c0 - hit_point);
            
            vector norm = get_Ng();
            if (norm . dir > 0)
                norm = -norm;
            float uf = clamp(norm . hit_to_light, 0, 1);
            float I  = clamp(0.6 * uf + 0.4, 0, 1);
            return get_Cs() * I;
        }
        
        color m(point P) {
            point  orig   = (0.0, 0.0, 0.0);
            vector dir0   = (0.0, 0.0, 1.0);
            vector diri   = (2.0,-2.0, 0.0);
            vector s      = (0.5, 0.5, 0.0);
            vector dir    = normalize(dir0 + (P - s) * diri);
          
            return trace(orig, dir);
        }"
        res = Driver.compile_and_run_shader(__LINE__, f, :x => 400, :y => 400, :scene => "../hardware_emulator/bunny-compiled.dat", :entry => 'm',
                                            :"C31.x" => 0.01, :"C31.y" => 1000.0)
        assert_md5("628c4bb41405fe0c4f1357ecc603c331", res, "mini-Phong shader should work")
    end

    def test_miniphong_surface_shader
        f = "surface s() {
            vector norm = N;
            if (norm . I > 0)
                norm = -norm;

            vector light_source = vector(-0.2, 0.5, 0.0);
            vector hit_to_light = normalize(light_source - P);
            
            float uf = clamp(norm . hit_to_light, 0, 1);
            float i  = clamp(0.6 * uf + 0.4, 0, 1);
            Ci = Cs * i;
        }
        
        color m(point P) {
            point  orig   = (0.0, 0.0, 0.0);
            vector dir0   = (0.0, 0.0, 1.0);
            vector diri   = (2.0,-2.0, 0.0);
            vector s      = (0.5, 0.5, 0.0);
            vector dir    = normalize(dir0 + (P - s) * diri);
          
            return trace(orig, dir);
        }"
        res = Driver.compile_and_run_shader(__LINE__, f, :x => 400, :y => 400, :scene => "../hardware_emulator/bunny-compiled.dat", :entry => 'm',
                                            :"C31.x" => 0.01, :"C31.y" => 1000.0)
        assert_md5("628c4bb41405fe0c4f1357ecc603c331", res, "mini-Phong shader should work")
    end

    def test_depth_separate_function
        f = "color s(vector orig, dir) {
            vector hit = get_hit();
            float dist = zcomp(hit);
            
            float g = 1.8 - dist;
            float rb = 1.0 - g;
            color c = (rb, g, rb);
            return c;
        }
        
        color m(point P) {
            point  orig   = (0.0, 0.0, 0.0);
            vector dir0   = (0.0, 0.0, 1.0);
            vector diri   = (2.0,-2.0, 0.0);
            vector s      = (0.5, 0.5, 0.0);
            vector dir    = normalize(dir0 + (P - s) * diri);
            vector params = (0.01, 1000.0, 0.0); /* (eps, infty, 0) */
            
            vector hit    = trace_instr(orig, dir, params);
            if(zcomp(hit) > 0) {
                return s(orig, dir);
            } else {
                return 0;
            }
        }"
        res = Driver.compile_and_run_shader(__LINE__, f, :x => 400, :y => 400, :scene => "../hardware_emulator/bunny-compiled.dat", :entry => 'm')
        assert_md5("436777d3cd31f0aa24b9b725620ed50b", res, "Depth shader for bunny should work")
    end

    def test_depth_lowlevel_surfaceshader
        f = "surface s() {
            vector hit = get_hit();
            float dist = zcomp(hit);
            
            float g = 1.8 - dist;
            float rb = 1.0 - g;
            Ci = (rb, g, rb);
        }
        
        color m(point P) {
            point  orig   = (0.0, 0.0, 0.0);
            vector dir0   = (0.0, 0.0, 1.0);
            vector diri   = (2.0,-2.0, 0.0);
            vector s      = (0.5, 0.5, 0.0);
            vector dir    = normalize(dir0 + (P - s) * diri);
            vector params = (0.01, 1000.0, 0.0); /* (eps, infty, 0) */
            
            vector hit    = trace_instr(orig, dir, params);
            if(zcomp(hit) > 0) {
                return s(orig, dir);
            } else {
                return 0;
            }
        }"
        res = Driver.compile_and_run_shader(__LINE__, f, :x => 400, :y => 400, :scene => "../hardware_emulator/bunny-compiled.dat", :entry => 'm')
        assert_md5("436777d3cd31f0aa24b9b725620ed50b", res, "Depth shader for bunny should work")
    end

    def test_depth_highlevel_surfaceshader
        f = "surface s() {
            float dist = distance(P, E);
            
            float g = 1.8 - dist;
            float rb = 1.0 - g;
            Ci = (rb, g, rb);
        }
        
        color m(point P) {
            point  orig   = (0.0, 0.0, 0.0);
            vector dir0   = (0.0, 0.0, 1.0);
            vector diri   = (2.0,-2.0, 0.0);
            vector s      = (0.5, 0.5, 0.0);
            vector dir    = normalize(dir0 + (P - s) * diri);
          
            return trace(orig, dir);
        }"
        res = Driver.compile_and_run_shader(__LINE__, f, :x => 400, :y => 400, :scene => "../hardware_emulator/bunny-compiled.dat", :entry => 'm',
                                            :"C31.x" => 0.01, :"C31.y" => 1000.0)
        assert_md5("436777d3cd31f0aa24b9b725620ed50b", res, "Depth shader for bunny should work")
    end

    def test_depth
        f = "color s(point P) {
            point  orig   = (0.0, 0.0, 0.0);
            vector dir0   = (0.0, 0.0, 1.0);
            vector diri   = (2.0,-2.0, 0.0);
            vector s      = (0.5, 0.5, 0.0);
            vector dir    = normalize(dir0 + (P - s) * diri);
            vector params = (0.01, 1000.0, 0.0); /* (eps, infty, 0) */
            vector hit    = trace_instr(orig, dir, params);
            
            float dist = zcomp(hit);
            float g = 1.8 - dist;
            float rb = 1.0 - g;
            color c = (rb, g, rb);
            
            return c;
        }"
        res = Driver.compile_and_run_shader(__LINE__, f, :x => 400, :y => 400, :scene => "../hardware_emulator/bunny-compiled.dat")
        assert_md5("436777d3cd31f0aa24b9b725620ed50b", res, "Depth shader for bunny should work")
    end
end

# INACTIVE
class Test_Matrix # < Test::Unit::TestCase
   def test_matrix
       f = 'point f(point p) {
           matrix m = (
             5, 0, 0, 0,
             0, 7, 0, 0,
             0, 0,11, 0,
             0, 0, 0, 1
           );
           return transform(m, p);
       }'
       res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0])
       assert_equal([5.0, 14.0, 33.0], res[:vector], "Matrices should work")
       
       f = 'point f(point p) {
           matrix m = (
             1, 0, 0, 0,
             0, 1, 0, 0,
             0, 0, 1, 0,
             4, 6, 9, 1
           );
           return transform(m, p);
       }'
       res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0])
       assert_equal([5.0, 8.0, 12.0], res[:vector], "Matrices should work")
       
       f = 'point f(point p) {
           matrix m = (
             1, 0, 0, 0,
             0, 1, 0, 0,
             0, 0, 1, 0,
             4, 6, 9, 1
           );
           return transform(m+m, p);
       }'
       res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0])
       assert_equal([10.0, 16.0, 24.0], res[:vector], "Matrices should work")
      
       f = 'point f(point p) {
           matrix m = (
             1, 0, 0, 0,
             0, 1, 0, 0,
             0, 0, 1, 0,
             4, 6, 9, 1
           );
           return transform(m+m, p);
       }'
       res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0])
       assert_equal([9.0, 14.0, 21.0], res[:vector], "Matrices should work")


       f = 'point f(point p) {
           matrix m = 5;
           return transform(m, p);
       }'
       res = Driver.compile_and_run(__LINE__, f, [1.0, 2.0, 3.0])
       assert_equal([5.0, 10.0, 15.0], res[:vector], "Matrices should work")
   end
end

class Test_String < Test::Unit::TestCase
   def test_string
       f = 'float f(string x) {
           if(x == "one")
               return 1.0;
           else if(x == "two")
               return 2.0;
           else if(x == "three")
               return 3.0;
           else if(x == "four")
               return 4.0;
           else if(x == "five")
               return 5.0;
           else if(x == "six")
               return 6.0;
           else if(x == "seven")
               return 7.0;
           else if(x == "eight")
               return 8.0;
           else
              return 0.0; }'
       ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight"].each_with_index{|s,i|
           res = Driver.compile_and_run(__LINE__, f, s)
           assert_equal(i.to_f, res[:scalar], "Strings should work")
       }
   end
end

class Test_Cpp < Test::Unit::TestCase
    def test_cpp
        f = '#define FOO 2.0
             float f() { return FOO; }'
        res = Driver.cpp_compile_and_run(__LINE__, f)
        assert_equal(2.0, res[:scalar], "cpp should work")
    end
end

# TODO: Real option parsing ?
if ARGV[0] == "--coverage"
   ARGV.shift
   File.delete "coverage.data" if File.exists? "coverage.data"
   $coverage_testing = true
end
