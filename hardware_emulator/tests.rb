#!/usr/bin/ruby

require 'test/unit'
require 'digest/md5'

class Test_VM < Test::Unit::TestCase
    def assert_md5(expected_digest, fn)
        assert_equal(expected_digest, Digest::MD5.hexdigest(File.read(fn)))
    end
    def make_test(i)
        system "make native-code SUBPROJS=test#{i}" or raise "Building test#{i} failed"
    end
    def make(fn)
        system "make #{fn}" or raise "Building #{fn} failed"
    end
    def test_1
        make_test 1
        system "./test1" or raise "Running test1 failed"
        assert_md5("1ebb2870ccfeaef7cf320bea547a64f2", "test1.pnm")
    end
    def test_2
        make_test 2
        system "./test2" or raise "Running test2 failed"
        assert_md5("beb33609c159b8649610f86090fbbe2e", "test2.pnm")
    end
    def test_3
        make_test 3
        system "./test3" or raise "Running test3 failed"
        assert_md5("03002aed35e3f0f2423123687b216433", "test3_closeup_bilinear_b.pnm")
        assert_md5("03002aed35e3f0f2423123687b216433", "test3_closeup_bilinear_f4.pnm")
        assert_md5("03002aed35e3f0f2423123687b216433", "test3_closeup_bilinear.pnm")
        assert_md5("3b807b18612223231f086437a22c0f4d", "test3_closeup_ln_b.pnm")
        assert_md5("3b807b18612223231f086437a22c0f4d", "test3_closeup_ln_f4.pnm")
        assert_md5("3b807b18612223231f086437a22c0f4d", "test3_closeup_ln.pnm")
        assert_md5("51ae1e5e83c06ffd6163db4b6cdae47a", "test3_closeup_nl_b.pnm")
        assert_md5("51ae1e5e83c06ffd6163db4b6cdae47a", "test3_closeup_nl_f4.pnm")
        assert_md5("51ae1e5e83c06ffd6163db4b6cdae47a", "test3_closeup_nl.pnm")
        assert_md5("2c9dfe133c2ab717fd6ff1ebd4e5bead", "test3_closeup_nn_b.pnm")
        assert_md5("2c9dfe133c2ab717fd6ff1ebd4e5bead", "test3_closeup_nn_f4.pnm")
        assert_md5("2c9dfe133c2ab717fd6ff1ebd4e5bead", "test3_closeup_nn.pnm")
        assert_md5("b5fd5c8a1dc51cf9e5f4733316d6e663", "test3_full.pnm")
        assert_md5("dedcc624dea950fc9248796fed036aab", "test3_uv.pnm")
    end
    def test_4
        make_test 4
        make "bunny-compiled.dat"
        system "./test4" or raise "Running test4 failed"
        assert_md5("436777d3cd31f0aa24b9b725620ed50b", "test4.pnm")
    end
    def test_5
        make_test 5
        make "bunny-compiled.dat"
        make "depth.shr"
        system "./test5" or raise "Running test5 failed"
        assert_md5("436777d3cd31f0aa24b9b725620ed50b", "test5.pnm")
    end
    def test_6
        make_test 6
        make "bunny-compiled.dat"
        make "phong.shr"
        system "./test6" or raise "Running test6 failed"
        assert_md5("628c4bb41405fe0c4f1357ecc603c331", "test6.pnm")
    end
    def test_7
        make_test 7
        make "bunny-compiled.dat"
        make "phong_trace.shr"
        system "./test7" or raise "Running test7 failed"
        assert_md5("9f0e196a582f550c3804f954e6904eb7", "test7.pnm")
    end
    def test_8
        make_test 8
        make "edgar-compiled.dat"
        make "phong_trace.shr"

        system "./test8" or raise "Running test8 failed"
        assert_md5("37a400d820ef07c6d9fe543a9351ccea", "test8.pnm")
    end
    def test_9
        make_test 9
        make "edgar-compiled.dat"
        make "phong_normal.shr"
        system "./test9" or raise "Running test9 failed: #{$!}"
        assert_md5("ed7a4d42e3e4de302dfa62555325d557", "test9.pnm")
    end
end
