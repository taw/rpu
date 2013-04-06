require 'digest/md5'

$coverage_testing = false

# Needs to be identical to the one used in the compiler for relevant results
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

class Driver
    def self.save(f, fn)
        File.open(fn, "w") {|fh| fh.print f}
    end
    def self.compile(fn)
        if $coverage_testing
            system "rcov --aggregate coverage.data --no-html -t ../codegen/codegen.rb -- #{fn}.sl -o #{fn}.asm >/dev/null" or raise "Cannot compile #{fn}.sl"
        else
            system "../codegen/codegen.rb #{fn}.sl -o #{fn}.asm" or raise "Cannot compile #{fn}.sl"
        end
        system "./asm <#{fn}.asm >#{fn}.shr 2>#{fn}.epts" or raise "Cannot assemble #{fn}.asm"
    end
    def self.cpp_and_compile(fn)
        if $coverage_testing
            system "rcov --aggregate coverage.data --no-html -t ../codegen/codegen.rb -- -p #{fn}.sl -o #{fn}.asm >/dev/null" or raise "Cannot compile #{fn}.sl"
        else
            system "../codegen/codegen.rb -p #{fn}.sl -o #{fn}.asm" or raise "Cannot compile #{fn}.sl"
        end
        system "./asm <#{fn}.asm >#{fn}.shr 2>#{fn}.epts" or raise "Cannot assemble #{fn}.asm"
    end
    def self.vmrun_function(shader_fn, *args)
        # This is so totally ain't gonna work.
        # No, seriously, I mean - we cannot get a meaningful return value
        # unless we know something about the function.
        # For example if we don't know whether the function is supposed
        # to return a scalar or a vector (not to mention returning multiple values)
        # then we cannot get a meaningful answer from the machine state.
        # So we're going to cheat here, by returning
        # {:scalar => sth, :vector => sth}
        # and the test function will do something about it. Ugly.
        a = ABI.prepare(args)
        rv = `./vm_function #{shader_fn}.shr #{a}`
        ABI.parse_retval(rv)
    end
    def self.resolve_entry_point(shader_fn, entry_point)
        entry_points = {}
        File.read("#{shader_fn}.epts").each_line{|line|
            line =~ /^(\S+)\s+(\d+)$/ or raise "Bad format of entry-points file #{shader_fn}.epts: #{line}"
            entry_points[$1] = $2.to_i
        }
        entry_points["SUBROUTINE_ENTRY_#{entry_point}"] or raise "Entry point #{entry_point} in #{shader_fn}.shr unknown"
    end
    def self.vmrun_shader(shader_fn, kwargs)
        entry = kwargs.delete(:entry)
        kwargs["pc"] = (entry ? resolve_entry_point(shader_fn, entry) : 0)
        kwargs["fn"] = "#{shader_fn}.pnm"
        system "./vm_shader #{shader_fn}.shr #{kwargs.map{|k,v| "#{k}=#{v}"}.join ' '}"
    end
    def self.vmrun_function_multifun(shader_fn, entry_point, *args)
        e = resolve_entry_point(shader_fn, entry_point)

        a = ABI.prepare(args)
        rv = `./vm_function #{shader_fn}.shr pc=#{e} #{a}`
        ABI.parse_retval(rv)
    end
    def self.compile_and_run(n, f, *args)
        save(f, "#{n}.sl")
        compile(n)
        vmrun_function(n, *args)
    end
    def self.cpp_compile_and_run(n, f, *args)
        save(f, "#{n}.sl")
        cpp_and_compile(n)
        vmrun_function(n, *args)
    end
    def self.compile_run_and_return_asm(n, f, *args)
        save(f, "#{n}.sl")
        compile(n)
        [File.read("#{n}.asm"), vmrun_function(n, *args)]
    end
    def self.compile_and_run_multifun(n, f, *args)
        save(f, "#{n}.sl")
        compile(n)
        vmrun_function_multifun(n, *args)
    end
    def self.compile_and_run_shader(n, f, kwargs)
        save(f, "#{n}.sl")
        compile(n)
        vmrun_shader(n, kwargs)
        #return Digest::MD5.hexdigest(File.read("#{n}.pnm"))
        return "#{n}.pnm"
    end
end

# This class manages ABI conventions
# They are basically pretty simple - we use Ri.xyz as vector registers,
# Ri.w as scalar registers, and fill arguments into first free registers
# of the right kind.
class ABI
    def self.prepare(args)
        scalars = 0
        vectors = 0
        r = []
        args.each{|a|
            # Matrices and Strings are not supported
            if a.is_a? Array # Vector (vector/normar/point/color)
                r << "R#{vectors}.x=#{a[0]}"
                r << "R#{vectors}.y=#{a[1]}"
                r << "R#{vectors}.z=#{a[2]}"
                vectors += 1
            elsif a.is_a? Float
                r << "R#{scalars}.w=#{a}"
                scalars += 1
            elsif a.is_a? String
                r << "R#{scalars}.w=#{a.compiler_hash}"
                scalars += 1
            else
                raise "Unknown type of argument: #{a}"
            end
        }
        # Return a shell-friendly version, like "R0.x=0.0 R0.y=1.0 R0.z=2.0 R0.w=3.0 R1.w=-4.2"
        r.join " "
    end
    def self.parse_retval(rv)
        regs = Hash.new(0.0)
        # Not very nice
        rv.scan(/(R\d+\..)\s*=\s*(\S+)/) {
            regs[$1] = $2.to_f
        }
        return {:scalar => regs["R0.w"],
                :vector => [regs["R0.x"], regs["R0.y"], regs["R0.z"]]}
    end
end
