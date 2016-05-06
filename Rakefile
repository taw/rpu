desc "Build"
task "build" do
  Dir.chdir("hardware_emulator") do
    sh "make"
    sh "make bunny-compiled.dat triangles.dat"
  end
  Dir.chdir("slcompiler_minimal") do
    sh "make"
  end
  Dir.chdir("slcompiler_old") do
    sh "make"
  end
end

desc "Test"
task "test" do
  Dir.chdir "tests" do
    sh "ruby -I. tests.rb"
  end
  Dir.chdir "hardware_emulator" do
    sh "./tests.rb"
  end
end
