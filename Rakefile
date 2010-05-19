require "yaml"

LISP_JS = "lisp"
LISP_JS_MIN = "lisp.min"

BUILD_CONFIG = "build.yaml"
BUILD_DIRECTORY = "build"

JSL_BINARY = "./support/bin/jsl"
JSL_CONFIG = "./support/jsl.conf"

task :default => :buildall

# Build the javascript lisp library and core lisp library
task :buildall => [:build, :minify]

task :build do
  puts "+ Building project"
  File.open(BUILD_CONFIG) do |out|
    config = YAML::load(out)
    config.each do |outfilename, list|
      outfilename = File.join(BUILD_DIRECTORY, outfilename)
      File.delete(outfilename) if File.exists?(outfilename)
      puts "Writing #{outfilename}"
      File.open(outfilename, 'w') do |outfile|
        list.each do |filename|
          puts "  - writing #{filename} to #{outfilename}"
          outfile << File.read(filename)
        end
      end
    end
  end
end

task :minify do
  puts "+ Minifying #{LISP_JS}.js"
  minfile = "#{BUILD_DIRECTORY}/#{LISP_JS_MIN}.js"
  File.delete(minfile) if File.exists?(minfile)
  sh "
    ruby support/bin/jsmin.rb < #{BUILD_DIRECTORY}/#{LISP_JS}.js > #{minfile}
  "
end

task :test => [:build] do
  sh "
    # Press Ctrl-C to shutdown the test server
    open http://localhost:8000/tests/tests.html
    python -c 'import SimpleHTTPServer; SimpleHTTPServer.test()'
  "
end

task :lint => [:build] do
  puts "+ Running jsl (javascript lint)"
  sh "
    #{JSL_BINARY} -conf #{JSL_CONFIG} -process #{BUILD_DIRECTORY}/#{LISP_JS}.js | less
  "
end

task :nodetest => [:build] do
  puts "+ Running tests with node.js"
  sh "
    cd tests
    node node-tests.js
  "
end

# task :chrome_extension do
#   
# end
