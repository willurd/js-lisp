require "yaml"

LISP_JS = "lisp"
LISP_JS_MIN = "lisp.min"

DOCS_ROOT = "docs"

BUILD_CONFIG = "build.yaml"
BUILD_DIRECTORY = "build"

JSL_BINARY = "support/bin/jsl"
JSL_CONFIG = "support/jsl.conf"

JSDOC_ROOT = "support/bin/jsdoc-toolkit"

task :default => :buildall

task :watch do
  sh "python support/bin/watch.py"
end

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
      system("mkdir -p #{File.dirname(outfilename)}")
      File.open(outfilename, 'w') do |outfile|
        list.each do |filename|
          puts "  - writing #{filename} to #{outfilename}"
          outfile << File.read(filename) + "\n"
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

task :doc do
  sh "
    rm -rf #{DOCS_ROOT}/css #{DOCS_ROOT}/symbols #{DOCS_ROOT}/*.html
    mkdir -p #{DOCS_ROOT}
    java -jar #{JSDOC_ROOT}/jsrun.jar #{JSDOC_ROOT}/app/run.js -a -d=#{DOCS_ROOT} \
      -t=#{JSDOC_ROOT}/templates/codeview #{BUILD_DIRECTORY}/#{LISP_JS}.js
  "
end

task :webserver do
  sh "
    # Press Ctrl-C to shutdown the test server
    python -c 'import SimpleHTTPServer; SimpleHTTPServer.test()'
  "
end

task :test => [:build] do
  sh "
    open http://localhost:8000/tests/
  "
  Rake::Task["webserver"].invoke
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

task :line, :filename, :num do |t, args|
  # Because the files that actually get run in the browser are compiled
  # files, put together from a bunch of smaller files, the line numbers
  # that are reported when errors occur, and by programs like js lint, are
  # not the line numbers actually needed to easily find the problems in
  # code. This task takes one of the files in the build.yaml file and a
  # line number, and returns the actual file/line that we want.
  File.open(BUILD_CONFIG) do |out|
    config = YAML::load(out)
    files = config[args.filename]
    if not files
      raise "No build file #{args.filename}"
    end
    line = args.num.to_i
    current_line = 0
    found = false
    files.each do |filename|
      content = File.read(filename)
      file_line_count = content.lines.count + 1 # For the extra newline placed after each file in :build
      current_line += file_line_count
      if current_line >= line
        actual_line = file_line_count - (current_line - line)
        found = true
        puts "File: #{filename}, line: #{actual_line}"
        sh "if which mate; then mate #{filename} -l #{actual_line}; fi"
        break
      end
    end
    if not found
      raise "File not found with line number #{line}"
    end
  end
end
