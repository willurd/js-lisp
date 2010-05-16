task :chrome_extension do
  
end

task :test do
  sh "
    # Press Ctrl-C to shutdown the test server
    cd src
    open http://localhost:8000/tests/tests.html
    python -c 'import SimpleHTTPServer; SimpleHTTPServer.test()'
  "
end

task :nodetest do
  sh "
    cd src/tests
    node node-tests.js
  "
end
