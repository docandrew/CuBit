--  The source tree tests/ccl-build/run.sh serves as a gzip'd tar archive:
--  a library from outside the repository, fetched by ccl-build.
package Fetched_Greeting with Pure is
   function Text return String;
end Fetched_Greeting;
