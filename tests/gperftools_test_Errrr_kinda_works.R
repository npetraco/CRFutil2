#outf <- paste0(tempdir(), "/profile.out")
outf <- paste0("/Users/karen2/codes/R/CRFutil2/tests/profile.out")
outf

start_profiler(outf)
#run_your_cpp_stuff()
# Run Rcpp code for at least 10 seconds:
for(i in 1:10000000) {
  junk <- ff0_C(st = "b", ss_dim = 2, st_vec = c("a","b"))
}
stop_profiler()

file.edit(outf) # The output is a binary file so you may see nothing.

#Works:
#pprof --text /Users/karen2/codes/R/CRFutil2/src/CRFutil2.so /Users/karen2/codes/R/CRFutil2/tests/profile.out
#pprof --web /Users/karen2/codes/R/CRFutil2/src/CRFutil2.so /Users/karen2/codes/R/CRFutil2/tests/profile.out

#Doesn't work:
#pprof --gv /Users/karen2/codes/R/CRFutil2/src/CRFutil2.so /Users/karen2/codes/R/CRFutil2/tests/profile.out
#pprof --evince /Users/karen2/codes/R/CRFutil2/src/CRFutil2.so /Users/karen2/codes/R/CRFutil2/tests/profile.out


# Output type:
#   --text              Generate text report
# --stacks            Generate stack traces similar to the heap profiler (requires --text)
# --callgrind         Generate callgrind format to stdout
# --gv                Generate Postscript and display
# --evince            Generate PDF and display
# --web               Generate SVG and display
# --list=<regexp>     Generate source listing of matching routines
# --disasm=<regexp>   Generate disassembly of matching routines
# --symbols           Print demangled symbol names found at given addresses
# --dot               Generate DOT file to stdout
# --ps                Generate Postscript to stdout
# --pdf               Generate PDF to stdout
# --svg               Generate SVG to stdout
# --gif               Generate GIF to stdout
# --raw               Generate symbolized pprof data (useful with remote fetch)
# --collapsed         Generate collapsed stacks for building flame graphs
