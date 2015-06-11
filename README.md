# Matrix Operations

There are three operations we need to perform on matrixes:

1. pseudoinverse (pinv, ^-1)
2. transposition (tr, ^T)
3. multiplication (mul, X)

To handle larger sets of data, these will need to be performed on
subsets of the data, stored in scratch space, and then folded into the
final resulting matrix. The processing of these subsets can be
scheduled weighting cache locality and freshness to minimize cache
churn across cores.

# CAPS Matrix Multiplication

[CAPS][caps] is a communication-efficient method of parallizing
Strassen's algorithm. It's well-suited to the non-uniform memory and
processing architectures we're targeting. It "asymptotically minimizes
computa- tional and bandwidth costs over all parallel Strassen-based
algorithms. It also minimizes latency cost up to a logarithmic factor
in the number of processors."

Strassen divides the matrix into a 7-ary tree, and CAPS recurses the
problem tree in either of two ways: breadth-first and
depth-first. Breadth-first (BFS) will divide the 7 subproblems across
processors, so each processor will work on 1/7 of the
problem. Depth-first (DFS) will use all processors on each subproblem
in turn. BFS uses more memory while reducing communication, while DFS
uses less memory but requires more communication overhead. CAPS
minimizes communication costs by choosing an ordering of BFS/DFS that
maximizes memory usage.

## Unlimited Memory Scheme


# References

1. [AVX Matrix transposition][avxmat]
2. [Survey of parallel matrix multiplication][matmult] 
3. [Communication-Optimal Parallel Algorithm for Strassen’s Matrix Multiplication][caps]

[avxmat]: http://airccse.org/journal/jcsit/6314ijcsit05.pdf

[matmult]: http://www.eecs.berkeley.edu/Pubs/TechRpts/2013/EECS-2013-100.pdf

[caps]: http://www.eecs.berkeley.edu/Pubs/TechRpts/2012/EECS-2012-32.pdf
