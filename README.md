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

## Pipeline and Fusion

The basic linear regression is:

    ols x y = (pinv ((tr x) `mul` x )) `mul` ((tr x) `mul` y)

Breaking this down into operations:

    i0 = x^T
    i1 = i0 X x
    pi = i1^-1
    i3 = i0 X y
    i4 = pi X i3

The data dependency graph:

    i0 <- x
    i1 <- x, i0
    i2 <- i1
    i3 <- i0, y
    i4 <- i2, i3

And the data pipeline:

       /----------\
       |          |
    B1 x -> i0 -> i1 -> pi ---\
             |             B3 |--> i4
    B2       |     y -> i3 ---/
             |          |
             `----------/ 

# References

1. [AVX Matrix transposition][avxmat]
2. [Survey of parallel matrix multiplication][matmult] 
3. [Communication-Optimal Parallel Algorithm for Strassen’s Matrix Multiplication][caps]

[avxmat]: http://airccse.org/journal/jcsit/6314ijcsit05.pdf

[matmult]: http://www.eecs.berkeley.edu/Pubs/TechRpts/2013/EECS-2013-100.pdf

[caps]: http://www.eecs.berkeley.edu/Pubs/TechRpts/2012/EECS-2012-32.pdf
