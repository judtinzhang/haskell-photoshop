# haskell-photoshop
Names: Jason Yan and Justin Zhang
Pennkeys: jasyan and judtin

We created Haskell Photoshop, which processes images and performs various
manipulations on them. A key component of our photoshop program is our
extensible design that allows multiple implementations of the same image
processing functions using different image representations. Since they all
implement the same functions, a user can pick and choose an implementation
that is best suited for their task. Becuase of our multiple implementations,
we put an emphasis on rigorous testing and profiling of them.

## Module organization

I would reccomend start reading from the (short) app/Main.hs or app/Benchmarking.hs 
in order to get a sense of how the image processing functions are used. Then, move 
onto src/PPM.hs, our simpler implementation, before reading src/QuadTree.hs. 

app/Main.hs:
Home of our main function, uses supplied by a image representation file in order
to manipulate images at a high level.

app/Benchmarking.hs:
Runs a benchmark suite on the image processing functions available, timing their
runtimes. Run suite with `stack bench`.

src/PPM.hs:
An implementation of our image processing functions using the PPM (Portable
Pixel Map) format, which is just a 2D array of pixels.

src/QuadTree.hs:
An implementation of our image processing functions using the QuadTree format.
The QuadTree fully supports rectangular images. In addition to general functions
listed below, supports an extra lossyCompression function. 

## Image Processing Functions

Here are the functions supported by all image representations

readInput
toJpg
toPng

blur
changeColor
crop
grayscale
reflectHorizontal
reflectVertical
rotateLeft
rotateRight
saturate