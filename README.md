This is a library (or maybe set of libraries) to do statistical clustering of data. Right now, it includes a library to create datasets with some number of clusters (i.e. test data), and a k-means library to model clusters.

The k-means library is fairly rudimentary but generalized, with the following features:

- works on a Vector typeclass, where a vector is a mathematical representation of a point in space, together with a distance metric. this is as opposed to other libraries that work on hardcoded 2-tuples of floats or doubles, because I understand you may want to define your own vector types.
- works over the Traversable typeclass, so that you can do something quick and dirty with lists, or a vector when some speed would be nice.
- support for a different datatype for the cluster centers / accumulating vectors. this can really help (1) avoid overflow problems and (2) improve speed on certain datasets.
- early stopping.
- the k-means++ seeding algorithm by Arthur and Vassilvitskii (2007).

There's also some work being done to replace the normaldistribution package with a ziggurat algorithm, and to refactor the RandomGaussian code so that it does not depend on the Vector typeclass.