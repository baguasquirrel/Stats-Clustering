This is a library (or maybe set of libraries) to do statistical clustering of data. Right now, it includes a library to create datasets with some number of clusters (i.e. test data), and a k-means library to model clusters.

The k-means library is fairly rudimentary but generic, so that you can get it to work with you code with after a little instancing. The k-means library comes with the following features:

- works over the Traversable typeclass, so that you can choose which container to put your data in. do something quick and dirty with lists, and then switch to a Data.Vector when some speed would be nice.
- instead of defining a point as a tuple of floats, use a Point typeclass, together with a distance metric.
- support for a different datatype for the cluster centers / accumulating vectors. this can really help (1) avoid overflow problems and (2) improve speed on certain datasets.
- early stopping.
- the k-means++ seeding algorithm by Arthur and Vassilvitskii (2007).

There's also some work being done to replace the normaldistribution package with a ziggurat algorithm, and to refactor the RandomGaussian code so that it does not depend on the Vector typeclass. The ziggurat algorithm itself probably needs to be moved out so we can use it to also generate randoms across any arbitrary distribution that is monotonically decreasing, like an exponential. It probably makes sense to break it out into a separate library within this repo.