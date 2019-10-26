package adpro.variance;

class A {};
class B extends A {};
class C extends A {};

class Variance {

  // Java arrays are covariant so if B < A then B[] < A[]
  static void problem () {
 
    // Create an array of B's
    B[] b = { new B() };

    // Create an array of A' and initialize it with b.  This is fine, as every
    // B is also an A (and arrays are covariant in Java.)
    A[] a = b;

    // Assign a[0] to c 
    a[0] = new C();
 
    // This compiles because C's are A's so they can be used in place of A's.
    // But we have a problem, because in reality a is initialized as an array
    // of B's, and anybody who uses the array b (that is aliased) expects B's
    // to be there.
  
    // For instance imagine what would happen if we now call b[0].funB where
    // funB is a method only available in B and not in any of the other
    // classes?

    // Covariants and assignments do not mix well ...
    // Scala arrays are invariant (so the above would not compile)

  }

};
