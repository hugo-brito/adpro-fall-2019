package adpro
import org.scalatest.FunSuite
import Stream._

class Stream_test extends FunSuite {
  
  test("This one always works: (-1) * (-1) = 1") {
    assert((-1)*(-1)==1);
  }
  
  //Sanity check of exercise 1
  test("The first element of Stream.from(3) is 3") {
    assert(from(3).headOption().contains(3));
  }

  test("The second element of Stream.from(3) is 4") {
    assert(from(3).tail.headOption().contains(4));
  }

	test("The first element of Stream.to(3) is 3") {
    assert(to(3).headOption().contains(3));
  }
  
	test("The second element of Stream.to(3) is 2") {
    assert(to(3).tail.headOption().contains(2));
  }

	test("The first element of naturals is 0"){
		assert(naturals.headOption().contains(0))
	}


	test("The Stream(1,2,3).toList is List(1,2,3) "){
		  val l2 :Stream[Int]= cons(1, cons(2, cons (3, empty)))
			assert(l2.toList(0) == 1)
			assert(l2.toList(1) == 2)
			assert(l2.toList(2) == 3)


	}

	test("naturals.take(3)  is Stream(1,2,3) "){
		  val l2 :Stream[Int]= naturals.take(3) 
			assert(l2.toList(0) == 0)
			assert(l2.toList(1) == 1)
			assert(l2.toList(2) == 2)
  }

  test("naturals.drop(3) is Stream(3,4,5,...) "){
		  val l2 :Stream[Int]= naturals.drop(3) 
			assert(l2.headOption().contains(3))
      assert(l2.tail.headOption().contains(4))
      assert(l2.tail.tail.headOption().contains(5))

			//assert(l2.toList(2) == 5)
  }
	


}