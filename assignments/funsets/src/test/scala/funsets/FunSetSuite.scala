package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton")
      assert(contains(s3, 3), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }
  
  test("intersect contains both all elements from s and t") {
    new TestSets {
      val su1 = union(s1, s2)
      val su2 = union(s2, s3)
      val su3 = union(s1, s3)
      val r1 = intersect(su1, su2)
      val r2 = intersect(su1, su3)
      val r3 = intersect(su2, su3)
      assert(contains(r1, 2), "Intersect 1")
      assert(contains(r2, 1), "Intersect 2")
      assert(contains(r3, 3), "Intersect 3")
    }
  }
  
  test("diff") {
    new TestSets {
      val su1 = union(s1, s2)
      val su2 = union(s2, s3)
      val r1 = diff(su1, su2)
      assert(contains(r1, 1), "s1 should be in su1")
      assert(!contains(r1, 2), "s2 should not be in su1")
      assert(!contains(r1, 3), "s3 should not be in su1")
    }
  }
  
  test("forall") {
    new TestSets {
      val su1 = union(s1, s2)
      val p1 = (x: Int) => x < 2
      assert(forall(su1, p1) == false, "s1 satisfies while s2 not")
      val p2 = (x: Int) => x < 3
      assert(forall(su1, p2) == true, "both s1 s2 satisfy")
      val p3 = (x: Int) => x > 3
      assert(forall(su1, p3) == false, "both s1 s2 do not satisfy")
    }
  }
  
  test("exist") {
     new TestSets {
      val su1 = union(s1, s2)
      val p1 = (x: Int) => x < 2
      assert(exists(su1, p1) == true, "at least s1 satisfies")
      val p2 = (x: Int) => x < 3
      assert(exists(su1, p2) == true, "both s1 s2 satisfy")
      val p3 = (x: Int) => x > 3
      assert(exists(su1, p3) == false, "both s1 s2 do not satisfy")
    }
  }
  
  test("map") {
    new TestSets {
      val f = (x: Int) => {x + 1}
      val su1 = union(s1, s2)
      val ret = map(su1, f)
      assert(contains(ret, 2), "1 -> 2")
      assert(contains(ret, 3), "2 -> 3")
    }
  }
}
