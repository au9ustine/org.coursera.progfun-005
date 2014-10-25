package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }
  
  test("until combine") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(until(singleton, combine)(leaflist) == List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e','t','x'), 7)))
  }

  test("createCodeTree") {
    val s = "xtxtxxe"
    assert(createCodeTree(s.toList) == Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4), List('e','t','x'), 7))
  }
  
  test("decode") {
    val d1 = (List(0,0,1,1,1,0,1), List('h'))
    val d2 = (List(0,0,1,1,1,0), Nil)
    val actual1 = decode(frenchCode, d1._1)
    val actual2 = decode(frenchCode, d2._1)
    assert(actual1 == d1._2)
    assert(actual2 == d2._2)
  }
  
  test("decodedSecret") {
    val expected =  List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')
    assert(decodedSecret == expected)
  }
  
  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  
  test("decode and encode trivial long") {
    val decoded = List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')
    val encoded = secret
    assert(decode(frenchCode, encode(frenchCode)(decoded)) == decoded)
  }
  
  test("convert") {
    assert(convert(frenchCode) == List(('s',List(0, 0, 0)), ('d',List(0, 0, 1, 0)), ('x',List(0, 0, 1, 1, 0, 0, 0)), ('j',List(0, 0, 1, 1, 0, 0, 1)), ('f',List(0, 0, 1, 1, 0, 1)), ('z',List(0, 0, 1, 1, 1, 0, 0, 0, 0)), ('k',List(0, 0, 1, 1, 1, 0, 0, 0, 1, 0)), ('w',List(0, 0, 1, 1, 1, 0, 0, 0, 1, 1)), ('y',List(0, 0, 1, 1, 1, 0, 0, 1)), ('h',List(0, 0, 1, 1, 1, 0, 1)), ('q',List(0, 0, 1, 1, 1, 1)), ('o',List(0, 1, 0, 0)), ('l',List(0, 1, 0, 1)), ('m',List(0, 1, 1, 0, 0)), ('p',List(0, 1, 1, 0, 1)), ('u',List(0, 1, 1, 1)), ('r',List(1, 0, 0, 0)), ('c',List(1, 0, 0, 1, 0)), ('v',List(1, 0, 0, 1, 1, 0)), ('g',List(1, 0, 0, 1, 1, 1, 0)), ('b',List(1, 0, 0, 1, 1, 1, 1)), ('n',List(1, 0, 1, 0)), ('t',List(1, 0, 1, 1)), ('e',List(1, 1, 0)), ('i',List(1, 1, 1, 0)), ('a',List(1, 1, 1, 1))))
  }
  
  test("quickEncode") {
    val decoded = List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')
    val expected = secret
    assert(quickEncode(frenchCode)(decoded) == expected)
  }
}
