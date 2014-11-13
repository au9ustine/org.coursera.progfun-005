package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }
  
  trait Level3 extends SolutionChecker {
    val level =
    """---------------  
      |------ooooooo--
      |oooo--ooo--oo--
      |ooooooooo--oooo
      |oSoo-------ooTo 
      |oooo-------oooo
      |------------ooo""".stripMargin
  }
  
  trait Level4 extends SolutionChecker {
    val level =
      """---------------  
      |---ooooooo----
      |---ooooooo----
      |oooo-----ooo--
      |ooo-------oo--
      |ooo-------oo--
      |oSo--ooooooooo
      |ooo--ooooooooo
      |-----oTo--oooo
      |-----ooo--oooo""".stripMargin 
  }
  
  trait Level6 extends SolutionChecker {
    val level =  
  """---------------
    |-----oooooo----
    |-----o--ooo----
    |-----o--ooooo--
    |Sooooo-----oooo
    |----ooo----ooTo
    |----ooo-----ooo
    |------o--oo----
    |------ooooo----
    |------ooooo----
    |-------ooo-----""".stripMargin
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }
  
  test("neighbors level 1") {
    new Level1 {
      val current = startBlock
      val expected = Set(
          (Block(Pos(1,2), Pos(1,3)), Right),
          (Block(Pos(-1,1), Pos(0,1)), Up),
          (Block(Pos(1,-1), Pos(1, 0)), Left),
          (Block(Pos(2, 1), Pos(3, 1)), Down))
      val actual = current.neighbors.toSet
      assert(expected === actual)
    }
    new Level1 {
      val current = Block(Pos(1,2), Pos(1,3))
      val expected = Set(
          (Block(Pos(1,4), Pos(1,4)), Right),
          (Block(Pos(0,2), Pos(0,3)), Up),
          (Block(Pos(1,1), Pos(1, 1)), Left),
          (Block(Pos(2, 2), Pos(2, 3)), Down))
      val actual = current.neighbors.toSet
      assert(expected === actual)
    }
  }
  
  test("legalNeighbors Level 1") {
    new Level1 {
      val current = startBlock
      val expected = Set(
          (Block(Pos(1,2), Pos(1,3)), Right),
          (Block(Pos(2, 1), Pos(3, 1)), Down))
      val actual = current.legalNeighbors.toSet
      assert(expected === actual)
    }
    new Level1 {
      val current = Block(Pos(1,2), Pos(1,3))
      val expected = Set(
          (Block(Pos(1,4), Pos(1,4)), Right),
          (Block(Pos(1,1), Pos(1, 1)), Left),
          (Block(Pos(2, 2), Pos(2, 3)), Down))
      val actual = current.legalNeighbors.toSet
      assert(expected === actual)
    }
  }
  
  test("isStanding Level 1") {
    new Level1 {
      val current = startBlock
      val expected = true
      val actual = current.isStanding
      assert(expected === actual)
    }
    new Level1 {
      val current = Block(Pos(1,2), Pos(1,3))
      val expected = false
      val actual = current.isStanding
      assert(expected === actual)
    }
  }
  
  test("done Level 1") {
    new Level1 {
      val current = startBlock
      val expected = false
      val actual = done(current)
      assert(expected === actual)
    }
    new Level1 {
      val current = Block(Pos(4,7), Pos(4,7))
      val expected = true
      val actual = done(current)
      assert(expected === actual)
    }
  }
  
  test("neighborsWithHistory Level 1") {
    new Level1 {
      val b: Block = startBlock
      val history: List[Move] = Nil
      val actual: Stream[(Block, List[Move])] = neighborsWithHistory(b, history)
      val expected = Set(
          (Block(Pos(1,2),Pos(1,3)),List(Right)),
          (Block(Pos(2,1),Pos(3,1)),List(Down)))
      assert(actual.toSet === expected)
    }
  }

  test("newNeighborsOnly Level 1") {
    new Level1 {
      val neighbors = neighborsWithHistory(startBlock, Nil)
      val explored = Set[Block](startBlock)
      val actual = newNeighborsOnly(neighbors, explored)
      val expected = List((Block(Pos(1, 4), Pos(1, 4)), List(Right, Right)),
        (Block(Pos(2, 2), Pos(2, 3)), List(Down, Right)),
        (Block(Pos(2, 2), Pos(3, 2)), List(Right, Down)))
      assert(actual.toList === expected)
    }
  }

  test("from Level 1") {
    new Level1 {
      val neighbors = neighborsWithHistory(startBlock, Nil)
      val explored = Set[Block](startBlock)
      val actual = from(neighbors, explored)
      val expected = List((Block(Pos(1, 2), Pos(1, 3)), List(Right)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down)),
        (Block(Pos(1, 4), Pos(1, 4)), List(Right, Right)),
        (Block(Pos(2, 2), Pos(2, 3)), List(Down, Right)),
        (Block(Pos(2, 2), Pos(3, 2)), List(Right, Down)),
        (Block(Pos(2, 4), Pos(3, 4)), List(Down, Right, Right)),
        (Block(Pos(2, 1), Pos(2, 1)), List(Left, Down, Right)))
      assert(actual.take(7) == expected)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
     assert(solve(solution) === Block(goal, goal))
    }
    new Level3 {
      assert(solve(solution) === Block(goal, goal))
    }
    new Level4 {
      assert(solve(solution) === Block(goal, goal))
    }
    new Level6 {
      assert(solve(solution) === Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
