package fintech.homework03
import org.scalatest.{FlatSpec, Matchers}

class PrefixTreeSpec extends FlatSpec with Matchers {
  it should "work well with strings" in {
    val tree: PrefixTree[Char, Int] = new MPrefixTree[Char, Int]()

    val with42: PrefixTree[Char, Int] = tree.put("abcd", 42)
    with42.sub("ab").sub("cd").get should be (42)

    val withDouble: PrefixTree[Char, AnyVal] = with42.put("abcde", 13.0)
    withDouble.sub("ab").sub("cd").get should be (42)
    withDouble.sub("ab").sub("cde").get should be (13.0)
  }

  it should "work with [Char, String]" in {
    val tree: PrefixTree[Char, String] = new MPrefixTree[Char, String]()

    val mTree: PrefixTree[Char, String] = tree
      .put("to", "to")
      .put("t", "o")
      .put("ten", "ten").put("ted", "ted").put("inn", "inn")
    mTree.sub("ten").get should be ("ten")
    mTree.sub("to").get should be ("to")
    mTree.sub("t").get should be ("o")
    mTree.sub("inn").get should be ("inn")
    mTree.sub("inn").get should be ("inn")
    an [NoSuchElementException] should be thrownBy mTree.sub("te").get
  }

  it should "work with lists" in {
    val tree: PrefixTree[Char, Int] = new MPrefixTree[Char, Int]()

    val mTree: PrefixTree[Char, Int] = tree
      .put(List('a', 'b', 'c', 'd'), 1)
      .put(List('a', 'b', 'e'), 2)
      .put(List('e', 'f'),3)
      .put(List('a', 'b'), 4)

    mTree.sub(List('a', 'b', 'c', 'd')).get should be (1)
    mTree.sub(List('a', 'b', 'e')).get should be (2)
    mTree.sub(List('e', 'f')).get should be (3)
    //and with string too
    mTree.sub("ab").get should be (4)
    an [NoSuchElementException] should be thrownBy mTree.sub("aaaa").get

    an [NoSuchElementException] should be thrownBy mTree.sub(List('a')).get
  }

  it should "work with empty string" in {
    val tree: PrefixTree[Char, Int] = new MPrefixTree[Char, Int]().put("", 100)

    tree.sub("").get should be (100)
  }
}
