package singularity.benchmarks

object HashFunc {
  // Hash function DJBX33A
  def php(ls: Seq[Char]) = {
    val cs = ls.toArray
    var hash = 5381
    for (i <- cs.indices) {
      hash = ((hash << 5) + hash) + cs(i)
    }
    hash
  }

  // Variant of DJBX33A
  def java(ls: Seq[Char]) = {
    val cs = ls.toArray
    var hash = 0
    for (i <- cs.indices) {
      hash = ((hash << 5) - hash) + cs(i)
    }
    hash
  }

  // Variant of DJBX33A
  def ruby(ls: Seq[Char]) = {
    val cs = ls.toArray
    var hash = 0
    for (i <- cs.indices) {
      hash = (hash * 65599) + cs(i)
    }
    hash + (hash >> 5)
  }

  // Hash function DJBX33X
  def aspDotNet(ls: Seq[Char]) = {
    val cs = ls.toArray
    var hash = 5381
    for (i <- cs.indices) {
      hash = ((hash << 5) + hash) ^ cs(i)
    }
    hash
  }

  // Variant of DJBX33X
  def python(ls: Seq[Char]) = {
    val cs = ls.toArray
    var hash = if (cs.isEmpty) 0 else cs(0) << 7
    for (i <- cs.indices) {
      hash = (1000003 * hash) ^ cs(i)
    }
    hash = hash ^ cs.length
    if (hash == -1) -2 else hash
  }

  // This is unique
  def v8(ls: Seq[Char]) = {
    val cs = ls.toArray
    val len = cs.length
    var hash = 0
    for (i <- cs.indices) {
      hash += cs(i)
      hash += (hash << 10)
      hash ^= (hash >> 6)
    }
    hash += (hash << 3)
    hash ^= (hash >> 11)
    hash += (hash << 15)
    if (hash == 0) 27 else hash
  }

  def murmur2(seed: Int)(ls: Seq[Char]) = {
    val cs = ls.toArray
    val len = cs.length
    val m = (0xc6a4a793 << 32) | 0x5bd1e995
    val r = 24

    var h = seed ^ len
    for (c <- cs) {
      var k = c.toInt
      k *= m
      k ^= k >> r
      k *= m
      h *= m
      h ^= k
    }

    // Assume the input len is a multiple of 8, the switch block will not be executed
    h ^= h >> 13
    h *= m
    h ^= h >> 15
    h
  }
}
