package muliao.contract.chain

/** 以太坊用的 Keccak-256（原始 Keccak 填充 0x01，不是 NIST SHA3-256 的 0x06），前后端共用（#1092）。 */
object Keccak256:
  private val Rate = 136 // 1088 位

  private val RoundConstants: Array[Long] = Array(
    0x0000000000000001L, 0x0000000000008082L, 0x800000000000808aL, 0x8000000080008000L,
    0x000000000000808bL, 0x0000000080000001L, 0x8000000080008081L, 0x8000000000008009L,
    0x000000000000008aL, 0x0000000000000088L, 0x0000000080008009L, 0x000000008000000aL,
    0x000000008000808bL, 0x800000000000008bL, 0x8000000000008089L, 0x8000000000008003L,
    0x8000000000008002L, 0x8000000000000080L, 0x000000000000800aL, 0x800000008000000aL,
    0x8000000080008081L, 0x8000000000008080L, 0x0000000080000001L, 0x8000000080008008L
  )

  // ρ 的位移与 π 的落点，按 x + 5y 编号
  private val Rotations: Array[Int] = Array(0, 1, 62, 28, 27, 36, 44, 6, 55, 20, 3, 10, 43, 25, 39, 41, 45, 15, 21, 8, 18, 2, 61, 56, 14)

  def apply(input: Array[Byte]): Array[Byte] =
    val state  = Array.fill(25)(0L)
    val padded = input.length / Rate * Rate + Rate
    val block  = Array.fill[Byte](padded)(0)
    System.arraycopy(input, 0, block, 0, input.length)
    block(input.length) = (block(input.length) ^ 0x01).toByte
    block(padded - 1) = (block(padded - 1) ^ 0x80).toByte
    (0 until padded by Rate).foreach { off =>
      (0 until Rate / 8).foreach(i => state(i) ^= lane(block, off + 8 * i))
      permute(state)
    }
    Array.tabulate(32)(i => (state(i / 8) >>> (8 * (i % 8))).toByte)

  private def lane(a: Array[Byte], at: Int): Long = (0 until 8).foldLeft(0L)((acc, i) => acc | ((a(at + i) & 0xffL) << (8 * i)))

  private def permute(s: Array[Long]): Unit =
    val b = Array.fill(25)(0L)
    val c = Array.fill(5)(0L)
    RoundConstants.foreach { rc =>
      // θ
      (0 until 5).foreach(x => c(x) = s(x) ^ s(x + 5) ^ s(x + 10) ^ s(x + 15) ^ s(x + 20))
      (0 until 5).foreach { x =>
        val d = c((x + 4) % 5) ^ java.lang.Long.rotateLeft(c((x + 1) % 5), 1)
        (0 until 25 by 5).foreach(y => s(y + x) ^= d)
      }
      // ρ 与 π
      for x <- 0 until 5; y <- 0 until 5 do b(y + 5 * ((2 * x + 3 * y) % 5)) = java.lang.Long.rotateLeft(s(x + 5 * y), Rotations(x + 5 * y))
      // χ
      for x <- 0 until 5; y <- 0 until 5 do s(x + 5 * y) = b(x + 5 * y) ^ (~b((x + 1) % 5 + 5 * y) & b((x + 2) % 5 + 5 * y))
      // ι
      s(0) ^= rc
    }
