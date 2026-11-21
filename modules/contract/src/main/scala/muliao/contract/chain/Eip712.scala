package muliao.contract.chain

import sttp.tapir.Schema
import zio.json.*
import zio.json.ast.Json

import java.nio.charset.StandardCharsets.UTF_8

/** EIP-712 结构化数据（`eth_signTypedData_v4` 的 JSON：types、primaryType、domain、message），前后端共用（#1092）。
  * 读的时候就把摘要算好：类型写错、字段缺了、多了或重复、数值越界的读不进来，所以手里的 [[TypedData]] 都能签。
  * `typeString` 是 primaryType 的完整类型编码（含被引用的结构），给用户看各字段的类型。
  */
final case class TypedData private (json: Json.Obj, primaryType: String, typeString: String, domain: Json.Obj, message: Json.Obj, digest: Array[Byte]):
  override def toString: String = s"TypedData($primaryType, ${Hex.prefixed(digest)})"

object TypedData:
  /** 一个字段：名字与类型。 */
  private final case class Field(name: String, tpe: String)

  private type Types = Map[String, List[Field]]

  /** domain 没在 types 里声明 EIP712Domain 时，按这几个字段的固定顺序取 domain 里有的（同 ethers、viem）。 */
  private val DomainFields = List("name" -> "string", "version" -> "string", "chainId" -> "uint256", "verifyingContract" -> "address", "salt" -> "bytes32")

  def parse(text: String): Either[String, TypedData] =
    text.fromJson[Json].left.map(e => s"不是 JSON：$e").flatMap(fromJson)

  def fromJson(j: Json): Either[String, TypedData] =
    for
      root    <- obj(j, "整体")
      typesJ  <- root.get("types").toRight("缺少 types").flatMap(obj(_, "types"))
      primary <- root.get("primaryType").collect { case Json.Str(s) => s }.toRight("缺少 primaryType（字符串）")
      domain  <- root.get("domain").toRight("缺少 domain").flatMap(obj(_, "domain"))
      message <- root.get("message").toRight("缺少 message").flatMap(obj(_, "message"))
      _       <- Either.cond(primary != "EIP712Domain", (), "primaryType 不能是 EIP712Domain")
      declared <- Eithers.traverse(typesJ.fields.toList) { (name, fs) =>
                    fs match
                      case Json.Arr(items) =>
                        Eithers.traverse(items.toList) {
                          case Json.Obj(f) =>
                            val m = f.toMap
                            (m.get("name"), m.get("type")) match
                              case (Some(Json.Str(n)), Some(Json.Str(t))) => Right(Field(n, t))
                              case _                                      => Left(s"types.$name 里每项要有字符串 name 与 type")
                          case _ => Left(s"types.$name 里每项要是对象")
                        }.map(name -> _)
                      case _ => Left(s"types.$name 要是数组")
                  }
      types = declared.toMap.updatedWith("EIP712Domain")(_.orElse(Some(DomainFields.collect { case (n, t) if domain.get(n).isDefined => Field(n, t) })))
      _          <- Either.cond(types.contains(primary), (), s"types 里没有 primaryType $primary")
      domainHash <- hashStruct(types, "EIP712Domain", domain, "domain")
      msgHash    <- hashStruct(types, primary, message, "message")
    yield TypedData(Json.Obj(root.fields), primary, encodeType(types, primary), domain, message, Keccak256(Array[Byte](0x19, 0x01) ++ domainHash ++ msgHash))

  given JsonCodec[TypedData] = JsonCodec[Json].transformOrFail(fromJson, _.json)
  given Schema[TypedData]    = Schema.any

  private def obj(j: Json, what: String): Either[String, Json.Obj] = j match
    case o: Json.Obj => Right(o)
    case _           => Left(s"$what 要是 JSON 对象")

  /** `Name(type1 name1,…)`，后面接按名字排序的被引用的结构。 */
  private def encodeType(types: Types, name: String): String =
    def deps(n: String, seen: Set[String]): Set[String] =
      types.get(n).fold(seen)(_.map(f => base(f.tpe)).filter(t => types.contains(t) && !seen(t)).foldLeft(seen + n)((s, t) => deps(t, s)))
    def one(n: String) = s"$n(${types(n).map(f => s"${f.tpe} ${f.name}").mkString(",")})"
    (name :: (deps(name, Set.empty) - name).toList.sorted).map(one).mkString

  /** 去掉数组后缀后的元素类型。 */
  private def base(t: String): String = t.indexOf('[') match
    case -1 => t
    case i  => t.take(i)

  private def hashStruct(types: Types, name: String, value: Json, path: String): Either[String, Array[Byte]] =
    for
      fields <- types.get(name).toRight(s"$path：types 里没有 $name")
      o      <- obj(value, path)
      keys    = o.fields.map(_._1)
      _      <- Either.cond(keys.distinct.size == keys.size, (), s"$path 有重复的字段")
      extra   = keys.filterNot(k => fields.exists(_.name == k))
      _      <- Either.cond(extra.isEmpty, (), s"$path 有 $name 里没声明的字段：${extra.mkString("、")}")
      parts <- Eithers.traverse(fields)(f => o.get(f.name).toRight(s"$path 缺少字段 ${f.name}").flatMap(encodeField(types, f.tpe, _, s"$path.${f.name}")))
    yield Keccak256(Keccak256(encodeType(types, name).getBytes(UTF_8)) ++ parts.toArray.flatten)

  private val ArrayType = """^(.*)\[(\d*)\]$""".r
  private val UintType  = """^uint(\d*)$""".r
  private val IntType   = """^int(\d*)$""".r
  private val BytesN    = """^bytes(\d+)$""".r

  /** 一个字段编码成 32 字节。 */
  private def encodeField(types: Types, tpe: String, v: Json, path: String): Either[String, Array[Byte]] =
    tpe match
      case ArrayType(elem, n) =>
        v match
          case Json.Arr(items) =>
            for
              _     <- Either.cond(n.isEmpty || n.toInt == items.size, (), s"$path 应有 $n 项，实际 ${items.size}")
              parts <- Eithers.traverse(items.toList.zipWithIndex)((x, i) => encodeField(types, elem, x, s"$path[$i]"))
            yield Keccak256(parts.toArray.flatten)
          case _ => Left(s"$path 要是数组")
      case t if types.contains(t) => hashStruct(types, t, v, path)
      case "string" => str(v, path).map(s => Keccak256(s.getBytes(UTF_8)))
      case "bytes"  => str(v, path).flatMap(s => Hex.decode(s).left.map(e => s"$path：$e")).map(Keccak256(_))
      case "bool" =>
        v match
          case Json.Bool(b) => Right(Eth.word(if b then 1 else 0))
          case _            => Left(s"$path 要是 true 或 false")
      case "address" => str(v, path).flatMap(s => EthAddress.parse(s).left.map(e => s"$path：$e")).map(a => Array.fill[Byte](12)(0) ++ a.bytes)
      case BytesN(n) if n.toInt >= 1 && n.toInt <= 32 =>
        for
          s <- str(v, path)
          b <- Hex.decode(s).left.map(e => s"$path：$e")
          _ <- Either.cond(b.length == n.toInt, (), s"$path 要是 ${n.toInt} 字节，实际 ${b.length}")
        yield b ++ Array.fill[Byte](32 - b.length)(0)
      case UintType(bits) =>
        width(bits, path).flatMap(w => integer(v, path).flatMap(x => Either.cond(x >= 0 && x < BigInt(2).pow(w), Eth.word(x), s"$path 超出 uint$w 的范围")))
      case IntType(bits) =>
        width(bits, path).flatMap { w =>
          val half = BigInt(2).pow(w - 1)
          integer(v, path).flatMap(x => Either.cond(x >= -half && x < half, Eth.word(if x < 0 then BigInt(2).pow(256) + x else x), s"$path 超出 int$w 的范围"))
        }
      case other => Left(s"$path：不认得的类型 $other")

  private def width(bits: String, path: String): Either[String, Int] =
    val w = if bits.isEmpty then 256 else bits.toInt
    Either.cond(w >= 8 && w <= 256 && w % 8 == 0, w, s"$path：整数位数要是 8 到 256 间 8 的倍数")

  private def str(v: Json, path: String): Either[String, String] = v match
    case Json.Str(s) => Right(s)
    case _           => Left(s"$path 要是字符串")

  /** 整数：JSON 数字，或十进制、`0x` 十六进制字符串（可带负号）。 */
  private def integer(v: Json, path: String): Either[String, BigInt] =
    val bad = s"$path 要是整数"
    v match
      case Json.Num(n) => scala.util.Try(BigInt(n.toBigIntegerExact)).toEither.left.map(_ => bad)
      case Json.Str(s) =>
        val (neg, body) = if s.startsWith("-") then (true, s.drop(1)) else (false, s)
        val abs =
          if body.startsWith("0x") || body.startsWith("0X") then Either.cond(body.length > 2 && body.drop(2).forall(c => Character.digit(c, 16) >= 0), BigInt(body.drop(2), 16), bad)
          else Either.cond(body.nonEmpty && body.forall(_.isDigit), BigInt(body), bad)
        abs.map(a => if neg then -a else a)
      case _ => Left(bad)
