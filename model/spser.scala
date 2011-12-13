
package model

import java.sql.{Array=>_, _}
// Joda time
import org.joda.time.DateTime
import net.liftweb.json._

package object spser {
//

// ref_718
sealed trait HProd
// ref_464
case class HTimes[+H,+T<:HProd](head: H, tail: T) extends HProd
// ref_220
case class HOne() extends HProd

implicit def hBuild[H<:HProd](h: H) = new {
    def :+:[A](x: A) = HTimes[A,H](x, h)
}
object :+: {
    def unapply[H,T<:HProd](p: HTimes[H,T]) = Some((p.head, p.tail))
}

type :+:[H,T<:HProd] = HTimes[H,T]

trait Equivalence[A] {
    type B
    def extract(a: A): B
    def compose(b: B): A
}

def a[T]: T = sys.error("This got called")

def as[T](a: Any) = a.asInstanceOf[T]

trait SQLType[A] {
    type CT
    val typeName: String
    def encode(a: A): CT
    def decode(a: CT): A
}

trait SQLEncode[-A] {
    type CT
    def encode(a: A): CT
}

implicit def typeEncode[A](implicit typ: SQLType[A]) = new SQLEncode[A] {
    type CT = typ.CT
    def encode(a: A) = typ.encode(a)
}

def sqlEncode[A:SQLType](a: A) = implicitly[SQLType[A]].encode(a)
def sqlDecode[A,C](c: C)(implicit sql: SQLType[A]{type CT=C}) = sql.decode(c)

implicit val sqlInt = new SQLType[Int] {
    type CT = Int
    val typeName = "INT"
    def encode(i: Int) = i
    def decode(a: Int) = a
}

trait StringType[A] extends SQLType[A] {
    import org.h2.jdbc._
    
    def sencode(a: A): String
    def sdecode(s: String): A
    
    type CT = AnyRef
    val typeName = "TEXT"
    def encode(a: A) = sencode(a)
    def decode(s: AnyRef) = s match {
        case s: String   => sdecode(s)
        case s: JdbcClob => sdecode {
            if (s.length == (0:Long)) ""
            else s.getSubString(1, s.length.intValue)
        }
        case x => sys.error("Oh dear type " + x.getClass)
    }
}

implicit val sqlString = new StringType[String] {
    def sencode(s: String) = s
    def sdecode(s: String) = s
}

implicit val sqlDateTime = new SQLType[DateTime] {
    type CT = Long
    val typeName = "LONG"
    def encode(d: DateTime) = d.getMillis
    def decode(t: Long) = new DateTime(t)
}

implicit val sqlBoolean = new SQLType[Boolean] {
    type CT = String
    val typeName = "VARCHAR"
    def encode(b: Boolean) = b.toString
    def decode(b: String) = java.lang.Boolean.parseBoolean(b)
}

// ref_231
trait SQLParameters[A] {
    def encode(a: A): List[Any]
    def decode(ls: List[Any]): A
    val length: Int
    val colTypes: List[String]
}

def makeSQLParameters[A:SQLParameters](a: A): List[Any] =
    implicitly[SQLParameters[A]].encode(a)

implicit def oneParameters = new SQLParameters[HOne] {
    def encode(i: HOne) = Nil
    def decode(ls: List[Any]) = HOne()
    val length = 0
    val colTypes = Nil
}

implicit def timesParameters[H,T<:HProd](implicit sql: SQLType[H], rest: SQLParameters[T]) =
    new SQLParameters[HTimes[H,T]]
{
    // ref_984
    def encode(p: HTimes[H,T]) = p match {
        case HTimes(h, t) => sqlEncode(h) :: makeSQLParameters(t)
    }
    // ref_704
    def decode(ls: List[Any]) = ls match {
        case h :: t => sql.decode(as[sql.CT](h)) :+: rest.decode(t)
        case _      => sys.error("yeah...")
    }
    val length = rest.length + 1
    val colTypes = sql.typeName :: rest.colTypes
}

case class Column(dataType: String)

trait SQLTable[A] {
    type P
    
    val name: String
    val colNames: List[String]
    val colTypes: List[String]
    val eqv: Equivalence[A] { type B=P }
    val parammer: SQLParameters[P]
    
    val size = parammer.length
    
    def encode(pre: PreparedStatement, a: A) {
        val indices = 1 to parammer.length
        val params = parammer.encode(eqv.extract(a))
        indices zip params foreach { case (n, param) =>
            pre.setObject(n, param)
        }
    }
        
    def getRow(rs: ResultSet): A = {
        val indices = 1 to parammer.length
        val params = (indices map rs.getObject _).toList
        eqv.compose(parammer decode params)
    }
}

class RealSQLTable[A,PP](mf: Manifest[A], val eqv: Equivalence[A]{type B=PP},
        val parammer: SQLParameters[PP])
    extends SQLTable[A]
{
    type P = PP
    
    val name = mf.erasure.getSimpleName
    val colNames = mf.erasure.getDeclaredFields.toList take
        parammer.length map (_.getName)
    val colTypes = parammer.colTypes
}

implicit def sqlTable[A,PP](implicit mf: Manifest[A], eqv: Equivalence[A]{type B=PP},
        parammer: SQLParameters[PP]) =
    new RealSQLTable(mf, eqv, parammer)

case class Where(text: String, param: Any)

implicit def whereOps(s: Symbol) = new {
    def ~=~[X](x: X)(implicit field: SQLEncode[X]) =
        Where("`%s`=?" format s.name, field.encode(x))
}

trait Schema {
    def tables: List[Table[_]] = Nil
    def makeConnection: Connection
    
    def inTransaction[A](a: => A): A = {
        threadLocalCon.get match {
            case None =>
                val con = makeConnection
                con.setAutoCommit(false)
                threadLocalCon set Some(con)
                try {
                    a
                }
                finally {
                    con.commit
                    con.close
                    threadLocalCon set None
                }
            case Some(_) => a
        }
    }
    
    private val threadLocalCon = new java.lang.ThreadLocal[Option[Connection]] {
        override val initialValue = None
    }
    
    private def con = threadLocalCon.get getOrElse {
        sys.error("Not in a transaction (use readDB)")
    }
    
    // ref_629
    class Table[A](table: SQLTable[A]) extends RefreshHub {
        def create_!() = {
            val stat = con.createStatement
            stat.executeUpdate("drop table if exists `%s`" format table.name)
            stat.executeUpdate("create table `%s` (%s)" format (table.name,
                table.colNames zip table.colTypes map { case (n, t) =>
                    ("`%s` %s" format (n,t))
                } mkString ", "
            ))
        }
        
        def createIfNecessary() {
            inTransaction {
                try {
                    headOption
                }
                catch { case _: SQLException =>
                    create_!
                }
            }
        }
        
        def insert(a: A) = {
            val pre = prepareStatement("insert into `%s` values (%s)" format (table.name,
                Iterator.fill(table.parammer.length)("?") mkString ","
            ))
            table.encode(pre, a)
            pre.executeUpdate()
        }
        
        def where(wh: Where) = Query(wh::Nil)
        
        def toList = Query(Nil).toList
        def headOption = Query(Nil).headOption
        
        def lookup(id: String) = this where ('id ~=~ id) headOption
        
        private def prepareStatement(s: String) = {
            con.prepareStatement(s)
        }
        
        case class Query(wheres: List[Where]) {
            def where(wh: Where) = copy(wheres = wh::wheres)
            
            def toList: List[A] = {
                inTransaction {
                    val rs = executeQuery()
                    var rows = List[A]()
                    
                    while (rs.next) rows ::= table.getRow(rs)
                    rows
                }
            }

            def headOption: Option[A] = {
                inTransaction {
                    val rs = executeQuery(limit = Some(1))
                    val end =
                        if (rs.next) Some(table.getRow(rs))
                        else None
                    end
                }
            }
            
            def set(a: A) {
                val pre = prepareStatement("update `%s` set %s where %s"
                    format (
                        table.name,
                        table.colNames map (name => "`%s`=?" format name) mkString ", ",
                        wheres map (_.text) mkString " AND "
                    )
                )
            
                // The thing being set
                table.encode(pre, a)
                
                // The WHERE clause
                val indices = (table.size+1) to (table.size+wheres.length)
                indices zip wheres foreach { case (n, Where(_, param)) =>
                    pre.setObject(n, param)
                }
                
                pre.executeUpdate
            }
            
            def delete() {
                val pre = prepareStatement("delete from `%s` where %s"
                    format (
                        table.name,
                        wheres map (_.text) mkString " AND "
                    )
                )
            
                val indices = 1 to wheres.length
                indices zip wheres foreach { case (n, Where(_, param)) =>
                    pre.setObject(n, param)
                }
                
                pre.executeUpdate
            }
            
            private def executeQuery(limit: Option[Int]=None): ResultSet = {
                val limitText = limit map ("limit " + _) getOrElse ""
                val whereTexts = (wheres map (_.text)) :+ "1"
                val pre = prepareStatement("select * from `%s` where %s " + limitText
                    format (table.name, whereTexts mkString " AND ")
                )
                val indices = 1 to wheres.length
                indices zip wheres foreach { case (n, Where(_, param)) =>
                    pre.setObject(n, param)
                }
                
                pre.executeQuery
            }
        }
    }
    
    def table[A](implicit table: SQLTable[A]) = new Table(table)
    
    def create_!() { tables foreach (_.create_!) }
    
    def createIfNecessary() { 
        inTransaction {
            tables foreach (_.createIfNecessary)
        }
    }
}

trait Backend extends Schema {
    def makeConnection = {
        Class.forName("org.h2.Driver")
        java.sql.DriverManager.getConnection("jdbc:h2:data;AUTO_SERVER=TRUE")
        //Class.forName("org.sqlite.JDBC")
        //DriverManager.getConnection("jdbc:sqlite:test.db")
    }
}

// ---------------------------------------------------------------------------
// You know, these things...

implicit def caseProd0[A<:Product](implicit con: () => A) = new Equivalence[A] {
    type B = HOne
    def extract(a: A) = HOne()
    def compose(b: HOne) = con()
}

implicit def caseProd1[A<:Product,X1]
        (implicit con: (X1) => A) = new Equivalence[A]
{
    type B = X1:+:HOne
    // ref_997
    def extract(a: A) = a.productIterator.toList match {
        case x1::Nil =>
            as[X1](x1):+:HOne()
        case _ => sys.error("Wrong product arity " + a)
    }
    // ref_662
    def compose(b: B) = b match {
        case x1:+:HOne() =>
            con(x1)
    }
}

implicit def caseProd2[A<:Product,X1,X2]
        (implicit con: (X1,X2) => A) = new Equivalence[A]
{
    type B = X1:+:X2:+:HOne
    def extract(a: A) = a.productIterator.toList match {
        case x1::x2::Nil =>
            as[X1](x1):+:as[X2](x2):+:HOne()
        case _ => sys.error("Wrong product arity " + a)
    }
    def compose(b: B) = b match {
        case x1:+:x2:+:HOne() =>
            con(x1,x2)
    }
}

implicit def caseProd3[A<:Product,X1,X2,X3]
        (implicit con: (X1,X2,X3) => A) = new Equivalence[A]
{
    type B = X1:+:X2:+:X3:+:HOne
    def extract(a: A) = a.productIterator.toList match {
        case x1::x2::x3::Nil =>
            as[X1](x1):+:as[X2](x2):+:as[X3](x3):+:HOne()
        case _ => sys.error("Wrong product arity " + a)
    }
    def compose(b: B) = b match {
        case x1:+:x2:+:x3:+:HOne() =>
            con(x1,x2,x3)
    }
}

implicit def caseProd4[A<:Product,X1,X2,X3,X4]
        (implicit con: (X1,X2,X3,X4) => A) = new Equivalence[A]
{
    type B = X1:+:X2:+:X3:+:X4:+:HOne
    def extract(a: A) = a.productIterator.toList match {
        case x1::x2::x3::x4::Nil =>
            as[X1](x1):+:as[X2](x2):+:as[X3](x3):+:as[X4](x4):+:HOne()
        case _ => sys.error("Wrong product arity " + a)
    }
    def compose(b: B) = b match {
        case x1:+:x2:+:x3:+:x4:+:HOne() =>
            con(x1,x2,x3,x4)
    }
}

implicit def caseProd5[A<:Product,X1,X2,X3,X4,X5]
        (implicit con: (X1,X2,X3,X4,X5) => A) = new Equivalence[A]
{
    type B = X1:+:X2:+:X3:+:X4:+:X5:+:HOne
    def extract(a: A) = a.productIterator.toList match {
        case x1::x2::x3::x4::x5::Nil =>
            as[X1](x1):+:as[X2](x2):+:as[X3](x3):+:as[X4](x4):+:as[X5](x5):+:HOne()
        case _ => sys.error("Wrong product arity " + a)
    }
    def compose(b: B) = b match {
        case x1:+:x2:+:x3:+:x4:+:x5:+:HOne() =>
            con(x1,x2,x3,x4,x5)
    }
}

implicit def caseProd6[A<:Product,X1,X2,X3,X4,X5,X6]
        (implicit con: (X1,X2,X3,X4,X5,X6) => A) = new Equivalence[A]
{
    type B = X1:+:X2:+:X3:+:X4:+:X5:+:X6:+:HOne
    def extract(a: A) = a.productIterator.toList match {
        case x1::x2::x3::x4::x5::x6::Nil =>
            as[X1](x1):+:as[X2](x2):+:as[X3](x3):+:as[X4](x4):+:as[X5](x5):+:as[X6](x6):+:HOne()
        case _ => sys.error("Wrong product arity " + a)
    }
    def compose(b: B) = b match {
        case x1:+:x2:+:x3:+:x4:+:x5:+:x6:+:HOne() =>
            con(x1,x2,x3,x4,x5,x6)
    }
}

implicit def caseProd7[A<:Product,X1,X2,X3,X4,X5,X6,X7]
        (implicit con: (X1,X2,X3,X4,X5,X6,X7) => A) = new Equivalence[A]
{
    type B = X1:+:X2:+:X3:+:X4:+:X5:+:X6:+:X7:+:HOne
    def extract(a: A) = a.productIterator.toList match {
        case x1::x2::x3::x4::x5::x6::x7::Nil =>
            as[X1](x1):+:as[X2](x2):+:as[X3](x3):+:as[X4](x4):+:as[X5](x5):+:as[X6](x6):+:as[X7](x7):+:HOne()
        case _ => sys.error("Wrong product arity " + a)
    }
    def compose(b: B) = b match {
        case x1:+:x2:+:x3:+:x4:+:x5:+:x6:+:x7:+:HOne() =>
            con(x1,x2,x3,x4,x5,x6,x7)
    }
}

implicit def caseProd8[A<:Product,X1,X2,X3,X4,X5,X6,X7,X8]
        (implicit con: (X1,X2,X3,X4,X5,X6,X7,X8) => A) = new Equivalence[A]
{
    type B = X1:+:X2:+:X3:+:X4:+:X5:+:X6:+:X7:+:X8:+:HOne
    def extract(a: A) = a.productIterator.toList match {
        case x1::x2::x3::x4::x5::x6::x7::x8::Nil =>
            as[X1](x1):+:as[X2](x2):+:as[X3](x3):+:as[X4](x4):+:as[X5](x5):+:as[X6](x6):+:as[X7](x7):+:as[X8](x8):+:HOne()
        case _ => sys.error("Wrong product arity " + a)
    }
    def compose(b: B) = b match {
        case x1:+:x2:+:x3:+:x4:+:x5:+:x6:+:x7:+:x8:+:HOne() =>
            con(x1,x2,x3,x4,x5,x6,x7,x8)
    }
}

implicit def caseProd9[A<:Product,X1,X2,X3,X4,X5,X6,X7,X8,X9]
        (implicit con: (X1,X2,X3,X4,X5,X6,X7,X8,X9) => A) = new Equivalence[A]
{
    type B = X1:+:X2:+:X3:+:X4:+:X5:+:X6:+:X7:+:X8:+:X9:+:HOne
    def extract(a: A) = a.productIterator.toList match {
        case x1::x2::x3::x4::x5::x6::x7::x8::x9::Nil =>
            as[X1](x1):+:as[X2](x2):+:as[X3](x3):+:as[X4](x4):+:as[X5](x5):+:as[X6](x6):+:as[X7](x7):+:as[X8](x8):+:as[X9](x9):+:HOne()
        case _ => sys.error("Wrong product arity " + a)
    }
    def compose(b: B) = b match {
        case x1:+:x2:+:x3:+:x4:+:x5:+:x6:+:x7:+:x8:+:x9:+:HOne() =>
            con(x1,x2,x3,x4,x5,x6,x7,x8,x9)
    }
}

implicit def caseProd10[A<:Product,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10]
        (implicit con: (X1,X2,X3,X4,X5,X6,X7,X8,X9,X10) => A) = new Equivalence[A]
{
    type B = X1:+:X2:+:X3:+:X4:+:X5:+:X6:+:X7:+:X8:+:X9:+:X10:+:HOne
    def extract(a: A) = a.productIterator.toList match {
        case x1::x2::x3::x4::x5::x6::x7::x8::x9::x10::Nil =>
            as[X1](x1):+:as[X2](x2):+:as[X3](x3):+:as[X4](x4):+:as[X5](x5):+:as[X6](x6):+:as[X7](x7):+:as[X8](x8):+:as[X9](x9):+:as[X10](x10):+:HOne()
        case _ => sys.error("Wrong product arity " + a)
    }
    def compose(b: B) = b match {
        case x1:+:x2:+:x3:+:x4:+:x5:+:x6:+:x7:+:x8:+:x9:+:x10:+:HOne() =>
            con(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10)
    }
}

def generateCaseProd(n: Int) = {
    val nums = 1 to n
    val types = nums map ("X%d" format _)
    val vars = nums map ("x%d" format _)
    
    val res = """
implicit def caseProd%d[A<:Product,%s]
        (implicit con: (%s) => A) = new Equivalence[A]
{
    type B = %s:+:HOne
    def extract(a: A) = a.productIterator.toList match {
        case %s::Nil =>
            %s:+:HOne()
        case _ => sys.error("Wrong product arity " + a)
    }
    def compose(b: B) = b match {
        case %s:+:HOne() =>
            con(%s)
    }
}
""" format (
        n,
        types mkString ",",
        types mkString ",",
        types mkString ":+:",
        vars mkString "::",
        types zip vars map { case (t,v) => "as[%s](%s)" format (t,v) } mkString ":+:",
        vars mkString ":+:",
        vars mkString ","
    )

    res
}

def generateCaseProds {
    (1 to 10) foreach { n =>
        print(generateCaseProd(n))
    }
}

//
}

