import java.time.Duration
import scala.Nil
import scala.collection.immutable.{AbstractSeq, LinearSeq, Nil}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map => muMap}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}
case class Info(address: String, emailOpt: Option[String])

case class User(id: Option[Long], name: String, ageOpt: Option[Int], infoOpt: Option[Info] = None)
object User {
  def empty: User = User(None, "empty", None)
}
val user1: User = User(None, "name1", ageOpt = Some(22), Some(Info("address1", None)))
val user2: User = User(Some(1), "name2", ageOpt = None)
val user3: User = User(Some(2), "name3", Some(0), Some(Info("", Some("email1"))))
val user4: User = User(Some(3), "name4", Some(35), Some(Info("address2", Some("email2"))))
val user5Opt: Option[User] = Some(User(Some(4), "name5", ageOpt = Some(45)))
val user6Opt: Option[User] = None

val seq1: Seq[User] = Seq(user1, user2, user3, user4)

val r = scala.util.Random
def getUser: Future[Option[User]] = {
  val value = r.nextInt(8)
  Future(seq1.lift(value))
}

val newSeq1 = seq1.map{
  user =>{
    if(user.ageOpt == Some(0))
    user.copy(ageOpt = Some(5))
    else
      user
  }
}

