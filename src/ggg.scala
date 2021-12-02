import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map => muMap}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

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

//1. в задачах использовать seq1
//1.1 Получить коллекцию с именами юзеров из коллекции seq1
val names: Seq[String] = seq1.map(user => user.name)
//Output:
//val names: Seq[String] = List(name1, name2, name3, name4)

//1.2 Получить коллекцию с возростами Seq[Int]
val age: Seq[Int] = seq1.map(user => user.ageOpt.getOrElse(0))
//Output:
//val age: Seq[Int] = List(22, 0, 0, 35)

//1.3 Выбрать у которых age > 0
seq1.filter(user => user.ageOpt.getOrElse(0) > 0)
//Output
//val res0: Seq[User] = List(User(None,name1,Some(22),Some(Info(address1,None))), User(Some(3),name4,Some(35),Some(Info(address2,Some(email2)))))

//1.4 Заменить age где age = 0 to age = 5
val newSeq1 = seq1.map{
  user =>{
    if(user.ageOpt == Some(0))
      user.copy(ageOpt = Some(5))
    else
      user
  }
}
//Output
//val newSeq1: Seq[User] = List(User(None,name1,Some(22),Some(Info(address1,None))), User(Some(1),name2,None,None), User(Some(2),name3,Some(5),Some(Info(,Some(email1)))), User(Some(3),name4,Some(35),Some(Info(address2,Some(email2)))))

//1.5 Получить сумму всех возрастов
seq1.map(user => user.ageOpt.sum).sum
//Output
//val res0: Int = 57

//1.6 Напечатать все имена одной строкой через запятую
seq1.foreach(user => print(user.name + ", "))
//Output
//name1, name2, name3, name4,

//1.7 Создать Map[Long, String] где Long это id и String это name
val mapUsers:Map[Long, String] = seq1.map{
  user =>{
    user.id.getOrElse(0.toLong) -> user.name
  }
}.toMap
//Output
//val mapUsers: Map[Long,String] = Map(0 -> name1, 1 -> name2, 2 -> name3, 3 -> name4)

//1.8 Сортировать по возврастам в прямом и обратном порядке
seq1.sortBy(user => user.ageOpt)
//Output
//val res0: Seq[User] = List(User(Some(1),name2,None,None), User(Some(2),name3,Some(0),Some(Info(,Some(email1)))), User(None,name1,Some(22),Some(Info(address1,None))), User(Some(3),name4,Some(35),Some(Info(address2,Some(email2)))))
seq1.sortBy(user => user.ageOpt).reverse
//Output
//val res0: Seq[User] = List(User(Some(3),name4,Some(35),Some(Info(address2,Some(email2)))), User(None,name1,Some(22),Some(Info(address1,None))), User(Some(2),name3,Some(0),Some(Info(,Some(email1)))), User(Some(1),name2,None,None))

//1.9 Создать user7 используя User.empty (без имени, id = 7, с возрасом 55)
val user7: User = User.empty.copy(Some(7),"empty", Some(55), None)
//Output
//val user7: User = User(Some(5),empty,Some(55),None)


//1.10 добавить user7 в начало и в конец seq1
user7 +: seq1 :+ user7
//Output
//val res0: Seq[User] = List(User(Some(7),empty,Some(55),None), User(None,name1,Some(22),Some(Info(address1,None))), User(Some(1),name2,None,None), User(Some(2),name3,Some(0),Some(Info(,Some(email1)))), User(Some(3),name4,Some(35),Some(Info(address2,Some(email2)))), User(Some(7),empty,Some(55),None))

//1.11 разделить коллекцию на 2 в одной возраст > 30 в другой меньше
val (ageMore30,ageSmaller30) = seq1.partition(user => user.ageOpt.getOrElse(0) > 30)
//Output
//val ageMore30: Seq[User] = List(User(Some(3),name4,Some(35),Some(Info(address2,Some(email2)))))
//val ageSmaller30: Seq[User] = List(User(None,name1,Some(22),Some(Info(address1,None))), User(Some(1),name2,None,None), User(Some(2),name3,Some(0),Some(Info(,Some(email1)))))

//1.13 вывести адреса в одну строку через запятую
seq1.foreach{
  user =>{
    if(user.infoOpt.isDefined)
      print(user.infoOpt.get.address + ", ")
  }
}
//Output
//address1, , address2,

//1.14 найти юзера с email = email2 если не существует вывести User.empty
seq1.foreach{
  user =>{
    if(user.infoOpt.isDefined && user.infoOpt.get.emailOpt.isDefined && user.infoOpt.get.emailOpt.get == "email2")
      println(user)
    else
      println(User.empty)
  }
}
//Output
//User(None,empty,None,None)
//User(None,empty,None,None)
//User(None,empty,None,None)
//User(Some(3),name4,Some(35),Some(Info(address2,Some(email2))))

//1.15 использовать foldLeft для получения суммы возрастов
val sumAgeUser = seq1.foldLeft(0)((age, user) => age + user.ageOpt.getOrElse(0))

//2. использовать ф-ю getUser
//2.1 вывести имя юзра если есть если нет вывести "юзер не существует"
val userResult: String = Await.result(
  getUser.map{
    case Some(user) => user.name
    case None => "Юзера не существует"
  },
  1.second
)
//2.2 вывести email, если email не существует то вывети "email не существует", если юзер не существует вывести  "юзер не существует"
val userResultEmail: String = Await.result(
  getUser.map{
    case Some(user) => user.infoOpt.get.emailOpt.getOrElse("Почты не существует")
    case None => "Юзера не существует"
  },
  1.second
)

//3
def dddd
//3.1 сздать git аккаунт
//3.2 решение добавить в Git