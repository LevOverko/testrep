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

//1.seq1
//1.1 Создать коллекцию с именами
def getName(seq1:Seq[User]): List[String] ={
  val name = new ListBuffer[String]()
  for(element <- seq1.indices){
    name += (seq1(element).name)
  }
  return name.toList
}
val nameUser: List[String] = getName(seq1)

//1.2 Создать коллекцию с возростами Seq[Int]
def  getAge(seq1:Seq[User]): Seq[Int] = {
  val age = new ListBuffer[Int]()
  for (element <- seq1.indices) {
    seq1(element).ageOpt.foreach(i => age += i)
  }
  return age.toSeq
}
val ageUser: Seq[Int] = getAge(seq1)

//1.3 Выбрать у которых age > 0
def ageNotZero(seq1:Seq[User]){
  for(element <- seq1.indices) {
    if (seq1(element).ageOpt.getOrElse(0) > 0) {
      println("Возраст " + seq1(element).name + " больше 0: " + seq1(element).ageOpt.get)
    }
  }
}
ageNotZero(seq1)
//1.4 Заменить age где age = 0 to age = 5

//1.5 Получить сумму всех возрастов
def  getSumAge(seq1:Seq[User]): Int = {
  val age = new ListBuffer[Int]()
  for (element <- seq1.indices) {
    seq1(element).ageOpt.foreach(i => age += i)
  }
  return age.sum
}
println(getSumAge(seq1))

//1.6 Напечатать все имена одной строкой через запятую
def printName(seq1:Seq[User]) {
  for(element <- seq1.indices){
    print(seq1(element).name + ",")
  }
}
printName(seq1)
//1.7 Создать Map[Long, String] где Long это id и String это name
def getMap(seq1:Seq[User]): muMap[Long,String] = {
  val mapUsers = muMap.empty[Long, String]
  for(element <- seq1.indices){
    if(seq1(element).id.isDefined)
      mapUsers += (seq1(element).id.get -> seq1(element).name)
  }
  return mapUsers
}
val mapUsers  = getMap(seq1)
//1.8 Сортировать по возврастам в прямом и обратном порядке
seq1.sortBy(user => user.ageOpt)
seq1.sortBy(user => user.ageOpt).reverse

//1.9 Создать user7 используя User.empty (без имени, без id, с возрасом 55)
val user7: User = User.empty


//1.10 добавить user7 в начало и в конец seq1
user7 +: seq1 :+ user7

//1.11 разделить коллекцию на 2 в одной возраст > 30 в другой меньше
val (ageMore30,ageSmaller30) = seq1.partition(user => user.ageOpt.getOrElse(0) > 30)

//1.13 вывести адреса в одну строку через запятую
def printAddress(seq1:Seq[User]) {
  for(element <- seq1.indices){
    if(seq1(element).infoOpt.isDefined){
      print(seq1(element).infoOpt.get.address + ",")
    }
  }
}
printAddress(seq1)

//1.14 найти юзера с email = email2 если не существует вывести User.empty
def findEmail(seq1:Seq[User]) {
  for(element <- seq1.indices){
    if(seq1(element).infoOpt.isDefined && seq1(element).infoOpt.get.emailOpt.isDefined && seq1(element).infoOpt.get.emailOpt.get == "email2")
      println(seq1(element))
    else {
      println(User.empty)
    }
  }
}
findEmail(seq1)

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
//3.1 сздать git аккаунт
//3.2 решение добавить в Git