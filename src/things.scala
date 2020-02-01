import java.text.{ParseException, SimpleDateFormat}
import java.util.Date
import com.google.gson.Gson
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import java.io._

object things {
  abstract class Things
  // 3 el
  case class Cat(name : String, race : String, age: Int) extends Things
  // 5 el
  case class Person(firstName: String, lastName: String, salary: Int, numberOfChildren : Int, age : Int) extends Things
  // 4 el
  case class Car(brand: String, countryOfBirth: String, maxSpeed: Int, speeds: Int) extends Things
  // 2 el 1 array
  case class Film(mainActors: Array[String], dateOfRelease: Date) extends Things
  // 2 el 1 array
  case class Actor(name: String, filmsPlayed: Array[String]) extends Things

  // Return empty class from given pattern
  def findClass(arr : Array[String]): Option[Things] = {
    arr.length match{
      case 3 =>Some(Cat(arr(0),arr(1),arr(2).toInt))
      case 4 =>Some(Car(arr(0),arr(1),arr(2).toInt, arr(3).toInt))
      case 5 =>Some(Person(arr(0),arr(1),arr(2).toInt, arr(3).toInt, arr(4).toInt))
      case 2=>{
        val DATE_FORMAT = "DD/MM/YYYY"
        val dateFormat = new SimpleDateFormat(DATE_FORMAT)

        try {
          //Try to parse date
          val pDate = dateFormat.parse(arr(1))
          Some(Film(arr(0).split(';').toArray,pDate))
        } catch {
          //no date => its an actor
          case e: ParseException =>
            Some(Actor(arr(0),arr(1).split(';').toArray))
        }
      }
      case _ => {
        Option.empty
      }
    }
  }
  def main(args: Array[String]): Unit = {
    // Import data
    val filename = "datasets/dataset.csv"
    // Setup arrays
    val arrCat = ArrayBuffer[Cat]()
    val arrPerson = ArrayBuffer[Person]()
    val arrCar = ArrayBuffer[Car]()
    val arrFilm = ArrayBuffer[Film]()
    val arrActor = ArrayBuffer[Actor]()
    // For each line of dataset
    for (line <- Source.fromFile(filename).getLines()){
      val cols = line.split(",").map(_.trim)
      // Find class correponding at this collumn
      val o = findClass(cols)
      // Find array to push corresponding class
      o.get match{
        case Cat(name,race,age) => {
          arrCat.addOne(Cat(name,race,age))
        }
        case Car(brand,countryOfBirth,maxSpeed,speeds)=>{
          arrCar.addOne(Car(brand,countryOfBirth,maxSpeed,speeds))
        }
        case Person(firstName, lastName, salary, numberOfChildren, age)=>{
          arrPerson.addOne(Person(firstName, lastName, salary, numberOfChildren, age))
        }
        case Film(mainActors, dateOfRelease)=>{
          arrFilm.addOne(Film(mainActors, dateOfRelease))
        }
        case Actor(name, filmsPlayed)=>{
          arrActor.addOne( Actor(name, filmsPlayed))
        }
      }

    }
    // write json
    val res = arrActor ++ arrCar ++ arrCat ++ arrFilm ++ arrPerson
    val gson = new Gson
    val jsonString = gson.toJson(res).stripMargin
    val file = new File("./jjj.json")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(jsonString)
    bw.close()
  }

}

