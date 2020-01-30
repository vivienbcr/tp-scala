import java.text.{ParseException, SimpleDateFormat}
import java.util.Date

import scala.io.Source

object things {
  abstract class Things
  // 3 el
  case class Cat(name : String, race : String, age: Int) extends Things
  // 5 el
  case class Person(firstName: String, lastName: String, salary: Int, numberOfChildren : Int, age : Int) extends Things
  // 4 el
  case class Car(brand: String, countryOfBirth: String, maxSpeed: Int, speeds: Int) extends Things
  // 2 el 1 array
  case class Film(mainActors: Seq[String], dateOfRelease: Date) extends Things
  // 2 el 1 array
  case class Actor(name: String, filmsPlayed: Seq[String]) extends Things


  def whichThings(thing : Things): String = thing match {
      case Cat(name,race,age) => s"cat $name"
      case Car(brand,countryOfBirth,maxSpeed,speeds) => s"car $brand"
      case _ => s"error"
  }
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
          Some(Film(arr(0).split(';').toSeq,pDate))
        } catch {
          //no date => its an actor
          case e: ParseException =>
            Some(Actor(arr(0),arr(1).split(';').toSeq))
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
    val arrCat = Array[Cat]()
    val arrPerson = Array[Person]()
    val arrCar = Array[Car]()
    val arrFilm = Array[Film]()
    val arrActor = Array[Actor]()
    // For each line of dataset => use patern matching to find class
    for (line <- Source.fromFile(filename).getLines()){
      val cols = line.split(",").map(_.trim)
      val o = findClass(cols)
      println(o)
    }



  }

}

