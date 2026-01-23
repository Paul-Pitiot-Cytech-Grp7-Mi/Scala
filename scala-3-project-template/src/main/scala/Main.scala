@main def hello(): Unit =
  // Tests isLeap
  println(s"2024 bissextile ? ${isLeap(2024)}")  // Some(true)
  println(s"2023 bissextile ? ${isLeap(2023)}")  // Some(false)
  println(s"1900 bissextile ? ${isLeap(1900)}")  // Some(false)
  println(s"2000 bissextile ? ${isLeap(2000)}")  // Some(true)
  println(s"-5 bissextile ? ${isLeap(-5)}")      // None

  // Tests numberOfDays
  println(s"Février 2024 : ${numberOfDays(true, 2)}")   // Some(29)
  println(s"Février 2023 : ${numberOfDays(false, 2)}")  // Some(28)
  println(s"Janvier : ${numberOfDays(false, 1)}")       // Some(31)
  println(s"Mois 13 : ${numberOfDays(false, 13)}")      // None

  println(s"pi(10) = ${pi(10)}")
  println(s"pi(100) = ${pi(100)}")
  println(s"pi(1000) = ${pi(1000)}")
  println(s"pi(10000) = ${pi(10000)}")
  println(s"pi(10000) = ${pi(10000)}")
  println(s"Vrai pi = ${math.Pi}")
  println(s"piMC(1000) = ${piMC(1000)}")
  println(s"piMC(10000) = ${piMC(10000)}")
  println(s"piMC(100000) = ${piMC(100000)}")
  println(s"piMC(1000000) = ${piMC(1000000)}")
  println(s"Vrai pi = ${math.Pi}")

// a. Vérifie si une année est bissextile
def isLeap(year: Int): Option[Boolean] =
  if year < 0 then None
  else Some(year % 4 == 0 && (year % 100 != 0 || year % 400 == 0))

// b. Nombre de jours dans un mois
def numberOfDays(leap: Boolean, month: Int): Option[Int] =
  month match
    case 1 | 3 | 5 | 7 | 8 | 10 | 12 => Some(31)
    case 4 | 6 | 9 | 11              => Some(30)
    case 2                            => if leap then Some(29) else Some(28)
    case _                            => None

def pi(n: Int): Double =
  @scala.annotation.tailrec
  def somme(k: Int, acc: Double): Double =
    if k > n then acc
    else somme(k + 1, acc + 1.0 / (k * k))
  
  math.sqrt(6 * somme(1, 0.0))

def piMC(n: Int): Double =
  val rand = new util.Random()
  def count(k: Int, acc: Int): Int =
    if k >= n then acc
    else
      val x = rand.nextDouble()
      val y = rand.nextDouble()
      val inside = if x*x + y*y <= 1 then 1 else 0
      count(k + 1, acc + inside)
  
  4.0 * count(0, 0) / n