object Chap18 {
  class BankAccount {
    private var bal: Int = 0
    def balance: Int = bal
    def deposit(amount: Int): Unit = {
      require(amount > 0)
      bal += amount
    }
    def withdraw(amount: Int): Boolean = {
      if (amount > bal) false
      else {
        bal -= amount
        true
      }
    }
  }

  class Keyed {
    def computeKey: Int = ???
  }

  class MemoKeyed extends Keyed {
    private var keyCache: Option[Int] = None

    override def computeKey: Int = {
      if (!keyCache.isDefined)
        keyCache = Some(super.computeKey)
      keyCache.get
    }
  }

  class Time {
    private[this] var h = 12
    private[this] var m = 0

    def hour: Int = h
    def hour_=(x: Int)=  {
      require(x >= 0 && x < 24)
      h = x
    }

    def minute: Int = m
    def minute_=(x: Int)= {
      require(0 <= x && x < 60)
      m = x
    }
  }

  class Thermoeter {
    var celsius: Float = _

    def fahrenheit = celsius * 9 / 5 + 32
    def fahrenheit_=(f: Float) = {
      celsius = (f - 32) * 5 / 9
    }

    override def toString: String =
      fahrenheit + "F/"+ celsius +"C"
  }


  class Wire

  def inverter(input: Wire, output: Wire) = ???
  def andGate(a1: Wire, a2: Wire, output: Wire) = ???
  def orGate(o1: Wire, o2: Wire, output: Wire) = ???

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire) = {
    val d, e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire) = {
    val s, c1, c2 = new Wire
    halfAdder(a, cin, s, c1)
    halfAdder(b, s, sum, c2)
    orGate(c1, c2, cout)
  }

  
  val account = new BankAccount
  account deposit 100
  account withdraw 80
  account withdraw 80

  val t = new Time
  t.hour
  t.hour = 23
  t.hour

}