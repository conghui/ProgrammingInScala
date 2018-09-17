
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

  abstract class Simulation {
    type Action = () => Unit

    case class WorkItem(time: Int, action: Action)

    private var curtime: Int = 0
    def currentTime: Int = curtime

    private var agenda: List[WorkItem] = List()

    private def insert(ag: List[WorkItem], item: WorkItem): List[WorkItem] = ag match {
      case Nil => List(item)
      case x :: xs =>
        if (item.time < x.time) item :: ag
        else x :: insert(xs, item)
    }

    def afterDelay(delay: Int)(block: => Unit): Unit = {
      val item = WorkItem(currentTime + delay, () => block)
      agenda = insert(agenda, item)
    }

    private def next(): Unit = {
      (agenda: @unchecked) match {
        case item :: rest =>
          agenda = rest
          curtime = item.time
          item.action()
      }
    }

    def run(): Unit = {
      afterDelay(0) {
        println("*** simulation started, time = "+ currentTime +" ***")
      }
      while (!agenda.isEmpty) next()
    }
  }

  abstract class BasicCircuitSimulation extends Simulation {
    def InverterDelay: Int
    def AndGateDelay: Int
    def OrGateDealy: Int

    class Wire {
      private var sigVal: Boolean = false
      private var actions: List[Action] = List()

      def getSignal = sigVal
      def setSignal(s: Boolean) = {
        if (s != sigVal) {
          sigVal = s
          actions foreach (_())
        }
      }

      def addAction(a: Action) = {
        actions = a :: actions
        a()
      }
    }

    def inverter(input: Wire, output: Wire): Unit = {
      def invertAction(): Unit = {
        afterDelay(InverterDelay) {
          output setSignal !input.getSignal
        }
      }

      input addAction invertAction
    }

    def andGate(a1: Wire, a2: Wire, output: Wire): Unit = {
      def andAction(): Unit = {
        afterDelay(AndGateDelay) {
          output setSignal (a1.getSignal & a2.getSignal)
        }
      }

      a1 addAction andAction
      a2 addAction andAction
    }

    def orGate(o1: Wire, o2: Wire, output: Wire): Unit = {
      def orAction(): Unit = {
        afterDelay(OrGateDealy) {
          output setSignal (o1.getSignal | o2.getSignal)
        }
      }

      o1 addAction orAction
      o2 addAction orAction
    }

    def probe(name: String, wire: Wire): Unit = {
      def probeAction(): Unit = {
        println(name +" "+ currentTime +" new-value = "+ wire.getSignal)
      }

      wire addAction probeAction
    }
  }

  abstract class CircuitSimulation extends BasicCircuitSimulation {
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
  }

  object MySimulation extends CircuitSimulation {
    override def InverterDelay: Int = 1

    override def AndGateDelay: Int = 3

    override def OrGateDealy: Int = 5
  }

  val account = new BankAccount
  account deposit 100
  account withdraw 80
  account withdraw 80

  val t = new Time
  t.hour
  t.hour = 23
  t.hour

  import MySimulation._

  val input1, input2, sum, carry = new Wire
  probe("sum", sum)
  probe("carry", carry)
  halfAdder(input1, input2, sum, carry)
  input1 setSignal true
  run()
  input2 setSignal true
  run()
}