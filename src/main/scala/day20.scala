import scala.annotation.tailrec

object day20 {
  
  enum PulseType {
    case Low, High, Off
  }
  
  import PulseType.*

  case class Pulse(src: String, pulseType: PulseType, target: String)

  abstract sealed class Module(pulseType: PulseType) {
    def process(p: Pulse): Module
    def getPulseType: PulseType = pulseType
  }

  case class Broadcaster(pulseType: PulseType = Low) extends Module(pulseType) {
    override def process(p: Pulse) = Broadcaster(p.pulseType)
  }

  case class FlipFlop(on: Boolean = false, pulseType: PulseType = Off) extends Module(pulseType) {
    override def process(p: Pulse) = p.pulseType match {
      case Low => FlipFlop(!on, if (on) Low else High)
      case _ => FlipFlop(on, Off)
    }
  }

  case class Conjunction(inputs: Int, state: Map[String, PulseType] = Map.empty, pulseType: PulseType = Off) extends Module(pulseType) {
    override def process(p: Pulse) = {
      val newState = state.updated(p.src, p.pulseType)
      val newPulseType = if (newState.size < inputs || newState.values.exists(_!=High)) High else Low
      Conjunction(inputs, newState, newPulseType)
    }
  }

  case class ModuleConfig(name: String, module: Module, targets: Seq[String]) {
    def process(p: Pulse): ModuleConfig = copy(module = module.process(p))

    def pulses: Seq[Pulse] = module.getPulseType match {
      case Off => Seq.empty
      case pt => targets.map(Pulse(name, pt, _))
    }
  }

  type Config = Map[String, ModuleConfig]

  def parseInput(input: List[String]): Config = {
    def parseTargets(s: String) = s.split(',').map(_.trim).toSeq

    def countInputs(name: String) = {
      input.flatMap{line =>
        parseTargets(line.split('>')(1))
      }.count(_ == name)
    }

    val broadcasterRe = """(broadcaster) -> (.+)""".r
    val flipFlopRe = """%(.+) -> (.+)""".r
    val conjunctionRe = """&(.+) -> (.+)""".r
    input.map{
      case broadcasterRe(name, targets) => name -> ModuleConfig(name, Broadcaster(), parseTargets(targets))
      case flipFlopRe(name, targets) => name -> ModuleConfig(name, FlipFlop(), parseTargets(targets))
      case conjunctionRe(name, targets) => name -> ModuleConfig(name, Conjunction(countInputs(name)), parseTargets(targets))
    }.toMap
  }

  def part1(input: List[String]): Long = {

    @tailrec
    def rec(config: Config, pulses: Seq[Pulse], low: Long = 0, high: Long = 0): (Config, Long, Long) = {
      if (pulses.isEmpty) (config, low, high)
      else {
        val pulse = pulses.head
        val (lo, hi) = if (pulse.pulseType == Low) (low+1, high) else (low, high+1)
        config.get(pulse.target).map(_.process(pulse)) match {
          case None => rec(config, pulses.tail, lo, hi)
          case Some(m) => rec(config.updated(pulse.target, m), pulses.tail ++ m.pulses, lo, hi)
        }
      }
    }

    val startConfig = parseInput(input)
    val startPulse = Seq(Pulse("button", Low, "broadcaster"))

    val (_, low, high) = (1 to 1000).foldLeft((startConfig, 0L, 0L)){
      case ((config,lo,hi), _) => rec(config, startPulse, lo, hi)
    }
    low * high
  }

  def part2(input: List[String]): Long = {
    val startConfig = parseInput(input)
    val startPulse = Seq(Pulse("button", Low, "broadcaster"))

    //rx has one input and all it's inputs need to be sent low
    val rxInput = startConfig.values.find(_.targets.contains("rx")).map(_.name).get
    val inputs = startConfig.values.filter(_.targets.contains(rxInput)).map(_.name).toSet

    @tailrec
    def rec(config: Config, pulses: Seq[Pulse], inputCounts: Map[String,Long] = Map.empty, count: Long = 1): Long = {
      if (inputCounts.size == inputs.size) inputCounts.values.product
      else if (pulses.isEmpty) rec(config, startPulse, inputCounts, count+1)
      else {
        val pulse = pulses.head
        val newInputCounts = if (inputs.contains(pulse.target) && pulse.pulseType == Low) {
          inputCounts.updated(pulses.head.target, count)
        } else inputCounts
        config.get(pulse.target).map(_.process(pulse)) match {
          case None => rec(config, pulses.tail, newInputCounts, count)
          case Some(m) => rec(config.updated(pulse.target, m), pulses.tail ++ m.pulses, newInputCounts, count)
        }
      }
    }
    rec(startConfig, startPulse)
  }
}
