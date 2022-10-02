package org.pawelsberg.scala.calculator
import scala.swing._
 
@main
def main(): Unit =
  // initial state
  var state: State = State.clear
  // define keys
  val digitKeys: Seq[Key] = (1 to 9).appended(0).map(d => DigitKey(d))
  val specialKeys: Seq[Key] = List(PlusKey(),MinusKey(),MultiplicationKey(),DivisionKey(),EqualsKey(),CeKey(),CKey())
  val calculatorKeys: Seq[Key] = digitKeys ++ specialKeys
 
  val f: Frame = new Frame
  f.title = "Calculator"
  f.contents = new BoxPanel(Orientation.Vertical)
  {
    val display = new EditorPane() { text = state.display }
    contents += display
    contents += new GridPanel(6, 3) {
      contents ++= calculatorKeys.map(k=> new Button(k.label) {
        reactions +=
          {
            case event.ButtonClicked(_)=>
              state = state.act(k)
              state.show()
              display.text = state.display
          }
      }
      )
    }
  }
 
  f.pack()
  f.centerOnScreen()
  f.open()
 
case class State(
                  resultRegister: BigDecimal,
                  argumentRegister: BigDecimal,
                  lastOperation: (BigDecimal,BigDecimal) => BigDecimal,
                  showResult: Boolean,
                ):
  def show(): Unit = println(s"Result: $resultRegister Argument: $argumentRegister ShowResult: $showResult lastOperation: $lastOperation")
  def performLastOperation: State =
    this.copy(resultRegister = lastOperation(resultRegister,argumentRegister), showResult = true, lastOperation = State.takeResultOperation)
  def act(key: Key): State = key.actOn(this)
  def display: String =
    val displayValue = if showResult then resultRegister else argumentRegister
    displayValue.toString()
 
case object State:
  val takeResultOperation: (BigDecimal, BigDecimal) => BigDecimal = (result, _) => result
  val takeArgumentOperation: (BigDecimal, BigDecimal) => BigDecimal = (_, argument) => argument
  val clear: State = State(resultRegister = 0, argumentRegister = 0, lastOperation = takeArgumentOperation, showResult = false)
 
trait Key(label0: String):
  def actOn(state: State) : State
  val label = label0
 
case class DigitKey(digit: BigInt) extends Key(label0 = digit.toString):
  override def actOn(state: State): State = state.copy(
    argumentRegister = if state.showResult then BigDecimal(digit) else state.argumentRegister.addDigit(digit),
    showResult = false)
 
case class CeKey() extends Key(label0 = "CE"): // clear entry - just the last digit
  override def actOn(state: State): State = state.copy(
    argumentRegister = if state.showResult then 0 else state.argumentRegister.delDigit,
    showResult = false)
 
case class CKey() extends Key(label0 = "C"): // clear - clears everything
  override def actOn(state: State): State = State.clear
 
case class PlusKey() extends Key(label0 = "+"):
  override def actOn(state: State): State = state.performLastOperation.copy(lastOperation = (result,argument)=> result + argument)
 
case class MinusKey() extends Key(label0 = "-"):
  override def actOn(state: State): State = state.performLastOperation.copy(lastOperation = (result,argument)=> result - argument)
 
case class MultiplicationKey() extends Key(label0 = "*"):
  override def actOn(state: State): State = state.performLastOperation.copy(lastOperation = (result,argument)=> result * argument)
 
case class DivisionKey() extends Key(label0 = "/"):
  override def actOn(state: State): State = state.performLastOperation.copy(lastOperation = (result,argument)=> result / argument)
 
case class EqualsKey() extends Key(label0 = "="):
  override def actOn(state: State): State = state.performLastOperation.copy(lastOperation = State.takeResultOperation)
 
// Tools
extension (bd: BigDecimal)
  def delDigit: BigDecimal =
    val inString = bd.toString
    val outString =
      if inString.length > 1
        then inString.substring(0,inString.length - 1)
        else "0"
    BigDecimal(outString)
  def addDigit(digit : BigInt): BigDecimal =
    val inString = bd.toString
    val outString = inString + digit.toString
    BigDecimal(outString)
 
extension (str: String)
  def deleteLastCharIfExists = if str.length > 1 then str.substring(0, str.length -1) else 0