import java.io.IOException
import java.util.Scanner
import scala.collection.mutable.ListBuffer
import scalafx.scene.layout._
import scalafx.application.JFXApp
import scalafx.application.JFXApp._
import scalafx.scene.control._
import scalafx.stage._
import scalafx.scene.Scene
import scalafx.Includes._



/**
 * Created by Ryan Murphy on 7/26/15.
 *
 * Description: This app replicates Project 1 of my Sorcerer's Cave project. However, the key difference here
 *              is that this project is written in Scala rather than Java and *attempts* (key word being attempts)
 *              to leverage the power of Scala in this project. I am attempting to use some of the features of
  *             Scala to get a feel for how they work so I will be comfortable using them in the future.
 * Note (09/09/2015): This is still a rough first draft. The formatting of the strings in the text area needs to be
 *        worked on and the search functionality also needs to be implemented. Finally, I am not familiar with the
 *        spacing of Scala and need to read up on the proper spacing. There are probably serveral other issues that
 *        I need to fix that I am unaware of.
 */

trait GameElement {
  val index: Int
  val name: String
  val elementType: String
}
case class Cave(parties :ListBuffer[Party], excess :ListBuffer[Any])
case class Party(index :Int, name :String, elementType :String, partyMembers :ListBuffer[Creature]) extends GameElement
case class Creature(index :Int, name :String, elementType :String, memberOfPartyIndex :Int,
                    empathy :Int, fear :Int, carryingCapacity :Int, loot :ListBuffer[Treasure],
                    artifacts : ListBuffer[Artifact]) extends GameElement
case class Treasure(index :Int, name :String, elementType :String, ownedByIndex :Int, weight :Double, value :Int) extends GameElement
case class Artifact(index :Int, name :String, elementType :String, ownedByIndex :Int) extends GameElement

object SorcerersCave extends JFXApp{

  val cave = new Cave(ListBuffer(), ListBuffer())
  val informationText: TextArea = new TextArea()

  stage = new PrimaryStage {
      title = "Sorcerers Cave"
      width = 600
      height = 400

      val buttonPane = new FlowPane {
        new Button("Read") {
          onAction = handle { readFile }
        }
        new Button("Display"){
          onAction = handle { displayCave() }
        }
        val searchBy = new ComboBox[String](List("Index", "Name", "Type"))
        new Label("Search target")
        val searchInput = new TextField() {
          prefWidth = 10
        }
        new Button("Search"){
          onAction = handle { search(searchInput.getText.toLowerCase.trim, searchBy.toString()) }
        }
      }
      val scroller = new ScrollPane()
      scroller.setContent(informationText)

      scene = new Scene {
        content = new BorderPane {
             top = buttonPane
             center = scroller
        }
      }
  }

  def readFile: Unit ={
    try {
      val chooser = new FileChooser()
      var in: Scanner = null
      val selectedFile = chooser.showOpenDialog(stage)
      if (selectedFile != null) {
        in = new Scanner(selectedFile)
        while (in.hasNext) {
          val line = in.nextLine()
          processLine(line)
        }
      }
    }catch{
      case ioe: IOException => new Dialog {
        contentText = "File Not Found."
      }
    }finally{
      informationText.appendText("Your File has been loaded!")
    }
  }

  def displayCave(): Unit ={
    informationText.appendText("Displaying Data From Game:\n")
    informationText.appendText(cave.toString+"")
    cave.parties.foreach{
      p : Party => informationText.appendText("  " + p.toString)
      p.partyMembers.foreach{
        c: Creature => informationText.appendText("   " + c.toString)
            c.loot.foreach{
            t : Treasure => informationText.appendText("      " + t.toString)
          }
          c.artifacts.foreach{
            a : Artifact => informationText.appendText("      " + a.toString)
          }
      }
    }
  }


  def processLine(line: String): Unit ={

    val dataList = line.split(":").map(_.trim).toList
    var unaffiliated = true
    dataList.head match{
      case "p"  => cave.parties.append(new Party(dataList(1).toInt, dataList(2), "Party", ListBuffer()))
      case "c"  => val aCreature = new Creature(dataList(1).toInt, dataList(2), dataList(3), dataList(4).toInt,
                                                        dataList(5).toInt, dataList(6).toInt, dataList(7).toInt,
                                                        ListBuffer(), ListBuffer())
        cave.parties foreach{p :Party => aCreature.memberOfPartyIndex match {
          case p.index => p.partyMembers.append(aCreature); unaffiliated = false
          case _ => return
        }

        if (unaffiliated)
          cave.excess.append(aCreature)
      }
      case "t" =>  val aTreasure = new Treasure(dataList(1).toInt, dataList(2), "Treasure", dataList(3).toInt,
                                                         dataList(4).toDouble, dataList(5).toInt)
        cave.parties foreach{p: Party => p.partyMembers foreach {c: Creature => aTreasure.ownedByIndex match {
          case c.index => c.loot.append(aTreasure); unaffiliated = false
          case _ => return
        }
          if (unaffiliated)
            cave.excess.append(aTreasure)
        }
        }
      case "a" => val name = if(dataList.length < 5) "Unnamed" else dataList(4)
        val anArtifact = new Artifact(dataList(1).toInt, name, dataList(2), dataList(3).toInt)
        cave.parties foreach{p: Party => p.partyMembers foreach {c: Creature => anArtifact.ownedByIndex match {
          case c.index => c.artifacts.append(anArtifact); unaffiliated = false
          case _ => return
        }
          if (unaffiliated)
            cave.excess.append(anArtifact)
        }
        }
      case _ => return
    }
  }
  def search(input: String, searchCategory: String): Unit ={

  }

}
