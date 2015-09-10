import java.io.IOException
import scala.collection.mutable.ListBuffer
import scala.swing._
import scala.swing.event.ButtonClicked
import java.util.Scanner


/**
 * Created by Ryan Murphy on 7/26/15.
 *
 * Description: This app replicates Project 1 of my Sorcerer's Cave project. However, the key difference here
 *              is that this project is written in Scala rather than Java and *attempts* (key word being attempts)
 *              to leverage the power of Scala in this language.
 * Note (09/09/2015): This is still a rough first draft. The formatting of the strings in the text area needs to be
 *        worked on and the search functionality also needs to be implemented. Finally, I am not familiar with the
 *        spacing of Scala and need to read up on the proper spacing. There are probably serveral other issues that
 *        I need to fix that I am unaware of.
 */

object SorcerersCave extends App{

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

  val cave = new Cave(ListBuffer(), ListBuffer())
  val informationText: TextArea = new TextArea()
  val scroller = new ScrollPane(informationText)
  val readButton = new Button("Read")
  val displayButton = new Button("Display")
  val searchByComboBox = new ComboBox[String](List("Index", "Name", "Type"))
  val searchLabel = new Label("Search target")
  val searchInput = new TextField(10)
  val searchButton = new Button("Search")
  val buttonPanel = new FlowPanel{
    contents += (readButton, displayButton, searchByComboBox,searchLabel, searchInput, searchButton)
  }

  val sorcerersCaveGUI = new MainFrame{
      title = "Sorcerers Cave"
      size = new Dimension(800, 600)
      visible = true
      contents = new BorderPanel{
        layout += scroller -> BorderPanel.Position.Center
        layout += buttonPanel -> BorderPanel.Position.North
      }
      //val buttonPanel
      listenTo(searchButton, displayButton, readButton)
      reactions += {
        case ButtonClicked(`readButton`) =>     readFile
        case ButtonClicked(`displayButton`) =>  displayCave()
        case ButtonClicked(`searchButton`) =>   search(searchInput.text.toLowerCase.trim)
      }
  }

  def readFile: Unit ={
    try {
      val chooser = new FileChooser()
      var in: Scanner = null
      if (chooser.showOpenDialog(null) == FileChooser.Result.Approve) {
        val selectedFile = chooser.selectedFile
        in = new Scanner(selectedFile)
        while (in.hasNext) {
          val line = in.nextLine()
          processLine(line)
        }
      }
    }catch{
      case ioe: IOException => Dialog.showMessage(null, "File Not Found.")
    }finally{
      informationText.append("Your File has been loaded!")
    }
  }

  def displayCave(): Unit ={
    informationText.append("Displaying Data From Game:\n")
    informationText.append(cave.toString+"")
    cave.parties.foreach{
      p : Party => informationText.append("  " + p.toString)
      p.partyMembers.foreach{
        c: Creature => informationText.append("   " + c.toString)
          c.loot.foreach{
            t : Treasure => informationText.append("      " + t.toString)
          }
          c.artifacts.foreach{
            a : Artifact => informationText.append("      " + a.toString)
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
  def search(input: String): Unit ={

  }

}
