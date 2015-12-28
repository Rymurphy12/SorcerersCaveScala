import java.io.{File, IOException}
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
  *
  *      12/27/2015 - Migrated from Swing to ScalaFx
 */


object SorcerersCave extends JFXApp{

  val cave = new Cave(ListBuffer(), ListBuffer())
  val informationText: TextArea = new TextArea()

  stage = new PrimaryStage {
      title = "Sorcerers Cave"
      val searchBy = new ComboBox[String](List("Index", "Name", "Type"))
      val searchInput = new TextField() {prefWidth = 100}
      val buttonPane = new FlowPane {
        hgap = 5
        children = Seq(new Button("Read") { onAction = handle { readFile }} ,
                       new Button("Display"){ onAction = handle { displayCave() }},
                       searchBy,
                       new Label("Search target:"),
                       searchInput,
                       new Button("Search"){ onAction = handle { search(searchInput.getText.toLowerCase.trim, searchBy.toString()) }}
        )
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
      val userDirectoryString = System.getProperty("user.home")
      val userDirectory = new File(userDirectoryString)
      val chooser = new FileChooser(){
        initialDirectory = userDirectory
      }
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
    //informationText.appendText(cave.toString+"")
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
    dataList.head match{
      case "p"  =>   cave.parties.append(new Party(dataList(1).toInt, dataList(2), "Party", ListBuffer()))
      case "c"  =>   val aCreature = new Creature(dataList(1).toInt, dataList(2), dataList(3), dataList(4).toInt,
                                                  dataList(5).toInt, dataList(6).toInt, dataList(7).toInt,
                                                  ListBuffer(), ListBuffer())
                     addCreatureToParty(aCreature)

      case "t" =>  val aTreasure = new Treasure(dataList(1).toInt, dataList(2), "Treasure", dataList(3).toInt,
                                                dataList(4).toDouble, dataList(5).toInt)
                   addTreasureToCreature(aTreasure)

      case "a" =>  val name = if(dataList.length < 5) "Unnamed" else dataList(4)
                   val anArtifact = new Artifact(dataList(1).toInt, name, dataList(2), dataList(3).toInt)
                   addArtifactToCreature(anArtifact)

      case _ =>  return
    }
  }

  def addCreatureToParty(creatureToAdd: Creature): Unit ={
    var unaffiliated = true
    cave.parties foreach{p :Party => creatureToAdd.memberOfPartyIndex match {
      case p.index =>  p.partyMembers.append(creatureToAdd)
                       unaffiliated = false
      case _ =>  return
    }
      if (unaffiliated)
        cave.excess.append(creatureToAdd)
    }
  }

  def addTreasureToCreature(aTreasure: Treasure): Unit = {
    var unaffiliated = true
    cave.parties foreach{p: Party => p.partyMembers foreach {c: Creature => aTreasure.ownedByIndex match {
      case c.index =>  c.loot.append(aTreasure)
                       unaffiliated = false
      case _ =>  return
    }
      if (unaffiliated)
        cave.excess.append(aTreasure)
    }
    }
  }

  def addArtifactToCreature(anArtifact: Artifact): Unit = {
    var unaffiliated = true
    cave.parties foreach{p: Party => p.partyMembers foreach {c: Creature => anArtifact.ownedByIndex match {
      case c.index =>  c.artifacts.append(anArtifact)
                       unaffiliated = false
      case _ => return
    }
      if (unaffiliated)
        cave.excess.append(anArtifact)
    }
    }
  }

  def search(input: String, searchCategory: String): Unit ={

  }

}
