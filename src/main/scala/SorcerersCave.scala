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
 *        spacing of Scala and need to read up on the proper spacing. There are probably several other issues that
 *        I need to fix that I am unaware of.
  *
  *      12/27/2015 - Migrated from Swing to ScalaFx
  *      12/31/2015 - Implemented Search Functionality and fixed recurring issues adding game elements
  *      01/02/2015 - Adding GUI to begin implementation of sorting by various fields
 */


object SorcerersCave extends JFXApp{

  val cave = new Cave(ListBuffer(), ListBuffer())
  val informationText: TextArea = new TextArea()

  val sortByCreatureEmpathy = new CheckBox("Creature Empathy")
  val sortByCreatureCarryingCapacity = new CheckBox("Creature Carrying Capacity")
  val sortByCreatureFear = new CheckBox("Creature Fear")
  val sortByTreasureValue = new CheckBox("Treasure Value")
  val sortByTreasureWeight = new CheckBox("Treasure Weight")

  stage = new PrimaryStage {
      title = "Sorcerers Cave"
      val searchBy = new ComboBox[String](List("Index", "Name", "Type")){
        value = "Index"
      }
      val resizeableBox = new HBox(informationText){
        hgrow = Priority.Always
      }
      val searchInput = new TextField() {prefWidth = 100}
      val buttonPane = new FlowPane {
        hgap = 5
        children = Seq(new Button("Read") { onAction = handle { readFile }} ,
                       new Button("Display"){ onAction = handle { displayCave }},
                       searchBy,
                       new Label("Search target:"),
                       searchInput,
                       new Button("Search"){ onAction = handle { search(searchInput.getText.toLowerCase.trim, searchBy.value.value) }},
                       new Label("Sort By: "),
                       sortByCreatureEmpathy,
                       sortByCreatureCarryingCapacity,
                       sortByCreatureFear,
                       sortByTreasureValue,
                       sortByTreasureWeight
                      )
      }
      scene = new Scene {
        content = new BorderPane {
             top = buttonPane
             center = resizeableBox
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
      informationText.appendText("Your File has been loaded!\n")
    }
  }

  def displayCave: Unit ={
    informationText.appendText("Displaying Data From Game:\n")
    cave.parties foreach{
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
    informationText.appendText("Displaying Game Elements that are unassigned.\n")
    cave.excess foreach{e : GameElement => informationText.appendText("  " + e.toString)}
  }

  def processLine(line: String): Unit ={
    val dataList = line.split(":").map(_.trim).toList
    dataList.head match{
      case "p"  =>   cave.parties.append(new Party(dataList(1).toInt, dataList(2), "Party", ListBuffer()))
      case "c" =>    val aCreature = new Creature(dataList(1).toInt, dataList(2), dataList(3), dataList(4).toInt,
                                                  dataList(5).toInt, dataList(6).toInt, dataList(7).toInt,
                                                  ListBuffer(), ListBuffer())
                     addCreatureToParty(aCreature)

      case "t" =>  val aTreasure = new Treasure(dataList(1).toInt, dataList(2), "Treasure", dataList(3).toInt,
                                                dataList(4).toDouble, dataList(5).toInt)
                   addTreasureToCreature(aTreasure)

      case "a"  =>  val name = if(dataList.length < 5) "Unnamed" else dataList(4)
                   val anArtifact = new Artifact(dataList(1).toInt, name, dataList(2), dataList(3).toInt)
                   addArtifactToCreature(anArtifact)

      case _ =>  return
    }
  }

  def addCreatureToParty(creatureToAdd: Creature): Unit ={
    var unaffiliated = true
    cave.parties foreach{p :Party => creatureToAdd.memberOfPartyIndex match {
      case p.index =>  p.partyMembers.append(creatureToAdd); unaffiliated = false
      case _ =>  ;
    }}
    if (unaffiliated)
      cave.excess.append(creatureToAdd)
  }

  def addTreasureToCreature(aTreasure: Treasure): Unit = {
    var unaffiliated = true
    cave.parties foreach{p: Party => p.partyMembers foreach {c: Creature => aTreasure.ownedByIndex match {
      case c.index =>  c.loot.append(aTreasure); unaffiliated = false
      case _ =>  ;
    }}}
    if (unaffiliated)
      cave.excess.append(aTreasure)
  }

  def addArtifactToCreature(anArtifact: Artifact): Unit = {
    var unaffiliated = true
    cave.parties foreach{p: Party => p.partyMembers foreach {c: Creature => anArtifact.ownedByIndex match {
      case c.index =>  c.artifacts.append(anArtifact); unaffiliated = false
      case _ =>  ;
    }}}
    if (unaffiliated)
      cave.excess.append(anArtifact)
  }

  def search(input: String, searchCategory: String): Unit ={
    informationText.clear()
    val searchResults = new ListBuffer[GameElement]()
    for(party <- cave.parties){
      if((searchCategory == "Index" && party.index.toString == input) || (searchCategory == "Name"
        && party.name.toLowerCase.trim == input) || (searchCategory == "Type"
        && party.elementType.toLowerCase.trim == input))
        searchResults.append(party)
      for(creature <- party.partyMembers){
        if((searchCategory == "Index" && creature.index.toString == input) || (searchCategory == "Name"
          && creature.name.toLowerCase.trim == input) || (searchCategory == "Type"
          && creature.elementType.toLowerCase.trim == input))
          searchResults.append(creature)
        for(treasure <- creature.loot)
          if((searchCategory == "Index" && treasure.index.toString == input) || (searchCategory == "Name"
            && treasure.name.toLowerCase.trim == input) || (searchCategory == "Type"
            && treasure.elementType.toLowerCase.trim == input))
            searchResults.append(treasure)
        for(artifact <- creature.artifacts)
          if((searchCategory == "Index" && artifact.index.toString == input) || (searchCategory == "Name"
            && artifact.name.toLowerCase.trim == input) || (searchCategory == "Type"
            && artifact.elementType.toLowerCase.trim == input))
            searchResults.append(artifact)
      }
    }
    for(element <- cave.excess){
      if((searchCategory == "Index" && element.index.toString == input) || (searchCategory == "Name"
        && element.name.toLowerCase.trim == input) || (searchCategory == "Type"
        && element.elementType.toLowerCase.trim == input))
        searchResults.append(element)
    }
    if(searchResults.length > 0){
      informationText.appendText("We have found what you are looking for!\n")
      for(result <- searchResults)
        informationText.appendText(result.toString)
    }else
      informationText.appendText("We couldn't find what you are looking for, sorry!\n")
  }
}