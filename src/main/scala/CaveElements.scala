import scala.collection.mutable.ListBuffer
import scala.math.Ordered

/**
  * Created by rymurphy12 on 12/27/2015.
  * Description: Added file for the classes used by the game.
  */

abstract class GameElement {
  val index: Int
  val name: String
  val elementType: String
}
case class Cave(parties :ListBuffer[Party], excess :ListBuffer[GameElement])

case class Party(index :Int, name :String, elementType :String, partyMembers :ListBuffer[Creature]) extends GameElement{
  override def toString: String ={
    "\nParty:\n"+ "Name: " + name +"\n" + "Index: " + index +"\n"

  }
}
case class Creature(index :Int, elementType :String, name :String, memberOfPartyIndex :Int,
                    empathy :Int, fear :Int, carryingCapacity :Int, loot :ListBuffer[Treasure],
                    artifacts : ListBuffer[Artifact]) extends GameElement{
  override def toString(): String = {
      "\nCreature:\nName= " + name + "\nIndex= " + index +
      "\nType= " + elementType + "\nMember Of= " + memberOfPartyIndex + "\nEmpathy= " +
       empathy + "\nFear= " + fear +"\nCarrying Capacity = " +
       carryingCapacity+ "\n"

  }
}
case class Treasure(index :Int, name :String, elementType :String, ownedByIndex :Int, weight :Double, value :Int) extends GameElement{
  override def toString(): String = {
    "\nTreasure: \nName: " + name + "\nIndex: " + index +
      "\nOwned By= " + ownedByIndex + "\nweight= " + weight +
      "\nvalue= " + value + "\nType = " + elementType+"\n"
  }
}
case class Artifact(index :Int, name :String, elementType :String, ownedByIndex :Int) extends GameElement{
  override def toString(): String = {
    "\nArtifact:\nName: " + name +"\nOwned By=" + ownedByIndex + "\nType=" + elementType+"\nIndex: " + index +"\n"
  }
}


