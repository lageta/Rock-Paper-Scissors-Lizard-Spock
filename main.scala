sealed  trait  Choice  { 
   protected  def  beats:  List[Choice]  
 
   def  wins(other:  Choice):  Boolean  =
     beats.contains(other)  
}

/**
   *  Rock  crushes  Lizard
   *  Rock  crushes  Scissors.
   */
case  object  Rock  extends  Choice  {  //
   protected  val  beats  =  List(Lizard,  Scissors)
}
 
/**
   *  Paper  covers  Rock
   *  Paper  disproves  Spock
   */
case  object  Paper  extends  Choice  {  //
   protected  val  beats  =  List(Rock,  Spock)
}
 
/**
   *  Scissors  cuts  Paper
   *  Scissors  decapitates  Lizard
   */
case  object  Scissors  extends  Choice  {  //
   protected  val  beats  =  List(Paper,  Lizard)
}
 
/**
   *  Lizard  poisons  Spock
   *  Lizard  eats  Paper
   */
case  object  Lizard  extends  Choice  {  //
   protected  val  beats  =  List(Spock,  Paper)
}
 
/**
   *  Spock  smashes  Scissors
   *  Spock  vaporizes  Rock
   */
case  object  Spock  extends  Choice  {  //
   protected  val  beats  =  List(Scissors,  Rock)
}

case class Game(p1:Int, p2:Int){
  val sum = this.p1+this.p2
}

object Main {

  var s = Game(0,0)
  
  def main(args: Array[String]): Unit = {
    startGame()
   
  }

  def startGame() : Unit = {
    print("=== Starting Game ===\n")
    s = Game(0,0)
    gameLoop()
  }

  def showScore() : Unit = {
    print("\nplayer1 win " + s.p1 + " games  \nplayer2 win " + s.p2 +"\nin "+s.sum+" games\n" )
  
  }

  def gameOver():Unit = {
    print("=== GAME OVER ===\n\n")
    showScore()
  }

  def error():Unit = {
    print("=== ERROR ===\n\n")
  }

  def inputCommand() : String = {
    print("\n-(r)ock, \n-(p)aper, \n-(sc)issors, \n-(l)izard, \n-(sp)ock \nor (q)uit: _ (n)ew_game \n")
    var input = scala.io.StdIn.readLine()
    
    return input
  }

    def  fromString(text:  String):  Choice  =
       text.trim.toLowerCase  match  {
           case  "r"  =>  Rock
           case  "p"  =>  Paper
           case  "sc"  =>  Scissors
           case  "l"  =>  Lizard
           case  "sp"  =>  Spock
           case  unknown  =>
             val  errorMsg  =  s"Unknown  symbol  $unknown.  "  +
               "Please  pick  a  valid  symbol  [Rock,  Paper,  Scissors,  Lizard,  Spock]\n"
             throw  new  IllegalArgumentException(errorMsg)
       }

  
  def gameLoop():Unit={
    val round = s.sum + 1
    print("\n ----------"+"Round "+round+" ------------- \n ")
    var p1=inputCommand()
   

    p1 match {
      case "n" | "q" => {
        gameOver()
        if (p1 == "n") startGame()
      } 

      case _ => {
        var p2 = inputCommand()
        p2 match {
          case "n" | "q" => {
          gameOver()
        if (p2 == "n") startGame()
      } 
          case _ => {
                      try {
             var p1_choice = fromString(p1)
             var p2_choice=fromString(p2)
             p1_choice.wins(p2_choice) match {
               case true  => {
                   print("\n=== Player 1 Win  ===\n")
                   s = Game(s.p1+1, s.p2) 
               }
               case false => {
                 p2_choice.wins(p1_choice) match {
                    case true => {
                    print("\n=== Player 2 Win  ===\n")
                    s = Game(s.p1, s.p2+1) 
                 } 
                   case false => {
                      print("\n=== Player 2 Tie  ===\n")
                   }
               } 
               }
           
             }
            showScore()
            
          } catch {
              case _ => {
                error()
                print(_)
              }
          }
          gameLoop()
            
          }
        }

        
      }
    }
  }
  
}
