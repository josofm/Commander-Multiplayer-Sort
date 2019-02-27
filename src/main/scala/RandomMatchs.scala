import java.io.{File, PrintWriter}

import scala.util.Random

object RandomMatchs extends App {

  val random = new Random
  val commanders = io.Source.fromFile("../CommanderMultiplayerSort/src/commanders.txt").getLines().toList
  val games = new PrintWriter(new File("../CommanderMultiplayerSort/src/games.txt"))
  var matchCount = 1

  def getMatchs(commanders: List[String]): Map[String, List[String]] = {
    def getMatchsHelper(matchMap: Map[String, List[String]], commanderList: List[String]): Map[String, List[String]] = {
      if (commanderList.isEmpty) matchMap
      else {
        val newMap = matchMap + (f"Jogo_$matchCount" -> getMatch(commanderList))
        matchCount+=1
        getMatchsHelper(newMap, commanderList.filterNot(name => newMap.exists(_._2.contains(name))))
      }
    }
    getMatchsHelper(Map[String, List[String]](), commanders)
  }


  def getMatch(commanders: List[String]): List[String] = {
    def getMatchHelper(commanders: List[String], _match: List[String], random: Random) : List[String] = {
      if (_match.length == 4) _match
      else {
        val sortedCommander = getRandomElement(commanders.filterNot(_match.contains(_)), random)
        getMatchHelper(commanders.filterNot(_match.contains(_)).filter(c => c != sortedCommander), _match :+ sortedCommander, random)
      }
    }
    getMatchHelper(commanders, Nil, random)

  }


  def getMatchRounds(commanders: List[String]): Map[String, List[String]] = {
    def getMatchRoundsHelper(rounds: Int, commanders: List[String], roundsSorted: Map[String, List[String]]):  Map[String, List[String]] = {
      if (rounds == 3) roundsSorted
      else if (rounds == 0) getMatchRoundsHelper(rounds+1, commanders, getMatchs(commanders))
      else {
        val _matchs = getMatchs(commanders)
        if (repeatedMaps(_matchs, roundsSorted)) getMatchRoundsHelper(rounds+1, commanders, roundsSorted ++ _matchs)
        else getMatchRoundsHelper(rounds, commanders, roundsSorted)

      }
    }
    getMatchRoundsHelper(0, commanders, Map[String, List[String]]())
  }

  def repeatedMaps(m1: Map[String, List[String]], m2: Map[String, List[String]]): Boolean = {
    for((_, l1) <- m1)
      for((_,l2) <- m2)
        if(l1.union(l2).length > 6) false
    true
  }

  def getRandomElement(list: Seq[String], random: Random): String = list(random.nextInt(list.length))


//  for ((k, v) <- getMatchRounds(scala.util.Random.shuffle(commanders)))  println(s" $k: $v")
  for ((k, v) <- getMatchRounds(scala.util.Random.shuffle(commanders)))  games.write(s"$k: %s\n".format(v.mkString(", ")))
  games.close()


}
