object CommandAnalyzer {

  case class GameObj(desc: String, kind: String)

  val grammar: List[String] = List(
    "look",
    "inventory",
    "go {direction}",
    "climb {object}",
    "take {object}",
    "drop {object}",
    "examine {object}",
    "search {object}",
    "sit on {object}",
    "lie on {object}",
    "open {container}",
    "close {container}",
    "lock {container}",
    "unlock {container}",
    "put {item} in {container}",
    "put {item} on {supporter}",
    "wear {clothing}",
    "take off {clothing}",
    "tie {item} to {object}",
    "talk to {person}")

  val world: List[GameObj] = List(
    GameObj("north", "direction"),
    GameObj("south", "direction"),
    GameObj("east", "direction"),
    GameObj("west", "direction"),
    GameObj("comfy chair", "object"),
    GameObj("shabby twin bed", "object"),
    GameObj("elegant carpet", "object"),
    GameObj("soccer ball", "item"),
    GameObj("beach ball", "item"),
    GameObj("small green frog", "item"),
    GameObj("small tree frog", "item"),
    GameObj("large wooden box", "container"),
    GameObj("flimsy cardboard box", "container"),
    GameObj("solid wooden table", "supporter"),
    GameObj("glass side stand", "supporter"),
    GameObj("purple hoodie", "clothing"),
    GameObj("leather jacket", "clothing"),
    GameObj("very old man", "person"),
    GameObj("very young woman", "person"))

  // ==========================================================
  // helper functions
  // ==========================================================

  // turn a string of words separated by spaces into a list of those words
  def getWordList(words: String): List[String] =
    words.split(" ").toList

  // ==========================================================
  // functions on world
  // ==========================================================

  // Note: All lists of string that are returned from the following methods should be distinct (no duplicates) and they
  // should be in sorted order.

  // returns a list of all the adjectives (all but last words in desc) in world
  // example: cardboard, flimsy, very [ but not 'box' ]
  def getAdjectives: List[String] =
    world.flatMap(g => getWordList(g.desc).init).distinct.sorted

  // returns a list of nouns (last words in desc) from world
  // example: north, bed, man
  def getNouns: List[String] =
    world.flatMap(g => List(getWordList(g.desc).last)).distinct.sorted

  // returns a list of game-object kinds from world
  // example: direction, item, object
  def getGameObjectKinds: List[String] =
    world.flatMap(g => getWordList(g.kind)).distinct.sorted

  // returns a list of game-objects associated with a particular noun (no need to be sorted)
  // example: getGameObjects("frog") =>
  //   List(GameObj("small green frog", "item"), GameObj("small tree frog", "item"))
  def getGameObjects(noun: String): List[GameObj] =
    world.filter(g => g.desc.contains(noun))

  // ==========================================================
  // functions on grammar
  // ==========================================================

  // Note: All lists of string that are returned from the following methods should be distinct (no duplicates) and they
  // should be in sorted order.

  // returns a list of all verbs (first words) in grammar
  // example: look, examine, put
  def getVerbs: List[String] =
    grammar.flatMap(g => List(getWordList(g).head)).distinct.sorted

  // returns a list of all prepositions in grammar
  // prepositions are words in grammar strings that are not verbs and not in curly braces
  // example: in, on, to
  def getPrepositions: List[String] =
    grammar.flatMap(g => getWordList(g).drop(1).filterNot(g => g.contains("{"))).distinct.sorted

  // returns a list of all actions from grammar
  // actions are formed by combining the verb with its preposition (if any) with an underscore between them
  // example: look, examine, sit_on, put_in
  def getActions: List[String] =
    grammar.flatMap(g =>
      if getWordList(g).filterNot(g => g.contains("{")).length == 2
      then List(getWordList(g).filterNot(g => g.contains("{")).mkString("_"))
      else getWordList(g).filterNot(g => g.contains("{"))
    ).distinct.sorted

  // returns a list of game-object kinds from grammar
  // kinds are all the words in curly braces
  // example: direction, item, object
  def getGrammarObjectKinds: List[String] =
    grammar.flatMap(g => getWordList(g).filter(g => g.contains("{")).map(g => g.tail.init)).distinct.sorted

  // given a verb, returns a list of all grammar strings associated with it
  // example: getGrammarStrings("put") =>
  //   List("put {item} in {object}", "put {item} on {object}")
  def getGrammarStrings(verb: String): List[String] =
    grammar.filter(g => g.contains(verb))

  // ==========================================================
  // getVocab function
  // ==========================================================

  // returns a list of all known words (from grammar and world)
  // note: does not include words with curly braces
  // should include object, direction, etc., but not {object}, {direction}, etc.
  def getVocab: List[String] =
    (world.flatMap(g => getWordList(g.desc)) :::
      world.flatMap(g => getWordList(g.kind)) :::
      grammar.flatMap(g => getWordList(g).filterNot(g => g.contains("{"))))
        .distinct.sorted

  // ==========================================================
  // command related function
  // ==========================================================

  // return true if the words contain a preposition
  def hasPrep(words: String): Boolean =
    if getPrepositions.contains(words)
    then true
    else false

  // return true if the words match the specified game object for a string of words to match a game object, the last
  // word in the string must be identical to the description noun of the game object.
  // The rest of the words in the string must be words contained in the description adjectives of the game object.
  // example: wordsMatchGameObj("tree tree frog", GameObj("small tree frog", "item")) ==> true
  // example: wordsMatchGameObj("small tree", GameObj("small tree frog", "item")) ==> false
  // example: wordsMatchGameObj("small ball", GameObj("beach ball", "item")) ==> false
  def wordsMatchGameObj(words: String, gameObj: GameObj): Boolean =
    if getWordList(words).last.equals(getWordList(gameObj.desc).last)
    then if getWordList(words).forall(w => getWordList(gameObj.desc).contains(w))
    then true
    else false
    else false


  // return true if the words (cmdWords) match the specified grammar (pattern).
  // Take "put {item} in {container}" as an example pattern. The first word in cmdWords must be "put". The following
  // word(s) must match the desc of an "item" kind GameObj based on wordsMatchGameObj function. The next word must be
  // "in". The ending word(s) must match the desc of a "container" kind GameObj based on wordsMatchGameObj function
  def wordsMatchPattern(cmdWords: String, pattern: String): Boolean =
    val wList = getWordList(cmdWords)
    val pList = getWordList(pattern)

    pList.foldLeft(Option(wList)) {  //go through each word in pattern
      case (Some(words), p) if p.startsWith("{") && p.endsWith("}") =>  //if action then compare actions
        val kind = p.slice(1, p.length - 1)
        (1 to words.length).collectFirst {
          case i if world.exists(o =>   //try to match words to fit corresponding action
            o.kind == kind && wordsMatchGameObj(words.take(i).mkString(" "), o)
          ) => words.drop(i)
        }
      case (Some(w :: ws), p) if w == p => Some(ws) //if verb or preposition then compare
      case _ => None //else nothing
    }.contains(Nil)   //after comparing all the list will be empty
}