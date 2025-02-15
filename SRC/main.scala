import scala.io.Source
import java.io.PrintWriter
import scala.util.control.Breaks.*
import scala.io.StdIn.readLine
import edu.stanford.nlp.simple.*
import scala.collection.mutable.ListBuffer
import java.util.Properties
import scala.jdk.CollectionConverters.*


//this function greet the user and checks if this is the first time and if not it restore his preferences
def greetUser(): List[String] = {
  //first print the greeting statement
  println("Hello! I am hatla2ee your car buying assistant.")
  //getting the users name
  val usersName=readLine("Please enter your name:").toLowerCase()
  //checking for previous activity and get preferences
  val preferences=getUserPreferences(usersName)
  //if preferences is empty create a new account else we will return the preferences
  preferences match {
    case Nil => val pref=userAccount(usersName)
      storeUserPreferences(pref)
      pref
    case _ => println(s"Welcome back $usersName")
       preferences
  }
}

//this function create an account for the user
//contains: name, favorite brand , favorite model , price range
def userAccount(name:String):List[String]={
  println(s"Hello $name let's create an account with your preferences in it.")
  println("Please enter your preferences:")
  //reading all the users preferences
  var carBrand=readLine("Enter your favorite car brand:").toLowerCase().filter(_!=' ')
  while(!isBrand(carBrand)) {carBrand=readLine("There maybe a spelling error please check it and re-enter:")}
  var carModel=readLine("Enter your favorite model:").toLowerCase().filter(_!=' ')
  while(!isModel(carModel)) {carModel=readLine("There maybe a spelling error please check it and re-enter:")}
  var color=readLine("Enter your favorite color:").toLowerCase().filter(_!=' ')
  while(!isColor(color)) {color=readLine("There maybe a spelling error please check it and re-enter:")}
  val Budget=readLine("Enter your price range:").toLowerCase().filter(_!=' ')
  //concatenating the preferences into one string
  val pref=List(name,carBrand,carModel,color,"$"+Budget)
  pref
}

//this function show the use some of the actions that he can do
def options(): String = {
  //asking the user if he wants to use the preferences to search or not
  //we split the answer into a list to be more easier to analyze
  val answer=readLine("Do you want me find you a car based on your preferences?\n").toLowerCase.split("\\s+").toList
  //matching the users answer to the suitable action
  answer match
    case ans if ans.contains("yes") && !ans.contains("no") =>"with preferences"
    case ans if ans.contains("no") && !ans.contains("yes")=>"without preferences"
    case _ => "can't understand"
}
//also some instructions to ensure he gets what he wants
def instructions():Unit={
  //first print some instructions
  println("Some instructions first:)" )
  println("1-if you want to enter a price write before it the dollar sign.[example'$800000']")
  println("2-if you want to find a car and its name consists of multiple words put a dash between them.[example 'g-36-amg']")
  println("3-you can search by 5 things a (car brand name), (car model), (car color), (car make year) or (a budget or price).")
  println("4-if you want enter a mileage right the number and then put dash next to it [example: 10000-km]")
  println("5-if you want to end your search just say [bye].")
}


def storeUserPreferences(preference: List[String]): Unit = {
  try {
    val writer = new PrintWriter(new java.io.FileWriter("userPreferences.txt", true))
    writer.println(preference.mkString(" "))
    writer.close()
  } catch {
    case e: Exception => println("Error: " + e.getMessage)
  }
}

def getUserPreferences(name:String): List[String] = {
  try {
    val source = Source.fromFile("userPreferences.txt")
    val lines = source.getLines().toList
    source.close()
    lines.find(_.startsWith(name)) match {
      case Some(line) => line.split("\\s+").toList
      case None => List.empty[String]
    }
  } catch {
    case e: Exception =>
      println("Error: " + e.getMessage)
      List()
  }
}

//set that contains all the cars brands
val brands: Set[String] = Set("kia", "chery", "fiat", "hyundai", "bmw", "chevrolet", "ford", "nissan",
  "mitsubishi", "opel", "toyota", "jac", "skoda", "mg", "daewoo", "brilliance", "changan", "mercedes",
  "jeep", "daihatsu", "lada", "citroën", "byd", "renault", "honda", "seat", "zotye", "suzuki", "volkswagen",
  "speranza", "senova", "peugeot", "isuzu", "geely", "saipa", "canghe", "audi", "jetour", "mazda", "proton",
  "dodge", "emgrand", "volvo", "lifan", "dfsk", "haval", "ssang yong", "porsche", "mini", "bentley", "mclaren",
  "faw", "subaru", "forthing", "maserati", "baic", "ds", "haima", "land rover", "chrysler", "cupra", "kenbo",
  "aston martin", "smart", "jaguar", "kaiyi", "abarth", "mahindra", "lancia", "dongfeng", "alfa romeo", "lincoln",
  "bestune", "gmc", "acura", "hawtai", "foton", "tesla", "tata", "karry", "soueast", "chana", "infiniti", "gaz",
  "hummer", "gac", "great wall", "zeekr", "jonway", "datsun", "lexus", "hafei", "sokon", "lotus", "keyton",
  "buick", "cadillac", "domy", "bugatti", "landwind","perodua")
//check if this token is a car Brand
def isBrand(input:String):Boolean={
  brands.contains(input.split("-").mkString(" "))
}

//set that contains all the car models
val models: Set[String] =Set("sportage", "arrizo 5", "tipo", "tucson turbo gdi", "x1", "optra", "verna", "focus", "tiida",
  "lancer puma", "grandland", "sunny", "uno", "spectra", "corolla", "j3", "kodiaq", "5", "lanos", "octavia a4",
  "nubira 2", "frv", "sephia", "benni", "astra", "tiggo 3", "e 250", "forte", "liberty", "grand terios", "1500",
  "c4 picasso", "f3", "logan", "accord", "cerato", "renegade", "e200", "zs", "c 200", "punto", "128", "ibiza",
  "lancer ex shark", "cherokee", "tiggo 7", "lacetti", "t600", "5008", "fluence", "avante", "ciaz", "kadjar",
  "golf", "a620", "500c", "s2", "c 180", "elantra ad", "a113", "a1", "js4", "frv cross", "301", "x 30",
  "octavia a8", "6", "405", "200", "lancer", "d max", "emgrand x7", "pride", "accent rb", "cruze", "fabia",
  "rio", "passat", "406", "maruti", "q35", "j7", "soul", "elantra hd", "n300", "elantra md", "a7", "getz",
  "duster", "excel", "lanos 2", "ecosport", "c5", "matrix", "elantra", "520", "superb", "emgrand 7", "doblo",
  "tiggo", "x70s", "tiguan", "octavia a7", "alto", "fantasia", "qashqai", "sandero step way", "accent",
  "grand cherokee", "3", "gla", "tucson", "glk 250","saga","cla 200","elantra cn7","250","insignia","rush",
  "carens","aveo","tiggo 8 pro","i10","leon","charger","urvan","swift","ix 35","yaris","vectra","civic","granta",
  "ec 7","k3","a516","s presso","eos","megane","j2","c elysee","508","atos","belta","cordoba","cool ray",
  "c5 aircross","xc60","amg gt","a 180","320","shahin","terios","niva","eagle 580","2105","2107","wrangler unlimited","sentra","xceed","grand cerato","emgrand","h6","mira","equinox","baleno","505","camry","tivoli xlv","cayenne","x6 m","440i","350","picanto","leganza","ceed","cooper","siena","corsa","3008","x7","flying spur","gt","cla amg","swift dzire","captur","golf 4","xc90","tiggo 8","beetle","arrizo 5 pro","grand i10","gla 180","x40","s4","galant","juliet","131","hilux","c 240","js3","x3","n200","captiva","envy","a3","jetta","ex 7","xv","glc 200","hs","308","toledo","bayon","nubira 1","panda","rcz","glc 300","t5 evo","c 200 amg","rx5","fortuner","x4","sirion","525","a4","ghibli","octavia","420","torres","xpander", "crossland", "500 x", "pandino", "e golf", "z4", "235", "cla 45 amg", "a 35 amg", "pointer", "tempra", "s30", "levante", "190", "xplosion", "jolion", "x6", "xtrail", "fsv", "2008", "land cruiser", "tucson gdi", "gx3 pro", "325", "hrv", "c 280", "s 500", "id 4", "cerato koup", "127", "van", "camaro", "h1", "elantra coupe", "c4 grand picasso", "x35", "b 200", "pajero", "galena", "kuga", "vitara", "530", "307", "spark", "regata", "ens1", "418", "eclipse", "tarraco", "504", "eclipse cross", "ateca", "116", "gentra", "318", "xsara", "juke", "316", "tiggo 8 pro e+", "2", "octavia a5", "accent hci", "c 300", "l3", "grand vitara", "avanza", "a15", "pickup", "polo", "330s", "id 6", "ram", "121", "arona", "a 200", "fiesta", "323", "sorento", "gran max", "wira", "trax", "e 230", "golf 2", "i20", "golf 7", "fox", "parati", "e 300", "tiggo 7 pro", "sonata", "ds7", "a5", "veloster", "s5", "thunderbird", "range rover vogue", "innova", "karoq", "neon", "cc", "familia", "s3", "matiz", "b 150", "c3", "impreza", "dart", "x70", "500", "minivan", "xd", "sandero", "v5", "v3", "m4", "persona", "s 550", "countryman s", "maybach s600", "m8", "m5", "vantage", "760", "durango", "felicia", "gen 2", "creta", "335", "c3 aircross", "408", "microbus", "929", "malibu", "town & country", "m11", "x70 plus", "ranger", "suran","q7", "s60", "v6", "clio", "city", "saipa", "ck2", "fortwo", "golf 6", "bravo", "e 200 amg", "e 180", "cla 180", "740", "i3", "e2000", "rx5 plus", "song plus", "ax", "kamiq", "yeti", "preve", "cayenne s", "carnival", "corolla cross", "celerio", "x-type", "x5", "waja", "518", "750", "t-series", "e 350", "2110", "n5", "alsvin", "commander", "c4", "ertiga", "macan", "t5", "n-series", "espero", "prado", "pick up", "330", "e 240", "a 160", "9", "x5 m", "280", "scenic", "scala", "206", "mirage", "asx", "s 450", "meriva", "ritmo", "solaris", "695", "hiace", "g class", "lancer crystala", "punto evo", "104", "sonic", "turan", "favorit", "creta grand", "25", "sebring", "1300", "escape", "fusion", "polonez", "crv", "touareg", "605", "dashing", "shuma", "coaster", "mokka", "q3", "scorpio", "dedra", "musso", "glc 200 amg imported", "racer", "range rover evoque", "ix1", "cs 15", "colt", "grecale", "118", "roomster", "clubman", "208", "132", "c 250", "seltos", "gls", "cross", "rapid", "a13", "felicia combi", "a30 shine", "wrangler", "106", "giulietta", "charmant", "h100", "ds5", "dyna", "eado plus", "v 60", "a11", "grand santa fe", "petra", "626", "x3 m", "corsair", "rainbow", "nubira", "523", "range rover sport", "pacifica", "fiorino", "linea", "taycan", "x4m", "glk 350", "b 180", "t55", "rodeo", "starlet", "jimmy","sx4", "ex7", "205", "grand c-max", "i30", "dogan", "pt cruiser", "tiburon", "superb combi", "3 series", "kicks", "124", "mini truck", "velar", "range rover", "c class", "300", "optima", "legacy", "veloz", "caprice", "607", "180", "q5", "discovery sport", "mdx", "galloper", "splendor", "escort", "patrol", "ls", "santamo", "amarok", "golf 3", "bora", "trial blazer", "corona", "maybach s500", "cascada", "620", "1200", "a25", "309", "tivoli", "360", "avensis", "m50", "230", "eqa", "e-tron", "voyager", "royal", "adam", "e 220", "c31", "livina", "model x", "marsh", "komodo", "305", "mini", "eagle pro", "cadenza", "18", "s-type", "228", "indigo", "730", "240", "jumpy", "c8", "kancil", "cs 35", "q22", "eado", "ideal", "dx8s coupe", "emgrand 5", "dream", "mcv", "carry", "a 10", "imperial", "new star", "xf", "view", "c-hr", "f0", "340", "407", "dx3", "john cooper", "flyer", "a 45 amg", "zx", "qx80", "tiba", "218 i", "b 160", "ix3", "ev6", "niro", "patrol safari", "golf 5", "cls class", "s7", "t77 pro", "gazelle", "h2", "lr2", "brava", "21", "a6", "e 53 amg", "aeolus a30", "laguna", "c2", "attrage", "emzoom", "navigator", "hover", "mondeo", "viva", "b70", "dx7", "glory", "e-2008", "s80", "z100", "emkoo", "1 series", "1", "charade", "element", "territory", "a380", "cs 85", "e", "wingle 5", "2017", "cooper roadster", "c30", "v7", "gle", "cielo", "t-roc", "stratus", "haima 1", "s 320", "pilot", "460", "308 sw", "cooper paceman", "cla class", "180b", "q8", "pegas", "integra", "materia", "creta su2", "7", "t5 evo", "es", "h530", "partner","eqe", "freelander", "lobo", "f3r", "golf 8", "explorer", "echo", "gen 3", "h3", "80", "exige", "b30", "frontera", "207", "ck", "125", "formentor", "omega", "berlingo", "acadia", "v101", "j15", "rx", "mini cooper s", "mk", "glory 330", "tiggo 2", "foison", "largus", "k01", "x90", "country man", "l300", "cressida", "m70", "cl class", "peri", "ds5", "avalanche", "rapid spaceback", "545", "cx 3", "skylark", "glc", "vita", "grandis", "k5", "tons2 / 2.5", "giulietta quadrifoglio verde", "datsun", "x95", "discovery", "maple", "seal", "xc 40", "a213", "glory van", "prius", "cooper hatch", "sprint", "bronco", "x2", "rumion", "sel 280", "caddy", "fx", "e class", "x pandino", "auris", "panamera s", "dx5", "dbx", "samara", "220", "86", "escalade", "2111", "xa", "528", "altima", "journey", "m3", "kalina", "k900", "s 300", "click", "odyssey", "lx 570", "compass", "han", "fruits", "haima 3", "florida", "gle 350", "z", "solara", "gratour", "t700", "eqs", "146", "defender", "cs 55", "xe", "tiggo 4", "mg 4", "terrain", "chery qq", "okavango", "fronx", "v60", "rapide s", "ga3", "q2", "t300", "town", "s 350", "x25", "a8", "boxster s", "legend", "taunus", "naughty", "florid", "2106", "x3 pro", "c4x", "rav 4", "yrv", "eagle", "ka", "arrizo 3", "560", "maxima", "y140", "1007", "tercel", "tt", "m12", "model y", "207 sw", "veyron", "altea", "500 e", "wagoneer", "palio", "t500", "gs", "940", "zaz", "bluebird", "pathfinder", "apv", "teramont", "murano", "argenta", "qubo", "clk", "foreman", "qin pro", "tons 5.5", "transporter", "e 320", "j5", "aurion", "model 3", "visto", "m850", "family", "mrv", "symbol", "zs ev", "marea", "applause", "duster nova", "envoy", "g3", "pulsar", "244", "19", "oka", "blazer", "glk 300", "dargo", "multivan", "besturn", "windstar", "leaf", "convertible", "sparky", "genesis", "viano", "englon", "cs 35 plus", "spark ev", "156", "transit", "ds3", "grand c4 spacetourer", "mini van", "133", "terracan", "rv 1", "veryca", "c 230", "caravan", "g 36 amg", "mz 40", "s40", "232", "s90", "a 140", "taurus", "jimny", "1100", "tiggo 7 pro max", "c3 picasso", "silverado", "lodgy", "11","stelvio")
//check if this token is a car model
def isModel(input:String):Boolean={
  models.contains(input.split("-").mkString(" "))
}

//set that contains all the car models
val colors:Set[String]=Set("dark grey", "bronze", "petroleum", "black", "gray", "blue", "silver", "brown", "red",
  "white", "dark red", "dark blue", "mocha", "gold", "green", "eggplant", "cyan", "champagne", "yellow",
  "dark green", "olive", "purple", "beige","orange")
//check if this token is a car color
def isColor(input:String):Boolean={
  colors.contains(input)
}
//this function takes the input as a string and return a list of tokens in a certain order
def parseInput(input: String): List[String]={
  //initializing the tokens as a string variables
  var brand=""
  var model=""
  var date=""
  var price=""
  var color=""
  var mileage=""
  //creating an object from the library stanford nlp to analyze the input
  val sentence = new Sentence(input)
  //creating a list of words
  val tokens = sentence.words()
  //creating a list of nerTags for each corresponding word
  val nerTags = sentence.nerTags()
  //matching every word with its nerTag for extracting each token from the input
  tokens.asScala.zip(nerTags.asScala).foreach({
    case (token, nerTag) if nerTag == "DATE" => date = token
    case (token, nerTag) if nerTag == "MONEY" => price = token
    case (token, nerTag) if isModel(token) => model = token
    case (token, nerTag) if isBrand(token) => brand = token
    case (token, nerTag) if isColor(token) => color = token
    case (token, nerTag) if token.contains("-") => mileage = token.split("-").mkString("").replace("km", "")
    case (token, nerTag)=>//
  })
  //checking for any combinations
  val keywords = input.split(" ").toList
  for (item <- keywords) {
    if (item.contains("-")) {
      if(isModel(item.replace("-"," "))) model=item
      else mileage = item.replace("-km", "")
    }
  }
  //creating a list that contains all the tokens and returning it
  val Tokens:List[String]=List(brand,model,color,price,date,mileage)
  Tokens
}

var conversationTokens=List("","","","","","")
def updateList(tokens:List[String]):List[String]={
  if(tokens.apply(0)!="") {
    conversationTokens = tokens
    conversationTokens
  }else if (tokens.apply(1)!=""){
    val newElements = tokens.takeRight(5)
    val list=conversationTokens.patch(1, newElements, 5) // Replace 4 elements starting at startIndex
    conversationTokens=list
    list
  }else {
    val newElements=tokens.takeRight(4)
    val list=conversationTokens.patch(2, newElements, 4) // Replace 4 elements starting at startIndex
    conversationTokens=list
    list
  }

}

//this function is the brain of the chatbot
//it takes the users input as a string and do some descions and returns the response
def handleUserInput(track:String)(pref:List[String])(input:String):String={
  track match {
    case "with preferences" =>
      println("Hold on tight it might take several minutes!.")
      //generate response
      val tokens = parseInput(pref.mkString(" "))
      conversationTokens=tokens
      generateResponse(tokens)
    //return the response
    case "without preferences" =>
      val usersInput = readLine("what do you want me to find:").toLowerCase()
      println("Hold on tight it might take several minutes!.")
      val listOfTokens = parseInput(usersInput)
      listOfTokens match {
        case List("", "", "", "", "","") =>
          "I am sorry,i do not understand.\nI need a car to search for."
        case _ =>
          //generate the response using
          conversationTokens=listOfTokens
          generateResponse(listOfTokens)
        //return response
      }
    case "can't understand" =>
      val choice = options()
      val statment = readLine()
      handleUserInput(choice)(pref)(statment)
    case "next" =>
      val listOfTokens = parseInput(input)
      listOfTokens match {
        case List("", "", "", "", "","") =>
          "I am sorry, i do not understand."
        case list =>
          //generate the response using
          generateResponse(updateList(listOfTokens))
      }
  }
}

//this is the function that takes the tokens and returns the data
def generateResponse(tokens:List[String]):String={
  //"works"
  CSVReader.getResult(tokens).getOrElse("Default value")
}
@main def run(): Unit = {
  val usersPreferences = greetUser()
  instructions()
  val choice = options()
  val response = handleUserInput(choice)(usersPreferences)("")
  println(response)
  var status = true
  val handleFunction = handleUserInput("next")(usersPreferences)
  while (status) {
    val usersStatement = readLine("What do you want me to find next:").toLowerCase()
    if (usersStatement.contentEquals("bye")) {
      status = false
    } else {
      val answer = handleFunction(usersStatement)
      println(answer)
    }
  }
}

