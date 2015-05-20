package classifier


import java.io.{FileWriter, File}

import edu.stanford.nlp.dcoref.Dictionaries
import org.deeplearning4j.word2vec.Word2Vec
import resolver.parser.document.{PronounDictionary, lexicalCounter, FeatureSet, Document}

class FeatureExtractor(docs:Seq[Document], embedding:Word2Vec){
var change:Double=0
var count:Double =0
  val (featIndMap, featCount)=createFeatureIndex(docs)
  var cachedFeatures =Map[(String,Int,Int),Seq[Int]]()
  def extractFeatures(doc: Document, mention: Int, antecedent: Int): Seq[Int] = {
    cachedFeatures.get((doc.id,mention,antecedent)) match{
      case Some(featList) => {
        featList

      }
      case None => {
        val currentMention = doc.features (mention)
        val antMention = doc.features (antecedent)
        val featNames = buildFeatSet (currentMention, antMention)
        val featInd = featNames.map (name => featIndMap.getOrElse (name, {/*println(name);*/ - 1}) )
       cachedFeatures = cachedFeatures +((doc.id,mention,antecedent)-> featInd)
        featInd
      }
    }
  //  println("feature nonzero count: "+(0/:c){(a,b)=>if (b>0) a+1 else a})
  //   c
  }

  def dumpFeatures()={
    val f = new File("./features.txt")
    val write = new FileWriter(f)
    for(pair <- featIndMap)
      write.write(pair.toString()+"\n")
  }

  def featureVal(a: Boolean): Int = if (a) 1 else 0

  //needed feature
  //  type of mention
  //  type of antecedent mention (if link)
  def createFeatureIndex(docs: Seq[Document]): (scala.collection.mutable.Map[String, Int],Int) = {
    val features = scala.collection.mutable.Map[String, Int]()
    var ftInd = 0

    def addToMap(name: String): Unit = {
      if (!features.contains(name)) {
        features += name -> ftInd
        ftInd += 1
      }
    }

    for (doc: Document <- docs) {
      println("Begin feature extract: "+ doc.id)
      for (mention: FeatureSet <- doc.features) {
        for (antecedent: FeatureSet <- doc.features.slice(0, mention.refID+1)) {
         for(featName <- buildFeatSet(mention,antecedent)) {
           addToMap(featName)
         }
        }
      }
    }
    println(features.size+" reported size= "+ftInd)
    (features,ftInd)
  }

  def buildFeatSet(mention: FeatureSet, antecedent: FeatureSet): List[String] = {
    def parseMentionType(feat: FeatureSet): String = {
      if (feat.mentionType == "PRP")
        try {
          PronounDictionary.getCanonicalPronLc(feat.completeString)
        } catch {
          case e: Exception => {
            println(feat.completeString)
            "PRP"
          }
        }
      else
        feat.mentionType
    }
    def buildName(name: String, ment: FeatureSet, ant: FeatureSet): String = {
      val anaphor = ment.refID != ant.refID

      val n = name + "mentT=" + parseMentionType(ment)
      if (anaphor) n + "antT=" + parseMentionType(ant) else n + ""
    }

    def nameOrSpeech(s: String, pos: String): String = {
      if (lexicalCounter.wordCountMap.getOrElse(s, 0) > 20) s
      else pos
    }

    val strings = scala.collection.mutable.MutableList[String]()
    val anaphor = mention.refID != antecedent.refID
    val length = mention.completeString.split(" ").length
    strings += buildName("anaph=" + anaphor + "hw=" + mention.semanticHead, mention, antecedent)
    if (!(length == 1)) {
      strings += buildName("anaph=" + anaphor + "firstWord=" + nameOrSpeech(mention.firstWord, mention.partsOfSpeech.first), mention, antecedent)
      strings += buildName("anaph=" + anaphor + "lastWord=" + nameOrSpeech(mention.lastWord, mention.partsOfSpeech.last), mention, antecedent)
    }
    strings += buildName("anaph=" + anaphor + "precedingWord=" + nameOrSpeech(mention.previousWord, mention.partsOfSpeech.prev), mention, antecedent)
    strings += buildName("anaph=" + anaphor + "followingword=" + nameOrSpeech(mention.nextWord, mention.partsOfSpeech.next), mention, antecedent)
    strings += buildName("anaph=" + anaphor + "mentLength=" + length, mention, antecedent)

    //features on antecedent(ONLY if a[i] != new
    if (anaphor) {
      val antLength = antecedent.completeString.split(" ").length
      strings += buildName("anthw=" + antecedent.semanticHead, mention, antecedent)
      if (!(antLength == 1)) {
        strings += buildName("antfirstWord=" + nameOrSpeech(antecedent.firstWord, antecedent.partsOfSpeech.first), mention, antecedent)
        strings += buildName("antlastWord=" + nameOrSpeech(antecedent.lastWord, antecedent.partsOfSpeech.last), mention, antecedent)
      }
      strings += buildName("antprecedingWord=" + nameOrSpeech(antecedent.previousWord, antecedent.partsOfSpeech.prev), mention, antecedent)
      strings += buildName("antfollowingword=" + nameOrSpeech(antecedent.nextWord, antecedent.partsOfSpeech.next), mention, antecedent)
      strings += buildName("antLength=" + antLength, mention, antecedent)

      //features on pair
      strings += buildName("exactStringMatch=" + (antecedent.completeString == mention.completeString), mention, antecedent)
      strings += buildName("headWordMatch=" + (antecedent.semanticHead == mention.semanticHead), mention, antecedent)
      strings += buildName("sentenceDistance=" + Math.min(mention.sentenceNum - antecedent.sentenceNum, 10), mention, antecedent)
      strings += buildName("mentionDistance=" + Math.min(mention.refID - antecedent.refID, 10), mention, antecedent)
      //      if(embedding.similarity(mention.semanticHead, antecedent.semanticHead)<.05) {
      //
      strings += buildName("cosineSimilarity=" + (embedding.similarity(mention.completeString.replace(" ", "_"), antecedent.completeString.replace(" ", "_")) > 0), mention, antecedent)
      //      println("cosine similarity: " + embedding.similarity(mention.completeString.replace(" ", "_"), antecedent.completeString.replace(" ", "_")))
      //      strings += buildName("cosineSimilarityFirst=" + embedding.similarity(mention.firstWord, antecedent.firstWord), mention, antecedent)

      //      strings += buildName("cosineSimilarityPrev=" + embedding.similarity(mention.previousWord, antecedent.previousWord), mention, antecedent)
      strings += buildName("eDistHead=" + calculate_euclidian_distance(mention.completeString.replace(" ", "_"), antecedent.completeString.replace(" ", "_")), mention, antecedent)


      strings += buildName("haDistHead=" + calculate_hamming_distance_analog(mention.completeString.replace(" ", "_"), antecedent.completeString.replace(" ", "_"),.001), mention, antecedent)
      strings += "haDistHead=" + calculate_hamming_distance_analog(mention.completeString.replace(" ", "_"), antecedent.completeString.replace(" ", "_"),.001)

      //      println("euclidian distance is:" + calculate_euclidian_distance(mention.semanticHead, antecedent.semanticHead))
      //      strings += buildName("eDistFirst=" + calculate_euclidian_distance(mention.firstWord, antecedent.firstWord), mention, antecedent)
      //      strings += buildName("eDistLast=" + calculate_euclidian_distance(mention.lastWord, antecedent.lastWord), mention, antecedent)

      //      }
    }
    strings.toList

  }

  def calculate_euclidian_distance(mention: String, antecedent: String): Double = {
      val mVec = embedding.getWordVector(mention).toList
      val antVec = embedding.getWordVector(antecedent).toList
      val dist = math.sqrt(mVec.zip(antVec).foldLeft(0.0)((a: Double, b: (Double, Double)) => math.pow((b._1 - b._2), 2.0)))
      if (dist < .005) return 1
      if (dist < .009) return 2
      if (dist < .02) return 3
      return 4
    }

    def calculate_hamming_distance_analog(mention: String, antecedent: String, thresh: Double): Int = {
      val mVec = embedding.getWordVector(mention).toList
      val antVec = embedding.getWordVector(antecedent).toList
      val dist = (mVec.zip(antVec)).foldLeft(0)((a: Int, b: (Double, Double)) => {
//        change += math.abs(b._1 - b._2);
//        count += 1;
        a + (if (math.abs(b._1 - b._2) > thresh*3) 3
          else if (math.abs(b._1 - b._2) > thresh*2) 2
        else if (math.abs(b._1 - b._2) > thresh) 1
        else 0)
      })
      return dist
    }

}





