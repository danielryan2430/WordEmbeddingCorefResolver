package resolver.parser.document

import java.io.File


import org.deeplearning4j.word2vec.Word2Vec
import org.deeplearning4j.word2vec.inputsanitation.InputHomogenization
import org.deeplearning4j.word2vec.sentenceiterator.{SentencePreProcessor, CollectionSentenceIterator}

import scala.collection.mutable.ListBuffer

object Parser {

     def parse(file:File):(List[Document], Seq[String]) = {
       println("Parse begin: "+file.getName)
       val sentences = CoNLLParser.parse(file)

        val a:Seq[String] = sentences.foldLeft(Seq[String]())(( b:Seq[String], container:ConLLSentenceContainer) =>
            b++container.sentenceList.map(
              (
                s:CoNLLSentence) => {
//                println(s.getEmbeddingSentence())
                s.getEmbeddingSentence()})

        )
       println("parsed the files, now we will convert to mentions")
       val hold = new ListBuffer[Document]
       for(sentence <- sentences) {
         val features = SentenceToMentionConverter.extract(sentence)
         hold+=new Document(features.toArray, sentence.id)
       }
       println("Finished parsing document name: "+file.getName)
       (hold.toList, a)
     }




}
