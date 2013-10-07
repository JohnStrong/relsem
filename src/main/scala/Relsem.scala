package relsem.master

//case classes for relatedness feature
case class Source(id:Int, term:String)
case class Target(source:Int, target:Int)

class Grammer(letter:String) {

	private lazy val vowel = List("a", "e", "i", "o", "u")

	def endWithDup():Boolean => //todo
	def isVowel():Boolean = vowel.contains(letter)
}
// INCLUDES: stemming
// INCLUDES: term-relatedness evaluator

// TODO: implement stop word removal
class RelSem() {
	
	import math._

	//collection of documents we already have stored
	private lazy val collection = Map(
		1 -> "computer technology computer",
		2 -> "scala operating system computer",
		3 -> "scala programming music technology"
	)

	// list of terms we know about
	private lazy val sources = List(
		Source(1, "comput"),
		Source(2, "technol"),
		Source(3, "oper"),
		Source(4, "system"),
		Source(5, "scala"),
		Source(6, "programm")
	)

	// links between terms (relatedness)
	private lazy val targets = List(
		Target(1, 2),
		Target(1, 4),
		Target(1, 6),
		Target(2, 4),
		Target(4, 3),
		Target(6, 5)
	)

	// common words that can be removed from the query as they do not provide value
	private lazy val stopwords = List(
		"and", "or","the",
		"an", "in", "for",
		"do", "what", "be",
		"had", "did"
	)

	// res => { q1(document_results), ....., qn(document_results) }
	def calculate(query: List[String]):List[Iterable[Double]] = {
	
		query.filterNot(term => {

			stopwords.contains(term.toLowerCase)

		}).map(term => {

			stem(term)

		}).map(term => {

			// measure the relatedness of a query term to each document
			val tfrs = termFreqRelatedness(term)
			val totalTf = tfrs.values.map(x => x._1 )
			val totalTr = tfrs.values.map(x => x._2 )
			val idf = inverseDocFrequency(totalTf)

			normalize(for(t <-
				(totalTf, totalTr).zipped.map(_ + _)
			) yield (t * idf))
		})		
	}

	private def termFreqRelatedness(term:String):Map[Int, (Double, Double)]  = {
		for(document <- collection) yield (document._1, 
			termFrequency(term, document._2))
	}

	// returns the idf for each term in the query (helps to reduce the common word problem)
	private def inverseDocFrequency(ttf:Iterable[Double]):Double = {
		log((collection.size + 1)/(
			ttf.filter( x => x > 0.0 ).size + 1.0))/log(2)
	}

	private def stem(term:String):String = {

		implicit def gram(letter:String) = new Grammer(letter)

		val lList = term.split("")
		var vowelCount = 0; var slicePoint = 0

		lList.zipWithIndex.foreach{ 
			case (l, ith) if vowelCount < 3 => {
				if(l isVowel) {
					vowelCount += 1; slicePoint = ith
				}
			}
			case _ => // do nothing
		}

		vowelCount match {
			case vi:Int if vi == 3 => lList.slice(0, slicePoint).reduce(_ + _)
			case _ => term
		}
	}

	// (TermFrequency, TermToDocument Relatedness)
	private def termFrequency(qTerm:String, document:String):Tuple2[Double, Double] = {
		
		// stemming on documents
		var terms = document.split(" ").map(term => stem(term))
		var tf = terms.filter(term => term == qTerm).size

		// find the queried term id
		val sourceQTerm = sources.find(s => s.term == qTerm).getOrElse(
			throw new Exception("failed to find matching source\n\n"))

		// filter out every term in the document matching the query term
		// map each resulting element against relatednes
		val termRelTotal = terms.filterNot(term => term == qTerm).map(term => {
			sources.find(source => source.term == term) match {
				case Some(s) => relatedness(sourceQTerm.id, s.id, 1)
				case _ => 0
			}
		}).foldRight(0.0)(_ + _)

		(tf, termRelTotal)
	}

	//relatedness measured by multiplying the base value (.5) by (1 / the depth at which is term was found)
	private def relatedness(queryTermId:Int, docTermId:Int, depth:Int):Double = {
		
		// refractor at some stage
		var found = false

		targets.filter(target => target.source == queryTermId).map( term => term match {
			case t:Target if t.target == docTermId => found = true; 0.5 * (1.0/depth)
			case t:Target if t.target != docTermId && found == false => relatedness(t.target, docTermId, depth = depth + 1)
			case _ => 0
		}).foldRight(0.0)(_ + _)
	}

	// normalize the document to get the distance between the vectors
	private def normalize(doc:Iterable[Double]):Iterable[Double] = {
		sqrt(doc.map(x => x * x).sum) match {
			case t:Double if t > 0 => doc.map(x => x/total)
			case _ =>	doc.map(x => 0.0)
		}
	}
}

object Test extends App {
	
	val results = new RelSem().calculate(
		List("computer", "programming", "in", "scala")
	)

	for(result <- results){
		println
		println("Document results: " + result) 
	}
}