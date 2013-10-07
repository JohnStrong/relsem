package relsem.master

//case classes for relatedness feature
case class Source(id:Int, term:String)
case class Target(source:Int, target:Int)

/**
*	for removing stop words and do stemming on terms
**/ 
object Grammer {

	private val VOWEL_CUTOFF = 3

	private lazy val VOWELS = List("a", "e", "i", "o", "u")

	private lazy val STOP_WORDS = List(
		"the", "be", "to", "of", "and", "a", "in",
		"that", "have", "i", "it", "for", "not", "on",
		"with", "he", "as", "you", "do", "at"
	)


	def removeStopwords(term:String):Boolean = 
		STOP_WORDS.contains(term.toLowerCase)

	def termStemming(term:String):String = {
		
		var vowelCount = 0; var slicePoint = 0
		val lList = term.split("")

		lList.zipWithIndex.foreach{ 
			case (l, ith) if vowelCount < VOWEL_CUTOFF => {
				if(VOWELS.contains(l)) {
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
}

class RelSem {
	
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

	// res => { q1(document_results), ....., qn(document_results) }
	def calculate(query: List[String]):List[Iterable[Double]] = {
		
		query.filterNot(term => {

			Grammer.removeStopwords(term)

		}).map(term => {

			Grammer.termStemming(term)

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

	/** 
	*	find the queried term id
	* 	filter out every term in the document matching the query term
	* 	map each resulting element against relatednes
	**/	
	private def termFrequency(qTerm:String, document:String):Tuple2[Double, Double] = {
		
		var terms = document.split(" ").map(term => Grammer.termStemming(term))
		var tf = terms.filter(term => term == qTerm).size

		val sourceQTerm = sources.find(s => s.term == qTerm).getOrElse(
			throw new Exception("failed to find matching source\n\n"))

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
			case t:Target if t.target != docTermId && found == false => 
				relatedness(t.target, docTermId, depth = depth + 1)
			case _ => 0
		}).foldRight(0.0)(_ + _)
	}

	// normalize the document to get the distance between the vectors
	private def normalize(doc:Iterable[Double]):Iterable[Double] = {
		sqrt(doc.map(x => x * x).sum) match {
			case t:Double if t > 0 => doc.map(x => x/t)
			case _ =>	doc.map(x => 0.0)
		}
	}
}

// quick test
// runner singleton
object Test {
	
	val relsem = new RelSem()

	def main(args:Array[String]) {

		// calculate under stemming of the model
		println(calUnderStemming(
			List(
				"reactive", 
				"reacting", 
				"reactor", 
				"react",
				"reaction", 
				"reacts"
			), "react"))

		// calculate over stemming of the model
		println(calOverStemming(
			List(
				"programming",
				"program",
				"programme"
			), "programm"))

		// rate documents for query
		for(result <- doVectorSpaceModel("computer programming in scala")) {
			println
			println("Document results: " + result) 
		}
	}

	def calUnderStemming(unStemmed:List[String], desired:String):Double = {
		1 - ((unStemmed.map( _ match {
			case term:String if Grammer.termStemming(term) == desired => 1
			case _ => 0
		}).foldRight(0.0)(_ + _))/unStemmed.size)
	}

	def calOverStemming(unStemmed:List[String], undesired:String):Double = {
		1 - ((unStemmed.map( _ match {
			case term:String if Grammer.termStemming(term) != undesired => 1
			case _ => 0
		}).foldRight(0.0)(_ + _))/unStemmed.size)
	}

	def doVectorSpaceModel(query:String):List[Iterable[Double]] = {
	 	relsem.calculate(query.split(" ").toList)
	}
}