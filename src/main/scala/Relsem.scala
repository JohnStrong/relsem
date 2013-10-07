package relsem.master

/**	
*				[[	EXERCISE 1  ]]
*
*	for removing stop words and do stemming on terms
*	
**/ 
object Grammer {

	private lazy val VOWELS = List("a", "e", "i", "o", "u")

	private lazy val STOP_WORDS = List(
		"the", "be", 
		"to", "of", 
		"and", "a", 
		"in", "that", 
		"have", "i", 
		"it", "for", 
		"not", "on",
		"with", "he", 
		"as", "you", 
		"do", "at"
	)

	private lazy val PUNCUATION = List(
		".", ",", 
		";", ":", 
		"(", ")",
		"!", "?"
	)


	def isStopword(term:String):Boolean = 
		STOP_WORDS.contains(term.toLowerCase)

	def removePunctuation(term:String):String = {
		term.split("").filterNot(l => {
			PUNCUATION.contains(l)
		}).foldRight("")(_ + _)
	}

	def termStemming(term:String):String = {
		
		val VOWEL_CUTOFF = 3

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

//case classes for relatedness feature
case class Source(id:Int, term:String)
case class Target(source:Int, target:Int)

/**	
*				[[	EXERCISE 2 ]]
*
*	for removing stop words and do stemming on terms
*	
**/ 
object RelSem {
	
	import math._

	//collection of documents we already have stored
	private lazy val collection = Map(
		1 ->   "hurricane isis",
		2 ->   "mozambique joey sister",
		3 ->   "isis hurricane joey",
		4 ->   "diamond bay mozambique"
	)

	// list of terms we know about
	private lazy val sources = List(
		Source(1, "hurric"),
		Source(2, "isis"),
		Source(3, "mozamb"),
		Source(4, "joey"),
		Source(5, "sister"),
		Source(6, "diamo"),
		Source(8, "shelter"),
		Source(9, "bay")
	)

	// links between terms (relatedness)
	private lazy val targets = List(
		Target(1, 4),
		Target(1, 2),
		Target(5, 1),
		Target(4, 8)
	)

	// res => { q1(document_results), ....., qn(document_results) }
	def calculate(query: List[String]):List[Iterable[Double]] = {
		
		query.filterNot(term => {

			Grammer.isStopword(term)

		}).map(term => {

			Grammer.removePunctuation(term)

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
		
		var dTerms = document.split(" ").map(term => {

			Grammer.removePunctuation(term)

		}).map(term => {

			Grammer.termStemming(term)
		})

		var tf = dTerms.filter(term => term == qTerm).size
		
		val sourceQTerm = sources.find(s => s.term == qTerm).getOrElse(
			throw new Exception("failed to find matching source\n\n"))

		val termRelTotal = dTerms.filterNot(term => term == qTerm).map(term => {
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
	
	def main(args:Array[String]) {

		// EXERCISE 1:
		// 
		// calculate under stemming of the model  
		// calculate level of compression
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

		// EXERCISE 2:
		//
		// calculate document relatedness
		for(result <- doVectorSpaceModel("hurricane joey in mozambique")) {
			println("\nDocument results: " + result + "\n=============================") 
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
	 	RelSem.calculate(query.split(" ").toList)
	}
}