package relsem.master

/** any term can link to any other term
* the more directly terms are linked, the better their similarity
*/
case class Source(id:Int, term:String)
case class Target(source:Int, target:Int)

class RelSem(query: List[String]) {
	
	import math._

	//collection of documents we already have stored
	private lazy val collection = Map(
		1 -> "computer technology computer",
		2 -> "free music on computer",
		3 -> "scala programming"
	)

	// list of terms we know about
	private lazy val sources = List(
		Source(1, "computer"),
		Source(2, "technology"),
		Source(3, "free"),
		Source(4, "music"),
		Source(5, "scala"),
		Source(6, "programming")
	)

	// links between terms (relatedness)
	private lazy val targets = List(
		Target(1, 2),
		Target(1, 6),
		Target(2, 4),
		Target(4, 3),
		Target(6, 5)
	)

	def calculate = {

		val normVecs = query map(term => {

			val tfs = for(document <- collection) yield (document._1, 
				termFrequency(term, document._2))
			
			val idf = inverseDocumentFrequency(tfs.values)
			val tfidf = for(tf <- tfs.values) yield tf * idf
			
			normalize(tfidf)

		})

		// dotprod => doc1 * doc2 * doc3 * .... * docn
		//val similarity = for(List(a, b) <- normVecs.sliding(2)) yield sim(a,b)
		
	}

	// compute the similarity between 2 documents
	def sim(a: List[Double], b:List[Double]):List[Double] = {
		(a,b).zipped.map(_ * _)
	}

	// returns the idf for each term in the query (helps to reduce the common word problem)
	private def inverseDocumentFrequency(tv:Iterable[Double]):Double = {

		val occurences = tv.filter( x => x > 0.0 )
		log((collection.size + 1)/(occurences.size + 1.0))/log(2)
	}

	// termFreq * termRelatedness => TermFrequencyAndRelatednessTotal
	private def termFrequency(qTerm:String, document:String):Double = {
		
		// term frequency by size of the result matching queried term
		var terms = document.split(" ")
		var tf = terms.filter(term => term == qTerm).size

		// find the queried term id
		val tId = sources.find(s => s.term == qTerm) match {
			case Some(t) => Some(t.id)
			case _ => None
		}

		// filter out every term in the document matching the query term
		// map each resulting element against relatednes
		val relatednessTotal = terms.filterNot(term => term == qTerm).map(term => {
			sources.find(s => s.term == term) match {
				case Some(s) => relatedness(tId, s.id)
				case _ => 0.0
			}
		}).foldRight(0.0)(_ + _)

		// query term frequency + total relatedness of document to query term
		tf + relatednessTotal
	}

	// returns a value of relatedness each term has with the queried term
	private def relatedness(queryTermId:Option[Int], docTermId:Int):Double = {
		// TODO: implement relatedness method
		
		//println
		//println(queryTermId)
		//println(docTermId)

		0.1
	}

	// normalize the document to get the distance between the vectors
	private def normalize(doc:Iterable[Double]):Iterable[Double] = {
		
		val total = sqrt(doc.map(x => x * x).sum)

		if(total > 0.0)
			doc.map(x => x/total)
		else
			doc.map(x => 0.0)
	}	
}

object Test extends App {
	val query = List("computer", "programming", "scala")
	new RelSem(query).calculate
}