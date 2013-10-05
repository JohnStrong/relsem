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
		2 -> "free music computer",
		3 -> "scala programming music technology"
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

			def tFRS:Map[Int, (Double, Double)]  = {
				for(document <- collection) yield (document._1, 
					termFrequency(term, document._2))
			}
			
			val tfrs = tFRS
			
			val ttf = tfrs.values.map(x => x._1 )
			val ttr = tfrs.values.map(x => x._2 )

			val idf = inverseDocumentFrequency(ttf)

			normalize(for(t <-
				(ttf, ttr).zipped.map(_ + _)) yield (t * idf))

		})

		// dotprod => doc1 * doc2 * doc3 * .... * docn
		// calculate similarity
		
	}

	// compute the similarity between 2 documents
	def sim(a: Iterable[Double], b:Iterable[Double]):Iterable[Double] = {
		(a,b).zipped.map(_ * _)
	}

	// returns the idf for each term in the query (helps to reduce the common word problem)
	private def inverseDocumentFrequency(ttf:Iterable[Double]):Double = {
		log((collection.size + 1)/(
			ttf.filter( x => x > 0.0 ).size + 1.0))/log(2)
	}

	// (TermFrequency, TermToDocument Relatedness)
	private def termFrequency(qTerm:String, document:String):Tuple2[Double, Double] = {
		
		// term frequency by size of the result matching queried term
		var terms = document.split(" ")
		var tf = terms.filter(term => term == qTerm).size

		// find the queried term id
		val sourceQTerm = sources.find(s => s.term == qTerm).getOrElse(
			throw new Exception("failed to find matching source"))

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

	/**	relatedness is measured by multiplying the base value (.5) by 
	*	(1 / the depth at which is term was found) 
	**/
	private def relatedness(queryTermId:Int, docTermId:Int, depth:Int):Double = {
		
		var found = false

		// fix later...	
		targets.filter(target => target.source == queryTermId).map( term => term match {
			case t:Target if t.target == docTermId => found = true; 0.5 * (1.0/depth)
			case t:Target if t.target != docTermId && found == false => relatedness(t.target, docTermId, depth = depth + 1)
			case _ => 0
		}).foldRight(0.0)(_ + _)
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