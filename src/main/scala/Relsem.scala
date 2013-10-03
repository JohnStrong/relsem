package relsem.master

class RelSem(query: List[String], collection:Map[Int, String]) {
	
	import math._

	def calculate = {

		//"computer", "technology", "bad"
		val vecs = query map(term => {

			val tfs = for(document <- collection) yield (document._1, termFrequency(term, document._2))
			val idf = inverseDocumentFrequency(tfs)
			val tfidf = for(tf <- tfs.values) yield tf * idf

			normalize(tfidf.toList)

		})

		val simulaity = for(List(a, b) <- vecs.sliding(2)) yield sim(a,b)
		similarity foreach( println(_) )
		
	}

	// compute the similarity between 2 documents
	def sim(a: List[Double], b:List[Double]):List[Double] = {
		(a,b).zipped.map(_ * _)
	}

	// takes a map of tfs per document and returns the idf of the collection
	private def inverseDocumentFrequency(tfs:Map[Int,Int]):Double = {
		
		val occurences = tfs.values.filter {
			case x:Int if x > 0 => true
			case _ => false
		}

		log((collection.size + 1)/(occurences.size + 1))
	}

	// fix this unhealty thing....
	private def termFrequency(t:String, d:String):Int = {
		var f = 0

		d.split(" ").toList foreach(w => {
			if(w.toLowerCase == t.toLowerCase)
				f = f + 1
		})

		f
	}

	// normalize the document to get the distance between the vectors
	private def normalize(doc:List[Double]):List[Double] = {
		
		val total = sqrt(doc.map(x => x * x).sum)

		doc.map(x => {
			if( x > 0)
				x/total
			else
				0
		})
	}	
}

object Test extends App {

	val query = List("computer", "technology", "bad")

	val collection = Map(
		1 -> "computer science information technology computer",
		2 -> "Fox News science misleading",
		3 -> "Walking Dead AMC show",
		4 -> "download free music computer wrong",
		5 -> "scala programming tutorial"
	)

	new RelSem(query, collection).calculate
}