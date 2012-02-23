import com.codahale.jerkson._
import scala.io._
import scala.math._

/* Warning: this script currently assumes that the JSON files exist, that the
   JSON is valid, and that every JSON object in them contains the
   "manufacturer" key.  I'm not hugely familiar with the error handling
    mechanisms in Scala, so I'm going to leave this for now and focus on the
    algorithm.
*/

object Main extends App {
	type StringMap = Map[String,String]

	def lcsubs(xs: String, ys: String) = {
		def csubs(xs: String, ys: String) = {
			(xs.zip(ys)).scanRight(List[Char]()) { case ((x,y),acc) =>
				if(x == y) {
					x::acc
				} else {
					List[Char]()
				}
			}
		}

		((xs.tails).flatMap(csubs(ys, _)) ++
			(ys.tails.drop(1)).flatMap(csubs(xs, _))).maxBy { _.length }
	}

	def parseJsonObjects(fh: Source) =
		fh.getLines.map(Json.parse[Map[String,String]])

	def bucketOnMfg(l: Iterable[StringMap], p: Iterable[StringMap]) = {
		val mfgs = (p.map({ _("manufacturer") })).toList.distinct
		// Pre-group items to reduce number of expensive fuzzy matches
		(l.groupBy { lst => lst("manufacturer") }).
			foldLeft(List[(String, Iterable[StringMap])]()) { case (m, (k, group)) =>
			// Find normalized manufacturer for this group
			mfgs.find({ mfg =>
				// Get length of smallest mfg string and common substring
				// If the common substring length is >= the length of
				// the smallest mfg string, then this mfg is a good match
				lcsubs(mfg,k).length >= min(mfg.length, k.length)
			}) match {
				case Some("") => m // Ignore empty manufacturer strings
				case Some(mfg) => (mfg,group)::m
				case _ => m
			}
		} groupBy { case (k,group) => k} mapValues { x =>
			(x.map { _ _2 }).flatten
		}
	}

	val listings = Source.fromFile("listings.txt")
	val products = Source.fromFile("products.txt")
	try {
		val l = parseJsonObjects(listings)
		val p = parseJsonObjects(products)
		println(bucketOnMfg(l.toIterable,p.toIterable))
	} finally {
		listings.close
		products.close
	}
}
