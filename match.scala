import java.lang.Character._
import com.codahale.jerkson._
import scala.io._
import scala.math._

/* Warning: this script currently assumes that the JSON files exist, that the
   JSON is valid, and that every JSON object in them contains the
   "manufacturer" key.  I'm not hugely familiar with the error handling
    mechanisms in Scala, so I'm going to leave this for now and focus on the
    algorithm.
*/

object Match extends App {
	type StringMap = Map[String,String]
	type DictMap = Map[String, List[StringMap]]

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

	def bucketOnMfg(l: Iterable[StringMap], mfgs: Iterable[String]) = {
		// Pre-group items to reduce number of expensive fuzzy matches
		(l.groupBy { x =>
			(if(x("manufacturer") == "") {
				x("title")
			} else {
				x("manufacturer")
			}).toLowerCase
		}).foldLeft(List[(String, Iterable[StringMap])]()) { case (m, (k, group)) =>
			// Find normalized manufacturer for this group
			mfgs.find({ mfg =>
				// Get length of smallest mfg string and common substring
				// If the common substring length is >= the length of
				// the smallest mfg string, then this mfg is a good match
				lcsubs(mfg,k).length >= min(mfg.length, k.length)
			}) match {
				case Some(mfg) => (mfg,group)::m
				case _ => m
			}
		} groupBy { case (k,group) => k} mapValues { x =>
			(x.map { _ _2 }).flatten
		}
	}

	def cleanString(s: String): String = {
		def nspaces(l: Int, xs:List[Char]): List[Char] = {
			if(l < 1) {
				xs
			} else {
				nspaces(l-1, ' ' :: xs)
			}
		}

		def tokens(str: Seq[Char], xs: List[String]): List[String] = {
			def continueTokens(str: Seq[Char], xs: List[String]) = {
				val (token, rest) = str.span(isLetterOrDigit)
				val recurse = tokens(rest, xs)
				(token.mkString) :: (if(token.last.isDigit) {
					" " :: recurse
				} else {
					recurse
				})
			}

			str.span({ x => ! (x.isLetterOrDigit) }) match {
				case (_, Nil) => xs
				case (s, cleanStr) =>
					if(s.length < 2) {
						continueTokens(cleanStr, xs)
					} else {
						nspaces(s.length,List()).mkString :: continueTokens(cleanStr, xs)
					}
			}
		}

		def addSpaces(str: Seq[Char], xs: List[Seq[Char]]): List[Seq[Char]] = (str, xs) match {
			case (Nil, xs) => xs
			case (str, xs) =>
				val (noNum, noRest) = str.span({ x => ! (x.isDigit)})
				noRest.span(isDigit) match {
					case (Nil, rest) => noNum.mkString :: addSpaces(rest, xs)
					case (num, rest) =>
						val recurs = addSpaces(rest, xs)
						if(recurs.length > 0 && recurs.head.head == ' ') {
							noNum :: num :: recurs
						} else {
							noNum :: num :: (' ' :: Nil) :: recurs
						}
				}
		}

		addSpaces(tokens(s.toLowerCase, Nil).flatten, Nil).flatten.mkString
	}

	def matchAllMfg(
		pByMfg: Map[String, Iterable[(String,String,String,String)]],
		lByMfg: DictMap
	): DictMap = {
		def matchOneMfg(
			mfgProducts: List[(String,String,String,String)],
			listing: StringMap
		): String = {
			val cleanTitle = cleanString(listing("title"))

			def stripPrefixes(s: String) =
				s.stripPrefix("dslr").stripPrefix("dsc").stripPrefix("slt")

			def continueWithStrip(xs: List[(String,String,String,String)]) = {
				xs.filter { case (_,_,query,_) =>
					// Filter by model, strip common prefixes
					cleanTitle contains stripPrefixes(query)
				} match {
					case Nil => "" // No product matched
					case (_,name,_,_)::Nil => name
					case ys => continueByFamily(ys)
				}
			}

			def continueByFamily(xs: List[(String,String,String,String)]) = {
				xs.filter { case (_,_,_,query) =>
					// Filter by family
					query.length > 0 && (cleanTitle contains query)
				} match {
					case Nil => "" // No product matched
					case (_,name,_,_)::Nil => name
					case ys => // Still more than one match.  Take the longest model
						val (_,name,_,_) = ys.maxBy { case (_,_,a,_) =>
							a.length
						}
						name
				}
			}

			mfgProducts.filter { case (_,_,query,_) =>
				// Filter by model
				cleanTitle contains query
			} match {
				case Nil => continueWithStrip(mfgProducts)
				case (_,name,_,_)::Nil => name
				case xs => continueByFamily(xs)
			}
		}

		lByMfg.foldRight(Map[String, List[StringMap]]()) { case ((mfg, grp), m) =>
			// Build up a map from product_name to list of listings
			m ++ (grp.groupBy { x => matchOneMfg(pByMfg(mfg).toList, x) })
		}
	}

	val listings = Source.fromFile("listings.txt")
	val products = Source.fromFile("products.txt")
	try {
		val l = parseJsonObjects(listings)
		val p = parseJsonObjects(products)
		val pByMfg = (p.map { x =>
			val y = x.withDefaultValue("")
			(y("manufacturer").toLowerCase, y("product_name"),
				cleanString(x("model")), cleanString(y("family")))
		}).toIterable.groupBy { _ _1 }
		for((product,listings) <- matchAllMfg(pByMfg, bucketOnMfg(l.toIterable, pByMfg.keys))) {
			if(product != "") {
				println(Json.generate(List(("product_name", product),("listings",listings)).toMap))
			}
		}
	} finally {
		listings.close
		products.close
	}
}
