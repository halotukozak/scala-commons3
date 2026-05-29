package com.avsystem.commons
package mongo.text

/**
 * Language supported by MongoDB text search.
 *
 * @param code
 *   ISO 639-1, ISO 639-3 or RLP code used by Mongo \$text operator
 * @see
 *   [[https://docs.mongodb.com/manual/reference/text-search-languages/#text-search-languages]]
 */
enum TextSearchLanguage(val code: String) {

  /** Uses simple tokenization with no list of stop words and no stemming. */
  case None extends TextSearchLanguage("none")
  case Danish extends TextSearchLanguage("da")
  case Dutch extends TextSearchLanguage("nl")
  case English extends TextSearchLanguage("en")
  case Finnish extends TextSearchLanguage("fi")
  case French extends TextSearchLanguage("fr")
  case German extends TextSearchLanguage("de")
  case Hungarian extends TextSearchLanguage("hu")
  case Italian extends TextSearchLanguage("it")
  case Norwegian extends TextSearchLanguage("nb")
  case Portuguese extends TextSearchLanguage("pt")
  case Romanian extends TextSearchLanguage("ro")
  case Russian extends TextSearchLanguage("ru")
  case Spanish extends TextSearchLanguage("es")
  case Swedish extends TextSearchLanguage("sv")
  case Turkish extends TextSearchLanguage("tr")
  case Arabic extends TextSearchLanguage("ara")
  case Dari extends TextSearchLanguage("prs")
  case IranianPersian extends TextSearchLanguage("pes")
  case Urdu extends TextSearchLanguage("urd")
  case SimplifiedChinese extends TextSearchLanguage("zhs")
  case TraditionalChinese extends TextSearchLanguage("zht")
}
object TextSearchLanguage {
  val Hans: TextSearchLanguage = SimplifiedChinese
  val Hant: TextSearchLanguage = TraditionalChinese
}
