package org.mdpeg.util

class StringSplitToTuple(s: String) {
  def splitToTuple(pattern: String): (String, String) = {
    s.split(pattern) match {
      case Array(str1, str2) => (str1, str2)
      case Array(str1) => (str1, "")
      case otherwise => sys.error("Split array contains too many elements")
    }
  }
}
